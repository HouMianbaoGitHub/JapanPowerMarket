"""构建并求解日本 9 区域 PyPSA 经济调度模型。"""
from __future__ import annotations

from dataclasses import dataclass, field
import logging
import warnings

import numpy as np
import pandas as pd
import pypsa

from data import (AREAS, CAPACITY, TECHS, INTERCONNECTORS, EFFICIENCY,
                  EMISSION_T_PER_MWH_TH, NUCLEAR_MC_JPY_MWH)

logging.getLogger("pypsa").setLevel(logging.WARNING)
logging.getLogger("linopy").setLevel(logging.WARNING)
warnings.filterwarnings("ignore", category=FutureWarning)
pypsa.options.api.legacy_string_dtype = True

SEASON_START = {"夏季 (8月)": "2025-08-04", "冬季 (1月)": "2025-01-13",
                "春季 (5月)": "2025-05-12", "秋季 (10月)": "2025-10-13"}

# 24 小时负荷形状（相对峰值）
DEMAND_SHAPE = {
    "summer": [.62, .59, .57, .56, .56, .58, .64, .72, .82, .90, .95, .98,
               .97, .99, 1.0, .99, .96, .92, .88, .84, .79, .74, .69, .65],
    "winter": [.70, .67, .65, .64, .65, .69, .78, .90, .96, .95, .92, .90,
               .88, .88, .88, .89, .92, .98, 1.0, .98, .94, .88, .81, .75],
    "autumn": [.58, .56, .55, .54, .55, .58, .65, .73, .78, .80, .80, .80,
               .78, .79, .79, .80, .82, .86, .87, .84, .79, .73, .67, .62],
    "spring": [.55, .53, .52, .51, .52, .55, .61, .66, .66, .64, .62, .61,
               .60, .61, .63, .67, .72, .78, .80, .78, .74, .69, .63, .58],
}
SEASON_KEY = {"夏季 (8月)": "summer", "冬季 (1月)": "winter",
              "春季 (5月)": "spring", "秋季 (10月)": "autumn"}


@dataclass
class Scenario:
    season: str = "夏季 (8月)"
    days: int = 7
    lng_usd_mmbtu: float = 12.0
    coal_usd_t: float = 130.0
    oil_usd_bbl: float = 80.0
    fx_jpy_usd: float = 150.0
    carbon_jpy_t: float = 0.0
    demand_scale: float = 1.0
    solar_scale: float = 1.0
    wind_scale: float = 1.0
    nuclear_avail: dict = field(default_factory=lambda: {a: 1.0 for a in AREAS})
    interconnector_scale: float = 1.0
    fc_gw: float = 2.1  # 东西变频站容量
    thermal_must_run: float = 0.15  # 煤电/LNG 最低出力比例（系统稳定所需）
    voll_jpy_kwh: float = 300.0
    seed: int = 42


def thermal_mc(s: Scenario) -> dict[str, float]:
    """燃料价格 -> 电厂边际成本 JPY/MWh_e（含碳价）。"""
    fuel_jpy_mwh_th = {
        "lng": s.lng_usd_mmbtu * 3.412 * s.fx_jpy_usd,
        "coal": s.coal_usd_t / 6.98 * s.fx_jpy_usd,
        "oil": s.oil_usd_bbl / 1.70 * s.fx_jpy_usd,
    }
    return {k: (fuel_jpy_mwh_th[k] + EMISSION_T_PER_MWH_TH[k] * s.carbon_jpy_t)
            / EFFICIENCY[k] for k in fuel_jpy_mwh_th}


def _profiles(s: Scenario, snapshots: pd.DatetimeIndex):
    rng = np.random.default_rng(s.seed)
    key = SEASON_KEY[s.season]
    hours = snapshots.hour.values
    weekend = np.where(snapshots.dayofweek.values >= 5, 0.88, 1.0)
    base = np.array(DEMAND_SHAPE[key])[hours] * weekend

    n_days = len(snapshots) // 24
    solar_peak = {"summer": .68, "winter": .55, "spring": .75, "autumn": .62}[key]
    sun_hours = {"summer": (5, 19), "winter": (7, 17), "spring": (5, 18), "autumn": (6, 17)}[key]
    rise, set_ = sun_hours
    bell = np.clip(np.sin(np.pi * (hours - rise) / (set_ - rise)), 0, None)
    bell[(hours < rise) | (hours > set_)] = 0

    demand, solar, wind = {}, {}, {}
    for a, (_, _, _, peak, _) in AREAS.items():
        noise = 1 + rng.normal(0, 0.015, len(snapshots))
        demand[a] = base * noise * peak * 1000 * s.demand_scale
        cloud = np.repeat(rng.uniform(0.35, 1.0, n_days), 24)
        snow = 0.6 if key == "winter" and a in ("Hokkaido", "Tohoku", "Hokuriku") else 1.0
        solar[a] = bell * solar_peak * cloud * snow
        mean_cf = 0.30 if a in ("Hokkaido", "Tohoku") else 0.22
        mean_cf *= {"summer": 0.7, "winter": 1.3, "spring": 1.0, "autumn": 1.0}[key]
        x, w = 0.0, np.zeros(len(snapshots))
        for t in range(len(snapshots)):
            x = 0.92 * x + rng.normal(0, 0.35)
            w[t] = mean_cf * np.exp(x * 0.6)
        wind[a] = np.clip(w, 0, 0.95)
    hydro_cf = {"summer": .45, "winter": .30, "spring": .60, "autumn": .40}[key]
    return (pd.DataFrame(demand, index=snapshots), pd.DataFrame(solar, index=snapshots),
            pd.DataFrame(wind, index=snapshots), hydro_cf)


def build_network(s: Scenario) -> pypsa.Network:
    snapshots = pd.date_range(SEASON_START[s.season], periods=24 * s.days, freq="h")
    n = pypsa.Network()
    n.set_snapshots(snapshots)
    demand, solar, wind, hydro_cf = _profiles(s, snapshots)
    mc = thermal_mc(s)
    n.add("Carrier", ["AC", "DC", *TECHS, "load_shedding", "spill"])

    for a, (cn, lon, lat, _, hz) in AREAS.items():
        n.add("Bus", a, x=lon, y=lat, carrier="AC")
        n.add("Load", f"{a} load", bus=a, p_set=demand[a])
        cap = dict(zip(TECHS, CAPACITY[a]))

        if cap["nuclear"] > 0:
            n.add("Generator", f"{a} nuclear", bus=a, carrier="nuclear",
                  p_nom=cap["nuclear"] * 1000, marginal_cost=NUCLEAR_MC_JPY_MWH,
                  p_max_pu=s.nuclear_avail.get(a, 1.0))
        # 每种火电拆成 3 个效率档，形成阶梯状供给曲线
        for tech in ("coal", "lng", "oil"):
            for i, (share, eff_adj) in enumerate([(0.3, 0.92), (0.4, 1.0), (0.3, 1.10)]):
                n.add("Generator", f"{a} {tech} {i}", bus=a, carrier=tech,
                      p_nom=cap[tech] * 1000 * share, marginal_cost=mc[tech] * eff_adj,
                      p_min_pu=s.thermal_must_run if tech in ("coal", "lng") else 0.0)
        n.add("Generator", f"{a} hydro", bus=a, carrier="hydro",
              p_nom=cap["hydro"] * 1000, p_max_pu=hydro_cf, marginal_cost=0.0)
        n.add("Generator", f"{a} solar", bus=a, carrier="solar",
              p_nom=cap["solar"] * 1000 * s.solar_scale, p_max_pu=solar[a], marginal_cost=0.0)
        n.add("Generator", f"{a} wind", bus=a, carrier="wind",
              p_nom=cap["wind"] * 1000 * s.wind_scale, p_max_pu=wind[a], marginal_cost=0.0)
        if cap["pumped"] > 0:
            n.add("StorageUnit", f"{a} pumped", bus=a, carrier="pumped",
                  p_nom=cap["pumped"] * 1000, max_hours=8, efficiency_store=0.86,
                  efficiency_dispatch=0.86, cyclic_state_of_charge=True, marginal_cost=10.0)
        n.add("Generator", f"{a} load_shedding", bus=a, carrier="load_shedding",
              p_nom=1e6, marginal_cost=s.voll_jpy_kwh * 1000)
        # 过剩电力出口（保证必开机组+核电过剩时可行），计入弃电
        n.add("Generator", f"{a} spill", bus=a, carrier="spill", p_nom=1e6,
              p_min_pu=-1.0, p_max_pu=0.0, marginal_cost=0.0)

    for b0, b1, gw, name in INTERCONNECTORS:
        cap = s.fc_gw if (b0, b1) == ("Tokyo", "Chubu") else gw * s.interconnector_scale
        n.add("Link", f"{b0}-{b1}", bus0=b0, bus1=b1, p_nom=cap * 1000,
              p_min_pu=-1.0, efficiency=1.0, marginal_cost=1.0, carrier="DC")
    return n


def solve(s: Scenario) -> dict:
    n = build_network(s)
    status, condition = n.optimize(solver_name="highs", log_to_console=False,
                                   include_objective_constant=False, progress=False)
    if status != "ok":
        raise RuntimeError(f"求解失败: {status} / {condition}")

    gen = n.generators_t.p
    carriers = n.generators.carrier
    spill = gen.loc[:, carriers == "spill"]
    gen = gen.loc[:, carriers != "spill"]
    carriers = carriers[carriers != "spill"]
    dispatch = gen.T.groupby(carriers).sum().T  # MW, by carrier
    if len(n.storage_units):
        dispatch["pumped"] = n.storage_units_t.p.sum(axis=1)

    by_area = gen.T.groupby([n.generators.bus[gen.columns], carriers]).sum().T
    store_by_area = n.storage_units_t.p.T.groupby(n.storage_units.bus).sum().T

    g = n.generators.loc[gen.columns]
    avail = (n.generators_t.p_max_pu.reindex(columns=g.index, fill_value=1.0).mul(g.p_nom))
    vre = g.carrier.isin(["solar", "wind"])
    curtail = (avail.loc[:, vre] - gen.loc[:, vre]).clip(lower=0)
    curtail_by = curtail.T.groupby([g.bus[vre], g.carrier[vre]]).sum().T
    spill_by = (-spill).rename(columns=lambda c: c.replace(" spill", ""))

    emis = sum(gen.loc[:, carriers == t].sum().sum() / EFFICIENCY[t]
               * EMISSION_T_PER_MWH_TH[t] for t in EFFICIENCY)

    prices = n.buses_t.marginal_price / 1000  # JPY/kWh
    load = n.loads_t.p_set.rename(columns=lambda c: c.replace(" load", ""))
    return {
        "network": n,
        "prices": prices,
        "flows": n.links_t.p0,  # MW, 正值 = bus0 -> bus1
        "link_cap": n.links.p_nom,
        "dispatch": dispatch,
        "by_area": by_area,
        "store_by_area": store_by_area,
        "load": load,
        "curtailment": curtail_by,
        "spill": spill_by,
        "emissions_t": emis,
        "objective_jpy": n.objective,
        "mc": thermal_mc(s),
    }


if __name__ == "__main__":
    r = solve(Scenario())
    print(r["prices"].mean().round(2))
    print("CO2 (Mt):", r["emissions_t"] / 1e6)
