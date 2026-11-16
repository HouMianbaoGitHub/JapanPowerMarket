"""日本 9 区域电力市场 PyPSA 交互界面。

运行:  streamlit run app.py
"""
from dataclasses import asdict
import io
import json

import pandas as pd
import plotly.graph_objects as go
import streamlit as st

from data import AREAS, CAPACITY, INTERCONNECTORS, TECH_COLORS, TECH_LABELS, TECHS
from model import SEASON_START, Scenario, solve

st.set_page_config(page_title="Japan PyPSA UI", page_icon="⚡", layout="wide")
CN = {a: v[0] for a, v in AREAS.items()}


@st.cache_data(show_spinner=False, max_entries=20)
def run_cached(scn_json: str) -> dict:
    r = solve(Scenario(**json.loads(scn_json)))
    r.pop("network")
    return r


# ---------------------------------------------------------------- 侧边栏
with st.sidebar:
    st.header("情景设定")
    season = st.selectbox("季节", list(SEASON_START))
    days = st.slider("模拟天数", 1, 14, 7)

    st.subheader("燃料与碳价")
    lng = st.slider("LNG (USD/MMBtu)", 4.0, 40.0, 12.0, 0.5)
    coal = st.slider("煤炭 (USD/t)", 50.0, 400.0, 130.0, 5.0)
    oil = st.slider("原油 (USD/bbl)", 40.0, 150.0, 80.0, 1.0)
    fx = st.slider("汇率 (JPY/USD)", 100.0, 180.0, 150.0, 1.0)
    carbon = st.slider("碳价 (JPY/tCO₂)", 0, 20000, 0, 500)

    st.subheader("供需")
    demand = st.slider("需求倍数", 0.7, 1.3, 1.0, 0.01)
    solar = st.slider("光伏装机倍数", 0.0, 3.0, 1.0, 0.1)
    wind = st.slider("风电装机倍数", 0.0, 5.0, 1.0, 0.1)
    must_run = st.slider("煤电/LNG 最低出力比例", 0.0, 0.5, 0.15, 0.05)
    with st.expander("核电可用率（按区域）"):
        nuc = {a: st.slider(f"{CN[a]} ({CAPACITY[a][0]:.1f} GW)", 0.0, 1.0, 1.0, 0.05,
                            key=f"nuc_{a}")
               for a in AREAS if CAPACITY[a][0] > 0}

    st.subheader("电网")
    ic = st.slider("联络线容量倍数", 0.0, 3.0, 1.0, 0.1)
    fc = st.slider("东西变频站 FC (GW)", 0.0, 10.0, 2.1, 0.1)
    voll = st.number_input("切负荷价格 VoLL (JPY/kWh)", 50.0, 1000.0, 300.0, 50.0)

    scn = Scenario(season=season, days=days, lng_usd_mmbtu=lng, coal_usd_t=coal,
                   oil_usd_bbl=oil, fx_jpy_usd=fx, carbon_jpy_t=carbon,
                   demand_scale=demand, solar_scale=solar, wind_scale=wind,
                   nuclear_avail={**{a: 1.0 for a in AREAS}, **nuc},
                   interconnector_scale=ic, fc_gw=fc, thermal_must_run=must_run,
                   voll_jpy_kwh=voll)
    st.divider()
    scn_name = st.text_input("情景名称", value=f"{season[:2]} LNG{lng:g}")
    save = st.button("保存到情景对比", use_container_width=True)

st.title("⚡ 日本 9 区域电力市场模拟 (PyPSA)")
st.caption("程式化经济调度模型：每小时最小化系统总成本，区域电价 = 节点影子价格。"
           "装机与联络线为示意值，可在 data.py 中替换为真实数据。")

with st.spinner("PyPSA 正在求解 …"):
    try:
        res = run_cached(json.dumps(asdict(scn), ensure_ascii=False))
    except Exception as e:  # noqa: BLE001
        st.error(f"求解失败：{e}")
        st.stop()

if "saved" not in st.session_state:
    st.session_state.saved = {}
if save:
    st.session_state.saved[scn_name] = {"scenario": asdict(scn), "result": res}
    st.toast(f"已保存情景「{scn_name}」")

prices, load, flows, dispatch = res["prices"], res["load"], res["flows"], res["dispatch"]
w_avg = (prices * load).sum().sum() / load.sum().sum()
shed = dispatch.get("load_shedding", pd.Series(0.0)).sum() / 1e3
curt = res["curtailment"].sum().sum() / 1e3 + res["spill"].sum().sum() / 1e3

# ---------------------------------------------------------------- 指标
c = st.columns(5)
c[0].metric("平均电价 (円/kWh)", f"{w_avg:.2f}", help="按需求加权的全国平均")
c[1].metric("系统总成本", f"{res['objective_jpy'] / 1e8:,.0f} 亿円")
c[2].metric("CO₂ 排放", f"{res['emissions_t'] / 1e6:.2f} Mt")
c[3].metric("弃风弃光+过剩", f"{curt:,.1f} GWh")
c[4].metric("切负荷", f"{shed:,.1f} GWh", delta=None if shed < 0.01 else "供电不足",
            delta_color="inverse")
mc = res["mc"]
st.caption(f"火电边际成本（中档机组）：煤 {mc['coal']/1000:.2f} ・ LNG {mc['lng']/1000:.2f} ・ "
           f"石油 {mc['oil']/1000:.2f} 円/kWh")

tabs = st.tabs(["🗾 地图与电价", "📈 价格时序", "🏭 发电结构", "🔌 联络线", "🆚 情景对比", "💾 数据下载"])

# ---------------------------------------------------------------- 地图
with tabs[0]:
    left, right = st.columns([3, 2])
    avg_p = prices.mean()
    util = flows.abs().div(res["link_cap"].replace(0, float("nan"))).mean()
    congested = (flows.abs() >= res["link_cap"] * 0.999).mean()
    fig = go.Figure()
    for b0, b1, _, name in INTERCONNECTORS:
        lk = f"{b0}-{b1}"
        u = util.get(lk, 0) if pd.notna(util.get(lk)) else 0
        net = flows[lk].mean()
        fig.add_trace(go.Scattergeo(
            lon=[AREAS[b0][1], AREAS[b1][1]], lat=[AREAS[b0][2], AREAS[b1][2]],
            mode="lines", line=dict(width=2 + 8 * u,
                                    color="#d62728" if congested[lk] > 0.3 else "#7f7f7f"),
            hoverinfo="text", showlegend=False,
            text=f"{name}<br>容量 {res['link_cap'][lk]/1000:.1f} GW<br>平均利用率 {u:.0%}"
                 f"<br>阻塞时段 {congested[lk]:.0%}<br>净流向: "
                 f"{CN[b0] + '→' + CN[b1] if net >= 0 else CN[b1] + '→' + CN[b0]} "
                 f"{abs(net)/1000:.2f} GW"))
    fig.add_trace(go.Scattergeo(
        lon=[v[1] for v in AREAS.values()], lat=[v[2] for v in AREAS.values()],
        mode="markers+text", text=[f"{CN[a]}<br>{avg_p[a]:.1f}" for a in AREAS],
        textposition="top center",
        marker=dict(size=[8 + v[3] ** 0.5 * 4 for v in AREAS.values()], color=avg_p.values,
                    colorscale="RdYlGn_r", showscale=True,
                    colorbar=dict(title="円/kWh"), line=dict(width=1, color="white")),
        hovertext=[f"{CN[a]}：平均 {avg_p[a]:.2f} 円/kWh" for a in AREAS], hoverinfo="text",
        showlegend=False))
    fig.update_geos(lonaxis_range=[128, 146], lataxis_range=[30, 46], showland=True,
                    landcolor="#eeeeee", showocean=True, oceancolor="#dfefff",
                    showcountries=False, projection_type="mercator")
    fig.update_layout(height=620, margin=dict(l=0, r=0, t=10, b=0))
    left.plotly_chart(fig, use_container_width=True)
    left.caption("线宽 = 平均利用率；红线 = 超过 30% 时段处于容量上限（阻塞）。")

    summary = pd.DataFrame({
        "区域": [CN[a] for a in AREAS],
        "平均电价": avg_p.values.round(2),
        "最高": prices.max().values.round(2),
        "最低": prices.min().values.round(2),
        "需求 GWh": (load.sum() / 1e3).values.round(0),
    })
    right.dataframe(summary, hide_index=True, use_container_width=True, height=360)
    spread = avg_p.max() - avg_p.min()
    right.info(f"区域间最大均价差 {spread:.2f} 円/kWh。价差来自联络线阻塞——"
               "试着调大联络线或 FC 容量观察价格收敛。")

# ---------------------------------------------------------------- 价格时序
with tabs[1]:
    sel = st.multiselect("区域", list(AREAS), default=["Hokkaido", "Tokyo", "Kansai", "Kyushu"],
                         format_func=lambda a: CN[a])
    fig = go.Figure()
    for a in sel:
        fig.add_trace(go.Scatter(x=prices.index, y=prices[a], name=CN[a], mode="lines"))
    fig.update_layout(height=420, yaxis_title="円/kWh", hovermode="x unified",
                      margin=dict(t=20))
    st.plotly_chart(fig, use_container_width=True)

    st.markdown("**价格持续曲线**")
    fig = go.Figure()
    for a in sel:
        s = prices[a].sort_values(ascending=False).reset_index(drop=True)
        fig.add_trace(go.Scatter(x=s.index / len(s) * 100, y=s, name=CN[a], mode="lines"))
    fig.update_layout(height=340, xaxis_title="时间占比 %", yaxis_title="円/kWh",
                      margin=dict(t=20))
    st.plotly_chart(fig, use_container_width=True)

# ---------------------------------------------------------------- 发电结构
with tabs[2]:
    area = st.selectbox("范围", ["全国", *AREAS], format_func=lambda a: CN.get(a, a))
    if area == "全国":
        d = dispatch.copy()
        dem = load.sum(axis=1)
    else:
        d = res["by_area"][area].copy()
        if area in res["store_by_area"]:
            d["pumped"] = res["store_by_area"][area]
        dem = load[area]
    order = [t for t in ["nuclear", "hydro", "coal", "lng", "oil", "wind", "solar",
                         "pumped", "load_shedding"] if t in d and d[t].abs().sum() > 1]
    fig = go.Figure()
    for t in order:
        fig.add_trace(go.Scatter(x=d.index, y=d[t].clip(lower=0) / 1000, name=TECH_LABELS[t],
                                 stackgroup="pos", line=dict(width=0),
                                 fillcolor=TECH_COLORS[t]))
    if "pumped" in d:
        fig.add_trace(go.Scatter(x=d.index, y=d["pumped"].clip(upper=0) / 1000,
                                 name="抽水（充电）", stackgroup="neg", line=dict(width=0),
                                 fillcolor="#aed6f1"))
    fig.add_trace(go.Scatter(x=dem.index, y=dem / 1000, name="需求", mode="lines",
                             line=dict(color="black", width=1.5, dash="dot")))
    fig.update_layout(height=480, yaxis_title="GW", hovermode="x unified", margin=dict(t=20))
    st.plotly_chart(fig, use_container_width=True)

    energy = d.clip(lower=0).sum() / 1e3
    energy = energy[energy > 0.1]
    fig = go.Figure(go.Pie(labels=[TECH_LABELS[t] for t in energy.index], values=energy.values,
                           marker_colors=[TECH_COLORS[t] for t in energy.index], hole=0.45))
    fig.update_layout(height=360, margin=dict(t=50, b=10), title="发电量构成 (GWh)")
    st.plotly_chart(fig, use_container_width=True)

# ---------------------------------------------------------------- 联络线
with tabs[3]:
    lk = st.selectbox("联络线", flows.columns,
                      format_func=lambda k: f"{CN[k.split('-')[0]]} ⇄ {CN[k.split('-')[1]]}")
    b0, b1 = lk.split("-")
    cap = res["link_cap"][lk] / 1000
    fig = go.Figure()
    fig.add_trace(go.Scatter(x=flows.index, y=flows[lk] / 1000, name="潮流", fill="tozeroy"))
    for y in (cap, -cap):
        fig.add_hline(y=y, line_dash="dash", line_color="red")
    fig.update_layout(height=380, yaxis_title=f"GW （正 = {CN[b0]}→{CN[b1]}）", margin=dict(t=20))
    st.plotly_chart(fig, use_container_width=True)
    tbl = pd.DataFrame({
        "联络线": [f"{CN[k.split('-')[0]]}⇄{CN[k.split('-')[1]]}" for k in flows.columns],
        "容量 GW": (res["link_cap"] / 1000).round(2).values,
        "平均利用率": (flows.abs().mean() / res["link_cap"]).round(3).values,
        "阻塞时段占比": (flows.abs() >= res["link_cap"] * 0.999).mean().round(3).values,
        "两端均价差 円/kWh": [round(abs(prices[k.split('-')[0]] - prices[k.split('-')[1]]).mean(), 2)
                         for k in flows.columns],
    })
    st.dataframe(tbl, hide_index=True, use_container_width=True)

# ---------------------------------------------------------------- 情景对比
with tabs[4]:
    saved = st.session_state.saved
    if not saved:
        st.info("在左侧调整参数后点「保存到情景对比」，即可比较多个情景的区域电价。"
                "例如：LNG 12 vs 25 美元，观察批发电价冲击在各区域的大小。")
    else:
        comp = pd.DataFrame({k: v["result"]["prices"].mean() for k, v in saved.items()})
        comp.index = [CN[a] for a in comp.index]
        fig = go.Figure([go.Bar(name=k, x=comp.index, y=comp[k]) for k in comp])
        fig.update_layout(barmode="group", height=420, yaxis_title="平均电价 円/kWh",
                          margin=dict(t=20))
        st.plotly_chart(fig, use_container_width=True)
        st.dataframe(comp.round(2), use_container_width=True)
        if st.button("清空情景"):
            st.session_state.saved = {}
            st.rerun()

# ---------------------------------------------------------------- 下载
with tabs[5]:
    st.markdown("导出结果，可直接用于 Stata 等后续计量分析（如批发→零售价格传导）。")
    long = prices.rename(columns=CN).stack().rename("price_jpy_kwh").reset_index()
    long.columns = ["datetime", "area", "price_jpy_kwh"]
    st.download_button("区域小时电价 (CSV, 长表)", long.to_csv(index=False).encode("utf-8-sig"),
                       "prices_long.csv", "text/csv")
    st.download_button("发电结构 (CSV)", dispatch.to_csv().encode("utf-8-sig"),
                       "dispatch.csv", "text/csv")
    st.download_button("联络线潮流 (CSV)", flows.to_csv().encode("utf-8-sig"),
                       "flows.csv", "text/csv")
    st.download_button("情景参数 (JSON)",
                       json.dumps(asdict(scn), ensure_ascii=False, indent=2).encode("utf-8"),
                       "scenario.json", "application/json")
