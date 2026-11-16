"""9 大区域（不含冲绳）的程式化假设数据。

所有数值都是公开资料量级上的“示意值”（GW），用于演示 PyPSA UI，
并非经过校准的官方数据。需要严肃分析时请替换为 OCCTO / 电力调查统计的真实数据。
"""

AREAS = {
    # name: (中文名, lon, lat, 峰值负荷 GW, 频率 Hz)
    "Hokkaido": ("北海道", 141.35, 43.06, 4.5, 50),
    "Tohoku":   ("东北",   140.87, 38.27, 13.5, 50),
    "Tokyo":    ("东京",   139.69, 35.69, 53.0, 50),
    "Chubu":    ("中部",   136.91, 35.18, 24.5, 60),
    "Hokuriku": ("北陆",   136.66, 36.56, 5.0, 60),
    "Kansai":   ("关西",   135.50, 34.69, 26.5, 60),
    "Chugoku":  ("中国",   132.46, 34.39, 10.5, 60),
    "Shikoku":  ("四国",   134.04, 34.34, 5.0, 60),
    "Kyushu":   ("九州",   130.40, 33.59, 16.0, 60),
}

# 装机容量 GW
CAPACITY = {
    #            nuclear coal  lng   oil  hydro  pumped solar wind
    "Hokkaido": (0.0,   2.5,  1.7,  1.5, 1.6,   0.8,   2.5,  0.9),
    "Tohoku":   (0.8,   5.5,  7.5,  1.5, 2.5,   0.5,  10.5,  3.0),
    "Tokyo":    (0.0,   7.5, 37.0,  3.0, 3.0,   9.0,  20.0,  0.6),
    "Chubu":    (0.0,   5.0, 20.0,  1.0, 2.5,   3.0,  12.5,  0.5),
    "Hokuriku": (0.0,   2.5,  0.4,  0.3, 1.9,   0.0,   1.3,  0.2),
    "Kansai":   (6.6,   1.8, 12.0,  2.0, 2.5,   4.9,   7.5,  0.2),
    "Chugoku":  (0.8,   4.5,  2.5,  1.5, 0.9,   2.1,   6.0,  0.4),
    "Shikoku":  (0.9,   1.8,  0.7,  0.5, 0.8,   0.7,   3.3,  0.3),
    "Kyushu":   (4.1,   4.0,  4.5,  1.0, 1.3,   2.3,  12.0,  0.8),
}
TECHS = ["nuclear", "coal", "lng", "oil", "hydro", "pumped", "solar", "wind"]

# 区域间联络线（GW，双向，示意值）
INTERCONNECTORS = [
    ("Hokkaido", "Tohoku", 1.2, "北本 HVDC"),
    ("Tohoku", "Tokyo", 5.7, "东北-东京"),
    ("Tokyo", "Chubu", 2.1, "50/60Hz 变频站 (FC)"),
    ("Chubu", "Hokuriku", 0.3, "南福光 BTB"),
    ("Chubu", "Kansai", 2.5, "中部-关西"),
    ("Hokuriku", "Kansai", 1.9, "北陆-关西"),
    ("Kansai", "Chugoku", 4.0, "关西-中国"),
    ("Kansai", "Shikoku", 1.4, "阿南-纪北 HVDC"),
    ("Chugoku", "Shikoku", 2.4, "本四联络线"),
    ("Chugoku", "Kyushu", 2.8, "关门联络线"),
]

# 发电效率与排放系数（每 MWh 燃料热量）
EFFICIENCY = {"coal": 0.40, "lng": 0.55, "oil": 0.38}
EMISSION_T_PER_MWH_TH = {"coal": 0.34, "lng": 0.20, "oil": 0.27}
NUCLEAR_MC_JPY_MWH = 2000.0  # 核电边际成本（燃料+可变运维）

TECH_COLORS = {
    "nuclear": "#8e44ad", "coal": "#5d4037", "lng": "#e67e22", "oil": "#c0392b",
    "hydro": "#2980b9", "pumped": "#5dade2", "solar": "#f1c40f", "wind": "#27ae60",
    "load_shedding": "#000000",
}
TECH_LABELS = {
    "nuclear": "核电", "coal": "煤电", "lng": "LNG", "oil": "石油", "hydro": "水电",
    "pumped": "抽水蓄能", "solar": "光伏", "wind": "风电", "load_shedding": "切负荷",
}
