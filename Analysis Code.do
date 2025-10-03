clear
//cd "C:\Users\zhang\My Drive\8_PassThrough" 
cd "D:\8_PassThrough-RetailMarket\0_Paper\1_STATA"
set more off





//=============================================================================
//
//步骤四 描述性统计
//
//=============================================================================

*==========================================================
* Descriptive statistics table (clean labels & units)
*==========================================================
clear all
set more off

use "data_retail_new_plot.dta", clear

*-------------------------
* 1) 变量单位与标签规范化
*   - 价格: JPY/kWh
*   - 份额: %
*   - 发电/进口: GWh
*   - 工资: 千日元 (thousand JPY)
*-------------------------
label var plvr         "Retail Price (JPY/kWh)"
label var wholeprice   "Wholesale Price (JPY/kWh)"
label var lvr_share    "New Entrant Share (%)"

* 原始数据若为 MWh/百人，统一换算；此处按你给的写法缩放到 GWh、千日元
capture confirm variable import_M
if !_rc {
    replace import_M   = import_M/1000
    label var import_M "Imports (GWh)"
}

capture confirm variable nuclear_M
if !_rc {
    replace nuclear_M  = nuclear_M/1000
    label var nuclear_M "Nuclear Output (GWh)"
}

capture confirm variable solar_M
if !_rc {
    replace solar_M    = solar_M/1000
    label var solar_M  "Solar Output (GWh)"
}

capture confirm variable wind_M
if !_rc {
    replace wind_M     = wind_M/1000
    label var wind_M   "Wind Output (GWh)"
}

capture confirm variable salary
if !_rc {
    replace salary     = salary/1000
    label var salary   "Monthly Salary (thousand JPY)"
}

* 避免把 *_log 这种对数变量混入统计表，统计用水平量即可
* 你要统计的变量列表：
local statsvars plvr wholeprice lvr_share salary import_M nuclear_M wind_M solar_M

*-------------------------
* 2) 生成统计量（N, mean, median(p50), sd, min, max）
*-------------------------
cap which estpost
if _rc ssc install estout, replace

estpost tabstat `statsvars', ///
    statistics(N mean p50 sd min max) columns(stat)

*-------------------------
* 3) 导出到 RTF（带标题与注释；使用变量标签）
*-------------------------
esttab . using "statistics.rtf", replace label noobs compress ///
    title("Descriptive Statistics") ///
    cells("count(fmt(%9.0f)) mean(fmt(%9.3f)) p50(fmt(%9.3f)) sd(fmt(%9.3f)) min(fmt(%9.3f)) max(fmt(%9.3f))") ///
    addnotes("Prices in JPY/kWh; generation/imports in GWh; salary in thousand JPY.", ///
             "New Entrant Share reported in percent.")

display as text "Done. RTF saved to: statistics.rtf"


*/
//=============================================================================
//
//步骤五 试回归--固定效应模型
//
//=============================================================================

*==========================================================
* Fixed-effects panel (xtreg FE) — ordered columns & rows
*==========================================================
clear all
set more off

use "data_retail_new_plot.dta", clear

* 标签（精简且一致）
label var plvr             "Retail Price (JPY/kWh)"
label var lvr_share        "New Entrant Share (%)"
label var wholeprice       "Wholesale Price (JPY/kWh)"
label var t                "Time Trend"
label var salary_log       "Salary (log)"
label var import_M_log     "Imports (log)"
label var wind_M_log       "Wind Output (log)"
label var solar_M_log      "Solar Output (log)"
label var nuclear_M_log    "Nuclear Output (log)"
label var renewable_M_log  "Renewables (log)"

capture drop month
gen month = month(date)

xtset regioncode Mdate

*-----------------------------------------
* 估计（5列由简到繁）
*-----------------------------------------
cap which eststo
if _rc ssc install estout, replace
eststo clear

* (0) 最简：仅 lvr_share；区域FE
xtreg plvr lvr_share, fe
eststo baseline0
estadd local RegionFE "Yes"
estadd local MonthFE  "No"

* (1) + 时间（月度FE）
xtreg plvr lvr_share t i.Mdate, fe
eststo baseline1
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

* (2) + 批发价
xtreg plvr lvr_share t wholeprice i.Mdate, fe
eststo baseline2
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

* (3) + 工资/进口 + 风/光/核
xtreg plvr lvr_share t wholeprice salary_log import_M_log ///
      wind_M_log solar_M_log nuclear_M_log i.Mdate, fe
eststo baseline3
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

* (4) 用 renewables 替代风/光
xtreg plvr lvr_share t wholeprice salary_log import_M_log ///
      renewable_M_log nuclear_M_log i.Mdate, fe
eststo baseline4
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

*-----------------------------------------
* 导出（行顺序：竞争→批发/趋势→控制项）
*-----------------------------------------
local v_keep lvr_share wholeprice t ///
            salary_log import_M_log nuclear_M_log wind_M_log solar_M_log renewable_M_log

local m_titles "Baseline" "+ Time FE" "+ + Wholesale" ///
               "+ + Labor/Trade & Wind/Solar/Nuclear" "+ + Labor/Trade & Renewables"

esttab baseline0 baseline1 baseline2 baseline3 baseline4 ///
    using "fixed_panel_ordered.rtf", replace label noobs ///
    b(%9.3f) se star(* 0.10 ** 0.05 *** 0.01) compress nogaps ///
    mtitles(`m_titles') ///
    keep(`v_keep') order(`v_keep') ///
    varlabels( lvr_share        "New Entrant Share (%)" ///
               wholeprice       "Wholesale Price (JPY/kWh)" ///
               t                "Time Trend" ///
               salary_log       "Salary (log)" ///
               import_M_log     "Imports (log)" ///
               nuclear_M_log    "Nuclear Output (log)" ///
               wind_M_log       "Wind Output (log)" ///
               solar_M_log      "Solar Output (log)" ///
               renewable_M_log  "Renewables (log)" ) ///
    stats(N N_g r2_a F RegionFE MonthFE, ///
          labels("Samples" "Groups" "Adj. R^2" "F-stat" "Region FE" "Month FE") ///
          fmt(%9.0g %9.0g %9.3f %9.3f)) ///
    aic obslast interaction("×") ///
    title("Retail Price Fixed-Effects Panel Regressions") ///
    addnotes("All models use region fixed effects; month FE where indicated are i.Mdate.", ///
             "Dependent variable: Retail Price (JPY/kWh).", ///
             "Columns add controls cumulatively from left to right.")

display as text "Done. RTF saved to: fixed_panel_ordered.rtf"



use data_retail_new_plot.dta, clear

label variable plvr "Retail Price"
label variable lvr_share "Share of New Retailers(%)"
label variable wholeprice "Wholesale Price"
label variable t "t"
label variable wind_M_log "Wind Output(log)"
label variable solar_M_log "Solar Output(log)"
label variable import_M_log "Import(log)"
label variable salary_log "Salary(log)"
label variable nuclear_M_log "Nuclear Output(log)"
label variable import_M_log "Import(log)"
label variable solar_M "Solar Output"
label variable wind_M "Wind Output"
label variable nuclear_M "Nuclear Output"
label variable import_M "Import"
label variable salary "Salary"

drop month
gen month=month(date)

xtset regioncode Mdate



// 非常好的结果 上沿
xtreg plvr lvr_share t wholeprice wind_M_log solar_M_log   demand_M_log i.Mdate , fe 
xtreg plvr lvr_share t wholeprice wind_M_log i.yr i.month solar_M_log    i.Mdate , fe 
xtreg plvr lvr_share t wholeprice renewable_M_log      i.Mdate , fe 

// 非常好的结果 下沿

xtreg plvr lvr_share  , fe 
est store baseline0

xtreg plvr lvr_share  t i.Mdate , fe 
est store baseline1

xtreg plvr lvr_share t  wholeprice  i.Mdate , fe 
est store baseline2

xtreg plvr lvr_share t wholeprice salary_log import_M_log wind_M_log solar_M_log  nuclear_M_log  i.Mdate , fe 
est store baseline3

xtreg plvr lvr_share t wholeprice salary_log import_M_log renewable_M_log  nuclear_M_log  i.Mdate , fe 
est store baseline4



esttab baseline0 baseline1 baseline2 baseline3 baseline4, label b(%9.3f)  ///
	stats(    N        N_g      r2_a  F , ///
	labels("Samples" "Groups" "Adj R2"   "F" ) ///
	fmt(%9.0g %9.0g %9.3f)) ///
	se scalars(N r2_a) mtitles, using fixed_panel.rtf, ///
	replace  order(lvr_share wholeprice t  salary_log  import_M_log  salary_log nuclear_M_log  wind_M_log solar_M_log  Region_Dummy Month_Dummy)  ///
	drop(*.Mdate) ///
	aic obslast nogaps interaction("×") star(* 0.10 ** 0.05 *** 0.01)	

esttab baseline0 baseline1 baseline2 baseline3  baseline4, label wide b(%9.3f)  ///
	stats(N N_g r2_a  F , ///
	labels("Samples" "Groups" "Adj R2"   "F" ) ///
	fmt(%9.0g %9.0g %9.3f)) ///
	se scalars(N r2_a) mtitles, using fixed_panel_wide.rtf, ///
	replace  order(lvr_share wholeprice t  salary_log  import_M_log  salary_log nuclear_M_log  wind_M_log solar_M_log  Region_Dummy Month_Dummy)  ///
	drop(*.Mdate) ///
	aic obslast nogaps interaction("×") star(* 0.10 ** 0.05 *** 0.01)	

* 估计模型后：

* —— 通用模板参数（便于复用）——
local STAR "star(* 0.10 ** 0.05 *** 0.01)"
local STATS "stats(N N_g r2_a F, labels(`"Samples"' `"Groups"' `"Adj $R^2$"' `"F"') fmt(%9.0g %9.0g %9.3f %9.2f))"
local ORDER "order(lvr_share wholeprice t salary_log import_M_log nuclear_M_log wind_M_log solar_M_log)"
local DROP  "drop(*.Mdate)"      // 丢弃时间虚拟变量
local NUM   "collabels(none) numbers mtitles(`"baseline0"' `"baseline1"' `"baseline2"' `"baseline3"' `"baseline4"')"

* —— AER/JPE 风格：booktabs 三线表、原文等宽字体、无强制缩放 —— 
esttab baseline0 baseline1 baseline2 baseline3 baseline4 ///
    using fixed_panel_body.tex, replace fragment style(tex) booktabs ///
    label se b(%9.3f) `STAR' `STATS' `ORDER' `DROP' `NUM' ///
    nogaps obslast interaction("×")

esttab baseline0 baseline1 baseline2 baseline3 baseline4 ///
    using "D:\8_PassThrough-RetailMarket\0_Paper\2_Tables\fixed_panel_body.tex", replace fragment style(tex) booktabs ///
    label se b(%9.3f) `STAR' `STATS' `ORDER' `DROP' `NUM' ///
    nogaps obslast interaction("×")
	
	
	
	//=============================================================================
//
//步骤五 固定效应模型-考虑长面板的问题
//
//=============================================================================
	use data_retail_new_plot.dta, clear
drop month
gen month=month(date)

xtset regioncode Mdate

	
	xtscc plvr lvr_share t wholeprice salary_log import_M_log ///
      nuclear_M_log wind_M_log solar_M_log i.Mdate, fe
	
	
	
	xtscc plvr lvr_share  , fe 
est store baseline0

xtscc plvr lvr_share  t i.Mdate , fe 
est store baseline1

xtscc plvr lvr_share t  wholeprice  i.Mdate , fe 
est store baseline2

xtscc plvr lvr_share t wholeprice salary_log import_M_log wind_M_log solar_M_log  nuclear_M_log  i.Mdate , fe 
est store baseline3

xtscc plvr lvr_share t wholeprice salary_log import_M_log renewable_M_log  nuclear_M_log  i.Mdate , fe 
est store baseline4
	
	
esttab  baseline2 baseline3 baseline4, label b(%9.3f)  ///
	stats(    N        N_g      r2_a  F , ///
	labels("Samples" "Groups" "Adj R2"   "F" ) ///
	fmt(%9.0g %9.0g %9.3f)) ///
	se scalars(N r2_a) mtitles, using fixed_panel_long_panel.rtf, ///
	replace  order(lvr_share wholeprice t  salary_log  import_M_log  salary_log nuclear_M_log  wind_M_log solar_M_log  Region_Dummy Month_Dummy)  ///
	drop(*.Mdate) ///
	aic obslast nogaps interaction("×") star(* 0.10 ** 0.05 *** 0.01)	

esttab  baseline2 baseline3  baseline4, label wide b(%9.3f)  ///
	stats(N N_g r2_a  F , ///
	labels("Samples" "Groups" "Adj R2"   "F" ) ///
	fmt(%9.0g %9.0g %9.3f)) ///
	se scalars(N r2_a) mtitles, using fixed_panel_wide_long_panel.rtf, ///
	replace  order(lvr_share wholeprice t  salary_log  import_M_log  salary_log nuclear_M_log  wind_M_log solar_M_log  Region_Dummy Month_Dummy)  ///
	drop(*.Mdate) ///
	aic obslast nogaps interaction("×") star(* 0.10 ** 0.05 *** 0.01)	

* 估计模型后：

* —— 通用模板参数（便于复用）——
local STAR "star(* 0.10 ** 0.05 *** 0.01)"
local STATS "stats(N N_g r2_a F, labels(`"Samples"' `"Groups"' `"Adj $R^2$"' `"F"') fmt(%9.0g %9.0g %9.3f %9.2f))"
local ORDER "order(lvr_share wholeprice t salary_log import_M_log nuclear_M_log wind_M_log solar_M_log)"
local DROP  "drop(*.Mdate)"      // 丢弃时间虚拟变量
local NUM   "collabels(none) numbers mtitles(`"baseline0"' `"baseline1"' `"baseline2"' `"baseline3"' `"baseline4"')"

* —— AER/JPE 风格：booktabs 三线表、原文等宽字体、无强制缩放 —— 
esttab  baseline2 baseline3 baseline4 ///
    using fixed_panel_body_long_panel.tex, replace fragment style(tex) booktabs ///
    label se b(%9.3f) `STAR' `STATS' `ORDER' `DROP' `NUM' ///
    nogaps obslast interaction("×")

esttab  baseline2 baseline3 baseline4 ///
    using "D:\8_PassThrough-RetailMarket\0_Paper\2_Tables\fixed_panel_body_long_panel.tex", replace fragment style(tex) booktabs ///
    label se b(%9.3f) `STAR' `STATS' `ORDER' `DROP' `NUM' ///
    nogaps obslast interaction("×")
	
	
*==========================================================
* Fixed-effects panel (DK SE): ordered columns & rows
*==========================================================
clear all
set more off

use "data_retail_new_plot.dta", clear

*--- 基本变量与标签
capture drop month
gen month = month(date)

label var plvr             "Retail Price (JPY/kWh)"
label var wholeprice       "Wholesale Price (JPY/kWh)"
label var lvr_share        "New Entrant Share"
label var t                "Time Trend"
label var salary_log       "Wage (log)"
label var import_M_log     "Interregional Imports (log)"
label var wind_M_log       "Wind (log)"
label var solar_M_log      "Solar (log)"
label var nuclear_M_log    "Nuclear (log)"
label var renewable_M_log  "Renewables (log)"

* 面板设定
xtset regioncode Mdate

*--- 回归与结果存储（统一使用 i.Mdate 月份FE；xtscc, fe 自带区域FE）
cap which eststo
if _rc ssc install estout, replace
eststo clear

* baseline2: plvr ~ lvr_share + t + wholesale + month FE
xtscc plvr lvr_share t wholeprice i.Mdate, fe
eststo baseline2
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

* baseline3: + 人工/贸易/风光核
xtscc plvr lvr_share t wholeprice salary_log import_M_log ///
                 wind_M_log solar_M_log nuclear_M_log i.Mdate, fe
eststo baseline3
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

* baseline4: 把风光合为 renewables（你已有该变量）
xtscc plvr lvr_share t wholeprice salary_log import_M_log ///
                 renewable_M_log nuclear_M_log i.Mdate, fe
eststo baseline4
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

*--- 变量行顺序（先竞争→批发/趋势→控制项）
local v_keep lvr_share wholeprice t ///
            salary_log import_M_log nuclear_M_log wind_M_log solar_M_log renewable_M_log

* 列标题
local m_titles "Baseline (+Wholesale)" " + Labor/Trade & Wind/Solar/Nuclear" " + Labor/Trade & Renewables"

*--- 导出（不再使用 drop(*.Mdate)）
esttab baseline2 baseline3 baseline4 using "fixed_panel_ordered.rtf", replace ///
    label b(%9.3f) se star(* 0.10 ** 0.05 *** 0.01) compress nogaps ///
    mtitles(`m_titles') ///
    keep(`v_keep') order(`v_keep') ///
    varlabels( lvr_share        "New Entrant Share" ///
               wholeprice       "Wholesale Price (JPY/kWh)" ///
               t                "Time Trend" ///
               salary_log       "Wage (log)" ///
               import_M_log     "Interregional Imports (log)" ///
               nuclear_M_log    "Nuclear (log)" ///
               wind_M_log       "Wind (log)" ///
               solar_M_log      "Solar (log)" ///
               renewable_M_log  "Renewables (log)" ) ///
    stats(N N_g r2_a F RegionFE MonthFE, ///
          labels("Samples" "Groups" "Adj. R^2" "F-stat" "Region FE" "Month FE") ///
          fmt(%9.0g %9.0g %9.3f %9.3f)) ///
    aic obslast interaction("×") ///
    title("Retail Price Panel Regressions with Driscoll–Kraay SE (FE)") ///
    addnotes("All models include region FE (within) and month FE (i.Mdate); Driscoll–Kraay standard errors.", ///
             "Dependent variable: Retail Price (JPY/kWh).", ///
             "Baseline2 adds Wholesale & Time trend; Baseline3 adds Labor/Trade & Wind/Solar/Nuclear; Baseline4 replaces Wind/Solar with Renewables.")


	

//=============================================================================
//
//步骤五 工具变量方法
//
//=============================================================================
use data_retail_new_plot.dta, clear

drop month
gen month=month(date)

tab month
//sort  Mdate region
xtset regioncode Mdate 

//format Mdate %9.0g
//g t = Mdate - 674




//非常好的结果上沿
xtivreg plvr wholeprice t i.month i.year (lvr_share = cum_m_inside_t_ratio cum_m_inside_m_ratio cum_m_inside_f_ratio cum_m_in_t_ratio cum_m_in_m_ratio cum_m_in_f_ratio cum_m_out_t_ratio cum_m_out_m_ratio), fe first

xtivreg plvr wholeprice t i.month i.year (lvr_share =   cum_m_in_m_ratio  cum_m_in_f_ratio ), fe first small
xtivreg plvr wholeprice t i.Mdate (lvr_share =   cum_m_in_m_ratio  cum_m_in_f_ratio ), fe first small
xtivreg plvr wholeprice t i.Mdate (lvr_share =  cum_net_m_ratio    cum_net_f_ratio  ), fe first


//非常好的结果下沿

xtivreg plvr  (lvr_share =  cum_net_m_ratio    cum_net_f_ratio  ), fe first
est store ivregr0

xtivreg plvr wholeprice t   i.Mdate (lvr_share =  cum_net_m_ratio    cum_net_f_ratio  ), fe first
est store ivregr1

xtivreg plvr wholeprice t  salary_log import_M_log  i.Mdate (lvr_share =  cum_net_m_ratio    cum_net_f_ratio  ), fe first
est store ivregr2

xtivreg plvr wholeprice t  salary_log import_M_log nuclear_M_log  wind_M_log solar_M_log  i.Mdate (lvr_share =  cum_net_m_ratio    cum_net_f_ratio  ), fe first
est store ivregr3

esttab  ivregr0 ivregr1 ivregr2 ivregr3, label b(%9.3f)  ///
	stats(    N        N_g      r2_a  F , ///
	labels("Samples" "Groups" ) ///
	fmt(%9.0g %9.0g %9.3f)) ///
	se scalars(N r2_a) mtitles, using Panel_IV.rtf, ///
	replace  order(lvr_share wholeprice t  salary_log import_M_log nuclear_M_log  wind_M_log solar_M_log  Region_Dummy Month_Dummy)  ///
	drop(*.Mdate) ///
	aic obslast nogaps interaction("×") star(* 0.10 ** 0.05 *** 0.01)	

esttab  ivregr0 ivregr1 ivregr2 ivregr3, label wide b(%9.3f)  ///
	stats(N N_g r2_a  F , ///
	labels("Samples" "Groups" ) ///
	fmt(%9.0g %9.0g %9.3f)) ///
	se scalars(N r2_a) mtitles, using Panel_IV_wide.rtf, ///
	replace  order(lvr_share wholeprice t  salary_log import_M_log nuclear_M_log  wind_M_log solar_M_log  Region_Dummy Month_Dummy)  ///
	drop(*.Mdate) ///
	aic obslast nogaps interaction("×") star(* 0.10 ** 0.05 *** 0.01)	


	//—— 设置输出目录（按需修改/确保已存在）
local outdir "D:\8_PassThrough-RetailMarket\0_Paper\2_Tables"

// …（前面的 xtivreg 与 est store 代码保持不变）…

//=========================
// 窄表：RTF 与 LaTeX 一并输出
//=========================
esttab ivregr0 ivregr1 ivregr2 ivregr3 using Panel_IV.rtf, replace ///
    label b(%9.3f) ///
    stats(N N_g r2_a F, labels("Samples" "Groups") fmt(%9.0g %9.0g %9.3f)) ///
    se scalars(N r2_a) mtitles ///
    order(lvr_share wholeprice t salary_log import_M_log nuclear_M_log wind_M_log solar_M_log Region_Dummy Month_Dummy) ///
    drop(*.Mdate) aic obslast nogaps interaction("×") star(* 0.10 ** 0.05 *** 0.01)

// LaTeX 版
esttab ivregr0 ivregr1 ivregr2 ivregr3 using "D:\8_PassThrough-RetailMarket\0_Paper\2_Tables\Panel_IV.tex", replace ///
    label b(%9.3f) ///
    stats(N N_g r2_a F, labels("Samples" "Groups") fmt(%9.0g %9.0g %9.3f)) ///
    se scalars(N r2_a) mtitles ///
    order(lvr_share wholeprice t salary_log import_M_log nuclear_M_log wind_M_log solar_M_log Region_Dummy Month_Dummy) ///
    drop(*.Mdate) aic obslast nogaps interaction("×") star(* 0.10 ** 0.05 *** 0.01) booktabs

	

g Mdate2 = Mdate
tabulate Mdate2, generate(Mdate2_)
drop Mdate2_79
drop Mdate2_78

xtivreg plvr    t wholeprice nuclear_M_log  wind_M_log solar_M_log  import_M_log   Mdate2_* (lvr_share =  cum_net_m_ratio    cum_net_f_ratio  ), fe first
overid
overid, vceopt(cluster(regioncode))


xtivreg plvr    t wholeprice nuclear_M_log  wind_M_log solar_M_log  import_M_log   Mdate2_* (lvr_share =  cum_net_m_ratio    cum_net_f_ratio  ), fe first
overid, vceopt(cluster(regioncode))



*==========================================================
* IV-FE panel (xtivreg, DK-like FE): ordered columns & rows
*==========================================================
clear all
set more off

use "data_retail_new_plot.dta", clear

capture drop month
gen month = month(date)

* 基本标签
label var plvr             "Retail Price (JPY/kWh)"
label var wholeprice       "Wholesale Price (JPY/kWh)"
label var lvr_share        "New Entrant Share"
label var t                "Time Trend"
label var salary_log       "Wage (log)"
label var import_M_log     "Interregional Imports (log)"
label var wind_M_log       "Wind (log)"
label var solar_M_log      "Solar (log)"
label var nuclear_M_log    "Nuclear (log)"
label var renewable_M_log  "Renewables (log)"

* 仪器变量标签（按你的变量名）
label var cum_net_m_ratio  "Cumulative Male Migration (net)"
label var cum_net_f_ratio  "Cumulative Female Migration (net)"
label var cum_m_in_m_ratio "Cum. Male Moves In"
label var cum_m_in_f_ratio "Cum. Female Moves In"

* 面板设定
xtset regioncode Mdate

*======================
* 估计与存储（列顺序）
* (1) 最简IV
* (2) + Wholesale & Time Trend & Month FE
* (3) + Labor/Trade
* (4) + Labor/Trade & Wind/Solar/Nuclear
*======================
cap which eststo
if _rc ssc install estout, replace
eststo clear

* (1) plvr ~ lvr_share (IV = net male/female); 区域FE
xtivreg plvr (lvr_share = cum_net_m_ratio cum_net_f_ratio), fe first small
eststo iv1
estadd local RegionFE "Yes"
estadd local MonthFE  "No"

* (2) + 批发、时间趋势、月份FE
xtivreg plvr wholeprice t i.Mdate (lvr_share = cum_net_m_ratio cum_net_f_ratio), fe first small
eststo iv2
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

* (3) + 工资与跨区进口
xtivreg plvr wholeprice t salary_log import_M_log i.Mdate ///
    (lvr_share = cum_net_m_ratio cum_net_f_ratio), fe first small
eststo iv3
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

* (4) + 风/光/核
xtivreg plvr wholeprice t salary_log import_M_log nuclear_M_log wind_M_log solar_M_log ///
    i.Mdate (lvr_share = cum_net_m_ratio cum_net_f_ratio), fe first small
eststo iv4
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

*----------------------
* 行顺序（竞争→批发/趋势→控制项）
*----------------------
local v_keep lvr_share wholeprice t ///
            salary_log import_M_log nuclear_M_log wind_M_log solar_M_log

* 列标题
local m_titles "IV FE (baseline)" " + Wholesale & Trend & Month FE" ///
               " + Labor/Trade" " + Labor/Trade & Wind/Solar/Nuclear"

*----------------------
* 导出 RTF（不再使用 drop(*.Mdate)，用 keep/order 控制）
*----------------------
esttab iv1 iv2 iv3 iv4 using "Panel_IV_ordered.rtf", replace ///
    label b(%9.3f) se star(* 0.10 ** 0.05 *** 0.01) compress nogaps ///
    mtitles(`m_titles') ///
    keep(`v_keep') order(`v_keep') ///
    varlabels( lvr_share        "New Entrant Share (2SLS)" ///
               wholeprice       "Wholesale Price (JPY/kWh)" ///
               t                "Time Trend" ///
               salary_log       "Wage (log)" ///
               import_M_log     "Interregional Imports (log)" ///
               nuclear_M_log    "Nuclear (log)" ///
               wind_M_log       "Wind (log)" ///
               solar_M_log      "Solar (log)" ) ///
    stats(N N_g r2_a F RegionFE MonthFE, ///
          labels("Samples" "Groups" "Adj. R^2" "F-stat" "Region FE" "Month FE") ///
          fmt(%9.0g %9.0g %9.3f %9.3f)) ///
    obslast aic interaction("×") ///
    title("Retail Price IV-FE Panel (xtivreg, 2SLS)") ///
    addnotes("All specifications use region fixed effects; where shown, month FE = i.Mdate.", ///
             "Endogenous regressor: New Entrant Share; Instruments: Cumulative net male & female migration.", ///
             "Standard errors reported under coefficients; first-stage diagnostics printed in Stata output via option first.")

display as text "Done. RTF saved to: Panel_IV_ordered.rtf"



//==============================================================================
//First Step
//==============================================================================
xtivreg plvr  (lvr_share =  cum_net_m_ratio    cum_net_f_ratio  ), fe first
est store ivregr0
xtreg lvr_share  cum_net_m_ratio    cum_net_f_ratio  , fe
est store ivregr0

xtivreg plvr wholeprice t i.Mdate (lvr_share =  cum_net_m_ratio    cum_net_f_ratio  ), fe first 
xtreg lvr_share  cum_net_m_ratio    cum_net_f_ratio wholeprice t  i.Mdate, fe

est store ivregr1

xtivreg plvr wholeprice t salary_log i.Mdate (lvr_share =  cum_net_m_ratio    cum_net_f_ratio  ), fe first
xtreg lvr_share  cum_net_m_ratio    cum_net_f_ratio wholeprice t salary_log  import_M_log i.Mdate , fe

est store ivregr2

xtivreg plvr wholeprice t salary_log  nuclear_M_log  wind_M_log solar_M_log import_M_log i.Mdate (lvr_share =  cum_net_m_ratio    cum_net_f_ratio  ), fe first
xtreg lvr_share  cum_net_m_ratio    cum_net_f_ratio wholeprice t salary_log nuclear_M_log  wind_M_log solar_M_log import_M_log i.Mdate, fe

est store ivregr3



esttab ivregr0 ivregr1 ivregr2 ivregr3, b(%9.3f) label stats(N N_g r2_a F, ///
	labels("Samples" "Groups" "Adj R2"  "F" ) ///
	fmt(%9.0g %9.0g %9.3f)) ///
	se scalars(N r2_a) mtitles, using Panel_IV_first.rtf, ///
	replace  order(cum_net_m_ratio    cum_net_f_ratio  wholeprice t salary_log import_M_log nuclear_M_log  wind_M_log solar_M_log Region_Dummy Month_Dummy)  ///
	drop(*.Mdate ) ///
	aic obslast nogaps interaction("×") star(* 0.10 ** 0.05 *** 0.01)	

esttab ivregr0 ivregr1 ivregr2 ivregr3, b(%9.3f) label stats(N N_g r2_a F, ///
	labels("Samples" "Groups" "Adj R2"  "F" ) ///
	fmt(%9.0g %9.0g %9.3f)) ///
	se scalars(N r2_a) mtitles, using Panel_IV_first_wide.rtf, ///
	replace  order(cum_net_m_ratio    cum_net_f_ratio  wholeprice t salary_log import_M_log nuclear_M_log  wind_M_log solar_M_log  Region_Dummy Month_Dummy)  ///
	drop(*.Mdate ) ///
	aic obslast nogaps interaction("×") star(* 0.10 ** 0.05 *** 0.01)	
	
	
	// LaTeX 版
esttab ivregr0 ivregr1 ivregr2 ivregr3 using "D:\8_PassThrough-RetailMarket\0_Paper\2_Tables\Panel_IV_first.tex", replace ///
    label b(%9.3f) ///
    stats(N N_g r2_a F, labels("Samples" "Groups") fmt(%9.0g %9.0g %9.3f)) ///
    se scalars(N r2_a) mtitles ///
    order(lvr_share wholeprice t salary_log import_M_log nuclear_M_log wind_M_log solar_M_log Region_Dummy Month_Dummy) ///
    drop(*.Mdate) aic obslast nogaps interaction("×") star(* 0.10 ** 0.05 *** 0.01) booktabs
	
	
*==========================================================
* First-stage (xtreg FE) for IV specs — ordered & labeled
*==========================================================
clear all
set more off

use "data_retail_new_plot.dta", clear

* panel & month
capture drop month
gen month = month(date)
xtset regioncode Mdate

* labels
label var lvr_share        "New Entrant Share"
label var wholeprice       "Wholesale Price (JPY/kWh)"
label var t                "Time Trend"
label var salary_log       "Wage (log)"
label var import_M_log     "Interregional Imports (log)"
label var wind_M_log       "Wind (log)"
label var solar_M_log      "Solar (log)"
label var nuclear_M_log    "Nuclear (log)"
label var cum_net_m_ratio  "Cumulative Male Migration (net)"
label var cum_net_f_ratio  "Cumulative Female Migration (net)"

* estout
cap which eststo
if _rc ssc install estout, replace
eststo clear

*----------------------
* (FS1) 最简第一阶段（对应 iv1）
*----------------------
xtreg lvr_share ///
      cum_net_m_ratio cum_net_f_ratio, fe
eststo FS1
estadd local RegionFE "Yes"
estadd local MonthFE  "No"
test cum_net_m_ratio cum_net_f_ratio
estadd scalar InstF = r(F)

*----------------------
* (FS2) + 批发、时间趋势、月份FE（对应 iv2）
*----------------------
xtreg lvr_share ///
      cum_net_m_ratio cum_net_f_ratio ///
      wholeprice t i.Mdate, fe
eststo FS2
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"
test cum_net_m_ratio cum_net_f_ratio
estadd scalar InstF = r(F)

*----------------------
* (FS3) + 工资/跨区进口（对应 iv3）
*----------------------
xtreg lvr_share ///
      cum_net_m_ratio cum_net_f_ratio ///
      wholeprice t salary_log import_M_log i.Mdate, fe
eststo FS3
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"
test cum_net_m_ratio cum_net_f_ratio
estadd scalar InstF = r(F)

*----------------------
* (FS4) + 风/光/核（对应 iv4）
*----------------------
xtreg lvr_share ///
      cum_net_m_ratio cum_net_f_ratio ///
      wholeprice t salary_log import_M_log ///
      nuclear_M_log wind_M_log solar_M_log i.Mdate, fe
eststo FS4
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"
test cum_net_m_ratio cum_net_f_ratio
estadd scalar InstF = r(F)

* 变量行顺序（先工具→批发/趋势→控制项）
local v_keep cum_net_m_ratio cum_net_f_ratio ///
             wholeprice t salary_log import_M_log nuclear_M_log wind_M_log solar_M_log

* 列标题（与第二阶段列对应）
local m_titles "FS for IV(1) baseline" ///
               "FS for IV(2) +Wholesale&Trend&Month FE" ///
               "FS for IV(3) +Labor/Trade" ///
               "FS for IV(4) +Labor/Trade&Wind/Solar/Nuclear"

* 导出（不需要 drop(*.Mdate)，用 keep/order 控制显示）
esttab FS1 FS2 FS3 FS4 using "Panel_IV_first_ordered.rtf", replace ///
    label b(%9.3f) se star(* 0.10 ** 0.05 *** 0.01) compress nogaps ///
    mtitles(`m_titles') ///
    keep(`v_keep') order(`v_keep') ///
    varlabels( cum_net_m_ratio "Cumulative Male Migration (net)" ///
               cum_net_f_ratio "Cumulative Female Migration (net)" ///
               wholeprice       "Wholesale Price (JPY/kWh)" ///
               t                "Time Trend" ///
               salary_log       "Wage (log)" ///
               import_M_log     "Interregional Imports (log)" ///
               nuclear_M_log    "Nuclear (log)" ///
               wind_M_log       "Wind (log)" ///
               solar_M_log      "Solar (log)" ) ///
    stats(N N_g r2_a F InstF RegionFE MonthFE, ///
          labels("Samples" "Groups" "Adj. R^2" "Model F" "Instruments joint F" "Region FE" "Month FE") ///
          fmt(%9.0g %9.0g %9.3f %9.3f %9.3f)) ///
    obslast aic interaction("×") ///
    title("First-Stage Fixed-Effects Regressions for IV Specifications") ///
    addnotes("Dependent variable: New Entrant Share (first stage).", ///
             "All models use region FE; where indicated, month FE = i.Mdate.", ///
             "‘Instruments joint F’ reports joint significance (cum_net_m_ratio, cum_net_f_ratio).")

display as text "Done. RTF saved to: Panel_IV_first_ordered.rtf"

	
	
	


//=============================================================================
//
// 清理 wholeprice 异常值
//
//=============================================================================

use data_retail_new_plot.dta, clear

// 先检查异常值分布
tab wholeprice if wholeprice > 35

// 按地区和时间排序
sort regioncode Mdate

// 创建滞后一期的 wholeprice
by regioncode: gen L_wholeprice = wholeprice[_n-1]

// 标记异常值
gen outlier_flag = (wholeprice > 35)
tab outlier_flag

// 显示异常值的时间分布
bysort Mdate: tab outlier_flag if outlier_flag == 1

// 创建清理后的 wholeprice 变量
gen wholeprice_clean = wholeprice

// 将异常值替换为滞后值（如果存在）
replace wholeprice_clean = L_wholeprice if wholeprice > 35 & !missing(L_wholeprice)

// 对于第一期没有滞后值的情况，使用该地区的中位数
bysort regioncode: egen median_wholeprice = median(wholeprice) if wholeprice <= 35
bysort regioncode: replace median_wholeprice = median_wholeprice[1]
replace wholeprice_clean = median_wholeprice if wholeprice > 35 & missing(L_wholeprice)

// 检查清理结果
sum wholeprice wholeprice_clean, detail

// 比较清理前后的分布
histogram wholeprice, name(before, replace) title("Before Cleaning")
histogram wholeprice_clean, name(after, replace) title("After Cleaning")
graph combine before after, title("Wholesale Price Distribution")

// 显示被替换的观测值
list regioncode Mdate wholeprice L_wholeprice wholeprice_clean if wholeprice > 35, sep(0)

// 保存清理后的数据
save data_retail_clean_wholeprice.dta, replace

//=============================================================================
// 用清理后的数据重新运行主要回归
//=============================================================================
use data_retail_clean_wholeprice.dta, clear
//=============================================================================
//
//步骤五 固定效应模型-考虑长面板的问题
//
//=============================================================================
use data_retail_clean_wholeprice.dta, clear
	
label variable wholeprice_clean "Wholesale Price"
drop month
gen month=month(date)

xtset regioncode Mdate

	
	xtscc plvr lvr_share t wholeprice_clean salary_log import_M_log ///
      nuclear_M_log wind_M_log solar_M_log i.Mdate, fe
	
	
	
	xtscc plvr lvr_share  , fe 
est store baseline0

xtscc plvr lvr_share  t i.Mdate , fe 
est store baseline1

xtscc plvr lvr_share t  wholeprice_clean  i.Mdate , fe 
est store baseline2

xtscc plvr lvr_share t wholeprice_clean salary_log import_M_log wind_M_log solar_M_log  nuclear_M_log  i.Mdate , fe 
est store baseline3

xtscc plvr lvr_share t wholeprice_clean salary_log import_M_log renewable_M_log  nuclear_M_log  i.Mdate , fe 
est store baseline4
	

	
esttab  baseline2 baseline3 baseline4, label b(%9.3f)  ///
	stats(    N        N_g      r2_a  F , ///
	labels("Samples" "Groups" "Adj R2"   "F" ) ///
	fmt(%9.0g %9.0g %9.3f)) ///
	se scalars(N r2_a) mtitles, using fixed_panel_long_panel_outliers.rtf, ///
	replace  order(lvr_share wholeprice_clean t  salary_log  import_M_log  salary_log nuclear_M_log  wind_M_log solar_M_log  Region_Dummy Month_Dummy)  ///
	drop(*.Mdate) ///
	aic obslast nogaps interaction("×") star(* 0.10 ** 0.05 *** 0.01)	

esttab  baseline2 baseline3  baseline4, label wide b(%9.3f)  ///
	stats(N N_g r2_a  F , ///
	labels("Samples" "Groups" "Adj R2"   "F" ) ///
	fmt(%9.0g %9.0g %9.3f)) ///
	se scalars(N r2_a) mtitles, using fixed_panel_wide_long_panel_outliers.rtf, ///
	replace  order(lvr_share wholeprice_clean t  salary_log  import_M_log  salary_log nuclear_M_log  wind_M_log solar_M_log  Region_Dummy Month_Dummy)  ///
	drop(*.Mdate) ///
	aic obslast nogaps interaction("×") star(* 0.10 ** 0.05 *** 0.01)	

* 估计模型后：

* —— 通用模板参数（便于复用）——
local STAR "star(* 0.10 ** 0.05 *** 0.01)"
local STATS "stats(N N_g r2_a F, labels(`"Samples"' `"Groups"' `"Adj $R^2$"' `"F"') fmt(%9.0g %9.0g %9.3f %9.2f))"
local ORDER "order(lvr_share wholeprice_clean t salary_log import_M_log nuclear_M_log wind_M_log solar_M_log)"
local DROP  "drop(*.Mdate)"      // 丢弃时间虚拟变量
local NUM   "collabels(none) numbers mtitles(`"baseline0"' `"baseline1"' `"baseline2"' `"baseline3"' `"baseline4"')"

* —— AER/JPE 风格：booktabs 三线表、原文等宽字体、无强制缩放 —— 
esttab  baseline2 baseline3 baseline4 ///
    using fixed_panel_body_long_panel_outliers.tex, replace fragment style(tex) booktabs ///
    label se b(%9.3f) `STAR' `STATS' `ORDER' `DROP' `NUM' ///
    nogaps obslast interaction("×")

esttab  baseline2 baseline3 baseline4 ///
    using "D:\8_PassThrough-RetailMarket\0_Paper\2_Tables\fixed_panel_body_long_panel_outliers.tex", replace fragment style(tex) booktabs ///
    label se b(%9.3f) `STAR' `STATS' `ORDER' `DROP' `NUM' ///
    nogaps obslast interaction("×")
	
*==========================================================
* FE (long panel) with DK SE — ordered columns & rows
*==========================================================
clear all
set more off

use "data_retail_clean_wholeprice.dta", clear

*--- 标签
label var plvr               "Retail Price (JPY/kWh)"
label var wholeprice_clean   "Wholesale Price (JPY/kWh)"
label var lvr_share          "New Entrant Share"
label var t                  "Time Trend"
label var salary_log         "Wage (log)"
label var import_M_log       "Interregional Imports (log)"
label var wind_M_log         "Wind (log)"
label var solar_M_log        "Solar (log)"
label var nuclear_M_log      "Nuclear (log)"
label var renewable_M_log    "Renewables (log)"

capture drop month
gen month = month(date)

xtset regioncode Mdate

*--- 估计
cap which eststo
if _rc ssc install estout, replace
eststo clear

* baseline2: + 批发 & 时间趋势 & 月份FE
xtscc plvr lvr_share t wholeprice_clean i.Mdate, fe
eststo baseline2
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

* baseline3: + 劳动/贸易 & 风/光/核
xtscc plvr lvr_share t wholeprice_clean salary_log import_M_log ///
                 wind_M_log solar_M_log nuclear_M_log i.Mdate, fe
eststo baseline3
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

* baseline4: + 劳动/贸易 & Renewables（替代风/光）
xtscc plvr lvr_share t wholeprice_clean salary_log import_M_log ///
                 renewable_M_log nuclear_M_log i.Mdate, fe
eststo baseline4
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

*--- 行顺序（先竞争→批发/趋势→控制项）
local v_keep lvr_share wholeprice_clean t ///
            salary_log import_M_log nuclear_M_log wind_M_log solar_M_log renewable_M_log

* 列标题
local m_titles "Baseline (+Wholesale)" ///
               "+ Labor/Trade & Wind/Solar/Nuclear" ///
               "+ Labor/Trade & Renewables"

*--- 导出（不使用 drop(*.Mdate)）
esttab baseline2 baseline3 baseline4 using "fixed_panel_long_panel_outliers_ordered.rtf", replace ///
    label b(%9.3f) se star(* 0.10 ** 0.05 *** 0.01) compress nogaps ///
    mtitles(`m_titles') ///
    keep(`v_keep') order(`v_keep') ///
    varlabels( lvr_share         "New Entrant Share" ///
               wholeprice_clean  "Wholesale Price (JPY/kWh)" ///
               t                 "Time Trend" ///
               salary_log        "Wage (log)" ///
               import_M_log      "Interregional Imports (log)" ///
               nuclear_M_log     "Nuclear (log)" ///
               wind_M_log        "Wind (log)" ///
               solar_M_log       "Solar (log)" ///
               renewable_M_log   "Renewables (log)" ) ///
    stats(N N_g r2_a F RegionFE MonthFE, ///
          labels("Samples" "Groups" "Adj. R^2" "F-stat" "Region FE" "Month FE") ///
          fmt(%9.0g %9.0g %9.3f %9.3f)) ///
    aic obslast interaction("×") ///
    title("Retail Price Fixed-Effects (Long Panel) with Driscoll–Kraay SE") ///
    addnotes("All models include region FE (within) and month FE (i.Mdate); Driscoll–Kraay standard errors.", ///
             "Dependent variable: Retail Price (JPY/kWh).", ///
             "Baseline2 adds Wholesale & Time trend; Baseline3 adds Labor/Trade & Wind/Solar/Nuclear; Baseline4 replaces Wind/Solar with Renewables.")

display as text "Done. RTF saved to: fixed_panel_long_panel_outliers_ordered.rtf"

	
	
//=============================================================================
//
//步骤五 固定效应模型-考虑长面板的问题
//
//=============================================================================
use data_retail_clean_wholeprice.dta, clear
	
label variable wholeprice_clean "Wholesale Price"
drop month
gen month=month(date)

xtset regioncode Mdate

gen new_dep = plvr - wholeprice_clean
gen new_dep_share = ( plvr - wholeprice_clean ) / wholeprice_clean
xtscc new_dep lvr_share t wholeprice_clean salary_log import_M_log renewable_M_log  nuclear_M_log  i.Mdate , fe 
xtscc new_dep_share  t lvr_share wholeprice_clean salary_log import_M_log renewable_M_log  nuclear_M_log  i.Mdate , fe 
xtscc new_dep lvr_share t wholeprice_clean salary_log import_M_log renewable_M_log  nuclear_M_log  i.Mdate , fe 
xtscc new_dep_share c.lvr_share##c.wholeprice_clean t salary_log import_M_log renewable_M_log  nuclear_M_log  i.Mdate , fe 

	
*==========================================================
* Mechanism identification table (ordered columns & rows)
*==========================================================
clear all
set more off

use "data_retail_clean_wholeprice.dta", clear

*--- labels
label var plvr              "Retail Price (JPY/kWh)"
label var wholeprice_clean  "Wholesale Price (JPY/kWh)"
label var lvr_share         "New Entrant Share"
label var salary_log        "Wage (log)"
label var import_M_log      "Interregional Imports (log)"
label var renewable_M_log   "Renewables (log)"
label var nuclear_M_log     "Nuclear (log)"
label var t                 "Time Trend"

* panel settings
capture drop month
gen month = month(date)
xtset regioncode Mdate

*--- mechanism vars
capture drop new_dep new_dep_share
gen new_dep       = plvr - wholeprice_clean
label var new_dep "Retail Margin = Retail - Wholesale"

gen new_dep_share = (plvr - wholeprice_clean)/wholeprice_clean
label var new_dep_share "Markup Ratio = (Retail-Wholesale)/Wholesale"

* globals
global FE i.Mdate
global BASECTRLS t salary_log import_M_log renewable_M_log nuclear_M_log

* estout
cap which eststo
if _rc ssc install estout, replace
eststo clear

*======================
* Column order target:
* (1) Margin (baseline)
* (2) Margin + Interaction
* (3) Markup (baseline)
* (4) Markup + Interaction
*======================

* (1) Margin baseline
xtscc new_dep lvr_share wholeprice_clean $BASECTRLS $FE, fe
eststo C1
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

* (2) Margin + interaction
xtscc new_dep c.lvr_share##c.wholeprice_clean $BASECTRLS $FE, fe
eststo C2
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

* (3) Markup baseline
xtscc new_dep_share lvr_share wholeprice_clean $BASECTRLS $FE, fe
eststo C3
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

* (4) Markup + interaction
xtscc new_dep_share c.lvr_share##c.wholeprice_clean $BASECTRLS $FE, fe
eststo C4
estadd local RegionFE "Yes"
estadd local MonthFE  "Yes"

* 变量行顺序 & 标签（把交互项放在竞争变量下面）
local v_keep  lvr_share "c.lvr_share#c.wholeprice_clean" ///
             wholeprice_clean t salary_log import_M_log renewable_M_log nuclear_M_log

esttab C1 C2 C3 C4 using "mechanism_results_ordered.rtf", replace ///
    label b(%9.3f) se star(* 0.10 ** 0.05 *** 0.01) compress nogaps ///
    mtitles("Retail Margin" "Retail Margin + Int." "Markup Ratio" "Markup Ratio + Int.") ///
    keep(`v_keep') order(`v_keep') ///
    varlabels( lvr_share                     "New Entrant Share" ///
               "c.lvr_share#c.wholeprice_clean" "New Entrant Share × Wholesale Price (JPY/kWh)" ///
               wholeprice_clean              "Wholesale Price (JPY/kWh)" ///
               t                             "Time Trend" ///
               salary_log                    "Wage (log)" ///
               import_M_log                  "Interregional Imports (log)" ///
               renewable_M_log               "Renewables (log)" ///
               nuclear_M_log                 "Nuclear (log)" ) ///
    stats(N F RegionFE MonthFE, ///
          labels("Observations" "F-stat" "Region FE" "Month FE")) ///
    title("Margin vs. Markup Mechanism under Retail Competition (DK SE, FE)") ///
    addnotes("All models include region & month fixed effects (i.Mdate); Driscoll–Kraay SE.", ///
             "Columns (1)-(2) Depvar: Retail Margin = Retail - Wholesale.", ///
             "Columns (3)-(4) Depvar: Markup Ratio = (Retail-Wholesale)/Wholesale.", ///
             "Interaction term tests whether competition alters wholesale pass-through.")

display as text "Done. RTF saved to: mechanism_results_ordered.rtf"

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	