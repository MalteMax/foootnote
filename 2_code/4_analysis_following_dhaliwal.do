*** load data
cd /Users/maltemax/surfdrive/projects/footnote
use "3_pipeline/1_intermediate/sample_dhaliwal.dta", clear


*** declare panel vars
xtset gvkey fyear


*** replace missings by zero (Dhaliwal footnote 4)
foreach var of varlist spi xrd glp sppiv pidom {
	replace `var' = 0 if `var' == .
}
* regarding pidom: dhaliwal state "We define a loss firm as a firm that reports negative income before extraordinary items (Compustat data item IB) and does not report positive U.S. pretax income (Compustat data item PIDOM)" >> the PIDOM part suggests that they don't use pidom < 0, and leaves unclear whether missings on PIDOM are replaced by zero; if I don't do that, lose many observations

*** generate lagged total assets and replace by 10 if below 10 (p. 139)
*** to minimize influence of outliers
gen L1_at = L1.at
replace L1_at = 10 if L1_at < 10

*** create variables
** valuation allowance
* log of VA
gen ln_va = log(va + 1)
* increase in VA (continuous)
gen d_va = D1.va / L1.va
* increase in VA (dummy)
gen d_va_d = 0 if d_va != .
replace d_va_d = 1 if d_va > 0 & d_va_d != .
* VA scaled by gross deferred tax asset
gen dta_ratio = va / dta_gross
replace dta_ratio = . if dta_ratio != . & (dta_ratio > 1 | dta_ratio < 0)
// replace dta_ratio = 0 if va == 0
* change in dta_ratio (continuous)
gen d_dta_ratio = D1.dta_ratio
* change in dta_ratio (dummy)
gen d_dta_ratio_d = 0 if d_dta_ratio != .
replace d_dta_ratio_d = 1 if d_dta_ratio > 0 & d_dta_ratio_d != .

** identify loss observations
gen loss = 0 if ib != . & pidom != .
replace loss = 1 if ib < 0 & pidom <= 0 &  loss != .
* future loss
gen F1_loss = F1.loss

** Dhaliwal tax categories
* BN
gen bn = 0 if txdfed != .
replace bn = 1 if txdfed >= 0 & bn != .
* GN_VA
gen gn_va = 0 if txfed != . & txdfed != .
replace gn_va = 1 if txfed <= 0 & txdfed < 0 & gn_va != .
* GN_TI
gen gn_ti = 0 if txfed != . & txdfed != .
replace gn_ti = 1 if txfed > 0 & txdfed < 0 & gn_ti != .
* taxcatg
gen taxcatg = 0 if bn == 1
replace taxcatg = 1 if gn_va == 1
replace taxcatg = 2 if gn_ti == 1

** earnings
gen earnings = ib / L1_at
gen F1_earnings = F1.earnings
gen F2_earnings = F2.earnings
gen F3_earnings = F3.earnings

** earnings quality
* cashflow
gen cashflow = (oancf - xidoc) / L1_at
* absolute delta of earnings
gen d_earnings = abs(D1.earnings)

** transitory items
* negspiw
// gen negspiw = spi / L1_at if spi < 0
gen negspiw = 1 if spi < 0
replace negspiw = 0 if spi >= 0 & spi != .
* negnop
// gen negnop = nopi / L1_at if nopi < 0
gen negnop = 1 if nopi < 0
replace negnop = 0 if nopi >= 0 & nopi != .
* negglis
// gen negglis = glp / L1_at if glp < 0
gen negglis = 1 if glp < 0
replace negglis = 0 if glp >= 0 & glp != .
* negglcf
// gen negglcf = sppiv / L1_at if sppiv > 0
gen negglcf = 1 if sppiv > 0
replace negglcf = 0 if sppiv <= 0 & sppiv != .

** growth prospects
* salesgrowth
gen L1_sale = L1.sale
gen salesgrowth = D1.sale / L1.sale

* age
by gvkey: egen first_fyear = min(fyear)
gen age = fyear - first_fyear
drop first_fyear
* r&d
gen rd = xrd / L1_at if xrd > 0
replace rd = 0 if xrd <= 0

** frequency of losses
* firstloss
gen firstloss = 0 if earnings !=. & L1.earnings != .
replace firstloss = 1 if earnings < 0 & L1.earnings > 0 & firstloss != .
* lossseq: created in Python
* bigloss
gen bigloss = 0 if earnings != .
replace bigloss = 1 if earnings < -0.8 & bigloss != .
** financial stability
* size
gen size = log(prcc_f * csho)
* divdum
gen divdum = 0 if dvc != .
replace divdum = 1 if dvc > 0 & divdum != .
* divstop
gen divstop = 0 if dvc != . & L1.dvc != .
replace divstop = 1 if dvc == 0 & L1.dvc > 0 & divstop != .


*** sample selection
** Dhaliwal timeframe: 1993-2008
keep if inrange(fyear, 1993, 2023)
// keep if inrange(fyear, 2008, 2023)
** drop if not incorporated in the US
keep if hfic == "USA"
** drop if book value of equity (CEQ) negative
// drop if ceq < 0
** drop if utility or financial
foreach i of num 49 60/69 {
	drop if sic_group == `i'
}
** drop if no loss observation
// keep if loss == 1
** drop if variables are missing
foreach var of varlist bn gn_va gn_ti earnings F1_earnings cashflow d_earnings negspiw negnop negglis negglcf salesgrowth age rd firstloss lossseq bigloss size divdum divstop {
	drop if `var' == .
}

*** winsorize
winsor2 earnings F1_earnings F2_earnings F3_earnings cashflow salesgrowth d_dta_ratio d_va d_earnings rd, cuts (2 98) replace

*** label variables for esttab
label var earnings "Earnings"
label var F1_earnings "Earnings\$_{t+1}\$"
label var d_va "$\Delta\$VA"
label var ln_va "ln(VA)"
label var dta_ratio "\%VA"
label var cashflow "Cashflow"
label var d_earnings "|\$\Delta\$Earnings|"
// label var negspiw "Negspiw"
// label var negnop "Negnop"
// label var negglis "Negglis"
// label var negglcf "Negglcf"
label var salesgrowth "Salesgrowth"
label var age "Age"
label var rd "R\&D"
label var lossseq "Losseq"
label var size "Size"

*** descriptive stats
estpost tabstat d_va ln_va dta_ratio earnings F1_earnings, c(stat) stat(n mean sd p1 p25 median p75 p99)

#delimit ;
esttab using "3_pipeline/1_intermediate/descriptives.tex",
	cell((count(label("{N}")) mean(label("{Mean}") fmt(%9.3f)) sd(label("{SD}") fmt(%9.3f)) p1(label("{P1}") fmt(%9.3f)) p25(label("{P25}") fmt(%9.3f)) p50(label("{P50}") fmt(%9.3f)) p75(label("{P75}") fmt(%9.3f)) p99(label("{P99}") fmt(%9.3f))))
	replace
	booktabs
	nonumber
	noobs
	nomtitle
	label
	substitute("\_" "_")
;
#delimit cr

*** define controls
global controls c.cashflow c.d_earnings i.negspiw i.negnop i.negglis i.negglcf c.salesgrowth c.age c.rd i.firstloss c.lossseq i.bigloss c.size i.divdum i.divstop


*** regressions
** earnings persistence: LOSS FIRMS
reghdfe F1_earnings c.earnings##(i.taxcatg) $controls if loss == 1, absorb(fyear sic_group) cluster(gvkey fyear)
// matrix list e(b) 
estimates store model1
reghdfe F1_earnings c.earnings##(c.d_va) $controls if loss == 1, absorb(fyear sic_group) cluster(gvkey fyear)
estimates store model2
reghdfe F1_earnings c.earnings##(c.ln_va) $controls if loss == 1, absorb(fyear sic_group) cluster(gvkey fyear)
estimates store model3
reghdfe F1_earnings c.earnings##(c.dta_ratio) $controls if  loss == 1, absorb(fyear sic_group) cluster(gvkey fyear)
estimates store model4

// estfe model*, labels(fyear "Year FE" sic_group "Industry FE")
#delimit ;
esttab model* using "3_pipeline/1_intermediate/results_earnings_persistence_loss_firms.tex",
	b(3)
	se(2)
	indicate(`r(indicate_fe)')
	abs
	noconstant
	booktabs
	replace
	nobaselevels
	nogaps
	star(* 0.10 ** 0.05 *** 0.01)
	label
	se
	coeflabel(1.taxcatg "GN_VA" 2.taxcatg "GN_TI" 1.taxcatg#c.earnings "Earnings $\times$ GN_VA" 2.taxcatg#c.earnings "Earnings $\times$ GN_TI" 1.firstloss "Firstloss" 1.bigloss "Bigloss" 1.divdum "Divdum" 1.divstop "Divstop" 1.negspiw "Negspiw" 1.negnop "Negnop" 1.negglis "Negglis" 1.negglcf "Negglcf")
	order(earnings 1.taxcatg 2.taxcatg 1.taxcatg#c.earnings 2.taxcatg#c.earnings d_va c.earnings#c.d_va ln_va c.earnings#c.ln_va dta_ratio c.earnings#c.dta_ratio) mtitles("Dhaliwal" "\$\Delta\$VA" "ln(VA)" "\%VA")
;
#delimit cr


** earnings peristence: PROFIT FIRMS
reghdfe F1_earnings c.earnings##(i.taxcatg) $controls if loss == 0, absorb(fyear sic_group) cluster(gvkey fyear)
// matrix list e(b) 
estimates store model1
reghdfe F1_earnings c.earnings##(c.d_va) $controls if loss == 0, absorb(fyear sic_group) cluster(gvkey fyear)
estimates store model2
reghdfe F1_earnings c.earnings##(c.ln_va) $controls if loss == 0, absorb(fyear sic_group) cluster(gvkey fyear)
estimates store model3
reghdfe F1_earnings c.earnings##(c.dta_ratio) $controls if  loss == 0, absorb(fyear sic_group) cluster(gvkey fyear)
estimates store model4

// estfe model*, labels(fyear "Year FE" sic_group "Industry FE")
#delimit ;
esttab model* using "3_pipeline/1_intermediate/results_earnings_persistence_profit_firms.tex",
	b(3)
	se(2)
	indicate(`r(indicate_fe)')
	abs
	noconstant
	booktabs
	replace
	nobaselevels
	nogaps
	star(* 0.10 ** 0.05 *** 0.01)
	label
	se
	coeflabel(1.taxcatg "GN_VA" 2.taxcatg "GN_TI" 1.taxcatg#c.earnings "Earnings $\times$ GN_VA" 2.taxcatg#c.earnings "Earnings $\times$ GN_TI" 1.firstloss "Firstloss" 1.bigloss "Bigloss" 1.divdum "Divdum" 1.divstop "Divstop" 1.negspiw "Negspiw" 1.negnop "Negnop" 1.negglis "Negglis" 1.negglcf "Negglcf")
	order(earnings 1.taxcatg 2.taxcatg 1.taxcatg#c.earnings 2.taxcatg#c.earnings d_va c.earnings#c.d_va ln_va c.earnings#c.ln_va dta_ratio c.earnings#c.dta_ratio) mtitles("Dhaliwal" "\$\Delta\$VA" "ln(VA)" "\%VA")
;
#delimit cr

