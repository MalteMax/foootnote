*** dependencies
*ssc install ebalance, all replace



*** load data
cd "/Users/maltemax/ownCloud/SBE_ACC_Replacement_Surfdrive (Projectfolder)/SBE_ACC_Replacement_Surfdrive (Projectfolder)/projects/footnote"
use "3_pipeline/1_intermediate/sample.dta", clear


*** declare panel vars
xtset gvkey fyear


*** replace missing tlcf values by tlcf_imputed
replace tlcf = tlcf_imputed if tlcf == . & tlcf_imputed != .


*** get diff between tlcf and sum of (tlcf_pre_2018 + tlcf_post_2018); drop if there is a difference
gen diff = tlcf - tlcf_pre_2018 - tlcf_post_2018
drop if diff > 0.1 & diff != .
drop diff


*** replace missings by zero (Dhaliwal footnote 4)
foreach var of varlist spi xrd glp sppiv {
	replace `var' = 0 if `var' == .
}

*** generate lagged total assets and replace by 10 if below 10 (p. 139)
*** to minimize influence of outliers
gen L1_at = L1.at
// replace L1_at = 10 if L1_at < 10 //taken this out because it seems unreasonable?

*** create variables
** VA scaled by gross deferred tax asset
gen dta_ratio = va / dta_gross
replace dta_ratio = . if dta_ratio != . & (dta_ratio > 1 | dta_ratio < 0)
** share of indefinite TLCFs
gen tlcf_indefin = tlcf_post_2018 / tlcf
** identify loss observations
gen loss_d = 0 if ib != .
gen us_positive_pretax_inc = 1 if pidom > 0 & pidom != .
replace us_positive_pretax_inc = 0 if us_positive_pretax_inc == . // the dummy is 0 when: 1) firms report negative U.S. income or when they do not report any U.S. income (i.e., when it is missing)
replace loss_d = 1 if ib < 0 & us_positive_pretax_inc == 0 & loss_d != .
replace loss_d = . if ib < 0 & loss_d == 0
* there are a few observations that have a pidom > 0 but ib < 0; according to the definition above (which follows Dhaliwal), these are no loss firms because pidom is > 0; and so they become profit firms - which also is incorrect; I set these to missing
* regarding pidom: dhaliwal state "We define a loss firm as a firm that reports negative income before extraordinary items (Compustat data item IB) and does not report positive U.S. pretax income (Compustat data item PIDOM)" >> the PIDOM part suggests that they don't use pidom < 0 to identify "does not report positive U.S. pretax income", but rather that they only check whether pidom is available and positive, and then define all other cases to be "anything that is not <<does not report positive U.S. pretax income>>". This makes sense because pidom is very often missing.

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

** earnings/loss
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
// gen negspiw = spi / L1_at
gen negspiw = 1 if spi < 0
replace negspiw = 0 if spi >= 0 & spi != .
* negnop
// gen negnop = nopi / L1_at
gen negnop = 1 if nopi < 0
replace negnop = 0 if nopi >= 0 & nopi != .
* negglis
// gen negglis = glp / L1_at
gen negglis = 1 if glp < 0
replace negglis = 0 if glp >= 0 & glp != .
* negglcf
// gen negglcf = sppiv / L1_at
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
gen rd = xrd / L1_at
replace rd = . if xrd < 0

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

** gen post_2018 dummy
gen post_2018 = 0
replace post_2018 = 1 if fyear >= 2018


** variables for value relevance regressions
gen ib_ps = (ni + D1.va) / csho if csho >= 1
gen at_ps = (at + va - dta_gross) / csho if csho >= 1
gen lt_ps = lt / csho if csho >= 1
gen dta_gross_ps = dta_gross / csho if csho >= 1
gen va_ps = va / csho if csho >= 1


** for inspection: lagged TLCF
gen L1_tlcf = L1.tlcf


*** for descriptive inspection
** three-year cumulative loss
rangestat (sum) ib (count) ib, interval (fyear -2 0) by(gvkey)
replace ib_sum = . if ib_count < 2
** other variables
gen zero = 0
replace zero = 1 if dta_ratio == 0
gen partial = 0
replace partial = 1 if dta_ratio > 0 & dta_ratio < 1
gen full = 0
replace full = 1 if dta_ratio >= 1 & dta_ratio != .


global controls c.d_earnings i.negspiw i.negnop i.negglis i.negglcf c.salesgrowth c.age c.rd i.firstloss c.lossseq i.bigloss c.size i.divdum i.divstop




*** sample selection
keep if inrange(fyear, 2009, 2024)
** drop if not incorporated in the US
keep if hfic == "USA"
** drop if lagged total assets <= 1 (million)
keep if L1_at >= 10
** drop if utility or financial
foreach i of num 49 60/69 {
	drop if sic_group == `i'
}
** drop if variables are missing
foreach var of varlist dta_ratio tlcf_indefin earnings F1_earnings cashflow d_earnings negspiw negnop negglis negglcf salesgrowth age rd firstloss lossseq bigloss size divdum divstop {
	drop if `var' == .
}

foreach var of varlist va_ps ib_ps at_ps {
	drop if `var' == .
}

// hist tlcf_indefin if tlcf_indefin <= 1 & loss == 0 & fyear >= 2018

** save for cohort formation in Python
cd "/Users/maltemax/ownCloud/SBE_ACC_Replacement_Surfdrive (Projectfolder)/SBE_ACC_Replacement_Surfdrive (Projectfolder)/projects/footnote/3_pipeline/1_intermediate"
save "sample_for_cohort_formation.dta", replace



** use pre-cohort sample for baseline results
cd "/Users/maltemax/ownCloud/SBE_ACC_Replacement_Surfdrive (Projectfolder)/SBE_ACC_Replacement_Surfdrive (Projectfolder)/projects/footnote/3_pipeline/1_intermediate"
use "sample_for_cohort_formation.dta", replace
xtset gvkey fyear


br gvkey hcik name fyear zero partial full va dta_ratio tlcf_pre_2018 tlcf_post_2018 tlcf_indefin if fyear >= 2018 & ib_sum < 0

tabstat zero partial full if fyear >= 2012, by(fyear) s(mean)
tabstat zero partial full if fyear >= 2012 & ib_sum < 0, by(fyear) s(mean)
tabstat zero partial full if fyear >= 2012 & ib_sum > 0, by(fyear) s(mean)



*** winsorize
winsor2 d_earnings rd, cuts(0 99) replace
winsor2 earnings F1_earnings F2_earnings F3_earnings cashflow salesgrowth, cuts (1 99) replace


global controls c.cashflow c.d_earnings i.negspiw i.negnop i.negglis i.negglcf c.salesgrowth c.age c.rd i.firstloss c.lossseq i.bigloss c.size i.divdum i.divstop

** replicating Dhaliwal resuts
reghdfe F1_earnings c.earnings##(c.dta_ratio) $controls if  loss_d == 1, absorb(fyear sic_group) cluster(gvkey fyear)
reghdfe F1_earnings c.earnings##(c.dta_ratio) $controls if  loss_d == 0, absorb(fyear sic_group) cluster(gvkey fyear)


** pre/post analysis using firms with no VA

reghdfe F1_earnings c.earnings##c.dta_ratio##i.post_2018 $controls if loss_d == 1 & dta_ratio == 0, absorb(fyear sic_group) cluster(gvkey fyear)
reghdfe F1_earnings c.earnings##c.dta_ratio##i.post_2018 $controls if loss_d == 0 & dta_ratio == 0, absorb(fyear sic_group) cluster(gvkey fyear)

reghdfe F1_earnings c.earnings##c.dta_ratio##i.post_2018 $controls if  loss_d == 0, absorb(fyear sic_group) cluster(gvkey fyear)



*** winsorize
winsor2 prcc_f va_ps at_ps lt_ps dta_gross_ps, cuts(0 99) replace
winsor2 ib_ps, cuts (1 99) replace


reghdfe prcc_f ib_ps at_ps lt_ps dta_ratio, absorb(fyear sic_group) cluster(gvkey fyear)



*** load data
cd "/Users/maltemax/ownCloud/SBE_ACC_Replacement_Surfdrive (Projectfolder)/SBE_ACC_Replacement_Surfdrive (Projectfolder)/projects/footnote/3_pipeline/1_intermediate"
use "sample_cohorts.dta", clear

// keep if loss_d == 1


drop zero partial full
gen zero = 0
replace zero = 1 if dta_ratio <= 0.1
gen partial = 0
replace partial = 1 if dta_ratio > 0.1 & dta_ratio < 0.9
gen full = 0
replace full = 1 if dta_ratio >= 0.9

reghdfe partial i.treated##i.post, noabsorb

*** create year/firm FE by cohort
egen fyear_by_cohort = group(fyear cohort)
egen gvkey_by_cohort = group(gvkey cohort)

*** define controls for regressions
global controls c.cashflow c.d_earnings i.negspiw i.negnop i.negglis i.negglcf c.salesgrowth c.age c.rd i.firstloss c.lossseq i.bigloss c.size i.divdum i.divstop

*** winsorize
winsor2 prcc_f va_ps at_ps lt_ps, cuts(0 99) replace
winsor2 ib_ps, cuts (1 99) replace

winsor2 d_earnings rd, cuts(0 99) replace
winsor2 earnings F1_earnings F2_earnings F3_earnings cashflow salesgrowth, cuts (1 99) replace



*** covariate balance
ebalance treated $controls, tar(2) generate(entropy_weight)

reghdfe F1_earnings c.earnings##c.dta_ratio##i.treated##(i.post) $controls if loss_d == 1 [pweight = entropy_weight], absorb(fyear_by_cohort gvkey_by_cohort) cluster(gvkey fyear)



reghdfe F1_earnings c.earnings##c.dta_ratio##i.treated##(i.post_min3 i.post_min2 i.post_0 i.post_plus1 i.post_plus2) $controls if loss_d == 1 [pweight = entropy_weight], absorb(fyear_by_cohort gvkey_by_cohort) cluster(gvkey fyear)


reghdfe F1_earnings c.earnings##c.dta_ratio##i.treated##(i.post_min3 i.post_min2 i.post_0 i.post_plus1 i.post_plus2) $controls if loss_d == 0 [pweight = entropy_weight], absorb(fyear_by_cohort gvkey_by_cohort) cluster(gvkey fyear)


**** VR tests
reghdfe prcc_f c.ib_ps at_ps lt_ps c.dta_ratio, absorb(fyear sic_group) cluster(gvkey fyear)
ebalance treated ib_ps at_ps lt_ps, tar(2)
reghdfe prcc_f c.ib_ps at_ps lt_ps c.dta_ratio##i.treated##i.post [pweight = _webal], absorb(fyear_by_cohort gvkey_by_cohort) cluster(gvkey fyear)

reghdfe prcc_f c.ib_ps at_ps lt_ps c.dta_ratio##i.treated##(i.post_min3 i.post_min2 i.post_0 i.post_plus1 i.post_plus2) [pweight = _webal], absorb(fyear_by_cohort gvkey_by_cohort) cluster(gvkey fyear)




*** label variables for esttab
label var earnings "Earnings"
label var F1_earnings "Earnings\$_{t+1}\$"
label var d_va "$\Delta\$VA"
label var ln_va "ln(VA)"
label var dta_ratio "\%VA"
label var cashflow "Cashflow"
label var d_earnings "|\$\Delta\$Earnings|"
label var negspiw "Negspiw"
label var negnop "Negnop"
label var negglis "Negglis"
label var negglcf "Negglcf"
label var salesgrowth "Salesgrowth"
label var age "Age"
label var rd "R\&D"
label var lossseq "Losseq"
label var size "Size"

*** descriptive stats
** loss observations
qui count if dta_ratio == 0 & loss == 1
local count_zero = r(N)

estpost tabstat dta_ratio earnings F1_earnings  if loss == 1, c(stat) stat(n mean sd p1 p25 median p75 p99)

#delimit ;
esttab using "3_pipeline/1_intermediate/descriptives_loss.tex",
	cell((count(label("{N}")) mean(label("{Mean}") fmt(%9.3f)) sd(label("{SD}") fmt(%9.3f)) p1(label("{P1}") fmt(%9.3f)) p25(label("{P25}") fmt(%9.3f)) p50(label("{P50}") fmt(%9.3f)) p75(label("{P75}") fmt(%9.3f)) p99(label("{P99}") fmt(%9.3f))))
	refcat(dta_ratio "$\textnormal{\%VA} = 0$", label(`count_zero') below)
	replace
	booktabs
	nonumber
	noobs
	collabels(none)
	label
	substitute("\_" "_")
;
#delimit cr

** profit observations
qui count if dta_ratio == 0 & loss == 0
local count_zero = r(N)

estpost tabstat dta_ratio earnings F1_earnings if loss == 0, c(stat) stat(n mean sd p1 p25 median p75 p99)

#delimit ;
esttab using "3_pipeline/1_intermediate/descriptives_profit.tex",
	cell((count(label("{N}")) mean(label("{Mean}") fmt(%9.3f)) sd(label("{SD}") fmt(%9.3f)) p1(label("{P1}") fmt(%9.3f)) p25(label("{P25}") fmt(%9.3f)) p50(label("{P50}") fmt(%9.3f)) p75(label("{P75}") fmt(%9.3f)) p99(label("{P99}") fmt(%9.3f))))
	refcat(dta_ratio "$\textnormal{\%VA} = 0$", label(`count_zero') below)
	replace
	booktabs
	nonumber
	noobs
	collabels(none)
	label
	substitute("\_" "_")
;
#delimit cr


*** regressions
** earnings persistence: LOSS FIRMS
reghdfe F1_earnings c.earnings##(i.bn) $controls if loss_d == 1, absorb(fyear sic_group) cluster(gvkey fyear)
// matrix list e(b) 
estimates store model1
reghdfe F1_earnings c.earnings##(c.dta_ratio) $controls if  loss_d == 1, absorb(fyear sic_group) cluster(gvkey fyear)

** stacked regression

reghdfe F1_earnings c.earnings##c.dta_ratio#i.treated_post $controls if  loss_d == 1, absorb(fyear#cohort treated#cohort sic_group#cohort) cluster(gvkey fyear)


estimates store model2

** earnings persistence: PROFIT FIRMS
reghdfe F1_earnings c.earnings##(i.bn) $controls if loss_d == 0, absorb(fyear sic_group) cluster(gvkey fyear)
estimates store model3
reghdfe F1_earnings c.earnings##(c.dta_ratio) $controls if  loss_d == 0, absorb(fyear sic_group) cluster(gvkey fyear)
estimates store model4


// estfe model*, labels(fyear "Year FE" sic_group "Industry FE")
#delimit ;
esttab model* using "3_pipeline/1_intermediate/results_earnings_persistence.tex",
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
	coeflabel(1.bn "BN" 1.bn#c.earnings "Earnings $\times$ BN" 1.firstloss "Firstloss" 1.bigloss "Bigloss" 1.divdum "Divdum" 1.divstop "Divstop" 1.negspiw "Negspiw" 1.negnop "Negnop" 1.negglis "Negglis" 1.negglcf "Negglcf")
	order(earnings 1.bn 1.bn#c.earnings dta_ratio c.earnings#c.dta_ratio) mtitles("Loss obs." "Loss obs." "Profit obs." "Profit obs.")
;
#delimit cr
