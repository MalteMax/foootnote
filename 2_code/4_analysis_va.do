*** load data
cd /Users/maltemax/surfdrive/projects/footnote
use "3_pipeline/1_intermediate/sample.dta", clear


*** declare panel vars
xtset gvkey fyear


** get 2-digit SIC
gen sic_twodigit = floor(sic / 100)


*** variables
* VA dummy
gen va_d = 0 if va != .
replace va_d = 1 if va > 0 & va != .
* logarithm of valuation allowance
gen ln_va = ln(va + 1)
* logarithm of tlcf
gen ln_tlcf = ln(tlcf + 1)
* size
gen size = ln(at)
* profit indicator
gen profit = 0 if ni != .
replace profit = 1 if ni > 0 & profit != .
* market-to-book
gen mtb = (at - ceq + prcc_f * csho) / at
label var mtb "MTB"
* leverage
gen lev = (dlc + dltt) / at
label var lev "Leverage"
* cash flow
gen cfo = oancf / at
* return on assets
gen roa = ni / at
* tax expense
gen tax = txt / at
* mne dummy CHECK IF CORRECT WITH MISSINGS
gen pifo_d = 0
replace pifo_d = 1 if pifo != 0 & pifo != .
gen txdfo_d = 0
replace txdfo_d = 1 if txdfo != 0 & txdfo != .
generate txfo_d = 0
replace txfo_d = 1 if txfo != 0 & txfo != .
gen mne = 0
replace mne = 1 if pifo_d == 1 | txdfo_d == 1 | txfo_d == 1


*** drop
keep if fyear >= 2012 & fyear <= 2023
* if footnote is shorter than X words
* if less than $10m total assets (to avoid small denominator problems)
drop if at < 10
// * if less than 100 words in footnote
// drop if words < 100
* if incorporated outside US
keep if fic == "USA"
* if financials and regulated
foreach i of num 49 60/69 {
	drop if sic_twodigit == `i'
}

save "3_pipeline/1_intermediate/sample_clean.dta", replace

*** winsorize
// * only top 99
// foreach var of varlist va_scaled tlcf_scaled {
// 	winsor2 `var', cuts(0 99) replace
// }
// * bottom 1 and top 99
// foreach var of varlist car3 car5 cfo roa tax sim_tax L1_sim_tax {
// 	winsor2 `var', cuts(1 99) replace
// }




*** declare
* controls
// global controls
global controls size roa sue_abs lev mtb numest
* fixed effects
global fe fyear sic_twodigit
* standard error clustering
global se cluster gvkey


*** regressions
foreach year of numlist 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 {
	reghdfe ln_va ln_tlcf if fyear == `year', absorb($fe) vce($se)
}




*** CAAR regressions
local cos_var cos_tax
foreach var of varlist abhar_1 abhar_3 abhar_30 abhar_60 abhar_90 abhar_180 abhar_360 {
	qui reghdfe `var' `cos_var' $controls cos_footnote_rest, absorb($fe) vce($se)
	esttab, beta keep(`cos_var') star(* 0.10 ** 0.05 *** 0.01)
}

* for standardised coefficients after reghdfe: esttab, beta not


// cd /Users/maltemax/surfdrive/projects/footnote/3_pipeline/2_final
// reghdfe caar3 ln_words $controls, absorb($fe) vce($se)
// outreg2 using CAAR_results.doc, label tex(frag) title("Cumulative Absolute Abnormal Returns") dec(3) adjr2 se nocons nonote addnote("All estimations include year and industry fixed effects. Robust standard errors clustered at the firm level are in parantheses.")
// reghdfe caar3 ln_numbers $controls, absorb($fe) vce($se)
// outreg2 using CAAR_results.doc, label tex(frag) dec(3) adjr2 se nocons nonote sortvar(ln_words ln_numbers $controls)
// reghdfe caar5 ln_words $controls, absorb($fe) vce($se)
//
//
// *** FO regressions
reghdfe prcc_f at_ps ni_ps c.dta_ps##c.sim_tax, absorb($fe) vce($se)
// outreg2 using FO_results.doc, label tex(frag) title("Market value") dec(3) adjr2 se nocons nonote addnote("All estimations include year and industry fixed effects. Robust standard errors clustered at the firm level are in parantheses.")
// reghdfe prcc_f_tr at_ps_tr ni_ps_tr dta_ps_tr, absorb($fe) vce($se)
