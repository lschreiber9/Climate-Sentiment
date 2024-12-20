*ssc install drdid
*ssc install csdid


//// Graph for Poster 

use final_data.dta, clear

twoway (scatter Sentiment believer if Year_ESG == 2021, mcolor(maroon%50)) (scatter Sentiment believer if Year_ESG == 2022, mcolor(nav%50)) (scatter Sentiment believer if Year_ESG == 2023, mcolor(dkgreen%50)) (lfitci Sentiment believer, lcolor(black) alc(white) acolor(gs10%30)), xlab(50 (10) 80,format(%4.0g)) xtitle("Belief that Climate Change affects the Weather (Percentage)") ytitle(ESG Sentiment Score) saving(belief_t, replace) title("ESG Sentiment and Climate Beliefs (Reporting Year)") legend(order(1 "2021" 2 "2022" 3 "2023") pos(2) ring(0) subtitle("Year") col(1) region(lcolor(black) margin(left right))) note("Note: This graph presents the stylized relationship between county level climate beliefs and ESG sentiment across Firms' locations." "Climate beliefs are surveyed in the reporting year of the ESG report. The diagram additionaly shows a fitted linear function" "including the 95% confidence band. Data:  Climate beliefs from Howe et al. (2015), Sentiment Scores from textual analysis.", size(vsmall) pos(6) justification(left))

graph export "/Users/ibrahimbenaraar/Desktop/graph_big_dta.png", replace

//// Relationship Sentiment  and Climatebeliefs

eststo: quietly reghdfe Sentiment beliefs Year_ESG, absorb(i.Number i.Statedum) vce(cluster Countydum)
estadd local a "Yes"
estadd local bc "Yes"
estadd local c "Yes"
eststo:quietly reghdfe Sentiment beliefcorp Year_ESG,  absorb(i.Number i.Statedum) vce(cluster Countydum)
estadd local a "Yes"
estadd local bc "Yes"
estadd local c "Yes"
eststo:quietly reghdfe Sentiment believer Year_ESG,  absorb( i.Number i.Statedum) vce(cluster Countydum)
estadd local a "Yes"
estadd local bc "Yes"
estadd local c "Yes"
eststo:quietly reghdfe Sentiment corpbeliever Year_ESG,  absorb(i.Number i.Statedum) vce(cluster Countydum)
estadd local a "Yes"
estadd local bc "Yes"
estadd local c "Yes"
esttab, compress label star(* 0.10 ** 0.05 *** 0.01) r2 keep(beliefs believer beliefcorp corpbeliever) scalars("a Firm FE" "bc State FE" "c Linear Time trends")

eststo clear


use final_data.dta, clear
bys Number: gen firstperiod = 1 if affected_county_year_ESG == 1 & Year_ESG == 2021
bys Number : egen first = min(firstperiod)
bys Number: gen secondperiod = 1 if affected_county_year_ESG == 1 & Year_ESG == 2022
bys Number : egen second = min(secondperiod)
bys Number: gen thirdperiod = 1 if  affected_county_year_ESG == 1 & Year_ESG == 2023
bys Number : egen third = min(thirdperiod)
bys Number: gen neverperiod = 1 if first ==. & second == . & third == . 
bys Number : egen never = min(neverperiod)

egen mean_first = mean(Sentiment), by(Year_ESG), if first ==1
egen mean_second = mean(Sentiment), by(Year_ESG), if second ==1
egen mean_third = mean(Sentiment),  by(Year_ESG), if third ==1
egen mean_never = mean(Sentiment), by(Year_ESG), if never == 1 

collapse (mean) mean_first mean_second mean_third mean_never, by(Year_ESG)

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// MOVEMENT OF ESG SENTIMENT FOR THE MAP  ///////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////

use final_data.dta, clear
xtset Year_ESG Number
gen change_sentiment = Sentiment - l.Sentiment
bys Number: gen firstperiod = 1 if affected == 1 & Year_ESG == 2021
bys Number : egen first = min(firstperiod)
bys Number: gen secondperiod = 1 if affected == 1 & Year_ESG == 2022
bys Number : egen second = min(secondperiod)
bys Number: gen thirdperiod = 1 if affected == 1 & Year_ESG == 2023
bys Number : egen third = min(thirdperiod)

gen treated = affected
bysort Number (Year_ESG): replace treated = 1 if affected[_n-1] == 1
gen yearxtreat = 0
replace yearxtreat = 2021 if first == 1 
replace yearxtreat = 2022 if second == 1
replace yearxtreat = 2023 if third == 1
bysort Number: egen gvar = max(yearxtreat)

gen pos_movement_early = . 
replace pos_movement_early = 1 if Year_ESG == 2022 & change_sentiment > 0 & gvar == 2022
gen neg_movement_early =.
replace neg_movement_early = 1 if Year_ESG == 2022 & change_sentiment < 0 & gvar == 2022
gen pos_movement_late = . 
replace pos_movement_late = 1 if Year_ESG == 2023 & change_sentiment > 0 & gvar == 2023
gen neg_movement_late = . 
replace neg_movement_late = 1 if Year_ESG == 2023 & change_sentiment < 0 & gvar == 2023

preserve 
keep if pos_movement_early == 1 
export delimited "/Users/ibrahimbenaraar/Desktop/positivo_2022.csv", replace
restore
preserve 
keep if neg_movement_early == 1 
export delimited "/Users/ibrahimbenaraar/Desktop/negativo_2022.csv", replace
restore
preserve 
keep if pos_movement_late == 1 
export delimited "/Users/ibrahimbenaraar/Desktop/positivo_2023.csv", replace
restore
preserve 
keep if neg_movement_late == 1 
export delimited "/Users/ibrahimbenaraar/Desktop/negativo_2023.csv", replace
restore
preserve
keep if gvar == 0 & Year_ESG == 2021
export delimited "/Users/ibrahimbenaraar/Desktop/controls.csv", replace 
restore

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// REGRESSION ANALYSIS ////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////

use final_data.dta, clear
xtset Year_ESG Number
gen treated = affected
bysort Number (Year_ESG): replace treated = 1 if affected[_n-1] == 1
gen treated_county = affected_county
bysort Number (Year_ESG): replace treated_county = 1 if affected_county[_n-1] == 1
gen treat = affected_year_ESG 
bysort Number (Year_ESG): replace treat = 1 if affected_year_ESG[_n-1] == 1
gen treat_county = affected_county_year_ESG
bysort Number (Year_ESG): replace treat_county = 1 if affected_county_year_ESG[_n-1] == 1


bys Number: gen fpd = 1 if affected == 1 & Year_ESG == 2021
bys Number : egen first = min(fpd)
bys Number: gen fpd_county = 1 if affected_county == 1 & Year_ESG == 2021
bys Number : egen first_county = min(fpd_county)
bys Number: gen fpd_yesg = 1 if affected_year_ESG == 1 & Year_ESG == 2021
bys Number : egen first_yesg = min(fpd_yesg)
bys Number: gen fpd_county_yesg = 1 if affected_county_year_ESG == 1 & Year_ESG == 2021
bys Number : egen first_county_yesg = min(fpd_county_yesg)

bys Number: gen second_yesg = 1 if affected_year_ESG == 1 & Year_ESG == 2022 & first_yesg !=1
bys Number : egen scond_yesg = min(second_yesg)

bys Number: gen third_yesg = 1 if affected_year_ESG == 1 & Year_ESG == 2023 & first_yesg !=1 & scond_yesg !=1
bys Number : egen thrd_yesg = min(third_yesg)

gen t= Year_ESG-2020

//// Preferred Estimation - Affected in ESG Year 
eststo: quietly areg Sentiment treat if first_yesg !=1, vce(cluster Countydum) absorb(Number Statedum Countydum)
estadd local a "No"
estadd local bc "No"
eststo: quietly areg Sentiment treat t if first_yesg !=1, vce(cluster Countydum) absorb(Number Statedum Countydum)
estadd local a "Yes"
estadd local bc "No"
eststo: quietly areg Sentiment treat t believer if first_yesg !=1, vce(cluster Countydum) absorb(Number Statedum Countydum)
estadd local a "Yes"
estadd local bc "Yes"
esttab using "/Users/ibrahimbenaraar/Desktop/table1.tex", compress star(* 0.10 ** 0.05 *** 0.01) r2(3) keep(treat) scalars("a Linear Time Trennds" "bc Belief Control" F) nomtitles nonumbers collabels("ESG Sentiment") se(2) replace
eststo clear


eststo: quietly areg Sentiment treat c.t#i.Countydum if first_yesg !=1, vce(cluster Countydum) absorb(Number Statedum)
estadd local a "No"
estadd local bc "Yes"
estadd local c "No"
estadd local da "No"
estadd local we "No"
eststo: quietly areg Sentiment treat c.t#i.Statedum if first_yesg !=1, vce(cluster Countydum) absorb(Number Statedum)
estadd local a "No"
estadd local bc "No"
estadd local c "Yes"
estadd local da "No"
estadd local we "No"


esttab, compress star(* 0.10 ** 0.05 *** 0.01) r2(3) keep(treat t 1.treat) scalars("a Linear Time Trennds" "bc County Specific Time Trends" "c State Specific Time Trends" "da Belief Control" "we Treatment*Beliefs Effect")

eststo clear



/// Callaway-Sant'Anna Estimator 

gen gvar =.
replace gvar = 0 if first_yesg == . & scond_yesg ==. & thrd_yesg == .
replace gvar = 2022 if scond_yesg == 1
replace gvar = 2023 if thrd_yesg == 1

csdid Sentiment treat, ivar(Number) time(Year_ESG) gvar(gvar)
estat simple


/// Conley Standard Errors: Code by Hsiang (PNAS 2010)
/// Need to run the ols_spatial_HAC.ado before running this code. Ado File freely avaliable at: http://www.fight-entropy.com/2010/06/standard-error-adjustment-ols-for.html 

preserve 
drop if first_yesg == 1
ols_spatial_HAC Sentiment treat, lat(Latitude) lon(Longitude) time(Year_ESG) panel(Number) distcutoff(50) lagcutoff(2) disp star
restore

preserve 
drop if first_yesg == 1
ols_spatial_HAC Sentiment treat t, lat(Latitude) lon(Longitude) time(Year_ESG) panel(Number) distcutoff(50) lagcutoff(2) disp star
restore

preserve 
drop if first_yesg == 1
ols_spatial_HAC Sentiment treat t believer, lat(Latitude) lon(Longitude) time(Year_ESG) panel(Number) distcutoff(50) lagcutoff(2) disp star
restore


///// Apppendix - Additonal Material 



//// Descriptive Statistics - Additional Relationships: Climate Beliefs and ESG Sentiment

use "/Users/ibrahimbenaraar/Desktop/bigdata/final_data.dta", clear

quietly: twoway (scatter Sentiment beliefs if Year_ESG == 2021, mcolor(maroon%50)) (scatter Sentiment beliefs if Year_ESG == 2022, mcolor(nav%50)) (scatter Sentiment beliefs if Year_ESG == 2023, mcolor(dkgreen%50)) (lfitci Sentiment beliefs, lcolor(black) alc(white) acolor(gs10%30)), xlab(50 (10) 85,format(%4.0g)) xtitle("Belief that Climate Change affects the Weather (Percentage)") ytitle(ESG Sentiment Score) legend(off) saving(belief_t_1, replace) 

quietly: twoway (scatter Sentiment beliefcorp if Year_ESG == 2021, mcolor(maroon%50)) (scatter Sentiment beliefcorp if Year_ESG == 2022, mcolor(nav%50)) (scatter Sentiment beliefcorp if Year_ESG == 2023, mcolor(dkgreen%50)) (lfitci Sentiment beliefcorp, lcolor(black) alc(white) acolor(gs10%30)), xlab(60 (10) 85,format(%4.0g)) xtitle("Belief that Corporations should do more to combat Climate Change (Percentage)") ytitle(ESG Sentiment Score) legend(off) saving(corp_t_1, replace)

quietly: twoway (scatter Sentiment believer if Year_ESG == 2021, mcolor(maroon%50)) (scatter Sentiment believer if Year_ESG == 2022, mcolor(nav%50)) (scatter Sentiment believer if Year_ESG == 2023, mcolor(dkgreen%50)) (lfitci Sentiment believer, lcolor(black) alc(white) acolor(gs10%30)), xlab(50 (10) 85,format(%4.0g)) xtitle("Belief that Climate Change affects the Weather (Percentage)") ytitle(ESG Sentiment Score) legend(off) saving(belief_t, replace) 

quietly: twoway (scatter Sentiment corpbeliever if Year_ESG == 2021, mcolor(maroon%50)) (scatter Sentiment corpbeliever if Year_ESG == 2022, mcolor(nav%50)) (scatter Sentiment corpbeliever if Year_ESG == 2023, mcolor(dkgreen%50)) (lfitci Sentiment corpbeliever, lcolor(black) alc(white) acolor(gs10%30)), xlab(60 (10) 85,format(%4.0g)) xtitle("Belief that Corporations should do more to combat Climate Change (Percentage)") ytitle(ESG Sentiment Score) legend(order(1 "2021" 2 "2022" 3 "2023") pos(2) ring(0) region(lcolor(black))) saving(corp_t, replace)

quietly: graph combine belief_t.gph corp_t.gph, title("Panel 1: ESG Sentiment and Climate Beliefs (Reporting Year)", size(medsmall)) saving(combi1, replace)
quietly: graph combine belief_t_1.gph corp_t_1.gph, title("Panel 2: ESG Sentiment and Climate Beliefs (Year prior to reporting Year)", size(medsmall)) saving(combi2, replace)

graph combine combi1.gph combi2.gph, col(1) note("Note: This graph shows the stylized relationship between climate beliefs at the county level and ESG sentiment across the Firms' locations. Panel 1 uses the values for climate beliefs" "of the ESG reporting year. Panel 2 uses the values for the year prior to the reporting year. All Diagrams additionaly show a fitted linear function including the 95% confidence band." "The datapoints refer to the repsective ESG reporting year. Data:  Climate beliefs from Howe et al. (2015), Sentiment Scores from textual analysis.", size(vsmall) pos(6) justification(left)) title("Sentiment of ESG Reports and Opinions on Climate Change", size(medium))

graph export "/Users/ibrahimbenaraar/Desktop/append.png"

graph drop _all


////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// Analysis: Affected (County or Neighbouring State) - Treatment in Year t-1 of ESG Report /////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////// 
use final_data.dta, clear

bys Number: gen firstperiod = 1 if affected == 1 & Year_ESG == 2021
bys Number : egen first = min(firstperiod)
bys Number: gen secondperiod = 1 if affected == 1 & Year_ESG == 2022
bys Number : egen second = min(secondperiod)
bys Number: gen thirdperiod = 1 if affected == 1 & Year_ESG == 2023
bys Number : egen third = min(thirdperiod)

gen treated = affected
bysort Number (Year_ESG): replace treated = 1 if affected[_n-1] == 1
gen yearxtreat = 0
replace yearxtreat = 2021 if first == 1 
replace yearxtreat = 2022 if second == 1
replace yearxtreat = 2023 if third == 1
bysort Number: egen gvar = max(yearxtreat)


egen mean_early = mean(Sentiment), by(Year_ESG), if gvar == 2022
egen mean_late = mean(Sentiment), by(Year_ESG), if gvar == 2023
egen mean_always = mean(Sentiment), by(Year_ESG), if gvar == 2021
egen mean_never = mean(Sentiment), by(Year_ESG), if gvar == 0

/// Climate Change - Sentiment Scores over time

twoway (line mean_early Year_ESG if Number ==2, lcolor(dkgreen) lwidth(medthick)) (scatter mean_early Year_ESG if Number ==2, mcolor(dkgreen)) ///
(line mean_late Year_ESG if Number == 16, lcolor(ebblue) lwidth(medthick)) (scatter mean_late Year_ESG if Number == 16, mcolor(ebblue)) ///
(line mean_always Year_ESG if Number == 10, lcolor(black) lwidth(medthick)) (scatter mean_always Year_ESG if Number == 10, mcolor(black)) ///
(line mean_never Year_ESG if Number == 3, lcolor(maroon) lwidth(medthick)) (scatter mean_never Year_ESG if Number == 3, mcolor(maroon)), ///
 xline(2021.5, lpattern(dash) lcolor(green%40)) xline(2022.5, lpattern(dash) lcolor(blue%40)) xlab(2021(1) 2023) xtitle("Years") ytitle("ESG Sentiment Score") legend(order(1 "Treated in 2022" 3 "Treated in 2023" 5 "Always Treated" 7 "Never Treated") pos(6) col(4)) ylab(-2(1)0) title("Average ESG Sentiment Scores by Group") subtitle("2021-2023") text(-0.18 2021.5 "Treatment Timing 1:" "Disaster between" "2021 and 2022" , box margin(l+2 t+1 b+2 r+3) bcolor(olive_teal) size(2.5)) text(-0.18 2022.5 "Treatment Timing 2:" "Disaster between" "2022 and 2023" , box margin(l+2 t+1 b+2 r+3) bcolor(bluishgray) size(2.5)) plotregion(margin(medium))
 
graph export "/Users/ibrahimbenaraar/Desktop/graph1.png", replace
 
 /// Climate change - Beliefs 
twoway	(lfitci beliefcorp Year if Year <= 2021, lcolor(ltblue) acolor(bluishgray%30) alc(white))  (lfitci beliefcorp Year if Year >= 2021, lcolor(ltblue) acolor(bluishgray%30) alc(white)) ///
(lfitci beliefs Year if Year <= 2021, lcolor(navy) acolor(navy%10) alc(white))  (lfitci beliefs Year if Year >= 2021, lcolor(navy) acolor(navy%10) alc(white)), ///
legend(order( 6 "Belief that Corporations should do more to" "address Climate Change" 2 "Belief that Climate Change" "affects the weather") pos(6) col(2)) xlab(2020(1) 2022) ylab(65(5)75) ytitle("Estimated Percentage") xtitle("Years") title("Evolution of Opinions about Climate Change") subtitle("2020-2022") plotregion(margin(medium)) clegend(off)

graph export "/Users/ibrahimbenaraar/Desktop/graph2.png", replace

/// Analysis 2: Affected at County Level - Treatment in Year t-1 of ESG Report 

use final_data.dta, clear

bys Number: gen firstperiod = 1 if affected_county == 1 & Year_ESG == 2021
bys Number : egen first = min(firstperiod)
bys Number: gen secondperiod = 1 if affected_county == 1 & Year_ESG == 2022
bys Number : egen second = min(secondperiod)
bys Number: gen thirdperiod = 1 if affected_county == 1 & Year_ESG == 2023
bys Number : egen third = min(thirdperiod)


gen treated = affected_county
bysort Number (Year_ESG): replace treated = 1 if affected_county[_n-1] == 1
gen yearxtreat = 0
replace yearxtreat = 2021 if first == 1 
replace yearxtreat = 2022 if second == 1
replace yearxtreat = 2023 if third == 1
bysort Number: egen gvar = max(yearxtreat)


egen mean_early = mean(Sentiment), by(Year_ESG), if gvar == 2022
egen mean_late = mean(Sentiment), by(Year_ESG), if gvar == 2023
egen mean_always = mean(Sentiment), by(Year_ESG), if gvar == 2021
egen mean_never = mean(Sentiment), by(Year_ESG), if gvar == 0

twoway (line mean_early Year_ESG if Number ==2, lcolor(dkgreen) lwidth(medthick)) (scatter mean_early Year_ESG if Number ==2, mcolor(dkgreen)) ///
(line mean_late Year_ESG if Number == 4, lcolor(ebblue) lwidth(medthick)) (scatter mean_late Year_ESG if Number == 4, mcolor(ebblue)) ///
(line mean_always Year_ESG if Number == 28, lcolor(black) lwidth(medthick)) (scatter mean_always Year_ESG if Number == 28, mcolor(black)) ///
(line mean_never Year_ESG if Number == 3, lcolor(maroon) lwidth(medthick)) (scatter mean_never Year_ESG if Number == 3, mcolor(maroon)), ///
 xline(2021.5, lpattern(dash) lcolor(green%40)) xline(2022.5, lpattern(dash) lcolor(blue%40)) xlab(2021(1) 2023) xtitle("Years") ytitle("ESG Sentiment Score") legend(order(1 "Treated in 2022" 3 "Treated in 2023" 5 "Always Treated" 7 "Never Treated") pos(6) col(4)) ylab(-2(1)0) title("Average ESG Sentiment Scores by Group") subtitle("2021-2023" "Treatment only at County Level") text(-0.18 2021.5 "Treatment Timing 1:" "Disaster between" "2021 and 2022" , box margin(l+2 t+1 b+2 r+3) bcolor(olive_teal) size(2.5)) text(-0.18 2022.5 "Treatment Timing 2:" "Disaster between" "2022 and 2023" , box margin(l+2 t+1 b+2 r+3) bcolor(bluishgray) size(2.5)) plotregion(margin(medium))

graph export "/Users/ibrahimbenaraar/Desktop/graph3.png", replace

//// Analysis 3: Treatment County or Neighbouring County - Treatment in Year of ESG 

use final_data.dta, clear

bys Number: gen firstperiod = 1 if affected_year_ESG == 1 & Year_ESG == 2021
bys Number : egen first = min(firstperiod)
bys Number: gen secondperiod = 1 if affected_year_ESG == 1 & Year_ESG == 2022
bys Number : egen second = min(secondperiod)
bys Number: gen thirdperiod = 1 if affected_year_ESG == 1 & Year_ESG == 2023
bys Number : egen third = min(thirdperiod)


gen treated = affected_year_ESG
bysort Number (Year_ESG): replace treated = 1 if affected_year_ESG[_n-1] == 1
gen yearxtreat = 0
replace yearxtreat = 2021 if first == 1 
replace yearxtreat = 2022 if second == 1
replace yearxtreat = 2023 if third == 1
bysort Number: egen gvar = max(yearxtreat)


egen mean_early = mean(Sentiment), by(Year_ESG), if gvar == 2022
egen mean_late = mean(Sentiment), by(Year_ESG), if gvar == 2023
egen mean_always = mean(Sentiment), by(Year_ESG), if gvar == 2021
egen mean_never = mean(Sentiment), by(Year_ESG), if gvar == 0

twoway (line mean_early Year_ESG if Number ==16, lcolor(dkgreen) lwidth(medthick)) (scatter mean_early Year_ESG if Number ==16, mcolor(dkgreen)) ///
(line mean_late Year_ESG if Number == 4, lcolor(ebblue) lwidth(medthick)) (scatter mean_late Year_ESG if Number == 4, mcolor(ebblue)) ///
(line mean_always Year_ESG if Number == 2, lcolor(black) lwidth(medthick)) (scatter mean_always Year_ESG if Number == 2, mcolor(black)) ///
(line mean_never Year_ESG if Number == 3, lcolor(maroon) lwidth(medthick)) (scatter mean_never Year_ESG if Number == 3, mcolor(maroon)), ///
 xline(2021.75, lpattern(dash) lcolor(green%40)) xline(2022.75, lpattern(dash) lcolor(blue%40)) xlab(2021(1) 2023) xtitle("Years") ytitle("ESG Sentiment Score") legend(order(1 "Treated in 2022" 3 "Treated in 2023" 5 "Always Treated" 7 "Never Treated") pos(6) col(4)) ylab(-2(1)0) title("Average ESG Sentiment Scores by Group") subtitle("2021-2023" "Treatment only at County Level") text(-0.18 2021.75 "Treatment Timing 1:" "Disaster in" "2022" , box margin(l+2 t+1 b+2 r+3) bcolor(olive_teal) size(2.5)) text(-0.18 2022.75 "Treatment Timing 2:" "Disaster in" "2023" , box margin(l+2 t+1 b+2 r+3) bcolor(bluishgray) size(2.5)) plotregion(margin(medium))
 
 graph export "/Users/ibrahimbenaraar/Desktop/graph4.png", replace

////// Analysis 4: Treatment County - Treatment in Year of ESG 

use final_data.dta, clear

bys Number: gen firstperiod = 1 if affected_county_year_ESG == 1 & Year_ESG == 2021
bys Number : egen first = min(firstperiod)
bys Number: gen secondperiod = 1 if affected_county_year_ESG == 1 & Year_ESG == 2022
bys Number : egen second = min(secondperiod)
bys Number: gen thirdperiod = 1 if  affected_county_year_ESG == 1 & Year_ESG == 2023
bys Number : egen third = min(thirdperiod)

gen treated = affected_county_year_ESG
bysort Number (Year_ESG): replace treated = 1 if affected_county_year_ESG[_n-1] == 1
gen yearxtreat = 0
replace yearxtreat = 2021 if first == 1 
replace yearxtreat = 2022 if second == 1
replace yearxtreat = 2023 if third == 1
bysort Number: egen gvar = max(yearxtreat)


egen mean_early = mean(Sentiment), by(Year_ESG), if gvar == 2022
egen mean_late = mean(Sentiment), by(Year_ESG), if gvar == 2023
egen mean_always = mean(Sentiment), by(Year_ESG), if gvar == 2021
egen mean_never = mean(Sentiment), by(Year_ESG), if gvar == 0

twoway (line mean_early Year_ESG if Number ==24, lcolor(dkgreen) lwidth(medthick)) (scatter mean_early Year_ESG if Number ==24, mcolor(dkgreen)) ///
(line mean_late Year_ESG if Number == 4, lcolor(ebblue) lwidth(medthick)) (scatter mean_late Year_ESG if Number == 4, mcolor(ebblue)) ///
(line mean_always Year_ESG if Number == 2, lcolor(black) lwidth(medthick)) (scatter mean_always Year_ESG if Number == 2, mcolor(black)) ///
(line mean_never Year_ESG if Number == 3, lcolor(maroon) lwidth(medthick)) (scatter mean_never Year_ESG if Number == 3, mcolor(maroon)), ///
 xline(2021.75, lpattern(dash) lcolor(green%40)) xline(2022.75, lpattern(dash) lcolor(blue%40)) xlab(2021(1) 2023) xtitle("Years") ytitle("ESG Sentiment Score") legend(order(1 "Treated in 2022" 3 "Treated in 2023" 5 "Always Treated" 7 "Never Treated") pos(6) col(4)) ylab(-2(1)0) title("Average ESG Sentiment Scores by Group") subtitle("2021-2023" "Treatment only at County Level") text(-0.18 2021.75 "Treatment Timing 1:" "Disaster in" "2022" , box margin(l+2 t+1 b+2 r+3) bcolor(olive_teal) size(2.5)) text(-0.18 2022.75 "Treatment Timing 2:" "Disaster in" "2023" , box margin(l+2 t+1 b+2 r+3) bcolor(bluishgray) size(2.5)) plotregion(margin(medium))
 graph export "/Users/ibrahimbenaraar/Desktop/graph5.png", replace
 
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Additional Regressions: Changing Treatment Timing and Introducing //////////////////////////////
//////////        Different Controls and Heterogenous Treatment Effects      ///////////////////////////// //////////////////////////////////////////////////////////////////////////////////////////////////////////
use final_data.dta, clear
xtset Number Year_ESG
gen treated = affected
bysort Number (Year_ESG): replace treated = 1 if affected[_n-1] == 1
gen treated_county = affected_county
bysort Number (Year_ESG): replace treated_county = 1 if affected_county[_n-1] == 1
gen treat = affected_year_ESG 
bysort Number (Year_ESG): replace treat = 1 if affected_year_ESG[_n-1] == 1
gen treat_county = affected_county_year_ESG
bysort Number (Year_ESG): replace treat_county = 1 if affected_county_year_ESG[_n-1] == 1

bys Number: gen fpd = 1 if affected == 1 & Year_ESG == 2021
bys Number : egen first = min(fpd)
bys Number: gen fpd_county = 1 if affected_county == 1 & Year_ESG == 2021
bys Number : egen first_county = min(fpd_county)
bys Number: gen fpd_yesg = 1 if affected_year_ESG == 1 & Year_ESG == 2021
bys Number : egen first_yesg = min(fpd_yesg)
bys Number: gen fpd_county_yesg = 1 if affected_county_year_ESG == 1 & Year_ESG == 2021
bys Number : egen first_county_yesg = min(fpd_county_yesg)
bys Number: gen second_yesg = 1 if affected_year_ESG == 1 & Year_ESG == 2022 & first_yesg !=1
bys Number : egen scond_yesg = min(second_yesg)
bys Number: gen third_yesg = 1 if affected_year_ESG == 1 & Year_ESG == 2023 & first_yesg !=1 & scond_yesg !=1
bys Number : egen thrd_yesg = min(third_yesg)

gen t= Year_ESG-2020
 
//// Baseline Estimates 
 
eststo: quietly areg Sentiment treated if first !=1, vce(cluster Countydum) absorb(Number Statedum Countydum)
eststo: quietly areg Sentiment treated_county if first_county !=1, vce(cluster Countydum) absorb(Number Statedum Countydum)
eststo: quietly areg Sentiment treat if first_yesg !=1, vce(cluster Countydum) absorb(Number Statedum Countydum)
eststo: quietly areg Sentiment treat_county if first_county_yesg !=1, vce(cluster Countydum) absorb(Number Statedum Countydum)
esttab using "/Users/ibrahimbenaraar/Desktop/baseline2.rtf", compress star(* 0.10 ** 0.05 *** 0.01) r2(2) collabel("Affected t-1" "Affected County Level t-1" "Affected t" "Affected County Level t") nomtitles nonumbers 
eststo clear

//// Including Time Trend
eststo: quietly areg Sentiment treated t if first !=1, robust absorb(Number Statedum)
eststo: quietly areg Sentiment treated_county t if first_county !=1, robust absorb(Number Statedum)
eststo: quietly areg Sentiment treat t   if first_yesg !=1, robust absorb(Number Statedum)
eststo: quietly areg Sentiment treat_county t if first_county_yesg !=1, robust absorb(Number Statedum)
esttab using "/Users/ibrahimbenaraar/Desktop/time_trend.rtf", compress star(* 0.10 ** 0.05 *** 0.01) r2(3) collabel("Affected t-1" "Affected County Level t-1" "Affected t" "Affected County Level t") nomtitles nonumbers indicate("Time Trend = t")
eststo clear

//// Year Fixed Effects instead of linear time trends 
eststo: quietly areg Sentiment treated t if first !=1, robust absorb(Number Statedum)
eststo: quietly areg Sentiment treated_county t if first_county !=1, robust absorb(Number Statedum)
eststo: quietly areg Sentiment treat t   if first_yesg !=1, robust absorb(Number Statedum)
eststo: quietly areg Sentiment treat_county t if first_county_yesg !=1, robust absorb(Number Statedum)
esttab using "/Users/ibrahimbenaraar/Desktop/year_fe2.rtf", compress star(* 0.10 ** 0.05 *** 0.01) r2(3) collabel("Affected t-1" "Affected County Level t-1" "Affected t" "Affected County Level t") nomtitles nonumbers indicate("Time Trend = t")
eststo clear

/// County Specific Time Trends 
eststo: quietly areg Sentiment treated c.t#i.Countydum if first !=1, vce(cluster Countydum) absorb(Number Statedum)
eststo: quietly areg Sentiment treated_county c.t#i.Number if first_county !=1, robust absorb(Number)
eststo: quietly areg Sentiment treat c.t#i.Number   if first_yesg !=1, robust absorb(Number)
eststo: quietly areg Sentiment treat_county c.t#i.Number if first_county_yesg !=1, robust absorb(Number)
esttab using "/Users/ibrahimbenaraar/Desktop/county_time.rtf", compress star(* 0.10 ** 0.05 *** 0.01) r2(3) collabel("Affected t-1" "Affected County Level t-1" "Affected t" "Affected County Level t") nomtitles nonumbers keep(treated_county treated treat_county treat)
eststo clear

/// Controlling for Beliefs
eststo: quietly areg Sentiment treated t beliefs if first !=1, robust absorb(Number Statedum)
eststo: quietly areg Sentiment treated_county t beliefs if first_county !=1, robust absorb(Number Statedum)
eststo: quietly areg Sentiment treat t  believer if first_yesg !=1, robust absorb(Number)
eststo: quietly areg Sentiment treat_county t believer if first_county_yesg !=1, robust absorb(Number)
esttab using "/Users/ibrahimbenaraar/Desktop/belief.rtf", compress star(* 0.10 ** 0.05 *** 0.01) r2(3) collabel("Affected t-1" "Affected County Level t-1" "Affected t" "Affected County Level t") nomtitles nonumbers indicate("Time Trend = t")
eststo clear


//// Heterogenous Effects - Interaction Treatment and Climate Beliefs 
eststo: quietly areg Sentiment i.treated##c.beliefs t if first !=1, robust absorb(Number)
eststo: quietly areg Sentiment i.treated_county##c.beliefs t if first_county !=1, robust absorb(Number)
eststo: quietly areg Sentiment i.treat##c.believer t   if first_yesg !=1, robust absorb(Number)
eststo: quietly areg Sentiment i.treat_county##c.believer t if first_county_yesg !=1, robust absorb(Number)
esttab using "/Users/ibrahimbenaraar/Desktop/het_effects.rtf", compress star(* 0.10 ** 0.05 *** 0.01) r2(3)  keep(1.treated 1.treated_county 1.treat 1.treat_county t)
eststo clear



eststo: quietly areg Sentiment i.treated##c.nb_articles_state t if first !=1, robust absorb(Number)
eststo: quietly areg Sentiment i.treated_county##c.nb_articles_state t if first_county !=1, robust absorb(Number)
eststo: quietly areg Sentiment i.treat##c.nb_articles_state t   if first_yesg !=1, robust absorb(Number)
eststo: quietly areg Sentiment i.treat_county##c.nb_articles_state t if first_county_yesg !=1, robust absorb(Number)
esttab using "/Users/ibrahimbenaraar/Desktop/het_effects3.rtf", compress star(* 0.10 ** 0.05 *** 0.01) r2(3)  keep(1.treated 1.treated_county 1.treat 1.treat_county t nb_articles_state)
eststo clear
 

//// Goodman Bacon Decomp 
*ssc install bacondecomp
preserve
drop if first_yesg == 1
xtdidregress (Sentiment i.Statedum i.Countydum) (treat), group(Number) time(Year_ESG)
estat bdecomp, gr
graph export "/Users/ibrahimbenaraar/Desktop/bacon.png"
restore

