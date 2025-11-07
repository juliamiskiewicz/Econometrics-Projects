* ---------- Coding Sample - Julia Miskiewicz ----------
/*
This code is for a report that examines the key economic outcomes in the United States for years 2019
and 2024, using data from the March Current Population Survey (CPS). Specifically, the report analyzes income,
earnings, educational attainment, and labor supply, as well as the conditional expectations and the returns to education.
*/

version 18
clear all
set more off
cd "C:\Users\48601\OneDrive - UCLA IT Services\UCLA MQE\Applied Econometrics Methods\PS1"

log using "julia_miskiewicz_ps1.log", replace text

* 1) IPUMS data
quietly infix                  ///
  int     year        1-4      ///
  long    serial      5-9      ///
  byte    month       10-11    ///
  double  hwtfinl     12-21    ///
  double  cpsid       22-35    ///
  byte    asecflag    36-36    ///
  double  asecwth     37-47    ///
  double  cpi99       48-51    ///
  byte    pernum      52-53    ///
  double  wtfinl      54-67    ///
  double  cpsidp      68-81    ///
  double  cpsidv      82-96    ///
  double  asecwt      97-107   ///
  byte    age         108-109  ///
  byte    sex         110-110  ///
  int     race        111-113  ///
  byte    qage        114-115  ///
  byte    qsex        116-117  ///
  byte    qrace       118-119  ///
  int     uhrsworkt   120-122  ///
  byte    quhrsworkt  123-124  ///
  byte    educ99      125-126  ///
  byte    qeduc       127-128  ///
  byte    wkswork1    129-130  ///  // continuous weeks
  byte    wkswork2    131-131  ///  // intervalled weeks
  byte    qwkswork    132-132  ///
  double  incwage     133-140  ///
  using "cps_00001.dat"

* IPUMS scaling
replace hwtfinl = hwtfinl/10000
replace asecwth = asecwth/10000
replace cpi99   = cpi99/1000
replace wtfinl  = wtfinl/10000
replace asecwt  = asecwt/10000

* Variable labels
label var year      "Survey year"
label var age       "Age"
label var sex       "Sex"
label var race      "Race"
label var educ99    "Educational attainment (1990 def.)"
label var wkswork1  "Weeks worked last year"
label var uhrsworkt "Usual hours/week (all jobs)"
label var incwage   "Wage & salary income (prev. year)"
label var cpi99     "CPI-U factor to 1999 dollars"
label var asecwt    "ASEC person weight"

* Quality flags
label var qage        "Quality flag: Age"
label var qsex        "Quality flag: Sex"
label var qrace       "Quality flag: Race"
label var qeduc       "Quality flag: Education"
label var qwkswork    "Quality flag: Weeks worked"
label var quhrsworkt  "Quality flag: Usual hours"

* Value labels
label define sex_lbl 1 "Male" 2 "Female" 9 "NIU"
label values sex sex_lbl

label define race_lbl 100 "White" 200 "Black" 300 "American Indian/Alaska Native" ///
                       650 "Asian or Pacific Islander" 651 "Asian only" 652 "Hawaiian/Pacific Islander only" ///
                       700 "Other single race" 801 "White-Black" 802 "White-American Indian" 803 "White-Asian" ///
                       804 "White-Hawaiian/Pacific Islander" 805 "Black-American Indian" 806 "Black-Asian" ///
                       807 "Black-Hawaiian/Pacific Islander" 808 "American Indian-Asian" 809 "Asian-Hawaiian/Pacific Islander" ///
                       810 "White-Black-American Indian" 811 "White-Black-Asian" 812 "White-American Indian-Asian" ///
                       813 "White-Asian-Hawaiian/Pacific Islander" 814 "White-Black-American Indian-Asian" ///
                       815 "American Indian-Hawaiian/Pacific Islander" 816 "White-Black–Hawaiian/Pacific Islander" ///
                       817 "White-American Indian-Hawaiian/Pacific Islander" 818 "Black-American Indian-Asian" ///
                       819 "White-American Indian-Asian-Hawaiian/Pacific Islander" 820 "Two or three races, unspecified" ///
                       830 "Four or five races, unspecified" 999 "Blank"
label values race race_lbl

label define educ99_lbl 00 "NIU" 01 "No school completed" 04 "1st–4th grade" ///
                        05 "5th–8th grade" 06 "9th grade" 07 "10th grade" 08 "11th grade" ///
                        09 "12th grade, no diploma" 10 "HS grad or GED" 11 "Some college, no degree" ///
                        12 "Associate, type not specified" 13 "Associate, occupational" 14 "Associate, academic" ///
                        15 "Bachelor's" 16 "Master's" 17 "Professional degree" 18 "Doctorate"
label values educ99 educ99_lbl

label define uhrsworkt_lbl 997 "Hours vary" 999 "NIU"
label values uhrsworkt uhrsworkt_lbl

label define qwkswork_lbl 0 "No change/children" 1 "Allocated"
label values qwkswork qwkswork_lbl

label define quhrs_lbl 00 "No change/children/armed forces" 10 "Value→value" 11 "Blank→value" 12 "Don't know→value" ///
                       13 "Refused→value" 20 "Value→longitudinal value" 21 "Blank→longitudinal value" 22 "Don't know→longitudinal value" ///
                       23 "Refused→longitudinal value" 40 "Value→allocated value" 41 "Blank→allocated value" 42 "Don't know→allocated value" ///
                       43 "Refused→allocated value" 50 "Value→blank" 52 "Don't know→blank"
label values quhrsworkt quhrs_lbl

label define qage_lbl 00 "No change" 01 "Blank→value" 02 "Value→value" 03 "Allocated" 04 "Value→allocated value" ///
                      05 "Blank→allocated value" 06 "Don't know→allocated value" 07 "Refused→allocated value" ///
                      08 "Blank→longitudinal value" 09 "Don't know→longitudinal value" 10 "Refused→longitudinal value"
label values qage qage_lbl

label define qsex_lbl 00 "No change" 01 "Blank→value" 02 "Value→value" 03 "Allocated" 04 "Don't know→value" 05 "Refused→value" ///
                      06 "Blank→allocated value" 07 "Don't know→allocated value" 08 "Refused→allocated value" 09 "Blank→longitudinal value" ///
                      10 "Don't know→longitudinal value" 11 "Refused→longitudinal value" 12 "Allocated by IPUMS"
label values qsex qsex_lbl

label define qrace_lbl 00 "No change/not allocated" 04 "Allocated (unspecified)" 10 "Value→value" 11 "Blank→value" 12 "Don't know→value" ///
                       13 "Refused→value" 20 "Value→longitudinal value" 21 "Blank→longitudinal value" 22 "Don't know→longitudinal value" ///
                       23 "Refused→longitudinal value" 30 "Value→allocated value long" 31 "Blank→allocated value long" ///
                       32 "Don't know→allocated value long" 33 "Refused→allocated value long" 40 "Value→allocated value" ///
                       41 "Blank→allocated value" 42 "Don't know→allocated value" 43 "Refused→allocated value" 50 "Value→blank" ///
                       52 "Don't know→blank" 53 "Refused→blank"
label values qrace qrace_lbl

* ---------- Keep necessary variables only ----------

* Keep ASEC 2019 and 2024
keep if asecflag==1 & inlist(year, 2019, 2024)

* Keep only the variables needed and additional quality indicators
keep year age sex race educ99 wkswork1 uhrsworkt incwage cpi99 asecwt ///
     qage qsex qrace qeduc qwkswork quhrsworkt

* Drop unused variables and weights
capture drop serial pernum cpsidv cpsid month asecflag
capture drop wtfinl hwtfinl asecwth

* ---------- Basic sample restriction: working age 16–70 ----------
keep if inrange(age, 16, 70)

* ---------- Define variables ----------

* i) Employment
gen employed = (incwage>0) if incwage<.
label var employed "Employed (incwage > 0)"

* Real total earnings in 1999 dollars
gen earn_real_1999 = incwage * cpi99 if incwage<.
label var earn_real_1999 "Real total earnings (1999 dollars)"

* ii) Total annual hours
gen hours_annual = wkswork1 * uhrsworkt if wkswork1>=1 & uhrsworkt>=1
label var hours_annual "Total annual hours"

* iii) Real weekly earnings
gen earn_weekly_real = earn_real_1999 / wkswork1 if wkswork1>=1 & earn_real_1999>0
label var earn_weekly_real "Real weekly earnings (1999 dollars)"

* iv) Real hourly wages
gen wage_hourly_real = earn_real_1999 / hours_annual if hours_annual>0 & earn_real_1999>0
label var wage_hourly_real "Real hourly wage (1999 dollars)"

* v) Logs of real earnings metrics (named with *_real)
gen ln_earn_real       = ln(earn_real_1999)   if earn_real_1999>0
gen ln_weekly_real     = ln(earn_weekly_real) if earn_weekly_real>0
gen ln_hourly_real     = ln(wage_hourly_real) if wage_hourly_real>0
label var ln_earn_real   "ln(real total earnings, 1999 dollars)"
label var ln_weekly_real "ln(real weekly earnings, 1999 dollars)"
label var ln_hourly_real "ln(real hourly wage, 1999 dollars)"

* vi) Female dummy
tab sex
tab sex, nolabel
gen female = (sex==2) if !missing(sex)
label var female "Female"
label define female_label 0 "Male" 1 "Female"
label values female female_label

* vii) Education: years + groups
gen educ_years = .
label var educ_years "Years of education (from EDUC99)"
replace educ_years = 0   if educ99==1
replace educ_years = 4   if educ99==4
replace educ_years = 6   if educ99==5
replace educ_years = 9   if educ99==6
replace educ_years = 10  if educ99==7
replace educ_years = 11  if educ99==8
replace educ_years = 12  if inlist(educ99,9,10)
replace educ_years = 14  if inlist(educ99,11,12,13,14)
replace educ_years = 16  if educ99==15
replace educ_years = 17  if educ99==16
replace educ_years = 18  if educ99==17
replace educ_years = 20  if educ99==18

* Four education groups: <12, =12, 13–15, 16+
gen educ = .
replace educ = 1 if educ_years<12  & educ_years>=0
replace educ = 2 if educ_years==12
replace educ = 3 if inrange(educ_years,13,15)
replace educ = 4 if educ_years>=16
label define educ_label 1 "<12 years" 2 "=12 years (HS/GED)" 3 "13–15 years" 4 "16+ years (BA+)"
label values educ educ_label
label var educ "Education group"

* Group dummies
gen ed_12      = (educ==1) if !missing(educ)
gen ed_equal12 = (educ==2) if !missing(educ)
gen ed_13_15   = (educ==3) if !missing(educ)
gen ed_16      = (educ==4) if !missing(educ)
label var ed_12      "<12 years"
label var ed_equal12 "=12 years"
label var ed_13_15   "13–15 years"
label var ed_16      "16+ years"

* College+ dummy
gen college_plus = (educ_years>=16) if !missing(educ_years)
label define college_lbl 0 "< BA" 1 "BA+"
label values college_plus college_lbl
label var college_plus "College or beyond (BA+)"

* viii) Race recode
tab race, nolabel
gen race4 = .
replace race4 = 1 if race==100
replace race4 = 2 if race==200
replace race4 = 3 if race==651
replace race4 = 4 if race<. & race!=999 & !inlist(race, 100, 200, 651)
label define race4_lbl 1 "White" 2 "Black" 3 "Asian only" 4 "Other"
label values race4 race4_lbl
label var race4 "Race (4 groups: White/Black/Asian-only/Other)"

gen nonwhite = (race4!=1) if !missing(race4)
label define nonwhite_lbl 0 "White" 1 "Non-white"
label values nonwhite nonwhite_lbl
label var nonwhite "Non-white (1) vs White (0)"

tab race4, missing
tab nonwhite, missing

* ix) Quality indicators (quick checks)
tab qage, missing
tab qsex, missing
tab qrace, missing
tab qwkswork, missing
tab quhrsworkt, missing

// I retain all allocated observations, because all allocated shares are small.

* ---------- Summary Statistics ----------

capture program drop _mkstats
program define _mkstats, rclass
    syntax varname [if]
    quietly summarize `varlist' `if', detail
    return scalar mean = r(mean)
    return scalar sd   = r(sd)
    return scalar min  = r(min)
    return scalar max  = r(max)
end

local cont age educ_years hours_annual wkswork1 earn_real_1999 wage_hourly_real

cap which outreg2
if _rc ssc install outreg2

outreg2 `cont' if year==2019 using "q1e_simple.tex", replace ///
    sum(detail) eqkeep(N mean sd min max) dec(2) label tex(frag) ctitle(2019)
outreg2 `cont' if year==2024 using "q1e_simple.tex", append ///
    sum(detail) eqkeep(N mean sd min max) dec(2) label tex(frag) ctitle(2024)

* ---------- Checking for and trimming extreme values ----------

* Hours and weeks
tempname P
postfile `P' str12 var int year double p1 p5 p95 p99 int dropped using q1f_post, replace

foreach vv in hours_annual wkswork1 {
    foreach yy in 2019 2024 {
        quietly summarize `vv' if year==`yy' & `vv'<., detail
        local p1  = r(p1)
        local p5  = r(p5)
        local p95 = r(p95)
        local p99 = r(p99)
        quietly count if year==`yy' & (`vv'<`p1' | `vv'>`p99') & `vv'<.
        local dropped = r(N)
        post `P' ("`vv'") (`yy') (`p1') (`p5') (`p95') (`p99') (`dropped')
    }
}
postclose `P'
preserve
use q1f_post, clear

putpdf clear
putpdf begin, pagesize(letter) font("Times New Roman", 11)
putpdf paragraph, halign(center)
putpdf text ("Q1(f): Trimming rules and counts (hours and weeks)")
putpdf paragraph
putpdf table F = (5,7), border(all, nil)
putpdf table F(1,1) = ("Variable"), bold
putpdf table F(1,2) = ("Year"),     bold
putpdf table F(1,3) = ("p1"),       bold
putpdf table F(1,4) = ("p5"),       bold
putpdf table F(1,5) = ("p95"),      bold
putpdf table F(1,6) = ("p99"),      bold
putpdf table F(1,7) = ("Dropped"),  bold

local rr = 1
quietly levelsof var, local(rows)
foreach v of local rows {
    quietly levelsof year if var=="`v'", local(yrs)
    foreach y of local yrs {
        local ++rr
        su p1 if var=="`v'" & year==`y'
        local s1 : display %9.2f r(mean)
        su p5 if var=="`v'" & year==`y'
        local s5 : display %9.2f r(mean)
        su p95 if var=="`v'" & year==`y'
        local s95 : display %9.2f r(mean)
        su p99 if var=="`v'" & year==`y'
        local s99 : display %9.2f r(mean)
        su dropped if var=="`v'" & year==`y'
        local dd : display %9.0f r(mean)

        putpdf table F(`rr',1) = ("`v'")
        putpdf table F(`rr',2) = ("`y'")
        putpdf table F(`rr',3) = ("`s1'")
        putpdf table F(`rr',4) = ("`s5'")
        putpdf table F(`rr',5) = ("`s95'")
        putpdf table F(`rr',6) = ("`s99'")
        putpdf table F(`rr',7) = ("`dd'")
    }
}
capture putpdf save "q1f_trim.pdf", replace
if _rc putpdf save "q1f_trim_alt.pdf", replace
restore

* Drop the p1 / p99
foreach vv in hours_annual wkswork1 {
    foreach yy in 2019 2024 {
        quietly summarize `vv' if year==`yy' & `vv'<., detail
        local p1  = r(p1)
        local p99 = r(p99)
        drop if year==`yy' & `vv'<. & (`vv'<`p1' | `vv'>`p99')
    }
}

* Earnings: drop below p1
tempname G
postfile `G' int year double p1 p5 p95 p99 int dropped using q1g_post, replace

foreach yy in 2019 2024 {
    quietly summarize earn_real_1999 if year==`yy' & earn_real_1999>0, detail
    local p1  = r(p1)
    local p5  = r(p5)
    local p95 = r(p95)
    local p99 = r(p99)
    quietly count if year==`yy' & earn_real_1999>0 & earn_real_1999<`p1'
    local dropped = r(N)
    post `G' (`yy') (`p1') (`p5') (`p95') (`p99') (`dropped')
}
postclose `G'
preserve
use q1g_post, clear

putpdf clear
putpdf begin, pagesize(letter) font("Times New Roman", 11)
putpdf paragraph, halign(center)
putpdf text ("Q1(g): Low-earnings rule and counts")
putpdf paragraph
putpdf table G = (3,6), border(all, nil)
putpdf table G(1,1) = ("Year"), bold
putpdf table G(1,2) = ("p1 (>$0)"), bold
putpdf table G(1,3) = ("p5 (>$0)"), bold
putpdf table G(1,4) = ("p95 (>$0)"), bold
putpdf table G(1,5) = ("p99 (>$0)"), bold
putpdf table G(1,6) = ("Dropped"),  bold

forvalues y = 2019(5)2024 {
    su p1 if year==`y'
    local s1 : display %12.2f r(mean)
    su p5 if year==`y'
    local s5 : display %12.2f r(mean)
    su p95 if year==`y'
    local s95 : display %12.2f r(mean)
    su p99 if year==`y'
    local s99 : display %12.2f r(mean)
    su dropped if year==`y'
    local dd : display %9.0f r(mean)

    local row = cond(`y'==2019, 2, 3)
    putpdf table G(`row',1) = ("`y'")
    putpdf table G(`row',2) = ("`s1'")
    putpdf table G(`row',3) = ("`s5'")
    putpdf table G(`row',4) = ("`s95'")
    putpdf table G(`row',5) = ("`s99'")
    putpdf table G(`row',6) = ("`dd'")
}
capture putpdf save "q1g_trim.pdf", replace
if _rc putpdf save "q1g_trim_alt.pdf", replace
restore

foreach yy in 2019 2024 {
    quietly summarize earn_real_1999 if year==`yy' & earn_real_1999>0, detail
    local p1 = r(p1)
    drop if year==`yy' & earn_real_1999>0 & earn_real_1999<`p1'
}

* Potential experience (Mincer): age - educ_years - 6
gen exp_potential = age - educ_years - 6 if age<. & educ_years<.
label var exp_potential "Potential experience (age - educ_years - 6)"
replace exp_potential = 0 if exp_potential<0 & exp_potential<.

* Counting & dropping zeros by year
quietly count if inlist(year,2019,2024)
local N_all_before = r(N)
quietly count if year==2019
local N2019_before = r(N)
quietly count if year==2024
local N2024_before = r(N)

quietly count if year==2019 & exp_potential==0
local Z2019 = r(N)
quietly count if year==2024 & exp_potential==0
local Z2024 = r(N)

di as txt "Zero-experience (to drop): 2019 = `Z2019', 2024 = `Z2024'"
tab year if exp_potential==0 & inlist(year,2019,2024), missing
tab educ if exp_potential==0 & inlist(year,2019,2024)
summarize age if exp_potential==0 & inlist(year,2019,2024), detail

drop if exp_potential==0 & inlist(year,2019,2024)

quietly count if year==2019
local N2019_after = r(N)
quietly count if year==2024
local N2024_after = r(N)

di as txt "Dropped due to zero experience: 2019 = `Z2019', 2024 = `Z2024'"
di as txt "Remaining sample sizes: 2019 = `N2019_after', 2024 = `N2024_after'"
tab year, missing

* ---------- The distribution of earnings, wages, hours ----------

cap which esttab
if _rc ssc install estout, replace

* ---------- Densities (levels) ----------

cap drop earn_k
gen earn_k = earn_real_1999/1000 if earn_real_1999>0
label var earn_k "Annual real earnings (1999 $, thousands)"

quietly summarize earn_k if inlist(year,2019,2024) & earn_k<., detail
local e1  = r(p1)
local e99 = r(p99)

twoway ///
  (kdensity earn_k if year==2019) ///
  (kdensity earn_k if year==2024), ///
  legend(order(1 "2019" 2 "2024") pos(6) ring(0) cols(2)) ///
  xtitle("Annual real earnings (1999 $, thousands)") ///
  ytitle("Density (per $1k)") ///
  xscale(range(`e1' `e99')) ///
  name(kd_earn_lvl, replace)
graph export "kd_earnings_levels.pdf", replace

* Hours (levels)
quietly summarize hours_annual if inlist(year,2019,2024) & hours_annual>0, detail
local h1  = r(p1)
local h99 = r(p99)

twoway ///
  (kdensity hours_annual if year==2019 & hours_annual>0) ///
  (kdensity hours_annual if year==2024 & hours_annual>0), ///
  legend(order(1 "2019" 2 "2024") pos(6) ring(0) cols(2)) ///
  xtitle("Annual hours") ytitle("Density") ///
  xscale(range(`h1' `h99')) ///
  name(kd_hours_lvl, replace)
graph export "kd_hours_levels.pdf", replace

* Wages (levels)
quietly summarize wage_hourly_real if inlist(year,2019,2024) & wage_hourly_real>0, detail
local w1  = r(p1)
local w99 = r(p99)

twoway ///
  (kdensity wage_hourly_real if year==2019 & wage_hourly_real>0) ///
  (kdensity wage_hourly_real if year==2024 & wage_hourly_real>0), ///
  legend(order(1 "2019" 2 "2024") pos(6) ring(0) cols(2)) ///
  xtitle("Hourly wage (1999 dollars)") ytitle("Density") ///
  xscale(range(`w1' `w99')) ///
  name(kd_wage_lvl, replace)
graph export "kd_wage_levels.pdf", replace

* --------- Logs vs Levels ----------
cap drop ln_earn ln_hours ln_wage
gen ln_earn  = ln(earn_real_1999)   if earn_real_1999>0  & inlist(year,2019,2024)
gen ln_hours = ln(hours_annual)     if hours_annual>0    & inlist(year,2019,2024)
gen ln_wage  = ln(wage_hourly_real) if wage_hourly_real>0& inlist(year,2019,2024)
label var ln_earn  "log annual earnings"
label var ln_hours "log annual hours"
label var ln_wage  "log hourly wage"

* Log Earnings (p1–p99)
quietly summarize ln_earn if ln_earn<., detail
local le1 = r(p1)
local le99 = r(p99)

twoway ///
  (kdensity ln_earn if year==2019) ///
  (kdensity ln_earn if year==2024), ///
  legend(order(1 "2019" 2 "2024") pos(6) ring(0) cols(2)) ///
  xtitle("log annual earnings") ytitle("Density") ///
  xscale(range(`le1' `le99')) ///
  name(kd_earn_log, replace)
graph export "kd_earnings_logs.pdf", replace

* Log Hours (p1–p99)
quietly summarize ln_hours if ln_hours<., detail
local lh1 = r(p1)
local lh99 = r(p99)

twoway ///
  (kdensity ln_hours if year==2019) ///
  (kdensity ln_hours if year==2024), ///
  legend(order(1 "2019" 2 "2024") pos(6) ring(0) cols(2)) ///
  xtitle("log annual hours") ytitle("Density") ///
  xscale(range(`lh1' `lh99')) ///
  name(kd_hours_log, replace)
graph export "kd_hours_logs.pdf", replace

* Log Wages (p1–p99)
quietly summarize ln_wage if ln_wage<., detail
local lw1 = r(p1)
local lw99 = r(p99)

twoway ///
  (kdensity ln_wage if year==2019) ///
  (kdensity ln_wage if year==2024), ///
  legend(order(1 "2019" 2 "2024") pos(6) ring(0) cols(2)) ///
  xtitle("log hourly wage") ytitle("Density") ///
  xscale(range(`lw1' `lw99')) ///
  name(kd_wage_log, replace)
graph export "kd_wage_logs.pdf", replace


cap which putpdf

* weighted summary (mean, p25, p50, p75)
capture program drop _q2_row
program define _q2_row, rclass
    syntax varname [if] [in]
    quietly summarize `varlist' `if' `in' [aw=asecwt], detail
    return scalar mean = r(mean)
    quietly _pctile `varlist' `if' `in' [aw=asecwt], p(25 50 75)
    return scalar p25 = r(r1)
    return scalar p50 = r(r2)
    return scalar p75 = r(r3)
end

local q2vars  earn_real_1999 hours_annual wage_hourly_real
local q2labs  "Annual Real Earnings (1999 $)" ///
              "Annual Hours Worked" ///
              "Hourly Wage (1999 $)"

* ----- 2019 -----
putpdf clear
putpdf begin, pagesize(letter) font("Times New Roman", 11)
putpdf paragraph, halign(center)
putpdf text ("Q2(c): Weighted Summaries -- 2019")
putpdf paragraph
putpdf text ("Statistics use CPS ASEC weights (asecwt).")
putpdf paragraph
putpdf table S19 = (4,5), border(all, nil)
putpdf table S19(1,1) = ("Variable"), bold
putpdf table S19(1,2) = ("Mean"),     bold
putpdf table S19(1,3) = ("Median"),   bold
putpdf table S19(1,4) = ("p25"),      bold
putpdf table S19(1,5) = ("p75"),      bold

local r = 1
local i = 0
foreach v of local q2vars {
    local ++i
    local lab : word `i' of `q2labs'
    quietly _q2_row `v' if year==2019
    local m   : display %9.2f r(mean)
    local p50 : display %9.2f r(p50)
    local p25 : display %9.2f r(p25)
    local p75 : display %9.2f r(p75)
    local ++r
    putpdf table S19(`r',1) = ("`lab'")
    putpdf table S19(`r',2) = ("`m'")
    putpdf table S19(`r',3) = ("`p50'")
    putpdf table S19(`r',4) = ("`p25'")
    putpdf table S19(`r',5) = ("`p75'")
}
putpdf save "q2c_2019.pdf", replace

* ----- 2024 -----
putpdf clear
putpdf begin, pagesize(letter) font("Times New Roman", 11)
putpdf paragraph, halign(center)
putpdf text ("Q2(c): Weighted Summaries -- 2024")
putpdf paragraph
putpdf text ("Statistics use CPS ASEC weights (asecwt).")
putpdf paragraph
putpdf table S24 = (4,5), border(all, nil)
putpdf table S24(1,1) = ("Variable"), bold
putpdf table S24(1,2) = ("Mean"),     bold
putpdf table S24(1,3) = ("Median"),   bold
putpdf table S24(1,4) = ("p25"),      bold
putpdf table S24(1,5) = ("p75"),      bold

local r = 1
local i = 0
foreach v of local q2vars {
    local ++i
    local lab : word `i' of `q2labs'
    quietly _q2_row `v' if year==2024
    local m   : display %9.2f r(mean)
    local p50 : display %9.2f r(p50)
    local p25 : display %9.2f r(p25)
    local p75 : display %9.2f r(p75)
    local ++r
    putpdf table S24(`r',1) = ("`lab'")
    putpdf table S24(`r',2) = ("`m'")
    putpdf table S24(`r',3) = ("`p50'")
    putpdf table S24(`r',4) = ("`p25'")
    putpdf table S24(`r',5) = ("`p75'")
}
putpdf save "q2c_2024.pdf", replace

* Conditional distribution of log hourly wages in 2024 
quietly summarize ln_wage if year==2024 & ln_wage<. [aw=asecwt], detail
local lx1  = r(p1)
local lx99 = r(p99)

twoway (kdensity ln_wage if year==2024 & female==0 [aw=asecwt]) ///
       (kdensity ln_wage if year==2024 & female==1 [aw=asecwt]), ///
       legend(order(1 "Male" 2 "Female") pos(6) ring(0) cols(2)) ///
       xtitle("log hourly wage (2024)") ytitle("Weighted density") ///
       xscale(range(`lx1' `lx99')) ///
       name(q2d_gender, replace)
graph export "q2d_wage_log_gender_w.pdf", replace

twoway (kdensity ln_wage if year==2024 & nonwhite==0 [aw=asecwt]) ///
       (kdensity ln_wage if year==2024 & nonwhite==1 [aw=asecwt]), ///
       legend(order(1 "White" 2 "Non-white") pos(6) ring(0) cols(2)) ///
       xtitle("log hourly wage (2024)") ytitle("Weighted density") ///
       xscale(range(`lx1' `lx99')) ///
       name(q2d_race, replace)
graph export "q2d_wage_log_race_w.pdf", replace

* Dispersion of log hourly wages (weighted)
quietly summarize ln_wage if year==2019 [aw=asecwt], detail
local sd19 = r(sd)
quietly _pctile ln_wage if year==2019 [aw=asecwt], p(10 50 90)
local p10_19 = r(r1)
local p50_19 = r(r2)
local p90_19 = r(r3)

quietly summarize ln_wage if year==2024 [aw=asecwt], detail
local sd24 = r(sd)
quietly _pctile ln_wage if year==2024 [aw=asecwt], p(10 50 90)
local p10_24 = r(r1)
local p50_24 = r(r2)
local p90_24 = r(r3)

local g9010_19 = `p90_19' - `p10_19'
local g9050_19 = `p90_19' - `p50_19'
local g5010_19 = `p50_19' - `p10_19'
local g9010_24 = `p90_24' - `p10_24'
local g9050_24 = `p90_24' - `p50_24'
local g5010_24 = `p50_24' - `p10_24'

local sd19s     : display %6.3f `sd19'
local g9010_19s : display %6.3f `g9010_19'
local g9050_19s : display %6.3f `g9050_19'
local g5010_19s : display %6.3f `g5010_19'
local sd24s     : display %6.3f `sd24'
local g9010_24s : display %6.3f `g9010_24'
local g9050_24s : display %6.3f `g9050_24'
local g5010_24s : display %6.3f `g5010_24'

putpdf clear
putpdf begin, pagesize(letter) font("Times New Roman", 11)
putpdf paragraph, halign(center)
putpdf text ("Q2(e): Dispersion of log hourly wages (weighted)")
putpdf paragraph
putpdf table D = (3,5), border(all, nil)
putpdf table D(1,1) = ("Year"),     bold
putpdf table D(1,2) = ("SD"),       bold
putpdf table D(1,3) = ("p90 - p10"),bold
putpdf table D(1,4) = ("p90 - p50"),bold
putpdf table D(1,5) = ("p50 - p10"),bold
putpdf table D(2,1) = ("2019")
putpdf table D(2,2) = ("`sd19s'")
putpdf table D(2,3) = ("`g9010_19s'")
putpdf table D(2,4) = ("`g9050_19s'")
putpdf table D(2,5) = ("`g5010_19s'")
putpdf table D(3,1) = ("2024")
putpdf table D(3,2) = ("`sd24s'")
putpdf table D(3,3) = ("`g9010_24s'")
putpdf table D(3,4) = ("`g9050_24s'")
putpdf table D(3,5) = ("`g5010_24s'")
putpdf save "q2e_dispersion.pdf", replace

* Weighted distributions of years of education, 2024
capture drop edu_year
generate byte edu_year = round(educ_years) if educ_years<.
replace edu_year = 0  if edu_year<0  & edu_year<.
replace edu_year = 20 if edu_year>20 & edu_year<.
label var edu_year "Years of education (integer)"

* Age groups
capture drop agegrp
generate byte agegrp = .
replace agegrp = 1 if inrange(age,25,44)
replace agegrp = 2 if inrange(age,45,64)
replace agegrp = 3 if age>=65 & age<.
capture label define ageg 1 "25-44" 2 "45-64" 3 "65+"
label values agegrp ageg

* Female vs Male
preserve
keep if year==2024 & edu_year<. & inlist(female,0,1)
collapse (sum) wt=asecwt, by(edu_year female)
bysort female: egen tot = total(wt)
gen share = 100*wt/tot
twoway (connected share edu_year if female==0, sort msymbol(o)) ///
       (connected share edu_year if female==1, sort msymbol(+)), ///
       legend(order(1 "Male" 2 "Female") pos(6) ring(0) cols(2)) ///
       xtitle("Years of education") ytitle("Share (%)") ///
       xlabel(0(2)20) yscale(range(0 .)) ///
       name(q2f_dist_sex, replace)
graph export "q2f_dist_sex.pdf", replace
restore

* White vs Non-white
preserve
keep if year==2024 & edu_year<. & inlist(nonwhite,0,1)
collapse (sum) wt=asecwt, by(edu_year nonwhite)
bysort nonwhite: egen tot = total(wt)
gen share = 100*wt/tot
twoway (connected share edu_year if nonwhite==0, sort msymbol(o)) ///
       (connected share edu_year if nonwhite==1, sort msymbol(+)), ///
       legend(order(1 "White" 2 "Non-white") pos(6) ring(0) cols(2)) ///
       xtitle("Years of education") ytitle("Share (%)") ///
       xlabel(0(2)20) yscale(range(0 .)) ///
       name(q2f_dist_race, replace)
graph export "q2f_dist_race.pdf", replace
restore

* Age groups: 25-44, 45-64, 65+
preserve
keep if year==2024 & edu_year<. & agegrp<.
collapse (sum) wt=asecwt, by(edu_year agegrp)
bysort agegrp: egen tot = total(wt)
gen share = 100*wt/tot
twoway (connected share edu_year if agegrp==1, sort msymbol(o)) ///
       (connected share edu_year if agegrp==2, sort msymbol(+)) ///
       (connected share edu_year if agegrp==3, sort msymbol(x)), ///
       legend(order(1 "25-44" 2 "45-64" 3 "65+") pos(6) ring(0) cols(3)) ///
       xtitle("Years of education") ytitle("Share (%)") ///
       xlabel(0(2)20) yscale(range(0 .)) ///
       name(q2f_dist_age, replace)
graph export "q2f_dist_age.pdf", replace
restore


* -------------- CEFs (weights = asecwt)--------------
graph set window fontface "Times New Roman"
set scheme s1color

* earnings & wages by gender
graph bar (mean) earn_real_1999 [aw=asecwt] if inlist(year,2019,2024), ///
    over(female, relabel(1 "Male" 2 "Female")) ///
    ytitle("Mean annual real earnings (1999 $)") ///
    title("Annual earnings by gender (weighted)") ///
    note("CPS ASEC person weights (asecwt)") ///
    bargap(30) blabel(bar, format(%9.0fc)) ///
    asyvars legend(off) ///
    name(q3a_earn_gender_w, replace) ///
    yscale(r(0 .))
graph export "q3a_earn_gender_w.pdf", replace

graph bar (mean) wage_hourly_real [aw=asecwt] if inlist(year,2019,2024), ///
    over(female, relabel(1 "Male" 2 "Female")) ///
    ytitle("Mean hourly wage (1999 $)") ///
    title("Hourly wages by gender (weighted)") ///
    note("CPS ASEC person weights (asecwt)") ///
    bargap(30) blabel(bar, format(%9.2f)) ///
    asyvars legend(off) ///
    name(q3a_wage_gender_w, replace) ///
    yscale(r(0 .))
graph export "q3a_wage_gender_w.pdf", replace

* by race (4 groups)
graph bar (mean) earn_real_1999 [aw=asecwt] if inlist(year,2019,2024), ///
    over(race4, relabel(1 "White" 2 "Black" 3 "Asian-only" 4 "Other")) ///
    ytitle("Mean annual real earnings (1999 $)") ///
    title("Annual earnings by race (weighted)") ///
    note("CPS ASEC person weights (asecwt)") ///
    bargap(20) blabel(bar, format(%9.0fc)) ///
    asyvars legend(off) ///
    name(q3a_earn_race_w, replace) ///
    yscale(r(0 .))
graph export "q3a_earn_race_w.pdf", replace

graph bar (mean) wage_hourly_real [aw=asecwt] if inlist(year,2019,2024), ///
    over(race4, relabel(1 "White" 2 "Black" 3 "Asian-only" 4 "Other")) ///
    ytitle("Mean hourly wage (1999 $)") ///
    title("Hourly wages by race (weighted)") ///
    note("CPS ASEC person weights (asecwt)") ///
    bargap(20) blabel(bar, format(%9.2f)) ///
    asyvars legend(off) ///
    name(q3a_wage_race_w, replace) ///
    yscale(r(0 .))
graph export "q3a_wage_race_w.pdf", replace

* by age group
graph bar (mean) earn_real_1999 [aw=asecwt] if inlist(year,2019,2024) & agegrp<., ///
    over(agegrp, relabel(1 "25–44" 2 "45–64" 3 "65+")) ///
    ytitle("Mean annual real earnings (1999 $)") ///
    title("Annual earnings by age group (weighted)") ///
    note("CPS ASEC person weights (asecwt)") ///
    bargap(25) blabel(bar, format(%9.0fc)) ///
    asyvars legend(off) ///
    name(q3a_earn_age_w, replace) ///
    yscale(r(0 .))
graph export "q3a_earn_age_w.pdf", replace

graph bar (mean) wage_hourly_real [aw=asecwt] if inlist(year,2019,2024) & agegrp<., ///
    over(agegrp, relabel(1 "25–44" 2 "45–64" 3 "65+")) ///
    ytitle("Mean hourly wage (1999 $)") ///
    title("Hourly wages by age group (weighted)") ///
    note("CPS ASEC person weights (asecwt)") ///
    bargap(25) blabel(bar, format(%9.2f)) ///
    asyvars legend(off) ///
    name(q3a_wage_age_w, replace) ///
    yscale(r(0 .))
graph export "q3a_wage_age_w.pdf", replace

* mean log hourly wage by education, by sex (2024)
graph bar (mean) ln_wage [aw=asecwt] if year==2024 & inlist(educ,1,2,3,4) & female==0 & ln_wage<., ///
    over(educ, relabel(1 "<12" 2 "=12" 3 "13–15" 4 "16+")) ///
    ytitle("Mean log hourly wage") ///
    title("Log hourly wage by education (Men, 2024; weighted)") ///
    note("CPS ASEC person weights (asecwt)") ///
    bargap(25) blabel(bar, format(%5.2f)) ///
    asyvars legend(off) ///
    name(q3b_male_w, replace)
graph export "q3b_logwage_educ_men_w.pdf", replace

graph bar (mean) ln_wage [aw=asecwt] if year==2024 & inlist(educ,1,2,3,4) & female==1 & ln_wage<., ///
    over(educ, relabel(1 "<12" 2 "=12" 3 "13–15" 4 "16+")) ///
    ytitle("Mean log hourly wage") ///
    title("Log hourly wage by education (Women, 2024; weighted)") ///
    note("CPS ASEC person weights (asecwt)") ///
    bargap(25) blabel(bar, format(%5.2f)) ///
    asyvars legend(off) ///
    name(q3b_female_w, replace)
graph export "q3b_logwage_educ_women_w.pdf", replace

* CEFs in 2024: E[ln earnings | years of education] vs E[ln hourly wage | years of education]
preserve
keep if year==2024 & edu_year<.
collapse (mean) mu_lnearn = ln_earn (mean) mu_lnwage = ln_wage [aw=asecwt], by(edu_year)
twoway ///
  (connected mu_lnearn edu_year, msymbol(o) lpattern(solid)  lwidth(med)) ///
  (connected mu_lnwage edu_year, msymbol(+) lpattern(dash)   lwidth(med)), ///
  legend(order(1 "E[ln annual earnings | edu]" 2 "E[ln hourly wage | edu]") pos(6) ring(0) cols(1)) ///
  xtitle("Years of education (integer)") ///
  ytitle("Conditional expectation (log units)") ///
  xlabel(0(2)20) ///
  title("CEF by years of education, 2024 (weighted)") ///
  name(q3c_cef_2024, replace)
graph export "q3c_cef_2024_earn_vs_wage_w.pdf", replace
restore

* CEF of log hourly wage, 2019 vs 2024
preserve
keep if inlist(year,2019,2024) & edu_year<. & ln_wage<.
collapse (mean) mu = ln_wage [aw=asecwt], by(year edu_year)
twoway ///
  (connected mu edu_year if year==2019, msymbol(o) lpattern(solid)  lwidth(med)) ///
  (connected mu edu_year if year==2024, msymbol(+) lpattern(dash)   lwidth(med)), ///
  legend(order(1 "2019" 2 "2024") pos(6) ring(0) cols(2)) ///
  xtitle("Years of education (integer)") ///
  ytitle("E[ln hourly wage | edu]") ///
  xlabel(0(2)20) ///
  title("CEF of log hourly wage: 2019 vs 2024 (weighted)") ///
  name(q3d_cef_wage_years, replace)
graph export "q3d_cef_logwage_2019vs2024_w.pdf", replace
restore


*--------------The OLS returns to education --------------

* Polynomials for experience
cap drop exp2 exp3 exp4
gen double exp2 = exp_potential^2
gen double exp3 = exp_potential^3
gen double exp4 = exp_potential^4

* 2024 only 
preserve
keep if year==2024


* Regressions
eststo clear
reg ln_wage c.educ_years c.exp_potential c.exp2, vce(robust)
eststo M1
reg ln_wage c.educ_years c.exp_potential c.exp2 i.female i.nonwhite, vce(robust)
eststo M2
reg ln_wage c.educ_years c.exp_potential c.exp2 c.exp3 c.exp4 i.female i.nonwhite, vce(robust)
eststo Main24
esttab M1 M2 Main24 using "q4a_main_2024_frag.tex", replace fragment booktabs ///
    label se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) compress nomtitles nonotes ///
    keep(educ_years exp_potential exp2 exp3 exp4 1.female 1.nonwhite) ///
    order(educ_years exp_potential exp2 exp3 exp4 1.female 1.nonwhite) ///
    stats(N r2 rmse, fmt(%9.0f %6.3f %6.3f) labels("Observations" "R-squared" "RMSE"))

* By gender 
eststo clear
eststo Male:   reg ln_wage c.educ_years c.exp_potential c.exp2 c.exp3 c.exp4 if female==0, vce(robust)
eststo Female: reg ln_wage c.educ_years c.exp_potential c.exp2 c.exp3 c.exp4 if female==1, vce(robust)
esttab Male Female using "q4a_byGender_2024_frag.tex", replace fragment booktabs ///
    label se(3) b(3) stats(N r2 rmse, fmt(%9.0f %6.3f %6.3f)) ///
    keep(educ_years exp_potential exp2 exp3 exp4) nonotes

* By race 
eststo clear
foreach g in 1 2 3 4 {
    eststo R`g': reg ln_wage c.educ_years c.exp_potential c.exp2 c.exp3 c.exp4 i.female ///
        if race4==`g', vce(robust)
}
esttab R1 R2 R3 R4 using "q4a_byRace_2024_frag.tex", replace fragment booktabs ///
    label se(3) b(3) stats(N r2 rmse, fmt(%9.0f %6.3f %6.3f)) ///
    keep(educ_years exp_potential exp2 exp3 exp4 1.female) nonotes
restore

* Fitted CEF with 95% bands 
preserve
keep if year==2024
reg ln_wage c.educ_years c.exp_potential c.exp2 c.exp3 c.exp4 i.female i.nonwhite, vce(robust)
cap drop yhat sehat lo hi
predict double yhat, xb
predict double sehat, stdp
gen double lo = yhat - 1.96*sehat
gen double hi = yhat + 1.96*sehat
gen byte edu_int = round(educ_years) if educ_years<.
collapse (mean) yhat lo hi ln_wage, by(edu_int)
twoway ///
    (rarea  hi  lo  edu_int, sort) ///
    (line   yhat    edu_int, sort lwidth(medthick)) ///
    (scatter ln_wage edu_int, msymbol(o) msize(small)) ///
,   legend(order(2 "Fitted ln wage" 1 "95% band" 3 "Mean ln wage by edu") pos(6) ring(0) cols(1)) ///
    xtitle("Years of education") ///
    ytitle("log hourly wage") ///
    title("Main spec: fitted CEF with 95% bands (2024)") ///
    name(q4b_cef_2024, replace)
graph export "q4b_cef_2024.pdf", replace
restore

* Annual vs weekly log earnings 
preserve
keep if year==2024
eststo clear
reg ln_annual c.educ_years c.exp_potential c.exp2 c.exp3 c.exp4 i.female i.nonwhite, vce(robust)
eststo LNY_annual
reg ln_week   c.educ_years c.exp_potential c.exp2 c.exp3 c.exp4 i.female i.nonwhite, vce(robust)
eststo LNY_week
esttab LNY_annual LNY_week using "q4c_annual_vs_weekly_2024_frag.tex", replace ///
    fragment booktabs label se(3) b(3) nonotes ///
    keep(educ_years exp_potential exp2 exp3 exp4 1.female 1.nonwhite) ///
    stats(N r2 rmse, fmt(%9.0f %6.3f %6.3f) labels("Obs." "R²" "RMSE"))
restore

* Education dummies specification
preserve
keep if year==2024
eststo clear
reg ln_wage i.educ c.exp_potential c.exp2 c.exp3 c.exp4 i.female i.nonwhite, vce(robust)
esttab using "q4d_edu_dummies_2024_frag.tex", replace fragment booktabs ///
    label se(3) b(3) nonotes ///
    keep(2.educ 3.educ 4.educ exp_potential exp2 exp3 exp4 1.female 1.nonwhite) ///
    order(2.educ 3.educ 4.educ exp_potential exp2 exp3 exp4 1.female 1.nonwhite) ///
    stats(N r2 rmse, fmt(%9.0f %6.3f %6.3f) labels("Obs." "R²" "RMSE"))
restore

log close
display "Done. Rerun anytime with: do julia_miskiewicz_ps1.do"
