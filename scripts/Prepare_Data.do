******************************************************************
******************************************************************
******************************************************************
clear all
set more off
capture log close

* Install soeptools
net install soeptools, from(http://ddionrails.github.io/soeptools/)

*** Define paths
* Stefans Paths
if c(username)=="stefz" & c(os) == "Windows"{ // Stefan DIW-PC
	* Data path to soepdata
	 global data "C:\datasets\soepdata\v36\" 
	 * meta path to variables.csv and variable_categories.csv
	 global meta "C:\git\Master-Project\metadata\p_data\"
	  * output path for dataset
	 global output "C:\datasets\platform_data\"
} 

* Deliverance Paths
if c(username)=="jdbou" & c(os) == "Windows"{ // Stefan DIW-PC
	* Data path to soepdata
	 global data "D:\Education\Thesis\cs-transfer\Stata" 
	 * meta path to variables.csv and variable_categories.csv
	 global meta "C:\Users\jdbou\Documents\GitHub\Master-Project\metadata\p_data\"
	  * output path for dataset
	 global output "D:\Education\Thesis\Output\"
} 

*** Load and merge datasets
use pid hid syear netto corigin phrf sex migback syear sampreg gebjahr germborn migback using ${data}\ppathl.dta, clear

merge 1:1 pid syear using ${data}\pl.dta, nogen keep (master match) /// 
          keepusing (plb0036_h plb0037_h plb0050 plb0195_h plb0196_h plb0197 plb0218 plb0219 pld0043 /// 
		pld0044 pld0045 pld0047 ple0004 ple0005 ple0006 ple0007 ple0008 ple0009 ple0011  /// 
		ple0012 ple0013 ple0014 ple0015 ple0016 ple0017 ple0018 ple0019 ple0020 ple0021  /// 
		ple0022 ple0024 ple0040 ple0041 ple0044_h ple0053 ple0055 ple0056 ple0081_h  /// 
		ple0097 ple0098_v5 ple0128_h ple0160 ple0162 plh0012_h plh0032 plh0033 plh0034  /// 
		plh0035 plh0036 plh0037 plh0038 plh0039 plh0040 plh0042 /// 
		plh0105 plh0106 plh0107 plh0108 plh0109 plh0110 plh0111 plh0112 plh0162  /// 
		plh0164 plh0171 plh0172 plh0173 plh0174 plh0175 plh0176 plh0177 plh0178   ///
		plh0179 plh0180 plh0182 plh0183 plh0184 plh0185 plh0186 plh0187 plh0188  /// 
		plh0189 plh0190 plh0191 plh0192 plh0193 plh0194 plh0195 plh0196 plh0204_h   /// 
		plh0206i01 plh0206i02 plh0206i03 plh0206i04 plh0206i05 plh0206i06 plh0212 plh0213 /// 
		plh0214 plh0215 plh0216 plh0217 plh0218 plh0219 plh0220 plh0221 plh0222 plh0223  ///
		plh0224 plh0225 plh0226 pli0059 pli0080 pli0081 pli0082 pli0083 pli0089 pli0091_h   /// 
		pli0092_h pli0093_h pli0095_h pli0096_h pli0097_h pli0098_h pli0165  /// 
		plj0014_v3 plj0046 plj0047 plj0587 plj0588 plj0589  ///
		  )

merge 1:1 pid syear using ${data}\pgen.dta, nogen keep (master match)  keepusing ( ///
		pgcasmin pgemplst  pgfamstd pgisced97 pglabgro pglabnet pgnation pgoeffd pgtatzeit ///
		pgvebzeit pgpsbil ) 
		  
merge 1:1 pid syear using ${data}\pequiv.dta, nogen keep (master match) keepusing (d11101 /// 
		d11106 d11107 d11109 e11102 e11103 h11101 i11101 i11102 i11103 i11106 ///
		i11107 m11104 m11124 m11125 m11126)
merge m:1 hid syear using ${data}\hbrutto.dta, nogen keep (master match) keepusing (bula_h regtyp)
merge m:1 hid syear using ${data}\hgen.dta, nogen keep (master match) keepusing (hgtyp1hh hghinc hgowner)
merge m:1 hid syear using ${data}\hl.dta, nogen keep (master match) keepusing (hlf0001_h hlf0019_h ///
		  hlf0021_h hlf0071_h hlf0094)

* Define Population
* Keep observations with valid interview
drop if netto>19
* Keep Observations with weighting factor
drop if phrf==0

* recode health variables
recode ple0012 ple0013 ple0014 ple0015 ple0016 ple0017 ple0018 ple0019 ple0020 ple0021 ple0022 ple0024 (-2 = 2)

*Missings to systemmissings NA
mvdecode _all, mv(-1/-8)

*** Edit variables
* Education level
gen education=pgpsbil
recode education (3=4) (7 8=6)

* age groups
gen age= syear-gebjahr if gebjahr>=0
* Keep adult observations
drop if age<=16
gen age_gr=age
recode age_gr (16/34=1) (35/65=2) (66/max=3)

* years in job
gen years_injob =  syear - plb0036_h if syear >= plb0036_h & (age >= (syear - plb0036_h))
drop plb0036_h

recode plb0037_h (3 4 = .)

* Change of insurance company in previous year
recode ple0160 (3=.)

* Party affiliation
gen party=plh0012_h
recode party (-5 -4 -1=.) (-2=0) (1 10=2) (2/3 13=1) (4 11 14 22 23=3) (5 9 15=4) (6 16 17 20 24=5) (7 12 18 19 21 25=6) (27 30 31=7) (8 26=8)

* Employment status
gen erwst=pgemplst
recode erwst (3 4 = 6) 

* bmi Index
gen bmi = round(ple0007/(ple0006/100)^2)

* Federal States
recode bula_h (10 = 7) // Saarland to Rheinland-Pfalz/Saarland

* Delete observations for 1984 to get consistent data:
* active sports, help with friends, clubs, parties.

foreach var of varlist  pli0092_h pli0095_h pli0096_h pli0097_h pli0098_h {
	recode `var' (6 7 8 =.) if syear==1984
}

* Big Five Personality Traits
* Recode negative Traits
foreach var of varlist  plh0218 plh0223 plh0214 plh0226 {
	gen `var'_new = `var'
	recode `var'_new (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1)
}

egen neur = rowmean(plh0216 plh0221 plh0226_new)
egen open = rowmean(plh0215 plh0220 plh0225)
egen extr = rowmean(plh0213 plh0219 plh0223_new) 
egen agre = rowmean(plh0214_new plh0217 plh0224)
egen conc = rowmean(plh0212 plh0218_new plh0222)

*1. equivalent income
*Why differences in household size?

*OECD person scale for equivalized income?
* https://de.wikipedia.org/wiki/OECD-Skala
gen aequiv_weight=1+(d11106-1-h11101)*0.5+h11101*0.3
gen aequiv_income=i11102/aequiv_weight if i11102>=0
gen hinceq = hghinc/aequiv_weight if hghinc>=0

// Transfer dependency
gen transab = (i11107/i11102)*100 
gen transab_cat = 0 if transab==0
replace transab_cat = 1 if inrange(transab, 1,25)
replace transab_cat = 2 if inrange(transab, 26,50)
replace transab_cat = 3 if inrange(transab, 51,75)
replace transab_cat = 4 if inrange(transab, 76,99)
replace transab_cat = 5 if transab==100

gen transab_cat2 = 0 if inrange(transab, 0,25)
replace transab_cat2 = 1 if inrange(transab, 26,50)
replace transab_cat2 = 2 if inrange(transab, 51,75)
replace transab_cat2 = 3 if inrange(transab, 76,100)

label define transab_cat 0 "No transfer dependency" 1 "Very low transfer dependency" ///
2 "Low transfer dependency" 3 "High transfer dependency" 4 "Very high transfer dependency" /// 
5 "Complete transfer dependency"

label value transab_cat transab_cat

label define transab_cat2 0 "Very low transfer dependency" ///
1 "Low transfer dependency" 2 "High transfer dependency" 3 "Very high transfer dependency"

label value transab_cat2 transab_cat2
recode hgtyp1hh (7 8 = .)

* drop variables
drop netto age plh0012_h gebjahr pgpsbil pgemplst plh0226_new plh0223_new plh0214_new /// 
     plh0218_new aequiv_weight d11106 d11107 h11101

		 
* save dataset
save "${output}\p_data.dta", replace

** Label dataset
* Create empty dataset with labels
soepinitdta, mdpath(${meta}) /// 
			 study(soep-core) /// 
			 version(v36) /// 
			 verbose
			 
append using "${output}\p_data.dta"
qui compress

save "${output}\p_data.dta", replace

********************************************************************************
