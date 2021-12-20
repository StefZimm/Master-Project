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
	 global data "C:\datasets\soepdata" 
	 * meta path to variables.csv and variable_categories.csv
	 global meta "C:\git\Master-Project\metadata\p_data\"
	  * output path for dataset
	 global output "C:\datasets\platform_data\"
} 

* Deliverance Paths
if c(username)=="Deliverance" & c(os) == "Windows"{ // Stefan DIW-PC
	* Data path to soepdata
	 global data "" 
	 * meta path to variables.csv and variable_categories.csv
	 global meta ""
	  * output path for dataset
	 global output ""
} 

*** Load and merge datasets
use pid hid syear netto phrf sex migback syear sampreg gebjahr migback using ${data}\ppathl.dta, clear

merge 1:1 pid syear using ${data}\pl.dta, nogen keep (master match) /// 
          keepusing (plh0012_h plh0182 plh0032 plh0033 plh0034 plh0035 plh0036 /// 
		  plh0037 plh0038 plh0039 plh0040 plh0042 plj0046 plj0047 pli0092_h pli0095_h /// 
		  pli0096_h pli0097_h pli0098_h plh0171 plh0164  /// 
		  plh0166 plh0171 plh0172 plh0173 plh0174 plh0175 plh0176 plh0177 plh0178 /// 
		  plh0179 plh0180 plh0181 plh0182 plh0183)

merge 1:1 pid syear using ${data}\pgen.dta, nogen keep (master match)  keepusing ( ///
          pglabgro pglabnet pgtatzeit pgvebzeit pgisced97 pgpsbil pgoeffd pgemplst) 
		  
merge 1:1 pid syear using ${data}\pequiv.dta, nogen keep (master match) keepusing (y11101)
merge m:1 hid syear using ${data}\hbrutto.dta, nogen keep (master match) keepusing (bula_h)

* Define Population
* Keep observations with valid interview
drop if netto>19
* Keep Observations with weighting factor
drop if phrf==0

*** Edit variables
* Education level
gen education=pgpsbil
recode education (3=4) (7 8=6)
label define education 1 "lower secondary degree" 2 "secondary school degree" 4 "college entrance qualification" 5 "Other degree" 6 "no degree/no degree yet"
label values education education

* age groups
gen age= syear-gebjahr if gebjahr>=0
label variable age "Age"
* Keep adult observations
drop if age<=16
gen age_gr=age
recode age_gr (16/34=1) (35/65=2) (66/max=3)
label variable age_gr "Altersgruppen"
label define age_gr 1 "16-34 y." 2 "35-65 y." 3 "66 and older"
label values age_gr age_gr

* Party affiliation
gen party=plh0012_h
recode party (-5 -4 -1=.) (-2=0) (1 10=2) (2/3 13=1) (4 11 14 22 23=3) (5 9 15=4) (6 16 17 20 24=5) (7 12 18 19 21 25=6) (27 30 31=7) (8 26=8)
label var party "Party affiliation"
label define party_lbl 0 "no party" 1 "CDU/CSU" 2 "SPD" 3 "FDP" 4 "Grüne" 5 "PDS/Linke" 6 "NPD Combination" 7 "AfD" 8 "Other"
label values party party_lbl

* Employment status
gen erwst=pgemplst
recode erwst (3 4 = 6) 
label define erwst_lbl 1 "Full-time" 2 "Part-time" 5 "Non-working" 6 "Other"
label values erwst erwst_lbl
lab var erwst "Employment status"

* Federal States
recode bula_h (10 = 7) // Saarland to Rheinland-Pfalz/Saarland

* Delete observations for 1984 to get consistent data:
* active sports, help with friends, clubs, parties.
recode pli0092_h (6 7 8 =.) if syear==1984
recode pli0095_h (6 7 8 =.) if syear==1984
recode pli0096_h (6 7 8 =.) if syear==1984
recode pli0097_h (6 7 8 =.) if syear==1984
recode pli0098_h (6 7 8 =.) if syear==1984
 
*Missings to systemmissings NA
mvdecode _all, mv(-1/-8)

* drop variables
drop netto age plh0012_h gebjahr pgpsbil pgemplst
		 
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

/*
*** create variables.csv 
global meanvar pglabgro pglabnet pgtatzeit pgvebzeit y11101 plh0164 plh0166 plh0171 plh0172 plh0173 plh0174 plh0175 plh0176 plh0177 plh0178 plh0179 plh0180 plh0181 plh0182 plh0183
global probvar party  pli0092_h pli0095_h pli0096_h pli0097_h pli0098_h plh0032 plh0033 plh0034 plh0035 plh0036 plh0037 plh0038 plh0039 plh0040 plh0042 plj0046 plj0047  erwst
global demo sampreg sex bula education alter_gr migback

label language EN

describe, replace clear
keep name type varlab

gen study = "soep-core"
gen dataset = "p_data"
gen version = "v36"
rename name variable
gen meantable = ""
gen probtable = ""
gen template_id = ""
gen label = ""
rename varlab label_de
gen concept = ""
gen description = ""
gen description_de = ""
gen minedition = ""

* Variablen als numerische oder kategoriale Variablen definieren
* meantable 
foreach var of global meanvar {
	replace meantable = "Yes" if variable == "`var'"
	replace probtable = "No" if variable == "`var'" 
}

* probtable
foreach var of global probvar {
replace meantable = "No" if variable == "`var'"
replace probtable = "Yes" if variable == "`var'" 
}

* dimensionen
foreach var of global demo {
replace meantable = "demo" if variable == "`var'"
replace probtable = "demo" if variable == "`var'" 
}


save "${meta}\variables_csv.dta", replace

*/
