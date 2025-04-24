* CHANGE IT TO YOUR USER PATH
use "C:\Users\rob\Box\LAPOP Shared\2_Projects\2023 AB\Core_Regional\Data Processing\GM\Grand Merge 2004-2023 LAPOP AmericasBarometer (v1.1s).dta", clear

*** if creating Spanish version, make two changes:

* 1) set labels to Spanish/English here 
lab lang en
lab lang es

* 2) save dataset as gm_es or gm_en depending on language


** Combine and recode a4 questions into four categories (economy, security, politics, other)
recode a4n 7 8 = 77, gen(a4n_collap)
*lab def a4n_collap 1 "Economic" 2 "Security" 3 "Basic Services" 4 "Politics" 5 "Unemployment" 6 "Corruption" 77 "Other", modify
* lab val a4n_collap a4n_collap 

#delimit ;
cap recode a4
(1697 1562 1563 1082 1092 10105 9 3 1 2 4 7 26 =1)
(1165 1687 1091 5 14 31 27 17 33 57 80 862 863 2465 =2)
(1564 19 18 21 24 22 60 55 23=3)
(471 472 473 474 1689 1071 1076 1077 1078 1079 1083 1084 1085 1099 10102 10103 30 13 56 59 15 61 85 =4)
(2466 2262 1496 1362 1471 10101 10108 10107 10106 10104 1087 1081 1073 6 58 32 25 11 20 10 16 12 70 864 865 71 86 =5)
, gen(a4r);
#delimit cr

clonevar a4r_combine=a4r
replace a4r_combine = 1 if a4n ==1 | a4n == 5
replace a4r_combine = 2 if a4n == 2
replace a4r_combine = 3 if a4n == 3
replace a4r_combine = 4 if inlist(a4n,4,6)
replace a4r_combine = 5 if inlist(a4n,7,8,77)

lab def a4r_combine 1 "Economic" 2 "Security" 3 "Basic Services" 4 "Politics" 5 "Other", modify
lab val a4r_combine a4r_combine

recode a4r_combine (1=1) (2=2) (4=3) (3 5 = 4), gen(a4r_new)
lab def a4r_new 1 "Economic" 2 "Security" 3 "Politics" 4 "Other", modify
lab val a4r_new a4r_new

recode a4r_new (1=1 "Selected") (2/4=0 "Not Selected"), gen(a4_econ)
recode a4r_new (2=1 "Selected") (1=0 "Not Selected") (3/4=0 "Not Selected"), gen(a4_security)
recode a4r_new (3=1 "Selected") (1/2=0 "Not Selected") (4=0 "Not Selected"), gen(a4_politics)
recode a4r_new (4=1 "Selected") (1/3=0 "Not Selected"), gen(a4_other)

** fix data error for fs2 coding in Canada
replace fs2=0 if fs2==1 & wave==2023 & pais==41
replace fs2=1 if fs2==2 & wave==2023 & pais==41

** Combine gi0 and gi0n (one response label changed)
replace gi0n = gi0 if missing(gi0n)

** Combine different ideology measures
replace l1 = ideology if missing(l1)
replace l1 = l1n if missing(l1)
replace l1 = l1bn if missing(l1)
replace l1 = l1b if missing(l1)

*lab def l1_en 1 "Left/liberal" 10 "Right/conservative", modify
*lab val l1 l1_en

** relabel j-series due to width in breakdown plot (ENGLISH)
lab def jc10r 1 "Yes" 2 "No"
lab val jc10 jc10r

lab def jc13r 1 "Yes" 2 "No"
lab val jc13 jc13r

lab def jc15ar 1 "Yes" 2 "No"
lab val jc15a jc15ar

lab def jc16ar 1 "Yes" 2 "No"
lab val jc16a jc16ar

** vb20 too long (ENGLISH)
lab def vb20r 1  "Wouldnʼt vote" 2  "Incumbent candidate/party" 3  "Opposition candidate/party" 4  "Intentional blank/canceled vote"
lab val vb20 vb20r

** relabel j-series due to width in breakdown plot (SPANISH)
lab def jc10r 1 "Sí" 2 "No"
lab val jc10 jc10r

lab def jc13r 1 "Sí" 2 "No"
lab val jc13 jc13r

lab def jc15ar 1 "Sí" 2 "No"
lab val jc15a jc15ar

lab def jc16ar 1 "Sí" 2 "No"
lab val jc16a jc16ar

* vb20 too long (SPANISH)
lab def vb20r 1  "No votaría" 2  "Candidato/partido actual" 3  "Candidato/partido opositor" 4  "Voto en blanco/anulado"
lab val vb20 vb20r

** reverse response label order for some variables (so that all go from low --> high)
foreach  var of varlist aoj11 aoj12 cp13 cp8 exc7 env2b gi0n idio2 it1 jc10 jc13 jc15a jc16a m1 mil10a mil10e np1 pn4 pol1 q10a q10e q14 q5a q5b sd2new2 sd3new2 sd6new2 soct2 vb10 vb2 vb50 vic1ext w14a wf1 {
	lpr_resc `var', onlyrev labv sufv(_r2)
	drop `var'
}

rename *_r2 * 

** IMPORTANT: save datasets outside of GitHub -- the files are too large for the repo
*save "C:\Users\rob\Box\Rob LAPOP\data\gm_en.dta", replace
*save "C:\Users\rob\Box\Rob LAPOP\data\gm_es.dta", replace

