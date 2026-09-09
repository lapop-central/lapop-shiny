clear all
set more off

* -----------------------------
* User settings
* -----------------------------
local data_dir26 "C:\Users\vidigar\Box\LAPOP Shared\2_Projects\2025-26 AB\Core_Regional\Data Processing\GM drafts"
local out_dir "C:\Users\vidigar\Documents\GitHub\lapop-shiny\Data Preprocessing"

local gm_2004_2026 "`data_dir26'\Grand Merge 2004-2026 AmericasBarometer ALLVARS (v1.0i).dta"

* -----------------------------
* Create both language versions from the 2026 GM only.
* The 2026 GM already contains all 2004-2026 variables/observations.
* -----------------------------
foreach lang in en es {

    use "`gm_2004_2026'", clear

    capture confirm variable wave
    if _rc {
        display as error "ERROR: wave variable not found in 2026 GM source."
        error 459
    }

    count if wave == 11
    display as text "2026 observations in source for `lang': " r(N)

    if r(N) == 0 {
        display as error "ERROR: No wave == 2026 observations found in 2026 GM source."
        error 459
    }

    capture label language `lang'

    * -----------------------------
    * A4 recodes
    * -----------------------------
    capture drop a4n_collap
    capture confirm variable a4n
    if !_rc {
        recode a4n (7 8 = 77), gen(a4n_collap)
    }

    capture drop a4r
    capture confirm variable a4
    if !_rc {
        #delimit ;
        recode a4
        (1697 1562 1563 1082 1092 10105 9 3 1 2 4 7 26 = 1)
        (1165 1687 1091 5 14 31 27 17 33 57 80 862 863 2465 = 2)
        (1564 19 18 21 24 22 60 55 23 = 3)
        (471 472 473 474 1689 1071 1076 1077 1078 1079 1083 1084 1085 1099 10102 10103 30 13 56 59 15 61 85 = 4)
        (2466 2262 1496 1362 1471 10101 10108 10107 10106 10104 1087 1081 1073 6 58 32 25 11 20 10 16 12 70 864 865 71 86 = 5),
        gen(a4r);
        #delimit cr
    }

    capture drop a4r_combine
    capture confirm variable a4r
    if !_rc {
        clonevar a4r_combine = a4r
    }
    else {
        gen a4r_combine = .
    }

    capture confirm variable a4n
    if !_rc {
        replace a4r_combine = 1 if inlist(a4n, 1, 5)
        replace a4r_combine = 2 if a4n == 2
        replace a4r_combine = 3 if a4n == 3
        replace a4r_combine = 4 if inlist(a4n, 4, 6)
        replace a4r_combine = 5 if inlist(a4n, 7, 8, 77)
    }

    label define a4r_combine 1 "Economic" 2 "Security" 3 "Basic Services" 4 "Politics" 5 "Other", replace
    label values a4r_combine a4r_combine

    capture drop a4r_new
    recode a4r_combine (1 = 1) (2 = 2) (4 = 3) (3 5 = 4), gen(a4r_new)

    label define a4r_new 1 "Economic" 2 "Security" 3 "Politics" 4 "Other", replace
    label values a4r_new a4r_new

    capture drop a4_econ a4_security a4_politics a4_other
    recode a4r_new (1 = 1 "Selected") (2/4 = 0 "Not Selected"), gen(a4_econ)
    recode a4r_new (2 = 1 "Selected") (1 3 4 = 0 "Not Selected"), gen(a4_security)
    recode a4r_new (3 = 1 "Selected") (1 2 4 = 0 "Not Selected"), gen(a4_politics)
    recode a4r_new (4 = 1 "Selected") (1/3 = 0 "Not Selected"), gen(a4_other)

    * -----------------------------
    * Fix known Canada 2023 fs2 issue
    * -----------------------------
    capture confirm variable fs2
    if !_rc {
        capture confirm variable wave
        if !_rc {
            capture confirm variable pais
            if !_rc {
                replace fs2 = 0 if fs2 == 1 & wave == 2023 & pais == 41
                replace fs2 = 1 if fs2 == 2 & wave == 2023 & pais == 41
            }
        }
    }

    * -----------------------------
    * Combine gi0 and gi0n
    * -----------------------------
    capture confirm variable gi0n
    if _rc {
        capture confirm variable gi0
        if !_rc {
            gen gi0n = gi0
        }
    }
    else {
        capture confirm variable gi0
        if !_rc {
            replace gi0n = gi0 if missing(gi0n)
        }
    }

    * -----------------------------
    * Combine ideology measures
    * -----------------------------
    capture confirm variable l1
    if _rc {
        gen l1 = .
    }

    foreach src in ideology l1n l1bn l1b {
        capture confirm variable `src'
        if !_rc {
            replace l1 = `src' if missing(l1) & !missing(`src')
        }
    }

    * -----------------------------
    * Shorter labels by language
    * -----------------------------
    if "`lang'" == "en" {
        label define yesno_r 1 "Yes" 2 "No", replace

        label define vb20r ///
            1 "Wouldn't vote" ///
            2 "Incumbent candidate/party" ///
            3 "Opposition candidate/party" ///
            4 "Intentional blank/canceled vote", replace
    }

    if "`lang'" == "es" {
        label define yesno_r 1 "Sí" 2 "No", replace

        label define vb20r ///
            1 "No votaría" ///
            2 "Candidato/partido actual" ///
            3 "Candidato/partido opositor" ///
            4 "Voto en blanco/anulado", replace
    }

    foreach v in jc10 jc13 jc15a jc16a {
        capture confirm variable `v'
        if !_rc {
            label values `v' yesno_r
        }
    }

    capture confirm variable vb20
    if !_rc {
        label values vb20 vb20r
    }

    * -----------------------------
    * Reverse response label order
    * Requires lpr_resc to be installed/available
    * -----------------------------
    local revvars aoj11 aoj12 cp13 cp8 exc7 env2b gi0n idio2 it1 jc10 jc13 ///
        jc15a jc16a m1 mil10a mil10e np1 pn4 pol1 q10a q10e q14 q5a ///
        q5b sd2new2 sd3new2 sd6new2 soct2 vb10 vb2 vb50 vic1ext w14a wf1

    foreach v of local revvars {
        capture confirm variable `v'
        if !_rc {
            capture noisily lpr_resc `v', onlyrev labv sufv(_r2)

            if !_rc {
                drop `v'
                rename `v'_r2 `v'
            }
        }
    }

    * -----------------------------
    * Save output
    * -----------------------------
    save "`out_dir'\gm_`lang'_2004_2026.dta", replace
}