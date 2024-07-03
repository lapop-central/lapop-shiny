# gm2121 <- read_dta("C:/Users/plutowl/Desktop/gm2123.dta")
# gm <- read_dta("C:/Users/plutowl/Box/LAPOP Shared/2_Projects/2023 AB/Core_Regional/Data Processing/GM/Grand Merge 2004-2023 LAPOP AmericasBarometer (v1.0s).dta")
gm <- read_dta("C:/Users/plutowl/Desktop/gm1.dta")
# gm2 <- readstata13::read.dta13("C:/Users/plutowl/Box/LAPOP Shared/2_Projects/2023 AB/Core_Regional/Data Processing/GM/Grand Merge 2004-2023 LAPOP AmericasBarometer (v1.0s).dta")
pais_lab <- read.csv("C:/Users/plutowl/Desktop/pais_lab.csv")
gm <- merge(gm, pais_lab, by.x = "pais", by.y = "pais_num")



gm$genderm <- NA
gm$genderm <- ifelse(gm$q1tc_r < 3, gm$q1tc_r, gm$genderm)
gm$genderm <- ifelse(is.na(gm$genderm) &  gm$q1tb < 3, gm$q1tb, gm$genderm)
gm$genderm <- ifelse(is.na(gm$genderm) &  gm$q1 < 3, gm$q1, gm$genderm)
gm$gendermc <- ifelse(gm$genderm == 1, "Men", "Women")


gm$edrer <- NA
gm$edrer <- ifelse(gm$edre < 3, 1,
                       ifelse(gm$edre <= 4, 2,
                              ifelse(gm$edre <= 6, 3, NA)))
gm$edrer <- factor(gm$edrer,
                       levels = c(1, 2, 3),
                       labels = c("None/Primary", "Secondary", "Superior"))

gm$wealth[gm$wealth == 6] <- NA
gm$wealthf <- factor(gm$wealth,
                        levels = c(1, 2, 3, 4, 5),
                        labels = c("Low", "2", "3", "4", "High"))

# vars <- c(
#   # "a4",
#   "aoj11",
#   "aoj12",
#   "b1",
#   "b12",
#   "b13",
#   "b18",
#   "b2",
#   "b20",
#   "b20a",
#   "b21",
#   "b21a",
#   "b3",
#   "b31",
#   "b32",
#   "b37",
#   "b4",
#   "b47a",
#   "b6",
#   # "cct1b",
#   # "clien1n",
#   # "clien1na",
#   # "cp13",
#   # "cp2",
#   # "cp20",
#   "cp6",
#   "cp7",
#   "cp8",
#   "d1",
#   "d2",
#   "d3",
#   "d4",
#   "d5",
#   "d6",
#   "drk1",
#   "dst1b",
#   "e5",
#   "ed",
#   "eff1",
#   "eff2",
#   "env1c",
#   "env2b",
#   "etid",
#   "exc11",
#   "exc13",
#   "exc14",
#   "exc15",
#   "exc16",
#   "exc18",
#   "exc2",
#   "exc20",
#   "exc6",
#   "exc7",
#   "exc7new",
#   "fs2",
#   "fs8",
#   "gi0",
#   "gi0n",
#   "idio2",
#   "infrax",
#   "ing4",
#   "it1",
#   "jc10",
#   "jc13",
#   "jc15a",
#   "jc16a",
#   "l1",
#   "l1b",
#   "leng1",
#   "lib1",
#   "lib2c",
#   "m1",
#   "mil10a",
#   "mil10e",
#   "np1",
#   "ocup1a",
#   "ocup4a",
#   "pn4",
#   "pol1",
#   "prot3",
#   # "q1",
#   # "q10a",
#   # "q10e",
#   # "q10new",
#   # "q11n",
#   # "q12",
#   # "q12bn",
#   # "q12c",
#   # "q14",
#   # "q2",
#   # "q3cn",
#   "q5a",
#   "q5b",
#   "redist1",
#   "redist2",
#   "redist2a",
#   "redist3",
#   "ros4",
#   "sd2new2",
#   "sd3new2",
#   "sd6new2",
#   "smedia1",
#   "smedia4",
#   "smedia7",
#   "soct2",
#   "vb10",
#   "vb2",
#   "vb20",
#   "vb50",
#   "vb51",
#   "vb52",
#   "vic1ext",
#   # "vic1exta",
#   # "w14a",
#   # "wf1",
#   "gendermc",
#   "wealthf",
#   "edrer",
#   "edad",
#   "ur",
#   "estratopri",
#   "pais",
#   "year",
#   "wave",
#   "pais_nam",
#   "pais_lab",
#   "weight1500",
#   "strata"
# )

vars <- c(
  "gendermc",
  "wealthf",
  "edrer",
  "edad",
  "ur",
  "estratopri",
  "pais",
  "year",
  "wave",
  "pais_nam",
  "pais_lab",
  "weight1500",
  "strata"
)

vars2 <- vars_labels$column_name
vars3 <- c(vars2, vars)

gmr <- gm[vars]

dstrata <- gmr %>%
  as_survey(strata = strata, weights = weight1500)

table(as_factor(dstrata$variables$pais))


saveRDS(dstrata, "C:/Users/plutowl/Desktop/gmrstrata.rds")

# write_dta(gm, "C:/Users/plutowl/Desktop/gmr.dta")


vars_labels <- read.csv("C:/Users/plutowl/Desktop/new variables for data playground_mr_lap.csv")
vars_labels$display_en <- paste0(vars_labels$category_short_en, ": ", vars_labels$question_short_en, 
                                 " (", vars_labels$column_name, ")", sep = "")

# names(vars_labels$column_name) <- vars_labels$display_en
# names(vars_labels$column_name) <- vars_labels$question_short_en

labs <- vars_labels$column_name
names(labs) <- vars_labels$display_en
labs[order(names(labs))]

saveRDS(labs, "C:/Users/plutowl/Desktop/labs.rds")



