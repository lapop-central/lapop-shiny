library(dplyr)
library(haven)
# library(labelled)
# library(sjlabelled)

# gm <- read_dta("C:/Users/plutowl/Box/LAPOP Shared/2_Projects/2023 AB/Core_Regional/Data Processing/GM/Grand Merge 2004-2023 LAPOP AmericasBarometer (v1.0s).dta")
gm <- haven::read_dta("C:/Users/plutowl/Desktop/gm2.dta")
pais_lab <- read.csv("C:/Users/plutowl/Desktop/pais_lab.csv")
gm <- merge(gm, pais_lab, by.x = "pais", by.y = "pais_num")

table(as_factor(gm$pais))

expss::add_val_lab(gm$pais) = expss::num_lab("24 Guyana")
# expss::val_lab(gm$pais)


# class(gm$pais)
# table(labelled(gm$pais))
# gm$pais <- remove_labels(gm$pais, labels = tagged_na("z"))


#gender - strictly men vs. women, only self-identification in 2021 and 2023
gm$genderm <- gm$sex
gm$genderm <- ifelse(is.na(gm$genderm) &  gm$q1tc_r < 3, gm$q1tc_r, gm$genderm)
gm$genderm <- ifelse(is.na(gm$genderm) &  gm$q1tb < 3, gm$q1tb, gm$genderm)
gm$genderm <- ifelse(is.na(gm$genderm) &  gm$usq1tc < 3, gm$usq1tc, gm$genderm)
gm$gendermc <- ifelse(gm$genderm == 1, "Men", "Women")

# table(gm$gendermc)
# table(gm$pais[is.na(gm$gendermc)])
# table(gm$pais[is.na(gm$gendermc)], gm$wave[is.na(gm$gendermc)])


gm$edrer <- NA
gm$edrer <- ifelse(gm$edre < 3, 1,
                       ifelse(gm$edre <= 4, 2,
                              ifelse(gm$edre <= 6, 3, NA)))

gm$edrer <- ifelse(is.na(gm$edrer), gm$edr, gm$edrer)

gm$edrr <- ifelse(gm$wave < 2021 & is.na(gm$edrer) & gm$pais %in% c(1, 2, 3, 4, 6, 7, 9, 10, 12, 13, 14, 15, 26, 28),
                     cut(gm$ed,
                         breaks = c(-1, 0, 6, 12, 20),
                         labels = c("0", "1", "2", "3")), 
                  NA)

gm$edrr <- ifelse(gm$wave < 2021 & is.na(gm$edrer) & is.na(gm$edrr) & gm$pais %in% c(5, 16, 11, 23, 24),
                  cut(gm$ed,
                      breaks = c(-1, 0, 6, 11, 20),
                      labels = c("0", "1", "2", "3")), 
                  gm$edrr)    

gm$edrr <- ifelse(gm$wave < 2021 & is.na(gm$edrer) & is.na(gm$edrr) & gm$pais %in% c(17, 30),
                  cut(gm$ed,
                      breaks = c(-1, 0, 7, 12, 20),
                      labels = c("0", "1", "2", "3")), 
                  gm$edrr)  

gm$edrr <- ifelse(gm$wave < 2021 & is.na(gm$edrer) & is.na(gm$edrr) & gm$pais == 8,
                  cut(gm$ed,
                      breaks = c(-1, 0, 5, 11, 20),
                      labels = c("0", "1", "2", "3")), 
                  gm$edrr)  

gm$edrr <- ifelse(gm$wave < 2021 & is.na(gm$edrer) & is.na(gm$edrr) & gm$pais == 21,
                  cut(gm$ed,
                      breaks = c(-1, 0, 8, 12, 20),
                      labels = c("0", "1", "2", "3")), 
                  gm$edrr) 

gm$edrr <- ifelse(gm$wave < 2021 & is.na(gm$edrer) & is.na(gm$edrr) & gm$pais == 22,
                  cut(gm$ed,
                      breaks = c(-1, 0, 7, 14, 20),
                      labels = c("0", "1", "2", "3")), 
                  gm$edrr) 

gm$edrr <- ifelse(gm$wave < 2021 & is.na(gm$edrer) & is.na(gm$edrr) & gm$pais == 25,
                  cut(gm$ed,
                      breaks = c(-1, 0, 5, 12, 20),
                      labels = c("0", "1", "2", "3")), 
                  gm$edrr) 
gm$edrr <- ifelse(gm$wave < 2021 & is.na(gm$edrer) & is.na(gm$edrr) & gm$pais == 27,
                  cut(gm$ed,
                      breaks = c(-1, 0, 6, 13, 20),
                      labels = c("0", "1", "2", "3")), 
                  gm$edrr) 
gm$edrr <- ifelse(gm$wave < 2021 & is.na(gm$edrer) & is.na(gm$edrr) & gm$pais == 40,
                  cut(gm$ed_usa,
                      breaks = c(-1, 0, 1, 2, 20),
                      labels = c("0", "1", "2", "3")), 
                  gm$edrr) 
gm$edrr <- ifelse(is.na(gm$edrer) & is.na(gm$edrr) & gm$pais == 41,
                  cut(gm$edcan,
                      breaks = c(-1, 0, 2, 4, 20),
                      labels = c("0", "1", "2", "3")), 
                  gm$edrr)
gm$edrr2 <- gm$edrr - 1

gm$edrer <- ifelse(is.na(gm$edrer), gm$edrr2, gm$edrer)
gm$edrer <- ifelse(gm$edrer == 0, 1, gm$edrer)

# table(gm$edrerf)

# sum(is.na(gm$edrer)) / length(gm$edrer)
# 
# table(gm$pais[is.na(gm$edrer)], gm$wave[is.na(gm$edrer)])
# 
# table(gm$ed, gm$edrr)
# table(gm$pais, gm$ed)

gm$edrerf <- factor(gm$edrer,
                       levels = c(1, 2, 3),
                       labels = c("None/Primary", "Secondary", "Superior"))


gm$wealth[gm$wealth == 6] <- NA
gm$wealthf <- factor(gm$wealth,
                        levels = c(1, 2, 3, 4, 5),
                        labels = c("Low", "2", "3", "4", "High"))

gm$gi0n <- ifelse(is.na(gm$gi0n), gm$gi0, gm$gi0n)

gm$l1 <- ifelse(is.na(gm$l1), gm$ideology, gm$l1)
gm$l1 <- ifelse(is.na(gm$l1), gm$l1n, gm$l1)
gm$l1 <- ifelse(is.na(gm$l1), gm$l1bn, gm$l1)
gm$l1 <- ifelse(is.na(gm$l1), gm$l1b, gm$l1)

gm <- gm %>%
  mutate(across(ur, ~ if_else(is.na(ur) & ur1new == 1, 1, .))) %>%
  mutate(across(ur, ~ if_else(is.na(ur) & ur1new <= 4, 2, .)))

vars <- c(
  "gendermc",
  "wealthf",
  "edrerf",
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


vars_labels <- read.csv("C:/Users/plutowl/Desktop/new variables for data playground_mr_lap.csv")
vars_labels$display_en <- paste0(vars_labels$category_short_en, ": ", vars_labels$question_short_en, 
                                 " (", vars_labels$column_name, ")", sep = "")



vars2 <- vars_labels$column_name
vars3 <- c(vars2, vars)


vars3 %in% names(gm)
vars3[!vars3 %in% names(gm)]

gmr <- gm[vars3]

library(srvyr)
dstrata <- gmr %>%
  as_survey(strata = strata, weights = weight1500)

table(as_factor(dstrata$variables$pais))


saveRDS(dstrata, "C:/Users/plutowl/Desktop/gmrstrata.rds")

# write_dta(gm, "C:/Users/plutowl/Desktop/gmr.dta")



# names(vars_labels$column_name) <- vars_labels$display_en
# names(vars_labels$column_name) <- vars_labels$question_short_en

labs <- vars_labels$column_name
names(labs) <- vars_labels$display_en
labs[order(names(labs))]

names(vars_labels$column_name) <- vars_labels$display_en
vars_labels$labs2 <- labs

vars_labels$question_en_comp <- paste0(vars_labels$question_en, vars_labels$responses_en_rec, sep = " ")

saveRDS(labs, "C:/Users/plutowl/Desktop/labs.rds")

table(as_factor(dstrata$variables$jc13))




# x <- which(vars_labels$column_name == "b21")
# 
# vars_labels$question_en[which(vars_labels$column_name == "b21")]



