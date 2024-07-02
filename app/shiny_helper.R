# gm2121 <- read_dta("C:/Users/plutowl/Desktop/gm2123.dta")
gm <- read_dta("C:/Users/plutowl/Box/LAPOP Shared/2_Projects/2023 AB/Core_Regional/Data Processing/GM/Grand Merge 2004-2023 LAPOP AmericasBarometer (v1.0s).dta")
# gm2 <- readstata13::read.dta13("C:/Users/plutowl/Box/LAPOP Shared/2_Projects/2023 AB/Core_Regional/Data Processing/GM/Grand Merge 2004-2023 LAPOP AmericasBarometer (v1.0s).dta")
pais_lab <- read.csv("C:/Users/plutowl/Desktop/pais_lab.csv")
gm <- merge(gm, pais_lab, by.x = "pais", by.y = "pais_num")


gm$genderm <- NA
gm$genderm <- ifelse(gm$q1tc_r == 1 | gm$q1tc_r == 2, gm$q1tc_r, gm$genderm)
gm$genderm <- ifelse(is.na(gm$genderm) &  gm$q1tb < 3, gm$q1tb, gm$genderm)
gm$genderm2 <- factor(gm$genderm, levels = c(1, 2), labels = c("Men", "Women"))

gm$wealth[gm$wealth == 6] <- NA

gm$edre <- NA
gm$edrer <- ifelse(gm$edre < 3, 1,
                       ifelse(gm$edre <= 4, 2,
                              ifelse(gm$edre <= 6, 3, NA)))
gm$edrer <- factor(gm$edrer,
                       levels = c(1, 2, 3),
                       labels = c("None/Primary", "Secondary", "Superior"))

gm$wealth <- factor(gm$wealth,
                        levels = c(1, 2, 3, 4, 5),
                        labels = c("Low", "2", "3", "4", "High"))


write_dta(gm, "C:/Users/plutowl/Desktop/gmr.dta")

