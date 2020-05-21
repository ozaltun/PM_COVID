library(rbokeh)
library("lfe")
library("dplyr")
library("pscl")
library("MASS")
library(NBZIMM)
library("lme4")



# Incorporating transportation method
output.new.3 <- felm(deaths_per_capita~ mean_pm25
                     + scale(share.drive) +scale(share.PublicTransit) +scale(share.Bike) +scale(share.Walk) +scale(share.Home)
                     + percent.smokers + percent.obese + percent.diabetic + percent.uninsured + scale(ICU.Beds)+scale(poverty) +scale(medianhousevalue) 
                     + scale(pct_owner_occ)  +scale(hispanic) 
                     + scale(pct_blk) + scale(older_pecent)
                     + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                     + scale(tests_per_capita)
                     + log(population)+factor(state)|factor(q_popdensity)+factor(first_case_week)+ factor(education_bins)+ factor(income_bins)  |0|state, cmethod="reghdfe", data = df)

summary(output.new.3)
