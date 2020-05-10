library("tidyr")
library("pscl")
library("MASS")
library(NBZIMM)
library("lfe")
library("lme4")
library("stargazer")
library("xtable")

source('Preprocessing_v2.R')


glmm.zinb.off = glmm.zinb(fixed = Deaths ~ mean_pm25 +scale(poverty) + scale(popdensity)  +scale(medianhousevalue) 
                          +scale(medhouseholdincome) + scale(pct_owner_occ)  +scale(hispanic) 
                          +scale(education)  +scale(pct_blk) + scale(older_pecent) 
                          + scale(totalTestResults) + 
                            + scale(beds) 
                          + scale(mean_bmi) + scale(smoke_rate)
                          + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                          + offset(log(population)), 
                          random = ~ 1 | state, data = (aggregate_pm_census_cdc_test_beds))


glmm.zinb.counties = glmm.zinb(fixed = Deaths ~ mean_pm25 
                               +scale(poverty) + scale(popdensity)  +scale(medianhousevalue) 
                               +scale(medhouseholdincome) + scale(pct_owner_occ)  +scale(hispanic) 
                               +scale(education)  +scale(pct_blk) + scale(older_pecent) 
                               + scale(totalTestResults)
                               + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                               + offset(log(population)), 
                               random = ~ 1 | state, data = (aggregate_pm_census_cdc_test_beds)) 


glmm.zinb.FE <- glmm.zinb(fixed = Deaths ~ mean_pm25
                          + scale(poverty)  + scale(medianhousevalue) 
                          + scale(pct_owner_occ)  + scale(hispanic) 
                          + scale(pct_blk) + scale(older_pecent) 
                          + scale(tests_per_capita)
                          + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                          + log(population)
                          + factor(state) + factor(first_case_week)+ factor(education_bins)+ factor(income_bins) + factor(q_popdensity),
                          random = ~ 1 | state, data = df)


glmm.zinb.health <- glmm.zinb(fixed = Deaths ~ mean_pm25
                              + percent.smokers + percent.obese + percent.diabetic + percent.uninsured + scale(ICU.Beds)
                              + scale(poverty)  + scale(medianhousevalue) 
                              + scale(pct_owner_occ)  + scale(hispanic) 
                              + scale(pct_blk) + scale(older_pecent) 
                              + scale(tests_per_capita)
                              + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                              + log(population)
                              + factor(state) + factor(first_case_week)+ factor(education_bins)+ factor(income_bins) + factor(q_popdensity),
                              random = ~ 1 | state, data = df)


glmm.zinb.transport <- glmm.zinb(fixed = Deaths ~ mean_pm25
                                 + scale(share.drive) +scale(share.PublicTransit) +scale(share.Bike) +scale(share.Walk) +scale(share.Home)
                                 + scale(poverty)  + scale(medianhousevalue) 
                                 + scale(pct_owner_occ)  + scale(hispanic) 
                                 + scale(pct_blk) + scale(older_pecent) 
                                 + scale(tests_per_capita)
                                 + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                                 + log(population)
                                 + factor(state) + factor(first_case_week)+ factor(education_bins)+ factor(income_bins) + factor(q_popdensity),
                                 random = ~ 1 | state, data = df)


glmm.zinb.health.transport <- glmm.zinb(fixed = Deaths ~ mean_pm25
                                        + scale(share.drive) +scale(share.PublicTransit) +scale(share.Bike) +scale(share.Walk) +scale(share.Home)
                                        + percent.smokers + percent.obese + percent.diabetic + percent.uninsured + scale(ICU.Beds)
                                        + scale(poverty)  + scale(medianhousevalue) 
                                        + scale(pct_owner_occ)  + scale(hispanic) 
                                        + scale(pct_blk) + scale(older_pecent) 
                                        + scale(tests_per_capita)
                                        + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                                        + log(population)
                                        + factor(state) + factor(first_case_week)+ factor(education_bins)+ factor(income_bins) + factor(q_popdensity),
                                        random = ~ 1 | state, data = df)


glmm.zinb.health.transport.capita <- glmm.zinb(fixed = deaths_per_capita ~ mean_pm25
                                               + scale(share.drive) +scale(share.PublicTransit) +scale(share.Bike) +scale(share.Walk) +scale(share.Home)
                                               + percent.smokers + percent.obese + percent.diabetic + percent.uninsured + scale(ICU.Beds)
                                               + scale(poverty)  + scale(medianhousevalue) 
                                               + scale(pct_owner_occ)  + scale(hispanic) 
                                               + scale(pct_blk) + scale(older_pecent) 
                                               + scale(tests_per_capita)
                                               + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                                               + log(population)
                                               + factor(state) + factor(first_case_week)+ factor(education_bins)+ factor(income_bins) + factor(q_popdensity),
                                               random = ~ 1 | state, data = df)



reg.list <- list(
  c("Original Setup",signif(intervals(glmm.zinb.off, level=0.95, which="fixed")$fixed[2,], 3)),
  c("Original Setup - Health Impacts",signif(intervals(glmm.zinb.counties, level=0.95, which="fixed")$fixed[2,], 3)),
  c("New Setup",signif(intervals(glmm.zinb.FE, level=0.95, which="fixed")$fixed[2,], 3)),
  c("New Setup + Health",signif(intervals(glmm.zinb.health, level=0.95, which="fixed")$fixed[2,], 3)),
  c("New Setup + Transport",signif(intervals(glmm.zinb.transport, level=0.95, which="fixed")$fixed[2,], 3)),
  c("New Setup + Heatlh + Transport",signif(intervals(glmm.zinb.health.transport, level=0.95, which="fixed")$fixed[2,], 3)))

reg.df <- data.frame(do.call(rbind, reg.list))
colnames(reg.df) <- c("Regression",  "Lower Bound", "Estimate","Upper Bound")

# output.file <- paste(env,"Tables/R/main_tables_lincom_",i,".tex", "", sep="")
print(xtable(reg.df, caption="Coefficients for mean PM2.5 with Deaths as dependent variable") ,include.rownames=FALSE, compress = FALSE)#file = output.file, 



# stargazer(glmm.zinb.off, align,align=TRUE, digits=4,keep=c("mean_pm25"), omit.stat=c("ser"),column.sep.width = "-10pt", dep.var.labels=c("Deaths"))

# stargazer(glmm.zinb.counties, glmm.zinb.FE, glmm.zinb.health, glmm.zinb.transport, glmm.zinb.health.transport, align=TRUE, digits=4, keep=c("mean_pm25"), omit.stat = c("ser"),column.sep.width = "-10pt"
  #        , dep.var.labels=c("Deaths")
   #       , add.lines=list(c("Old Controls + FE", "X","","","",""),c("New Controls + FE", "","X","X","X","X"), c("Health indicators", "","","X","","X"), c("Transportation Indicators", "","","","X","X")))

