library("lfe")
library("dplyr")
library("pscl")
library("MASS")
library(NBZIMM)
library("lme4")


## LOOKING AT ALL COUNTY
# Initial FE setup
output.new.1 <- felm(log(Deaths+1) ~ mean_pm25+scale(poverty) +scale(medianhousevalue) 
                      + scale(pct_owner_occ)  +scale(hispanic) 
                     + scale(pct_blk) + scale(older_pecent)
                     + scale(beds_per_capita) 
                     + scale(mean_bmi) + scale(smoke_rate)
                     + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                     + scale(totalTestResults)
                     + offset(log(population))|factor(q_popdensity) +state+factor(first_case_week)+ factor(education_bins)+ factor(income_bins)  |0|state, cmethod="reghdfe", data = df)

# Looking at deaths per capita
output.new.2 <- felm(deaths_per_capita ~ mean_pm25+scale(poverty) +scale(medianhousevalue) 
                     + scale(pct_owner_occ)  +scale(hispanic) 
                     + scale(pct_blk) + scale(older_pecent)
                     + scale(beds_per_capita) 
                     + scale(mean_bmi) + scale(smoke_rate)
                     + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                     + scale(totalTestResults)
                     + log(population)|factor(q_popdensity) +state+factor(first_case_week)+ factor(education_bins)+ factor(income_bins)  |0|state, cmethod="reghdfe", data = df)



# Incorporating transportation method
output.new.3 <- felm(log(Deaths+1) ~ mean_pm25+WRK_Drive +WRK_PublicTransit +WRK_Bike +WRK_Walk +WRKHOME+scale(poverty) +scale(medianhousevalue) 
                     + scale(pct_owner_occ)  +scale(hispanic) 
                     + scale(pct_blk) + scale(older_pecent)
                     + scale(beds_per_capita) 
                     + scale(mean_bmi) + scale(smoke_rate)
                     + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                     + scale(totalTestResults)
                     + log(population)|factor(q_popdensity) +state+factor(first_case_week)+ factor(education_bins)+ factor(income_bins)  |0|state, cmethod="reghdfe", data = df)

### LOOKING AT COUNTIES >0 deaths
# Initial FE setup
output.new.4 <- felm(log(Deaths) ~ mean_pm25+scale(poverty) +scale(medianhousevalue) 
                     + scale(pct_owner_occ)  +scale(hispanic) 
                     + scale(pct_blk) + scale(older_pecent)
                     + scale(beds_per_capita) 
                     + scale(mean_bmi) + scale(smoke_rate)
                     + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                     + scale(totalTestResults)
                     + log(population)|factor(q_popdensity) +state+factor(first_death_week)+ factor(education_bins)+ factor(income_bins)  |0|state, cmethod="reghdfe", data = subset(df, Deaths>0))


# Looking at deaths per capita
output.new.5 <- felm(deaths_per_capita ~ mean_pm25+scale(poverty) +scale(medianhousevalue) 
                     + scale(pct_owner_occ)  +scale(hispanic) 
                     + scale(pct_blk) + scale(older_pecent)
                     + scale(beds_per_capita) 
                     + scale(mean_bmi) + scale(smoke_rate)
                     + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                     + scale(totalTestResults)
                     + log(population)|factor(q_popdensity) +state+factor(first_death_week)+ factor(education_bins)+ factor(income_bins)  |0|state, cmethod="reghdfe", data = subset(df, Deaths>0))


# Incorporating transportation method
output.new.6 <- felm(log(Deaths) ~ mean_pm25+WRK_Drive +WRK_PublicTransit +WRK_Bike +WRK_Walk +WRKHOME+scale(poverty) +scale(medianhousevalue) 
                     + scale(pct_owner_occ)  +scale(hispanic) 
                     + scale(pct_blk) + scale(older_pecent)
                     + scale(beds_per_capita) 
                     + scale(mean_bmi) + scale(smoke_rate)
                     + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                     + scale(totalTestResults)
                     + log(population)|factor(q_popdensity) +state+factor(first_death_week)+ factor(education_bins)+ factor(income_bins)  |0|state, cmethod="reghdfe", data = subset(df, Deaths>0))




### LOOKING AT COUNTIES >0 deaths && Not NYC
# Initial FE setup
output.new.7 <- felm(log(Deaths) ~ mean_pm25+scale(poverty) +scale(medianhousevalue) 
                     + scale(pct_owner_occ)  +scale(hispanic) 
                     + scale(pct_blk) + scale(older_pecent)
                     + scale(beds_per_capita) 
                     + scale(mean_bmi) + scale(smoke_rate)
                     + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                     + scale(totalTestResults)
                     + log(population)|factor(q_popdensity) +state+factor(first_death_week)+ factor(education_bins)+ factor(income_bins)  |0|state, cmethod="reghdfe", data = subset(df, Deaths>0 & fips != 36061))


# Looking at deaths per capita
output.new.8 <- felm(deaths_per_capita ~ mean_pm25+scale(poverty) +scale(medianhousevalue) 
                     + scale(pct_owner_occ)  +scale(hispanic) 
                     + scale(pct_blk) + scale(older_pecent)
                     + scale(beds_per_capita) 
                     + scale(mean_bmi) + scale(smoke_rate)
                     + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                     + scale(totalTestResults)
                     + log(population)|factor(q_popdensity) +state+factor(first_death_week)+ factor(education_bins)+ factor(income_bins)  |0|state, cmethod="reghdfe", data = subset(df, Deaths>0& fips != 36061))



# Incorporating transportation method
output.new.9 <- felm(log(Deaths) ~ mean_pm25+WRK_Drive +WRK_PublicTransit +WRK_Bike +WRK_Walk +WRKHOME+scale(poverty) +scale(medianhousevalue) 
                     + scale(pct_owner_occ)  +scale(hispanic) 
                     + scale(pct_blk) + scale(older_pecent)
                     + scale(beds_per_capita) 
                     + scale(mean_bmi) + scale(smoke_rate)
                     + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                     + scale(totalTestResults)
                     + log(population)|factor(q_popdensity) +state+factor(first_death_week)+ factor(education_bins)+ factor(income_bins)  |0|state, cmethod="reghdfe", data = subset(df, Deaths>0& fips != 36061))


stargazer(output.new.1, output.new.2, output.new.3, title= "All counties", align=TRUE, digits=6, keep=c("mean_pm25"), omit.stat = c("ser"),column.sep.width = "-10pt"
          , dep.var.labels=c("log(Deaths)", "Deaths per Capita", "log(Deaths)"))


stargazer(output.new.4, output.new.5, output.new.6, title= "Counties with deaths", align=TRUE, digits=6, keep=c("mean_pm25"), omit.stat = c("ser"),column.sep.width = "-10pt"
          , dep.var.labels=c("log(Deaths)", "Deaths per Capita", "log(Deaths)"))

stargazer(output.new.7, output.new.8, output.new.9, title= "Counties with deaths and not New York City", align=TRUE, digits=6, keep=c("mean_pm25"), omit.stat = c("ser"),column.sep.width = "-10pt"
          , dep.var.labels=c("(Deaths)", "Deaths per Capita", "log(Deaths)"))




