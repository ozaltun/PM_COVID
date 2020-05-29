library("lfe")
library("dplyr")
library("pscl")
library("MASS")
library(NBZIMM)
library("lme4")

date_of_study = "05-17-2020"
df <- read.csv(paste0("/Users/ozaltun/Dropbox (MIT)/Data/Health/covid/",date_of_study,"_df.csv"))

# Matches Stata Initial_BO.do
output.new <- felm(deaths_per_thousand~ mean_pm25
                     + share.drive +share.PublicTransit +share.Bike +share.Walk +share.Home
                     + percent.smokers + percent.obese + percent.diabetic +  percent.ICU.Beds + poverty +medianhousevalue 
                     + pct_owner_occ + pct_white + hispanic 
                     + pct_blk + older_pecent
                     + scale(mean_summer_temp) + scale(mean_winter_temp)
                     + log(population)+factor(state)|factor(q_popdensity)+factor(first_case_week_numeric)+ factor(education_bins)+ factor(income_bins)  |0|state, cmethod="reghdfe", data = subset(df, fips != 36061))

summary(output.new)

# Esimating this for every time step.

date_list <- c()
state_list <- c()
coef_list <- c()
lb_list<- c()
ub_list<-c()

for(date in time_series_columns){
  df["deaths_per_capita_new"] <- df[date]/df["population"] *1000
  output.new.temp <- felm(deaths_per_capita_new~ mean_pm25
                          + share.drive +share.PublicTransit +share.Bike +share.Walk +share.Home
                          + percent.smokers + percent.obese + percent.diabetic +  percent.ICU.Beds + poverty +medianhousevalue 
                          + pct_owner_occ + pct_white + hispanic 
                          + pct_blk + older_pecent
                          + scale(mean_summer_temp) + scale(mean_winter_temp)
                          + log(population)+factor(state)|factor(q_popdensity)+factor(first_case_week_numeric)+ factor(education_bins)+ factor(income_bins)  |0|state, cmethod="reghdfe", data = subset(df, fips != 36061))
  
  states <- names(coef(output.new.temp))[grepl("state", names(coef(output.new.3)))]
  for(state in states){
    date_list <- c(date_list, date)
    state_list <- c(state_list, state)
    coef_list <- c(coef_list,coef(output.new.3)[state])
    lb_list <- c(lb_list, confint(output.new.temp, level = 0.95)[state,][1])
    ub_list <- c(ub_list, confint(output.new.temp, level = 0.95)[state,][2])  
  }
  
}


state_estimate <- data.frame(date = date_list,
                                   state = state_list,
                                   est = coef_list,
                                   lb = lb_list,
                                   ub = ub_list)


