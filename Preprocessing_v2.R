library("dplyr")
library("haven")
library(stringr)
library(RCurl)


system2("/anaconda3/bin/python3", args="preprocessing_getting_first_week.py")
date_of_study = "05-06-2020"
# Historical data
covid_hist = read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv"))
covid_us_hist = subset(covid_hist, Country_Region == "US" & is.na(FIPS)==F)

# Import outcome data from JHU CSSE
covid = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_of_study,".csv")))
covid_us = subset(covid,Country_Region == "US" & is.na(FIPS)!=T)
covid_us = rbind(covid_us,subset(covid_us_hist, (!(FIPS %in% covid_us$FIPS))  & Confirmed == 0 & Deaths == 0 & is.na(FIPS)==F))

# Import historical 
covid_historical <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>%
                    drop_na("FIPS")
harvard_idx <- grep("X4.4.20", colnames(covid_historical))
time_series_columns <- colnames(covid_historical)[-1:-(harvard_idx-1)]
df_historical <- covid_historical[,c("FIPS", time_series_columns)]


# Import exposure PM2.5 data
county_pm = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_pm25.csv"))

county_temp = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/temp_seasonal_county.csv"))
# Import census, brfss, testing, mortality, hosptial beds data as potential confounders
county_census = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/census_county_interpolated.csv"))
county_brfss = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/brfss_county_interpolated.csv"))

state_test = read.csv(text=getURL("https://covidtracking.com/api/v1/states/daily.csv"))
state_test = subset(state_test, date ==paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))[,-20]
statecode = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/statecode.csv"))

hospitals = read.csv(text=getURL("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"))
hospitals$BEDS[hospitals$BEDS < 0] = 0

county_base_mortality = read.table(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_base_mortality.txt"), sep = "",header = T)
county_old_mortality = read.table(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_old_mortality.txt"), sep = "",header = T)
colnames(county_old_mortality)[4] = c("older_Population")
county_base_mortality = merge(county_base_mortality,county_old_mortality[,c(2,4)] ,by = "County.Code")
county_base_mortality$older_pecent = county_base_mortality$older_Population/county_base_mortality$Population

# merging data
state_test = merge(state_test,statecode,by.x = "state" ,by.y = "Code" )
state_test$totalTestResults <- state_test$positive + state_test$negative
# pm average over 17 years
county_pm_aggregated = county_pm %>% 
  group_by(fips) %>% 
  summarise(mean_pm25 = mean(pm25))
# pm most recent 2016
#county_pm_aggregated = subset(county_pm , year==2016)
#county_pm_aggregated$mean_pm25 = county_pm_aggregated$pm25

# pm average over 17 years
county_temp_aggregated = county_temp %>% 
  group_by(fips) %>% 
  summarise(mean_winter_temp= mean(winter_tmmx),
            mean_summer_temp= mean(summer_tmmx),
            mean_winter_rm= mean(winter_rmax),
            mean_summer_rm= mean(summer_rmax))

county_pm_aggregated = merge(county_pm_aggregated,county_temp_aggregated,by="fips",all.x = T)

county_hospitals_aggregated = hospitals %>%
  group_by(COUNTYFIPS) %>%
  summarise(beds = sum(BEDS))

county_census_aggregated2 = subset(county_census, year==2016)
county_census_aggregated2$q_popdensity = 1
quantile_popdensity = quantile(county_census_aggregated2$popdensity,c(0.25,0.5,0.75))
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity<=quantile_popdensity[1]] = 1
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[1] &
                                         county_census_aggregated2$popdensity<=quantile_popdensity[2]] = 2
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[2] &
                                         county_census_aggregated2$popdensity<=quantile_popdensity[3]] = 3
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[3]] = 4

county_brfss_aggregated = subset(county_brfss, year==2012)

county_census_aggregated2 = merge(county_census_aggregated2,county_brfss_aggregated,
                                  by="fips",all.x=T)

aggregate_pm = merge(county_pm_aggregated,covid_us,by.x="fips",by.y = "FIPS")

aggregate_pm_census = merge(aggregate_pm,county_census_aggregated2,by.x="fips",by.y = "fips")

aggregate_pm_census_cdc = merge(aggregate_pm_census,county_base_mortality[,c(1,4:5,8:9)],by.x = "fips",by.y = "County.Code",all.x = T)

aggregate_pm_census_cdc = aggregate_pm_census_cdc[is.na(aggregate_pm_census_cdc$fips) ==F,]

aggregate_pm_census_cdc_test = merge(aggregate_pm_census_cdc,state_test,by.x="Province_State",by.y = "State")
aggregate_pm_census_cdc_test = aggregate_pm_census_cdc_test %>%
  group_by(Province_State) %>%
  mutate(population_frac_county = population/sum(population),
         totalTestResults_county = population_frac_county*totalTestResults)


aggregate_pm_census_cdc_test_beds = merge(aggregate_pm_census_cdc_test,county_hospitals_aggregated,by.x = "fips.x",by.y = "COUNTYFIPS",all.x = T)

# transportation data
transportation_data <- read_dta("/Users/ozaltun/Desktop/ACS_county.dta") %>% mutate(FIPS = as.integer(FIPS)) %>% dplyr::select(FIPS, WRK_Drive, WRK_PublicTransit, WRK_Bike, WRK_Walk, WRKHOME)

# Preconditions
preconditions_data <- read.csv("/Users/ozaltun/Dropbox (MIT)/Data/Health/county_health_ranking/county_health_ranking_mean.csv") %>% mutate(FIPS = as.integer(FIPS))
# beds
beds_data <- read.csv("/Users/ozaltun/Desktop/County_HCF+HospBeds.csv") %>% select(County.FIPS.code, ICU.Beds)

# population weighted pm values

pm_weighted <- read.csv("/Users/ozaltun/Desktop/pm25_county_weighted_sum_2000_2018.csv") %>% 
              mutate(fips = as.integer(state_fips*1000+county_fips)) %>% group_by(fips) %>% 
              summarise(weighted_mean_pm25 = mean(PM_weighted,na.rm = TRUE))

df <- merge(aggregate_pm_census_cdc_test_beds, transportation_data, by.x = "fips", by.y="FIPS", all.x=T)
df <- merge(df, pm_weighted, by.x="fips",by.y="fips", all.x=T)
first_date <- read.csv("Data/first_date.csv")

df <- merge(df, first_date, by.x="fips",by.y="fips",all.x=T)
df <- merge(df, preconditions_data, by.x="fips", by.y="FIPS", all.x=T)
df <- merge(df, beds_data, by.x="fips", by.y="County.FIPS.code", all.x=T)
df <- df%>% rename(percent.smokers=Adult.smoking...Smokers, percent.obese=Adult.obesity...Obese, count.diabetic=Diabetic.screening...Diabetics) %>% 
            mutate(percent.diabetic = count.diabetic/population,
                    percent.uninsured = count.uninsured/population,
                    share.drive = WRK_Drive/population,
                    share.PublicTransit = WRK_PublicTransit/population,
                    share.Bike = WRK_Bike/population,
                    share.Walk = WRK_Walk/population,
                    share.Home = WRKHOME/population,
                    deaths_per_tests = Deaths/totalTestResults, 
                    deaths_per_capita = Deaths/population, 
                    tests_per_capita = totalTestResults/population,
                    beds_per_capita = beds/population,
                    income_bins = ntile(medhouseholdincome, 5),
                    education_bins = ntile(education, 5),
                    popdensity_bins = ntile(popdensity, 10))
df <- merge(df, df_historical, by.x="fips", by.y = "FIPS", all.x = T)
df <- df[which(!(df$Deaths ==0 & df$first_death_date!="")),] # Some inconsistencies

rm("df_historical", "first_date", "hospitals", "pm_weighted", "state_test","statecode","transportation_data","aggregate_pm", "aggregate_pm_census", "aggregate_pm_census_cdc","aggregate_pm_census_cdc_test","county_base_mortality","county_brfss", "county_brfss_aggregated","county_census","county_census_aggregated2","county_hospitals_aggregated","county_old_mortality","county_pm","county_pm_aggregated","county_temp","county_temp_aggregated","covid","covid_hist","covid_us","covid_us_hist", "covid_historical")
rm("beds_data", "preconditions_data")