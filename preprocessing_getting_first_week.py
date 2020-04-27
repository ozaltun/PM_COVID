import numpy as np
import pandas as pd

def get_date_of_first(dates_list, row):
    if row[0] >0:
        return dates_list[0]

    for i in range(1, len(row)):
        if row[i]>0 and row[i-1]==0:
            return dates_list[i]

    return 'Not found'

df_cases = pd.read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
df_cases = df_cases[df_cases.FIPS.isna() == False]

df_deaths = pd.read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
df_deaths = df_deaths[df_deaths.FIPS.isna() == False]

dates_cases = [i for i in df_cases.columns if '/20' in i]
dates_deaths = [i for i in df_deaths.columns if '/20' in i]

cases_id_columns = [i for i in df_cases.columns if '/20' not in i]
deaths_id_columns = [i for i in df_deaths.columns if '/20' not in i]


cases_list = list(df_cases[dates_cases].values)
cases_fips = list(df_cases['FIPS'].values)
first_cases_list = []
for i in range(len(cases_list)):
    first_cases_list.append([int(cases_fips[i]),get_date_of_first(dates_cases, cases_list[i])])


deaths_list = list(df_deaths[dates_deaths].values)
deaths_fips = list(df_deaths['FIPS'].values)
first_deaths_list = []
for i in range(len(deaths_list)):
    first_deaths_list.append([int(deaths_fips[i]),get_date_of_first(dates_deaths, deaths_list[i])])

df_first_cases = pd.DataFrame(first_cases_list, columns=['fips','first_case_date'])
df_first_deaths = pd.DataFrame(first_deaths_list, columns = ['fips','first_death_date'])

df = pd.concat([df_first_cases.set_index('fips'), df_first_deaths.set_index('fips')], axis=1).reset_index()
df = df.replace('Not found', np.nan)
df['first_case_date'] = pd.to_datetime(df['first_case_date'])
df['first_death_date'] = pd.to_datetime(df['first_death_date'])

df['first_case_week'] = df['first_case_date'].dt.to_period('W').apply(lambda r:r.start_time)
df['first_death_week'] = df['first_death_date'].dt.to_period('W').apply(lambda r:r.start_time)

df.to_csv("/Users/ozaltun/Documents/GitHub/PM_COVID/Data/first_date.csv")
