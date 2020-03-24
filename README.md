# Visualisation and collation of COVID-19 official UK data

![Daily and cumulative cases, tests and deaths, 10.03.2020](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Summary_plot.png)

## Data sources

I am aware that various projects are now using the data collated here. For scientific research, I am more than happy to collate data in a format that works best for your project or try and find the additional metadata that you might need- don't hestiate to reach out.

### COVID-19 data

**UK cases, deaths and tests**


[COVID19_by_day.csv](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/COVID19_by_day.csv) contains daily numbers of cases, tests, deaths and recoveries
- The Department of Health and Social Care [twitter](https://twitter.com/DHSCgovuk) is first to update with numbers of new confirmed cases, deaths and tests and these figures are initally used for daily updates.
- Collated data on daily confirmed cases are updated by [PHE](https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67). Differences in case numbers can be observed relative to those provided on twitter, even for for data released on previous days (presumably differences are due to improvements in data cleaning). Once PHE data are available I update my collated data with these values.
- Numbers of recoveries are taken from the PHE dashbord each day after it updates

**Original cases by area** 


The total number of cases announced per day for each Upper Tier Local Authority (England),  country, NHS region and Scottish health board: [Wide format (COVID19_cum.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/COVID19_cum.csv), [Long format (cases_by_utla.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/cases_by_utla.csv)
- Cases per UTLA obtained daily from [PHE](https://www.arcgis.com/home/item.html?id=b684319181f94875a6879bbc833ca3a6) 
- Cases in each country collated from [PHE daily indicator updates](https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67)
- Cases in each NHS region collated from daily [PHE NHS regions updates](https://www.arcgis.com/home/item.html?id=ca796627a2294c51926865748c4a56e8)
- Cases in Scottish health boards collated from [Scottish government daily updates](https://www.gov.scot/coronavirus-covid-19/)


**NB DATA QUALIITY ISSUES: Total number of cases per geographic area sometimes decreases from one day to the next which makes little sense.**

**Expanded cases by area**
As original cases per area above but additionally with local-breakdown data for Wales and data prior to 07/03/2020 when available: [Wide format (COVID19_by_area.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/COVID19_by_area.csv), [Long format (cases_by_area.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/cases_by_area.csv)
- Cases in Welsh health boards collated from [Welsh government daily updates](https://covid19-phwstatement.nhs.wales/)
- Addtional data is from official sources only, e.g. statements and reports from governemnt and public health agencies




Previously I included a file containing calculated new cases per day that I was feeding into my own analyses/visualisations. I am no longer making these visualisations owing to the data quality concerns raised above. For the time being, I will only collate the raw PHE data and urge you to use that.

**Deaths by area**
The total number of deaths reported daily from each country of the UK: [Wide format (COVID19_deaths.csv)], (https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/COVID19_deaths.csv), [Long format (deaths_by_area.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/deaths_by_area.csv)
- Deaths in Scotland collated from [Scottish government daily updates](https://www.gov.scot/coronavirus-covid-19/)
- Deaths in Wales collated from [Welsh government daily updates](https://covid19-phwstatement.nhs.wales/)
- Deaths in Northern Ireland collated from [Northern Irish government daily updates](https://www.health-ni.gov.uk/news/)
- Deaths in UK collated from [Department of Health and Social Care twitter](https://twitter.com/DHSCgovuk)
- Deaths in England are not published but calculated by subtraction of total cases in Scotland, Wales and Northern Ireland from all UK cases

**[Case demographics for Northern Ireland]**
- [NI_cases_demographics.csv](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/NI_cases_demographics.csv) contains the data released so far from NI on about the age and sex of COVID-19 patients



### Metadata

These administrative geogrpahy codes are included in the [Long format (cases_by_area.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/cases_by_area.csv) and [Long format (deaths_by_area.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/deaths_by_area.csv)

**Administrative geography codes**
[Administrative_geography_data.csv](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/Administrative_geography_data.csv) contains geographic code and corresponding names for UTLAs and NHS regions as supplied for [PHE NHS regions updates](https://www.arcgis.com/home/item.html?id=ca796627a2294c51926865748c4a56e8) and [UTLA updates](https://www.arcgis.com/home/item.html?id=b684319181f94875a6879bbc833ca3a6)

[Register_of_Geographic_Codes_(December_2019)_UK.zip](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/Register_of_Geographic_Codes_(December_2019)_UK.zip) contains geographic codes for Welsh Health Boaords (W11) and Welsh Loacal Authorities (W06) from the [Office for National Statistics](http://geoportal.statistics.gov.uk/datasets/register-of-geographic-codes-december-2019-for-the-united-kingdom)

[00547737.xlsx](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/00547737.xlsx) contains geographic codes for Scottish Health Boards (S08_HB) from the [Scottish Standard Geography Code Reister](https://www2.gov.scot/Topics/Statistics/sns/SNSRef/StanGeoCodeRegister)

**ukmidyearestimates20182019ladcodes.xls** contains mid-2018 [population demographic data](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland) (age, gender, population, population density, deaths and more) from the Office for National Statistics for geographical areas reporting COVID-19 cases (Country, Region, Unitary authority corresponding with UTLAs, but EXCLUDING NHS REGIONS). If you need help extracting and formatting this data for your research project, let me know and I'll try to help.



## Other Analyses

Other people have nice visualisations [here](https://www.arcgis.com/home/webmap/viewer.html?webmap=2c122cca2af644339cb636a9844672af&extent=-1.8598,50.9565,2.1969,52.3368)
https://trafforddatalab.shinyapps.io/covid-19
