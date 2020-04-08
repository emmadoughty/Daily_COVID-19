# Collation and visualisation of official UK COVID-19 data

![Daily and cumulative cases, tests and deaths, 10.03.2020](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Summary_plot.png)

## COVID-19 data

**Many sources of official UK data only the present day's data and do not offer historical/time-series figures. Here, I am collating the UK data avilable on a daily basis as a resource for analysis and visualisation. If you are finding this helpful, I'd really appreciate appropriate creditation in your work as this is a largely manual and time-consuming process. Feel free to drop me an email at e.doughty@bham.ac.uk.**

Whilst I am trying hard to ensure accuracy, collation of this data is largely a manual process, owing to the inconsistency of reporting coming from the governments/public health agencies, and therefore **subject to error and change**. Please do raise issues via GitHub if you notice any discrepancies with the data or any queries and I will investigate them asap.

For most data, two formats are avialable: The "wide" format that I originally created and a "long" format that includes GSS_CD geographical codes and indication of data type. For researchers, I am happy to compile data in the formats needed for your project or to pull together data from additional sources, e.g. additional data from the [Public Health Wales dashboard](https://public.tableau.com/profile/public.health.wales.health.protection#!/vizhome/RapidCOVID-19virology-Public/Headlinesummary) (I have started taking screenshots to capture as much of this as possible on a daily basis and can enter the data into a csv if useful to you) or reformatting data on [daily deaths per NHS trust/NHS England region](https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/).

## Available files and COVID-19 data sources

### UK cases, deaths and tests


[COVID19_by_day.csv](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/COVID19_by_day.csv) contains daily numbers of cases, tests, deaths and recoveries
- The Department of Health and Social Care [twitter](https://twitter.com/DHSCgovuk) is first to update with numbers of confirmed cases, deaths and tests and these figures are initially used for daily updates.
- NB From 31/03 - 05/04/2020 inclusive, DHSC did not report the number of cumulative tests conducted but instead only the cumulative people tested. From 06/04/2020 onwards, they are reporting both number of tests conducted and people tested. See [reporting notes](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Reporting_notes.txt) for further information. From 31/03 - 06/04/2020, I switched the test figures included in the COVID19_by_day.csv file changed from being "tests concluded" to "people tested" but subsequently, this file contains number of tests conducted only (and NAs where these figures were not released). NewTests has been calculated by subtraction of cumulative tests that day from cumulative tests the day previously. This is NOT equal to the number of tests that were conducted on the day before reporting as included in tweets on 06/04 and subsequently (and it is not clear how the number of tests reported to have occurred on the previous day is calculated). 
- Collated data on daily confirmed cases (and deaths added on 27/03/2020 but backdated to 10/03/2020 with some ommissions) are updated by [PHE](https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67). Sometimes, differences in numbers have been observed relative to those provided on twitter (presumably differences are due to improvements in data cleaning) so once PHE data are available I update this collated data with these values.
- Numbers of recoveries are taken from the PHE [dashboard](https://fingertips.phe.org.uk/documents/Historic%20COVID-19%20Dashboard%20Data.xlsx) when available. I understand a new process for collecting numbers of recovered patients is in development.

### Cases by area


The total number of cases announced per day for each country, Upper Tier Local Authority (England), NHS region (England) and Scottish health board: [Wide format (COVID19_cum.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/COVID19_cum.csv), [Long format (cases_by_utla.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/cases_by_utla.csv)
- Cases in each country collated from [PHE daily indicator updates](https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67)
- Cases per UTLA obtained daily from [PHE](https://www.arcgis.com/home/item.html?id=b684319181f94875a6879bbc833ca3a6) 
- Cases in each NHS region collated from daily [PHE NHS regions updates](https://www.arcgis.com/home/item.html?id=ca796627a2294c51926865748c4a56e8) and previously from the "information to the public" page on the UK government [website](https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public)
- Cases in Scottish health boards initially collated from [Scottish government daily updates](https://www.gov.scot/coronavirus-covid-19/) before a second webpage was provided for [tests and cases in Scotland](https://www.gov.scot/publications/coronavirus-covid-19-tests-and-cases-in-scotland/)
- These files are subject to the minimum possible changes to structure and do not contain data for cases in unconfirmed locations.


**NB DATA QUALITY ISSUES: Early in the epidemic, total number of cases per geographic area sometimes decreased from one day to the next, particularly early in the epidemic, which makes little sense.**

### Expanded cases by area
As cases per area described above but additionally with local-breakdown of data for Wales and Northern Ireland, plus earlier data when available: [Wide format (COVID19_by_area.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/COVID19_by_area.csv), [Long format (cases_by_area.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/cases_by_area.csv)
- Data was initially collated from the [Welsh government daily updates](https://covid19-phwstatement.nhs.wales/) which fiest provided data from local authorities then moved to reporting for health boards only. Since the [PHW dashboard](https://public.tableau.com/profile/public.health.wales.health.protection#!/vizhome/RapidCOVID-19virology-Public/Headlinesummary) became available, data is now provided for both health boards and local authorities.
- Cases in Northern Irish Local Government Districts are collated from [Northern Ireland's Public Health Authority Surveillance reports](https://www.publichealth.hscni.net/publications/covid-19-surveillance-reports) and previously from [NI situation updates](https://www.publichealth.hscni.net/news/covid-19-coronavirus)
- Earlier data collated from ad hoc official sources only, e.g. statements and reports from government and public health agencies only
- These files are subject to change to provide the maximum information possible. They include cases with unconfirmed locations (English NHSRs and UTLAs; Welsh health boards (also inc residents outside Wales); NI LGDs)


### Deaths by area
The total number of deaths reported daily from each country of the UK: [Wide format (COVID19_deaths.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/COVID19_deaths.csv), [Long format (deaths_by_area.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/deaths_by_area.csv)
From 27/03/2020, deaths per country are reported daily and collated from [daily indicators](https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67). Prior to 27/03/2020:
- Deaths in Scotland collated from [Scottish government daily updates](https://www.gov.scot/coronavirus-covid-19/)
- Deaths in Wales collated from [Welsh government daily updates](https://covid19-phwstatement.nhs.wales/)
- Deaths in Northern Ireland collated from Northern Irish government daily updates [here](https://www.publichealth.hscni.net/news/covid-19-coronavirus) and [here](https://www.publichealth.hscni.net/publications/covid-19-surveillance-reports)
- Deaths in UK collated from [Department of Health and Social Care twitter](https://twitter.com/DHSCgovuk)
- Deaths in England were not published but calculated by subtraction of total cases in Scotland, Wales and Northern Ireland from all UK cases

NB [DHSC twitter](https://twitter.com/DHSCgovuk/status/1243237211119800323) announced on that from 25/03/2020 their reporting time period for deaths would change. From 25/03/2020, the latest figures counted from the previous day (for a period from 5pm-5pm) are being published at 2pm the following day. The figures for 25/03/2020 did not cover a full 24 hour period, comprising the period from 9am 24 March to 5pm on 24 March. Figures issued on 26/03/2020 are recorded as of 5pm 24 March to 5pm 25 March.

NB From 29/03/2020, I have noticed that the number of deaths reported by the governments for Scotland and Northern Ireland is not reported in the UK government daily indicators until the following day (i.e. there seems to be a one day lag).

NB From 02/04/2020, Scotland changed the way that deaths are [reported](https://www.gov.scot/news/new-process-for-reporting-covid-19-deaths/), increasing the number of deaths where the link with COVID-19 could be reported. "There is no change in the definition used to report deaths of COVID-19, which is defined as an individual who has died and has had a laboratory confirmed report of COVID-19 in the 28 days prior to death." Revised data for 31/03 to 02/04 were provided in line with the new definition, detailed [here](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Reporting_notes.txt).

### Case demographics for Northern Ireland
[NI_cases_demographics.csv](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/NI_cases_demographics.csv) contains the data released so far from NI on about the age and sex of COVID-19 patients from [Nothern Ireland's Public Health Authority Surveillance reports](https://www.publichealth.hscni.net/publications/covid-19-surveillance-reports)

### Death demographics for England and Wales
The Office for National Statistics publish [data](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/bulletins/deathsregisteredweeklyinenglandandwalesprovisional/latest) on the deaths across England and Wales each week. As of 31 March, these data include figures for the number of COVID-19 associated deaths in each age group for males and females, and in geographic regions. Data re-formatted for each of use in [Eng_Wales_Weekly_Deaths_Demographics.csv](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/Eng_Wales_Weekly_Deaths_Demographics.csv) with important explanations of where this data comes from [here](https://blog.ons.gov.uk/2020/03/31/counting-deaths-involving-the-coronavirus-covid-19/).

## Notes on COVID-19 reporting
- Reporting methods from the public health authorities have been variable between them and throughout the time of the epidemic. Discrepancies often seem to arise due to variations in time of data extraction/reporting on a given day. I have started compiling [notes](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Reporting_notes.txt) on reporting variations/changes that I have noticed.
- PHE offer an ["about the data" pdf](https://fingertips.phe.org.uk/documents/PHE%20COVID-19%20Dashboard%20Metadata.pdf) that describes how their data are collected. 

## Available metadata files

These administrative geography codes are included in the [Long format (cases_by_area.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/cases_by_area.csv) and [Long format (deaths_by_area.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/deaths_by_area.csv)

### Administrative geography codes
[Administrative_geography_data.csv](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/Administrative_geography_data.csv) contains geographic code and corresponding names for UTLAs and NHS regions as supplied for [PHE NHS regions updates](https://www.arcgis.com/home/item.html?id=ca796627a2294c51926865748c4a56e8) and [UTLA updates](https://www.arcgis.com/home/item.html?id=b684319181f94875a6879bbc833ca3a6)

[Register_of_Geographic_Codes_(December_2019)_UK.zip](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/Register_of_Geographic_Codes_(December_2019)_UK.zip) contains geographic codes for Welsh Health Boaords (W11) and Welsh Loacal Authorities (W06) from the [Office for National Statistics](http://geoportal.statistics.gov.uk/datasets/register-of-geographic-codes-december-2019-for-the-united-kingdom)

[00547737.xlsx](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/00547737.xlsx) contains geographic codes for Scottish Health Boards (S08_HB) from the [Scottish Standard Geography Code Register](https://www2.gov.scot/Topics/Statistics/sns/SNSRef/StanGeoCodeRegister)

[Local_Government_Districts_December_2019_Names_and_Codes_in_Northern_Ireland.csv](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/Local_Government_Districts_December_2019_Names_and_Codes_in_Northern_Ireland.csv) contains geographic codes for Northern Irish Local Government Districts from the [Office for National Statistics](https://geoportal.statistics.gov.uk/datasets/local-government-districts-december-2019-names-and-codes-in-northern-ireland)

### Population demographic data
[ukmidyearestimates20182019ladcodes.xls](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland) contains mid-2018 data on age, gender, population, population density, deaths etc for geographical areas reporting COVID-19 cases (Country, Region, Unitary authority corresponding with UTLAs, but EXCLUDING NHS REGIONS) from the Office for National Statistics.


## Other Visualisations and Analyses

Other people have nice visualisations of/including UK data:
- [Public Health England dashboard](https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14)
- [Public Health Wales dashboard](https://public.tableau.com/profile/public.health.wales.health.protection#!/vizhome/RapidCOVID-19virology-Public/Headlinesummary) (includes much more data)
- [Number of cases per day, maps showing number of cases and cases per capita](https://trafforddatalab.shinyapps.io/covid-19)
- [Interactive map showing cases per area with bar charts of the number of cases over time](https://www.arcgis.com/home/webmap/viewer.html?webmap=2c122cca2af644339cb636a9844672af&extent=-1.8598,50.9565,2.1969,52.3368)
- [Cases and deaths per day vs other countries](https://public.tableau.com/profile/andrewjmdata#!/vizhome/CoronaVirusMarch2020v4/Introduction?publish=yes)

