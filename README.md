# Collation and visualisation of official UK COVID-19 data

![Daily and cumulative cases, tests and deaths, 10.03.2020](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Summary_plot.png)

## COVID-19 data

**Many sources of official UK data only the present day's data and do not offer historical/time-series figures. Here, I am collating the UK data that is not availble from other UK sources as a resource for analysis and visualisation. If you are finding this helpful, I'd really appreciate appropriate creditation in your work as this is a largely manual and time-consuming process. Feel free to drop me an email at e.doughty@bham.ac.uk.**

Availability of time-series data from official sources has been improving and as it does, I intend to stop collating such data in this repo. The following are now available directly from these official UK sources in machine-readable format:
- [Daily COVID-19 cases by date of sampling in England](https://coronavirus.data.gov.uk/#) (for England, and the regions and upper tier local authorities of England)
- [Daily COVID-19 deaths in hospitals in England by reporting date](https://coronavirus.data.gov.uk/#) (for the UK, and each country of the UK)
- [Daily COVID-19 deaths in hospitals in England by by date of death](https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/)(total, and broken down for NHS trusts, regions and ages group)
- [Weekly COVID-19 deaths occuring in countries of the UK (mostly England and Wales)](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales) (in hospitals and in the community, by age group and gender, by location)

At present, collated data for Wales, Scotland and Northern Ireland is not availble via official sources and I am collating it here, alongside data for testing (more deatils on this to be privded soon). For researchers, I am happy to compile data in the formats needed for your project or to pull together data from additional sources, e.g. additional data for Wales, Scotland and Northern Ireland (I have started capturing as much of this data as possible on a daily basis so I have it available, even if I have not yet collated it).

Whilst I am trying hard to ensure accuracy, collation of this data is largely a manual process, owing to the inconsistency of reporting coming from the governments/public health agencies, and therefore **subject to error and change**. Please do raise issues via GitHub if you notice any discrepancies with the data or any queries and I will investigate them asap.

## Available files and COVID-19 data sources

### UK cases, deaths and tests


[COVID19_by_day.csv](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/COVID19_by_day.csv) contains daily numbers of cases, tests, deaths and recoveries
- The Department of Health and Social Care [twitter](https://twitter.com/DHSCgovuk) is first to update with numbers of confirmed cases, deaths and tests and these figures are initially used for daily updates.
- NB From 31/03 - 05/04/2020 inclusive, DHSC did not report the number of cumulative tests conducted but instead only the cumulative people tested. From 06/04/2020 onwards, they are reporting both number of tests conducted and people tested. See [reporting notes](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Reporting_notes.txt) for further information. From 31/03 - 06/04/2020, I switched the test figures included in the COVID19_by_day.csv file changed from being "tests concluded" to "people tested" but subsequently, this file contains number of tests conducted only (and NAs where these figures were not released). NewTests has been calculated by subtraction of cumulative tests that day from cumulative tests the day previously. This is NOT equal to the number of tests that were conducted on the day before reporting as included in tweets on 06/04 and subsequently (and it is not clear how the number of tests reported to have occurred on the previous day is calculated). 
- Collated data on daily confirmed cases (and deaths added on 27/03/2020 but backdated to 10/03/2020 with some ommissions) are updated by [PHE](https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67). Sometimes, differences in numbers have been observed relative to those provided on twitter (presumably differences are due to improvements in data cleaning) so once PHE data are available I update this collated data with these values. From 10/04/2020, positive cases arising from "pillar 2" tests (healthcare workers and their household members) were additonally reported. The number of cases in COVID19_by_day.csv currently only contains only the "pillar 1" tests.
- Numbers of recoveries are taken from the PHE [dashboard](https://fingertips.phe.org.uk/documents/Historic%20COVID-19%20Dashboard%20Data.xlsx) when available. I understand a new process for collecting numbers of recovered patients is in development.

### Cases by area
Cases in Wales, Scotland and Northern Ireland: [Wide format (COVID19_by_area.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/COVID19_by_area.csv), [Long format (cases_by_area.csv)](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/cases_by_area.csv)
- Cases in Scottish health boards initially collated from [Scottish government daily updates](https://www.gov.scot/coronavirus-covid-19/) before a second webpage was provided for [tests and cases in Scotland](https://www.gov.scot/publications/coronavirus-covid-19-tests-and-cases-in-scotland/)
- Data was initially collated from the [Welsh government daily updates](https://covid19-phwstatement.nhs.wales/) which first provided data from local authorities then moved to reporting for health boards only. Since the [PHW dashboard](https://public.tableau.com/profile/public.health.wales.health.protection#!/vizhome/RapidCOVID-19virology-Public/Headlinesummary) became available, data is now provided for both health boards and local authorities.
- Cases in Northern Irish Local Government Districts are collated from [Northern Ireland's Public Health Authority Surveillance reports](https://www.publichealth.hscni.net/publications/covid-19-surveillance-reports) and previously from [NI situation updates](https://www.publichealth.hscni.net/news/covid-19-coronavirus)
- Earlier data collated from ad hoc official sources only, e.g. statements and reports from government and public health agencies only
- These files are subject to change to provide the maximum information possible. They include cases with unconfirmed locations (English NHSRs and UTLAs; Welsh health boards (also inc residents outside Wales); NI LGDs)

## Notes on COVID-19 reporting
- Reporting methods from the public health authorities have been variable between them and throughout the time of the epidemic. Discrepancies often seem to arise due to variations in time of data extraction/reporting on a given day. I have started compiling [notes](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Reporting_notes.txt) on reporting variations/changes that I have noticed. Some of these notes are more relevan to older data formats that I am no longer maintaining in this repo
- PHE offer an ["about the data" page](https://coronavirus.data.gov.uk/about) that describes how their data are collected. 

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

