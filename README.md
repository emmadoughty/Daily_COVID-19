# Visualisation of COVID-19 official UK case data

## Data sources

I am aware that various projects are now using the data collated here. For scientific research, I am more than happy to collate data in a format that works best for your project or try and find the additional metadata that you might need- don't hestiate to reach out.

**COVID19_by_day.csv** contains daily numbers of cases, tests and deaths
- The Department of Health and Social Care [twitter](https://twitter.com/DHSCgovuk) is first to update with numbers of new confirmed cases, deaths and tests and these figures are initally used for daily updates.
- Collated data on daily confirmed cases are updated by [PHE](https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67). Differences in case numbers can be observed relative to those provided on twitter, even for for data released on previous days (presumably differences are due to improvements in data cleaning). Once PHE data are available I update my collated data with these values.

**COVID19_cum.csv** contains the total number of cases announced per day for each Upper Tier Local Authority (England),  country, NHS region and Scottish health board.


**NB DATA QUALIITY ISSUES: Total number of cases per geographic area sometimes decreases from one day to the next which makes little sense.**
- Cases per UTLA obtained daily from [PHE](https://www.arcgis.com/home/item.html?id=b684319181f94875a6879bbc833ca3a6). 
- Cases in each country collated from [PHE daily indicator updates](https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67)
- Cases in each NHS region collated from daily [PHE NHS regions updates](https://www.arcgis.com/home/item.html?id=ca796627a2294c51926865748c4a56e8)
- Cases in Scottish health boards collated from [Scottish government daily updates](https://www.gov.scot/coronavirus-covid-19/)

Previously I had a file containing calculated new cases per day that I was feeding into my own analyses/visualisations. I am no longer making these visualisations owing to the data quality concerns raised above. For the time being, I will only collate the raw PHE data and urge you to use that.

**ukmidyearestimates20182019ladcodes.xls** contains mid-2018 [population demographic data](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland) (age, gender, population, population density, deaths and more) from the Office for National Statistics for geographical areas reporting COVID-19 cases (Country, Region, Unitary authority corresponding with UTLAs). If you need help extracting and formatting this data for your research project, let me know and I'll try to help.

**Administrative_geography_data.csv** contains administrative geography codes for UTLAs and NHS regions as supplied for [PHE NHS regions updates](https://www.arcgis.com/home/item.html?id=ca796627a2294c51926865748c4a56e8) and [UTLA updates](https://www.arcgis.com/home/item.html?id=b684319181f94875a6879bbc833ca3a6)

## Analyses
#### Summary plots

![Daily and cumulative cases, tests and deaths, 10.03.2020](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Summary_plot.png)

