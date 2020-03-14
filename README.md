# Visualisation of COVID-19 official UK case data

## Data sources

**COVID19_by_day**.csv contains daily numbers of cases, tests and deaths
- The Department of Health and Social Care [twitter](https://twitter.com/DHSCgovuk) is first to update with numbers of new confirmed cases, deaths and tests and these figures are initally used for daily updates.

- Collated data on daily confirmed cases are updated by PHE [PHE](https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67). Differences in case numbers can be observed relative to those provided on twitter , even for for data released on previous days (presumably differences are due to improvements in data cleaning). Once PHE data are available I update my collated data with these values.

**COVID19_by_LA.csv** contains the number of. new cases per day for each Upper Tier Local Authority (England)
- Cumulative cases per UTLA obtained daily from [PHE](https://www.arcgis.com/home/item.html?id=b684319181f94875a6879bbc833ca3a6). When calculating new cases per UTLA, it can sometimes be observed that the cumulative numbers of cases per UTLA goes down from one day to the next (presumably also due to data cleaning).
  - In the data that I'm calculating for new cases per day, if cumulative cases decrease in a UTLA, I'm deducting cases from the latest day when positive cases were confirmed such that that total number of cases per UTLA is always in line with the most up-to-date cumulative counts from PHE.
  - See **Local_breakdown_raw.csv** for the original cumulative values 

**Local_regions.csv** contains  numbers of cases with national breakdowns, NHS regions and Scottish heath boards
- Data for cases in each nation collated from [PHE daily indicator updates](https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67)
- Data for cases in each NHS region collated from daily [PHE UTLA updates](https://www.arcgis.com/home/item.html?id=ca796627a2294c51926865748c4a56e8)
- Data for cases in Scottish health boards collated from [Scottish government daily updates](https://www.gov.scot/coronavirus-covid-19/)

## Analyses
#### Summary plots

![Daily and cumulative cases, tests and deaths, 10.03.2020](https://github.com/emmadoughty/Daily_COVID-19/blob/master/Summary_plot.png)

#### Investigation of numbers of reported cases

The number of daily tests conducted in the UK had been decreasing until 13/03/2020, whilst the number of cases has been increasing (see graphs above). I was interested in whether the number of reported cases is truly reflective of the number of infected people in the UK. 

To investigate this, I've calculated estimated case fatality rates (CFR) from available UK data which can be used to indicate total cases (if the number of reported cases is accurate, the calculated CFR should be equal to the expected CFR). The expectation is that CFR is around 3.4 % as reported by the [WHO](https://www.who.int/dg/speeches/detail/who-director-general-s-opening-remarks-at-the-media-briefing-on-covid-19---3-march-2020); CFR is variable between populations and calculation methods, however would fallen somewhere in this ball park. The cumulative number of deaths in the UK is reported each day, but these people would have been infected (and their cases reported) some time previously. Available [data](https://github.com/midas-network/COVID-19/tree/master/parameter_estimates/2019_novel_coronavirus) suggest that time from symptom onset to death is around 14-22 days and that time from symptom onset to reporting is around 5 days, so I have calculated that the time from reporting to death is in the realm of 9 to 17 days. I have plotted calculated CFR using total deaths as a percentage of cumulative cases reported at 7, 9, 11, 13, 15 and 17 days previously. 

Even with the minimum time from reporting to death estimated previously (9 days), the calculated CFR at time of estimated reporting is consistently above 10%. With longer estimates of time from reporting to death, the calculated CFR are even higher. Importantly, this is not consistent with the ball-park expected CFR (~3.4%), meaning that either the true UK CFR is much much higher than expected or, more likely I think, that the number of cases reported significantly underestimates the number of true cases in the UK population.

![Estimated case fatality rates for different times from reporting to death](https://github.com/emmadoughty/Daily_COVID-19/blob/master/CFR_Stats_plot.png)

