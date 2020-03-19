## Exploration of UK COVID-19 data: CFR statistics over time
## Emma Doughty (e.doughty6@gmail.com)

# Install libraries if necessary
#if(!require(ggpubr)) install.packages("ggpubr")
#if(!require(tidyverse)) install.packages("tidyverse")
#if(!require(scales)) install.packages("scales")

# Load libraries
library(readr)
library(tidyr)
library(plyr)
library(ggplot2)
library(scales)
library(ggpubr)

#Load data
COVID19_by_day <- read_csv("Data/COVID19_by_day.csv")
COVID19_by_day$Date <- as.Date(COVID19_by_day$Date, format = "%d/%m/%y")
COVID19_by_day <- COVID19_by_day[order(COVID19_by_day$Date),]

# Data taken from: https://github.com/midas-network/COVID-19/tree/master/parameter_estimates/2019_novel_coronavirus
# Looks case fatality rate in terms of number of deaths vs estimated number of cases at time of onset
# Fatality rate plotted for a range of reporting times 
# Reporting times chosen to fit with:
  # 14-23d from symptom onset to death
  # ~5d from onset to reporting
  # Therefore, time from reporting to death min =14-5 = 9d; max = 23-5 =18d; avg = 17d-5 = 12d
  # Chosen to show 19, 17, 15, 13, 11, 9d to give even spread across the middle/lower 
  # (4-7d exposure to symptom onset)

#### Generate dataframe for plot ####
# Cumulative deaths vs cumulative cases 17 days prior
df_Deaths17d <- data.frame(Date = COVID19_by_day$Date, 
                           CumCases = COVID19_by_day$CumCases, 
                           CumDeaths = COVID19_by_day$CumDeaths, 
                           DeathStat = "17 days", StatValue = "0", 
                           stringsAsFactors = FALSE)

df_Deaths17d[,c(5)] <- sapply(df_Deaths17d$StatValue, as.double)

for (i in 36:nrow(df_Deaths17d)) {
  df_Deaths17d[[i, 5]] <- (100 * (df_Deaths17d[i,3]/df_Deaths17d[i-17,2]))
}

# Cumulative deaths vs cumulative cases 15 days prior
df_Deaths15d <- data.frame(Date = COVID19_by_day$Date, 
                            CumCases = COVID19_by_day$CumCases, 
                            CumDeaths = COVID19_by_day$CumDeaths, 
                            DeathStat = "15 days", StatValue = "0", 
                            stringsAsFactors = FALSE)
 
df_Deaths15d[,c(5)] <- sapply(df_Deaths15d$StatValue, as.double)
 
for (i in 36:nrow(df_Deaths15d)) {
  df_Deaths15d[[i, 5]] <- (100 * (df_Deaths15d[i,3]/df_Deaths15d[i-15,2]))
}

# Cumulative deaths vs cumulative cases 13 days prior
df_Deaths13d <- data.frame(Date = COVID19_by_day$Date, 
                           CumCases = COVID19_by_day$CumCases, 
                           CumDeaths = COVID19_by_day$CumDeaths, 
                           DeathStat = "13 days", StatValue = "0", 
                           stringsAsFactors = FALSE)

df_Deaths13d[,c(5)] <- sapply(df_Deaths13d$StatValue, as.double)

for (i in 36:nrow(df_Deaths13d)) {
  df_Deaths13d[[i, 5]] <- (100 * (df_Deaths13d[i,3]/df_Deaths13d[i-13,2]))
}

# Cumulative deaths vs cumulative cases 11 days prior
df_Deaths11d <- data.frame(Date = COVID19_by_day$Date, 
                           CumCases = COVID19_by_day$CumCases, 
                           CumDeaths = COVID19_by_day$CumDeaths, 
                           DeathStat = "11 days", StatValue = "0", 
                           stringsAsFactors = FALSE)

df_Deaths11d[,c(5)] <- sapply(df_Deaths11d$StatValue, as.double)

for (i in 36:nrow(df_Deaths11d)) {
  df_Deaths11d[[i, 5]] <- (100 * (df_Deaths11d[i,3]/df_Deaths11d[i-11,2]))
}

# Cumulative deaths vs cumulative cases 9 days prior
df_Deaths9d <- data.frame(Date = COVID19_by_day$Date, 
                          CumCases = COVID19_by_day$CumCases, 
                          CumDeaths = COVID19_by_day$CumDeaths, 
                          DeathStat = "9 days", StatValue = "0", 
                          stringsAsFactors = FALSE)
df_Deaths9d[,c(5)] <- sapply(df_Deaths9d$StatValue, as.double)
for (i in 36:nrow(df_Deaths9d)) {
  df_Deaths9d[[i, 5]] <- (100 * (df_Deaths9d[i,3]/df_Deaths9d[i-9,2]))
}

# Merge data for all estimated dates of reporting
df_DeathsStats <- rbind(df_Deaths17d, df_Deaths15d, df_Deaths13d, df_Deaths11d, df_Deaths9d)
df_DeathsStats <-df_DeathsStats[!(df_DeathsStats$CumDeaths==0),]

#### Plot graph for mortality stats ####
DeathStats_Sigfig <- signif(sum(df_DeathsStats$StatValue))
DeathStats_Factor <- 10^(floor(log10(signif(max(df_DeathsStats$StatValue)))))
DeathStats_Max <- round_any(max(df_DeathsStats$StatValue), DeathStats_Factor, f=ceiling)
DeathStats_Breaks <- ceiling(DeathStats_Factor/2)

plot_CFR <- ggplot(data=df_DeathsStats, aes(x=Date, y=df_DeathsStats$StatValue, group=DeathStat)) +
  geom_line(aes(color=DeathStat))+
  geom_point()+
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(df_DeathsStats$Date), max(df_DeathsStats$Date)),
               breaks = seq(min(df_DeathsStats$Date), max(df_DeathsStats$Date), 1)) +
  scale_y_continuous(limits = c(0, 370), 
                     breaks = seq(0, 370, 10),
                     expand = c(0, 0)) +
  ylab("% reported cases that lead to death \n 
       based on estimated time from reporting to death specified in legend") + 
  xlab("Date deaths announced") +
  ggtitle("Deaths as a proportion of reported cases x days ago") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        legend.position = "bottom",
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank()) + 
  labs(colour = "Reporting to death:") + 
  scale_color_discrete(breaks=c("9 days","11 days","13 days","15 days","17 days")) +
  geom_hline(yintercept = 3.4, color="grey", linetype="dashed") +
  annotate("text", min(df_DeathsStats$Date), 3.4, 
           vjust = -1, hjust = -5.1, label = "3.4% (WHO estimated CFR)",
           size = 2.5, color="grey")

ggsave("CFR_Stats_plot.png")

# Average time from reporting to death must be fairly short, 
# testing must have typically only identified the most severe cases 
# (where all/most lead to death) and/or
# case fatality rate is very hgih in the UK



