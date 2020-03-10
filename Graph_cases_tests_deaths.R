#Load libraries
if(!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)
library(readr)
library(tidyr)
library(plyr)
library(ggplot2)
library(scales)

#Load data
COVID19_by_day <- read_csv("COVID19_by_day.csv")
COVID19_by_day$Date <- as.Date(COVID19_by_day$Date, format = "%d/%m/%y")
COVID19_by_day <- COVID19_by_day[order(COVID19_by_day$Date),]

#### CASES per day ####
# Read in new cases per day from summarised data, format date correctly
df_NewCases <- data.frame(Date=c(COVID19_by_day$Date), ntimes=c(COVID19_by_day$NewCases))
df_NewCases<- as.data.frame(lapply(df_NewCases, rep, df_NewCases$ntimes))

# Plot number of NEW CASES per day
Sigfig <- signif(max(COVID19_by_day$NewCases))
Factor <- 10^(floor(log10(Sigfig)))
MaxNewCases <- round_any(max(COVID19_by_day$NewCases), Factor, f=ceiling)
Breaks <- ceiling(Factor/2)

plot_NewCases <- ggplot(data=df_NewCases, aes(x=df_NewCases$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_day$Date), max(COVID19_by_day$Date)),
               breaks = seq(min(COVID19_by_day$Date), max(COVID19_by_day$Date), 1)) +
  scale_y_continuous(limits = c(0,MaxNewCases), 
                     breaks = seq(0, MaxNewCases, Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())

plot_NewCases

# Plot CUMULATIVE CASES per day
Sigfig <- signif(max(COVID19_by_day$CumCases))
Factor <- 10^(floor(log10(Sigfig)))
MaxCumCases <- round_any(max(COVID19_by_day$CumCases), Factor, f=ceiling)
Breaks <- ceiling(Factor/2)

plot_CumCases <- ggplot(data=df_NewCases, aes(x=df_NewCases$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, boundary = 1, colour="black") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_day$Date), max(COVID19_by_day$Date)),
               breaks = seq(min(COVID19_by_day$Date), max(COVID19_by_day$Date), 1)) +
  scale_y_continuous(limits = c(0, MaxCumCases), 
                     breaks = seq(0, MaxCumCases, Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())

plot_CumCases

#### TESTS per day ####
# Read in new tests per day from summarised data, format date correctly
df_NewTests <- data.frame(Date=c(COVID19_by_day$Date), ntimes=c(COVID19_by_day$NewTests))
df_NewTests<- as.data.frame(lapply(df_NewTests, rep, df_NewTests$ntimes))

# Plot NEW TESTS per day #
Sigfig <- signif(max(COVID19_by_day$NewTests))
Factor <- 10^(floor(log10(Sigfig)))
MaxNewTests <- round_any(max(COVID19_by_day$NewTests), Factor, f=ceiling)
Breaks <- ceiling(Factor/2)

plot_NewTests <- ggplot(data=df_NewTests, aes(x=df_NewTests$Date)) + 
  geom_histogram(binwidth=1, boundary = 1, colour="black") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_day$Date), max(COVID19_by_day$Date)),
               breaks = seq(min(COVID19_by_day$Date), max(COVID19_by_day$Date), 1)) +
  scale_y_continuous(limits = c(0, MaxNewTests), 
                     breaks = seq(0, MaxNewTests, Breaks), expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())

plot_NewTests

# Plot CUMULATIVE TESTS per day
Sigfig <- signif(max(COVID19_by_day$CumTests))
Factor <- 10^(floor(log10(Sigfig)))
MaxCumTests <- round_any(max(COVID19_by_day$CumTests), Factor, f=ceiling)
Breaks <- ceiling(Factor/2)

plot_CumTests <- ggplot(data=df_NewTests, aes(x=df_NewTests$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, boundary = 1, colour="black") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_day$Date), max(COVID19_by_day$Date)),
               breaks = seq(min(COVID19_by_day$Date), max(COVID19_by_day$Date), 1)) +
  scale_y_continuous(limits = c(0, MaxCumTests), 
                     breaks = seq(0, MaxCumTests, Breaks),  expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())

plot_CumTests

#### DEATHS per day ####
# Read in DEATHS per day from summarised data, format date correctly
df_NewDeaths <- data.frame(Date=c(COVID19_by_day$Date), ntimes=c(COVID19_by_day$NewDeaths))
df_NewDeaths<- as.data.frame(lapply(df_NewDeaths, rep, df_NewDeaths$ntimes))

# Plot NEW DEATHS per day #
Sigfig <- signif(max(COVID19_by_day$NewDeaths))
Factor <- 10^(floor(log10(Sigfig)))
MaxNewDeaths <- round_any(max(COVID19_by_day$NewDeaths), Factor, f=ceiling)
Breaks <- ceiling(Factor/2)

plot_NewDeaths <- ggplot(data=df_NewDeaths, aes(x=df_NewDeaths$Date)) + 
  geom_histogram(binwidth=1, boundary = 1, colour="black") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_day$Date), max(COVID19_by_day$Date)),
               breaks = seq(min(COVID19_by_day$Date), max(COVID19_by_day$Date), 1)) +
  scale_y_continuous(limits = c(0, MaxNewDeaths), 
                     breaks = seq(0, MaxNewDeaths, Breaks), expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())

plot_NewDeaths

# Plot CUMULATIVE DEATHS per day
Sigfig <- signif(max(COVID19_by_day$CumDeaths))
Factor <- 10^(floor(log10(Sigfig)))
MaxCumDeaths <- round_any(max(COVID19_by_day$CumDeaths), Factor, f=ceiling)
Breaks <- ceiling(Factor/2)

plot_CumDeaths <- ggplot(data=df_NewDeaths, aes(x=df_NewDeaths$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, boundary = 1, , colour="black") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_day$Date), max(COVID19_by_day$Date)),
               breaks = seq(min(COVID19_by_day$Date), max(COVID19_by_day$Date), 1)) +
  scale_y_continuous(limits = c(0, MaxCumDeaths), 
                     breaks = seq(0, MaxCumDeaths, Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())

plot_CumDeaths

figure <- ggarrange(plot_NewCases + font("x.text", size = 8), 
                    plot_CumCases + font("x.text", size = 8), 
                    plot_NewTests + font("x.text", size = 8), 
                    plot_CumTests + font("x.text", size = 8), 
                    plot_NewDeaths + font("x.text", size = 8), 
                    plot_CumDeaths + font("x.text", size = 8),
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3, align = "v")

annotate_figure(figure,
                top = text_grob("UK data for COVID-19", face = "bold", size = 14, vjust = 0),
                bottom = text_grob("A: New cases per day; B: Cumulative cases per day; C: New tests per day\n
                                   D: Cumulative tests per day; E: New fatalities per day; F: Cumulative fatalities per day",
                                   hjust = 1, x = 1, face = "bold", size = 8),
                right = text_grob("Data source: https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public#number-of-cases",
                                 rot = 90, , size = 6)
                )

figure
                