## Exploration of UK COVID-19 data
## Emma Doughty (e.doughty6@gmail.com)

# Install libraries if necessary
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(scales)) install.packages("scales")

# Load libraries
library(readr)
library(tidyr)
library(plyr)
library(ggplot2)
library(scales)
library(ggpubr)

#Load data
COVID19_by_day <- read_csv("COVID19_by_day.csv")
COVID19_by_day$Date <- as.Date(COVID19_by_day$Date, format = "%d/%m/%y")
COVID19_by_day <- COVID19_by_day[order(COVID19_by_day$Date),]

#### Generate plots ####

#### CASES per day ####
# Read in new cases per day from summarised data, format date correctly
df_NewCases <- data.frame(Date=c(COVID19_by_day$Date), ntimes=c(COVID19_by_day$NewCases))
df_NewCases<- as.data.frame(lapply(df_NewCases, rep, df_NewCases$ntimes))

# Plot number of NEW CASES per day
NewCases_Sigfig <- signif(max(COVID19_by_day$NewCases))
NewCases_Factor <- 10^(floor(log10(NewCases_Sigfig)))
NewCases_Max <- round_any(max(COVID19_by_day$NewCases), NewCases_Factor, f=ceiling)
NewCases_Breaks <- ceiling(NewCases_Factor/2)

plot_NewCases <- ggplot(data=df_NewCases, aes(x=df_NewCases$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_day$Date)-1, max(COVID19_by_day$Date)),
               breaks = seq(min(COVID19_by_day$Date)-1, max(COVID19_by_day$Date), 1)) +
  scale_y_continuous(limits = c(0, NewCases_Max), 
                     breaks = seq(0, NewCases_Max, NewCases_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date")+
  ggtitle("New UK cases per day") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())

# Plot CUMULATIVE CASES per day
CumCases_Sigfig <- signif(sum(COVID19_by_day$NewCases))
CumCases_Factor <- 10^(floor(log10(CumCases_Sigfig)))
CumCases_Max <- round_any(sum(COVID19_by_day$NewCases), CumCases_Factor, f=ceiling)
CumCases_Breaks <- ceiling(CumCases_Factor/2)

plot_CumCases <- ggplot(data=df_NewCases, aes(x=df_NewCases$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_day$Date)-1, max(COVID19_by_day$Date)),
               breaks = seq(min(COVID19_by_day$Date)-1, max(COVID19_by_day$Date), 1)) +
  scale_y_continuous(limits = c(0, CumCases_Max), 
                     breaks = seq(0, CumCases_Max, CumCases_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative UK cases per day") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


#### TESTS per day ####
# Read in new tests per day from summarised data, format date correctly
df_NewTests <- data.frame(Date=c(COVID19_by_day$Date), ntimes=c(COVID19_by_day$NewTests))
df_NewTests<- as.data.frame(lapply(df_NewTests, rep, df_NewTests$ntimes))

# Plot NEW TESTS per day #
NewTests_Sigfig <- signif(max(COVID19_by_day$NewTests))
NewTests_Factor <- 10^(floor(log10(NewTests_Sigfig)))
NewTests_Max <- round_any(max(COVID19_by_day$NewTests), NewTests_Factor, f=ceiling)
NewTests_Breaks <- ceiling(NewTests_Factor/2)

plot_NewTests <- ggplot(data=df_NewTests, aes(x=df_NewTests$Date)) + 
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#90BEDE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_day$Date)-1, max(COVID19_by_day$Date)),
               breaks = seq(min(COVID19_by_day$Date)-1, max(COVID19_by_day$Date), 1)) +
  scale_y_continuous(limits = c(0, NewTests_Max), 
                     breaks = seq(0, NewTests_Max, NewTests_Breaks), expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New UK tests per day") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())

# Plot CUMULATIVE TESTS per day
CumTests_Sigfig <- signif(sum(COVID19_by_day$NewTests))
CumTests_Factor <- 10^(floor(log10(CumTests_Sigfig)))
CumTests_Max <- round_any(sum(COVID19_by_day$NewTests), CumTests_Factor, f=ceiling)
CumTests_Breaks <- ceiling(CumTests_Factor/2)

plot_CumTests <- ggplot(data=df_NewTests, aes(x=df_NewTests$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#68EDC6") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_day$Date)-1, max(COVID19_by_day$Date)),
               breaks = seq(min(COVID19_by_day$Date)-1, max(COVID19_by_day$Date), 1)) +
  scale_y_continuous(limits = c(0, CumTests_Max), 
                     breaks = seq(0, CumTests_Max, CumTests_Breaks),  expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative UK tests per day") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


#### DEATHS per day ####
# Read in DEATHS per day from summarised data, format date correctly
df_NewDeaths <- data.frame(Date=c(COVID19_by_day$Date), ntimes=c(COVID19_by_day$NewDeaths))
df_NewDeaths<- as.data.frame(lapply(df_NewDeaths, rep, df_NewDeaths$ntimes))

# Plot NEW DEATHS per day #
NewDeaths_Sigfig <- signif(max(COVID19_by_day$NewDeaths))
NewDeaths_Factor <- 10^(floor(log10(NewDeaths_Sigfig)))
NewDeaths_Max <- round_any(max(COVID19_by_day$NewDeaths), NewDeaths_Factor, f=ceiling)
NewDeaths_Breaks <- ceiling(NewDeaths_Factor/2)

plot_NewDeaths <- ggplot(data=df_NewDeaths, aes(x=df_NewDeaths$Date)) + 
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#90F3FF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_day$Date)-1, max(COVID19_by_day$Date)),
               breaks = seq(min(COVID19_by_day$Date)-1, max(COVID19_by_day$Date), 1)) +
  scale_y_continuous(limits = c(0, NewDeaths_Max), 
                     breaks = seq(0, NewDeaths_Max, NewDeaths_Breaks), expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New UK deaths per day") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())

# Plot CUMULATIVE DEATHS per day
CumDeaths_Sigfig <- signif(sum(COVID19_by_day$NewDeaths))
CumDeaths_Factor <- 10^(floor(log10(CumDeaths_Sigfig)))
CumDeaths_Max <- round_any(sum(COVID19_by_day$NewDeaths), CumDeaths_Factor, f=ceiling)
CumDeaths_Breaks <- ceiling(CumDeaths_Factor/2)

plot_CumDeaths <- ggplot(data=df_NewDeaths, aes(x=df_NewDeaths$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, boundary = 1, 
                 colour="black", fill= "#8D80AD") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_day$Date)-1, max(COVID19_by_day$Date)),
               breaks = seq(min(COVID19_by_day$Date)-1, max(COVID19_by_day$Date), 1)) +
  scale_y_continuous(limits = c(0,CumDeaths_Max), 
                     breaks = seq(0, CumDeaths_Max, CumDeaths_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative UK deaths per day") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


#### Generate plots ####
# Cases, tests and deaths summary figure
Sum_figure <- ggarrange(plot_NewCases + font("x.text", size = 6), 
                    plot_CumCases + font("x.text", size = 6), 
                    plot_NewTests + font("x.text", size = 6), 
                    plot_CumTests + font("x.text", size = 6), 
                    plot_NewDeaths + font("x.text", size = 6), 
                    plot_CumDeaths + font("x.text", size = 6),
          ncol = 2, nrow = 3, align = "hv")

Sum_figure <- annotate_figure(Sum_figure,
                top = text_grob("UK data for COVID-19", face = "bold", size = 14))

pdf("Summary_plot.pdf", height = 8.27, width = 11.69)
Sum_figure
dev.off()

# Cases per day figure
Cases_figure <- ggarrange(plot_NewCases + font("x.text", size = 8), 
                          plot_CumCases + font("x.text", size = 8), 
                          ncol = 1, nrow = 2, align = "hv")

pdf("Cases_plot.pdf", height = 8.27, width = 11.69)
Cases_figure
dev.off()

# Tests per day figure
Tests_figure <- ggarrange(plot_NewTests + font("x.text", size = 8), 
                          plot_CumTests + font("x.text", size = 8), 
                          ncol = 1, nrow = 2, align = "hv")

pdf("Tests_plot.pdf", height = 8.27, width = 11.69)
Tests_figure
dev.off()

# Deaths per day figure
Deaths_figure <- ggarrange(plot_NewDeaths + font("x.text", size = 8), 
                           plot_CumDeaths + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Deaths_plot.pdf", height = 8.27, width = 11.69)
Deaths_figure
dev.off()
