## Exploration of UK COVID-19 LA data
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
COVID19_by_LA <- read_csv("COVID19_by_LA.csv")
COVID19_by_LA$Date <- as.Date(COVID19_by_LA$Date, format = "%d/%m/%y")
COVID19_by_LA <- COVID19_by_LA[order(COVID19_by_LA$Date),]


#### Barking_and_Dagenham plots ####
#Read in new cases per day from summarised data, format date correctly
Barking_and_Dagenham_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Barking_and_Dagenham))
Barking_and_Dagenham_data <- as.data.frame(lapply(Barking_and_Dagenham_data, rep, Barking_and_Dagenham_data$ntimes))


# Plot number of NEW CASES per day
Barking_and_DagenhamNew_Sigfig <- signif(max(COVID19_by_LA$Barking_and_Dagenham))
Barking_and_DagenhamNew_Factor <- 10^(floor(log10(Barking_and_DagenhamNew_Sigfig)))
Barking_and_DagenhamNew_Max <- round_any(max(COVID19_by_LA$Barking_and_Dagenham), Barking_and_DagenhamNew_Factor, f=ceiling)
Barking_and_DagenhamNew_Breaks <- ceiling(Barking_and_DagenhamNew_Factor/2)

plot_Barking_and_DagenhamNew <- ggplot(data=Barking_and_Dagenham_data, aes(x=Barking_and_Dagenham_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Barking_and_DagenhamNew_Max), 
                     breaks = seq(0, Barking_and_DagenhamNew_Max, Barking_and_DagenhamNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Barking and Dagenham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Barking_and_DagenhamCum_Sigfig <- signif(sum(COVID19_by_LA$Barking_and_Dagenham))
Barking_and_DagenhamCum_Factor <- 10^(floor(log10(Barking_and_DagenhamCum_Sigfig)))
Barking_and_DagenhamCum_Max <- round_any(sum(COVID19_by_LA$Barking_and_Dagenham), Barking_and_DagenhamCum_Factor, f=ceiling)
Barking_and_DagenhamCum_Breaks <- ceiling(Barking_and_DagenhamCum_Factor/2)

plot_Barking_and_DagenhamCum <- ggplot(data=Barking_and_Dagenham_data, aes(x=Barking_and_Dagenham_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Barking_and_DagenhamCum_Max), 
                     breaks = seq(0, Barking_and_DagenhamCum_Max, Barking_and_DagenhamCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Barking and Dagenham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Barking_and_Dagenham_figure <- ggarrange(plot_Barking_and_DagenhamNew + font("x.text", size = 8), 
                                         plot_Barking_and_DagenhamCum + font("x.text", size = 8), 
                                         ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Barking_and_Dagenham_cases_plot.pdf", height = 8.27, width = 11.69)
Barking_and_Dagenham_figure
dev.off()



#### Barnet plots ####
#Read in new cases per day from summarised data, format date correctly
Barnet_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Barnet))
Barnet_data <- as.data.frame(lapply(Barnet_data, rep, Barnet_data$ntimes))


# Plot number of NEW CASES per day
BarnetNew_Sigfig <- signif(max(COVID19_by_LA$Barnet))
BarnetNew_Factor <- 10^(floor(log10(BarnetNew_Sigfig)))
BarnetNew_Max <- round_any(max(COVID19_by_LA$Barnet), BarnetNew_Factor, f=ceiling)
BarnetNew_Breaks <- ceiling(BarnetNew_Factor/2)

plot_BarnetNew <- ggplot(data=Barnet_data, aes(x=Barnet_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0),
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BarnetNew_Max),
                     breaks = seq(0, BarnetNew_Max, BarnetNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Barnet") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BarnetCum_Sigfig <- signif(sum(COVID19_by_LA$Barnet))
BarnetCum_Factor <- 10^(floor(log10(BarnetCum_Sigfig)))
BarnetCum_Max <- round_any(sum(COVID19_by_LA$Barnet), BarnetCum_Factor, f=ceiling)
BarnetCum_Breaks <- ceiling(BarnetCum_Factor/2)

plot_BarnetCum <- ggplot(data=Barnet_data, aes(x=Barnet_data$Date)) +
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1,
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0),
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BarnetCum_Max),
                     breaks = seq(0, BarnetCum_Max, BarnetCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Barnet") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank())


# Generate figures
Barnet_figure <- ggarrange(plot_BarnetNew + font("x.text", size = 8),
                           plot_BarnetCum + font("x.text", size = 8),
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Barnet_cases_plot.pdf", height = 8.27, width = 11.69)
Barnet_figure
dev.off()


#### Barnsley plots ####
#Read in new cases per day from summarised data, format date correctly
Barnsley_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Barnsley))
Barnsley_data <- as.data.frame(lapply(Barnsley_data, rep, Barnsley_data$ntimes))


# Plot number of NEW CASES per day
BarnsleyNew_Sigfig <- signif(max(COVID19_by_LA$Barnsley))
BarnsleyNew_Factor <- 10^(floor(log10(BarnsleyNew_Sigfig)))
BarnsleyNew_Max <- round_any(max(COVID19_by_LA$Barnsley), BarnsleyNew_Factor, f=ceiling)
BarnsleyNew_Breaks <- ceiling(BarnsleyNew_Factor/2)

plot_BarnsleyNew <- ggplot(data=Barnsley_data, aes(x=Barnsley_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BarnsleyNew_Max), 
                     breaks = seq(0, BarnsleyNew_Max, BarnsleyNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Barnsley") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BarnsleyCum_Sigfig <- signif(sum(COVID19_by_LA$Barnsley))
BarnsleyCum_Factor <- 10^(floor(log10(BarnsleyCum_Sigfig)))
BarnsleyCum_Max <- round_any(sum(COVID19_by_LA$Barnsley), BarnsleyCum_Factor, f=ceiling)
BarnsleyCum_Breaks <- ceiling(BarnsleyCum_Factor/2)

plot_BarnsleyCum <- ggplot(data=Barnsley_data, aes(x=Barnsley_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BarnsleyCum_Max), 
                     breaks = seq(0, BarnsleyCum_Max, BarnsleyCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Barnsley") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Barnsley_figure <- ggarrange(plot_BarnsleyNew + font("x.text", size = 8), 
                             plot_BarnsleyCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Barnsley_cases_plot.pdf", height = 8.27, width = 11.69)
Barnsley_figure
dev.off()



#### Bath_and_North_East_Somerset plots ####
#Read in new cases per day from summarised data, format date correctly
Bath_and_North_East_Somerset_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Bath_and_North_East_Somerset))
Bath_and_North_East_Somerset_data <- as.data.frame(lapply(Bath_and_North_East_Somerset_data, rep, Bath_and_North_East_Somerset_data$ntimes))


# Plot number of NEW CASES per day
Bath_and_North_East_SomersetNew_Sigfig <- signif(max(COVID19_by_LA$Bath_and_North_East_Somerset))
Bath_and_North_East_SomersetNew_Factor <- 10^(floor(log10(Bath_and_North_East_SomersetNew_Sigfig)))
Bath_and_North_East_SomersetNew_Max <- round_any(max(COVID19_by_LA$Bath_and_North_East_Somerset), Bath_and_North_East_SomersetNew_Factor, f=ceiling)
Bath_and_North_East_SomersetNew_Breaks <- ceiling(Bath_and_North_East_SomersetNew_Factor/2)

plot_Bath_and_North_East_SomersetNew <- ggplot(data=Bath_and_North_East_Somerset_data, aes(x=Bath_and_North_East_Somerset_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Bath_and_North_East_SomersetNew_Max), 
                     breaks = seq(0, Bath_and_North_East_SomersetNew_Max, Bath_and_North_East_SomersetNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Bath and North East Somerset") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Bath_and_North_East_SomersetCum_Sigfig <- signif(sum(COVID19_by_LA$Bath_and_North_East_Somerset))
Bath_and_North_East_SomersetCum_Factor <- 10^(floor(log10(Bath_and_North_East_SomersetCum_Sigfig)))
Bath_and_North_East_SomersetCum_Max <- round_any(sum(COVID19_by_LA$Bath_and_North_East_Somerset), Bath_and_North_East_SomersetCum_Factor, f=ceiling)
Bath_and_North_East_SomersetCum_Breaks <- ceiling(Bath_and_North_East_SomersetCum_Factor/2)

plot_Bath_and_North_East_SomersetCum <- ggplot(data=Bath_and_North_East_Somerset_data, aes(x=Bath_and_North_East_Somerset_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Bath_and_North_East_SomersetCum_Max), 
                     breaks = seq(0, Bath_and_North_East_SomersetCum_Max, Bath_and_North_East_SomersetCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Bath and North East Somerset") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Bath_and_North_East_Somerset_figure <- ggarrange(plot_Bath_and_North_East_SomersetNew + font("x.text", size = 8), 
                                                 plot_Bath_and_North_East_SomersetCum + font("x.text", size = 8), 
                                                 ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Bath_and_North_East_Somerset_cases_plot.pdf", height = 8.27, width = 11.69)
Bath_and_North_East_Somerset_figure
dev.off()



#### Bedford plots ####
#Read in new cases per day from summarised data, format date correctly
Bedford_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Bedford))
Bedford_data <- as.data.frame(lapply(Bedford_data, rep, Bedford_data$ntimes))


# Plot number of NEW CASES per day
BedfordNew_Sigfig <- signif(max(COVID19_by_LA$Bedford))
BedfordNew_Factor <- 10^(floor(log10(BedfordNew_Sigfig)))
BedfordNew_Max <- round_any(max(COVID19_by_LA$Bedford), BedfordNew_Factor, f=ceiling)
BedfordNew_Breaks <- ceiling(BedfordNew_Factor/2)

plot_BedfordNew <- ggplot(data=Bedford_data, aes(x=Bedford_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BedfordNew_Max), 
                     breaks = seq(0, BedfordNew_Max, BedfordNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Bedford") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BedfordCum_Sigfig <- signif(sum(COVID19_by_LA$Bedford))
BedfordCum_Factor <- 10^(floor(log10(BedfordCum_Sigfig)))
BedfordCum_Max <- round_any(sum(COVID19_by_LA$Bedford), BedfordCum_Factor, f=ceiling)
BedfordCum_Breaks <- ceiling(BedfordCum_Factor/2)

plot_BedfordCum <- ggplot(data=Bedford_data, aes(x=Bedford_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BedfordCum_Max), 
                     breaks = seq(0, BedfordCum_Max, BedfordCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Bedford") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Bedford_figure <- ggarrange(plot_BedfordNew + font("x.text", size = 8), 
                            plot_BedfordCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Bedford_cases_plot.pdf", height = 8.27, width = 11.69)
Bedford_figure
dev.off()



#### Bexley plots ####
#Read in new cases per day from summarised data, format date correctly
Bexley_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Bexley))
Bexley_data <- as.data.frame(lapply(Bexley_data, rep, Bexley_data$ntimes))


# Plot number of NEW CASES per day
BexleyNew_Sigfig <- signif(max(COVID19_by_LA$Bexley))
BexleyNew_Factor <- 10^(floor(log10(BexleyNew_Sigfig)))
BexleyNew_Max <- round_any(max(COVID19_by_LA$Bexley), BexleyNew_Factor, f=ceiling)
BexleyNew_Breaks <- ceiling(BexleyNew_Factor/2)

plot_BexleyNew <- ggplot(data=Bexley_data, aes(x=Bexley_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BexleyNew_Max), 
                     breaks = seq(0, BexleyNew_Max, BexleyNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Bexley") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BexleyCum_Sigfig <- signif(sum(COVID19_by_LA$Bexley))
BexleyCum_Factor <- 10^(floor(log10(BexleyCum_Sigfig)))
BexleyCum_Max <- round_any(sum(COVID19_by_LA$Bexley), BexleyCum_Factor, f=ceiling)
BexleyCum_Breaks <- ceiling(BexleyCum_Factor/2)

plot_BexleyCum <- ggplot(data=Bexley_data, aes(x=Bexley_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BexleyCum_Max), 
                     breaks = seq(0, BexleyCum_Max, BexleyCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Bexley") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Bexley_figure <- ggarrange(plot_BexleyNew + font("x.text", size = 8), 
                           plot_BexleyCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Bexley_cases_plot.pdf", height = 8.27, width = 11.69)
Bexley_figure
dev.off()



#### Birmingham plots ####
#Read in new cases per day from summarised data, format date correctly
Birmingham_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Birmingham))
Birmingham_data <- as.data.frame(lapply(Birmingham_data, rep, Birmingham_data$ntimes))


# Plot number of NEW CASES per day
BirminghamNew_Sigfig <- signif(max(COVID19_by_LA$Birmingham))
BirminghamNew_Factor <- 10^(floor(log10(BirminghamNew_Sigfig)))
BirminghamNew_Max <- round_any(max(COVID19_by_LA$Birmingham), BirminghamNew_Factor, f=ceiling)
BirminghamNew_Breaks <- ceiling(BirminghamNew_Factor/2)

plot_BirminghamNew <- ggplot(data=Birmingham_data, aes(x=Birmingham_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BirminghamNew_Max), 
                     breaks = seq(0, BirminghamNew_Max, BirminghamNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Birmingham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BirminghamCum_Sigfig <- signif(sum(COVID19_by_LA$Birmingham))
BirminghamCum_Factor <- 10^(floor(log10(BirminghamCum_Sigfig)))
BirminghamCum_Max <- round_any(sum(COVID19_by_LA$Birmingham), BirminghamCum_Factor, f=ceiling)
BirminghamCum_Breaks <- ceiling(BirminghamCum_Factor/2)

plot_BirminghamCum <- ggplot(data=Birmingham_data, aes(x=Birmingham_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BirminghamCum_Max), 
                     breaks = seq(0, BirminghamCum_Max, BirminghamCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Birmingham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Birmingham_figure <- ggarrange(plot_BirminghamNew + font("x.text", size = 8), 
                               plot_BirminghamCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Birmingham_cases_plot.pdf", height = 8.27, width = 11.69)
Birmingham_figure
dev.off()



#### Blackburn_with_Darwen plots ####
#Read in new cases per day from summarised data, format date correctly
Blackburn_with_Darwen_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Blackburn_with_Darwen))
Blackburn_with_Darwen_data <- as.data.frame(lapply(Blackburn_with_Darwen_data, rep, Blackburn_with_Darwen_data$ntimes))


# Plot number of NEW CASES per day
Blackburn_with_DarwenNew_Sigfig <- signif(max(COVID19_by_LA$Blackburn_with_Darwen))
Blackburn_with_DarwenNew_Factor <- 10^(floor(log10(Blackburn_with_DarwenNew_Sigfig)))
Blackburn_with_DarwenNew_Max <- round_any(max(COVID19_by_LA$Blackburn_with_Darwen), Blackburn_with_DarwenNew_Factor, f=ceiling)
Blackburn_with_DarwenNew_Breaks <- ceiling(Blackburn_with_DarwenNew_Factor/2)

plot_Blackburn_with_DarwenNew <- ggplot(data=Blackburn_with_Darwen_data, aes(x=Blackburn_with_Darwen_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Blackburn_with_DarwenNew_Max), 
                     breaks = seq(0, Blackburn_with_DarwenNew_Max, Blackburn_with_DarwenNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Blackburn with Darwen") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Blackburn_with_DarwenCum_Sigfig <- signif(sum(COVID19_by_LA$Blackburn_with_Darwen))
Blackburn_with_DarwenCum_Factor <- 10^(floor(log10(Blackburn_with_DarwenCum_Sigfig)))
Blackburn_with_DarwenCum_Max <- round_any(sum(COVID19_by_LA$Blackburn_with_Darwen), Blackburn_with_DarwenCum_Factor, f=ceiling)
Blackburn_with_DarwenCum_Breaks <- ceiling(Blackburn_with_DarwenCum_Factor/2)

plot_Blackburn_with_DarwenCum <- ggplot(data=Blackburn_with_Darwen_data, aes(x=Blackburn_with_Darwen_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Blackburn_with_DarwenCum_Max), 
                     breaks = seq(0, Blackburn_with_DarwenCum_Max, Blackburn_with_DarwenCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Blackburn with Darwen") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Blackburn_with_Darwen_figure <- ggarrange(plot_Blackburn_with_DarwenNew + font("x.text", size = 8), 
                                          plot_Blackburn_with_DarwenCum + font("x.text", size = 8), 
                                          ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Blackburn_with_Darwen_cases_plot.pdf", height = 8.27, width = 11.69)
Blackburn_with_Darwen_figure
dev.off()



#### Blackpool plots ####
#Read in new cases per day from summarised data, format date correctly
Blackpool_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Blackpool))
Blackpool_data <- as.data.frame(lapply(Blackpool_data, rep, Blackpool_data$ntimes))


# Plot number of NEW CASES per day
BlackpoolNew_Sigfig <- signif(max(COVID19_by_LA$Blackpool))
BlackpoolNew_Factor <- 10^(floor(log10(BlackpoolNew_Sigfig)))
BlackpoolNew_Max <- round_any(max(COVID19_by_LA$Blackpool), BlackpoolNew_Factor, f=ceiling)
BlackpoolNew_Breaks <- ceiling(BlackpoolNew_Factor/2)

plot_BlackpoolNew <- ggplot(data=Blackpool_data, aes(x=Blackpool_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BlackpoolNew_Max), 
                     breaks = seq(0, BlackpoolNew_Max, BlackpoolNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Blackpool") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BlackpoolCum_Sigfig <- signif(sum(COVID19_by_LA$Blackpool))
BlackpoolCum_Factor <- 10^(floor(log10(BlackpoolCum_Sigfig)))
BlackpoolCum_Max <- round_any(sum(COVID19_by_LA$Blackpool), BlackpoolCum_Factor, f=ceiling)
BlackpoolCum_Breaks <- ceiling(BlackpoolCum_Factor/2)

plot_BlackpoolCum <- ggplot(data=Blackpool_data, aes(x=Blackpool_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BlackpoolCum_Max), 
                     breaks = seq(0, BlackpoolCum_Max, BlackpoolCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Blackpool") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Blackpool_figure <- ggarrange(plot_BlackpoolNew + font("x.text", size = 8), 
                              plot_BlackpoolCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Blackpool_cases_plot.pdf", height = 8.27, width = 11.69)
Blackpool_figure
dev.off()



#### Bolton plots ####
#Read in new cases per day from summarised data, format date correctly
Bolton_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Bolton))
Bolton_data <- as.data.frame(lapply(Bolton_data, rep, Bolton_data$ntimes))


# Plot number of NEW CASES per day
BoltonNew_Sigfig <- signif(max(COVID19_by_LA$Bolton))
BoltonNew_Factor <- 10^(floor(log10(BoltonNew_Sigfig)))
BoltonNew_Max <- round_any(max(COVID19_by_LA$Bolton), BoltonNew_Factor, f=ceiling)
BoltonNew_Breaks <- ceiling(BoltonNew_Factor/2)

plot_BoltonNew <- ggplot(data=Bolton_data, aes(x=Bolton_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BoltonNew_Max), 
                     breaks = seq(0, BoltonNew_Max, BoltonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Bolton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BoltonCum_Sigfig <- signif(sum(COVID19_by_LA$Bolton))
BoltonCum_Factor <- 10^(floor(log10(BoltonCum_Sigfig)))
BoltonCum_Max <- round_any(sum(COVID19_by_LA$Bolton), BoltonCum_Factor, f=ceiling)
BoltonCum_Breaks <- ceiling(BoltonCum_Factor/2)

plot_BoltonCum <- ggplot(data=Bolton_data, aes(x=Bolton_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BoltonCum_Max), 
                     breaks = seq(0, BoltonCum_Max, BoltonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Bolton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Bolton_figure <- ggarrange(plot_BoltonNew + font("x.text", size = 8), 
                           plot_BoltonCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Bolton_cases_plot.pdf", height = 8.27, width = 11.69)
Bolton_figure
dev.off()



#### Bournemouth plots ####
#Read in new cases per day from summarised data, format date correctly
Bournemouth_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Bournemouth))
Bournemouth_data <- as.data.frame(lapply(Bournemouth_data, rep, Bournemouth_data$ntimes))


# Plot number of NEW CASES per day
BournemouthNew_Sigfig <- signif(max(COVID19_by_LA$Bournemouth))
BournemouthNew_Factor <- 10^(floor(log10(BournemouthNew_Sigfig)))
BournemouthNew_Max <- round_any(max(COVID19_by_LA$Bournemouth), BournemouthNew_Factor, f=ceiling)
BournemouthNew_Breaks <- ceiling(BournemouthNew_Factor/2)

plot_BournemouthNew <- ggplot(data=Bournemouth_data, aes(x=Bournemouth_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BournemouthNew_Max), 
                     breaks = seq(0, BournemouthNew_Max, BournemouthNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Bournemouth") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BournemouthCum_Sigfig <- signif(sum(COVID19_by_LA$Bournemouth))
BournemouthCum_Factor <- 10^(floor(log10(BournemouthCum_Sigfig)))
BournemouthCum_Max <- round_any(sum(COVID19_by_LA$Bournemouth), BournemouthCum_Factor, f=ceiling)
BournemouthCum_Breaks <- ceiling(BournemouthCum_Factor/2)

plot_BournemouthCum <- ggplot(data=Bournemouth_data, aes(x=Bournemouth_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BournemouthCum_Max), 
                     breaks = seq(0, BournemouthCum_Max, BournemouthCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Bournemouth") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Bournemouth_figure <- ggarrange(plot_BournemouthNew + font("x.text", size = 8), 
                                plot_BournemouthCum + font("x.text", size = 8), 
                                ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Bournemouth_cases_plot.pdf", height = 8.27, width = 11.69)
Bournemouth_figure
dev.off()



#### Bracknell_Forest plots ####
#Read in new cases per day from summarised data, format date correctly
Bracknell_Forest_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Bracknell_Forest))
Bracknell_Forest_data <- as.data.frame(lapply(Bracknell_Forest_data, rep, Bracknell_Forest_data$ntimes))


# Plot number of NEW CASES per day
Bracknell_ForestNew_Sigfig <- signif(max(COVID19_by_LA$Bracknell_Forest))
Bracknell_ForestNew_Factor <- 10^(floor(log10(Bracknell_ForestNew_Sigfig)))
Bracknell_ForestNew_Max <- round_any(max(COVID19_by_LA$Bracknell_Forest), Bracknell_ForestNew_Factor, f=ceiling)
Bracknell_ForestNew_Breaks <- ceiling(Bracknell_ForestNew_Factor/2)

plot_Bracknell_ForestNew <- ggplot(data=Bracknell_Forest_data, aes(x=Bracknell_Forest_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Bracknell_ForestNew_Max), 
                     breaks = seq(0, Bracknell_ForestNew_Max, Bracknell_ForestNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Bracknell Forest") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Bracknell_ForestCum_Sigfig <- signif(sum(COVID19_by_LA$Bracknell_Forest))
Bracknell_ForestCum_Factor <- 10^(floor(log10(Bracknell_ForestCum_Sigfig)))
Bracknell_ForestCum_Max <- round_any(sum(COVID19_by_LA$Bracknell_Forest), Bracknell_ForestCum_Factor, f=ceiling)
Bracknell_ForestCum_Breaks <- ceiling(Bracknell_ForestCum_Factor/2)

plot_Bracknell_ForestCum <- ggplot(data=Bracknell_Forest_data, aes(x=Bracknell_Forest_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Bracknell_ForestCum_Max), 
                     breaks = seq(0, Bracknell_ForestCum_Max, Bracknell_ForestCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Bracknell Forest") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Bracknell_Forest_figure <- ggarrange(plot_Bracknell_ForestNew + font("x.text", size = 8), 
                                     plot_Bracknell_ForestCum + font("x.text", size = 8), 
                                     ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Bracknell_Forest_cases_plot.pdf", height = 8.27, width = 11.69)
Bracknell_Forest_figure
dev.off()



#### Bradford plots ####
#Read in new cases per day from summarised data, format date correctly
Bradford_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Bradford))
Bradford_data <- as.data.frame(lapply(Bradford_data, rep, Bradford_data$ntimes))


# Plot number of NEW CASES per day
BradfordNew_Sigfig <- signif(max(COVID19_by_LA$Bradford))
BradfordNew_Factor <- 10^(floor(log10(BradfordNew_Sigfig)))
BradfordNew_Max <- round_any(max(COVID19_by_LA$Bradford), BradfordNew_Factor, f=ceiling)
BradfordNew_Breaks <- ceiling(BradfordNew_Factor/2)

plot_BradfordNew <- ggplot(data=Bradford_data, aes(x=Bradford_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BradfordNew_Max), 
                     breaks = seq(0, BradfordNew_Max, BradfordNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Bradford") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BradfordCum_Sigfig <- signif(sum(COVID19_by_LA$Bradford))
BradfordCum_Factor <- 10^(floor(log10(BradfordCum_Sigfig)))
BradfordCum_Max <- round_any(sum(COVID19_by_LA$Bradford), BradfordCum_Factor, f=ceiling)
BradfordCum_Breaks <- ceiling(BradfordCum_Factor/2)

plot_BradfordCum <- ggplot(data=Bradford_data, aes(x=Bradford_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BradfordCum_Max), 
                     breaks = seq(0, BradfordCum_Max, BradfordCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Bradford") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Bradford_figure <- ggarrange(plot_BradfordNew + font("x.text", size = 8), 
                             plot_BradfordCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Bradford_cases_plot.pdf", height = 8.27, width = 11.69)
Bradford_figure
dev.off()



#### Brent plots ####
#Read in new cases per day from summarised data, format date correctly
Brent_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Brent))
Brent_data <- as.data.frame(lapply(Brent_data, rep, Brent_data$ntimes))


# Plot number of NEW CASES per day
BrentNew_Sigfig <- signif(max(COVID19_by_LA$Brent))
BrentNew_Factor <- 10^(floor(log10(BrentNew_Sigfig)))
BrentNew_Max <- round_any(max(COVID19_by_LA$Brent), BrentNew_Factor, f=ceiling)
BrentNew_Breaks <- ceiling(BrentNew_Factor/2)

plot_BrentNew <- ggplot(data=Brent_data, aes(x=Brent_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BrentNew_Max), 
                     breaks = seq(0, BrentNew_Max, BrentNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Brent") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BrentCum_Sigfig <- signif(sum(COVID19_by_LA$Brent))
BrentCum_Factor <- 10^(floor(log10(BrentCum_Sigfig)))
BrentCum_Max <- round_any(sum(COVID19_by_LA$Brent), BrentCum_Factor, f=ceiling)
BrentCum_Breaks <- ceiling(BrentCum_Factor/2)

plot_BrentCum <- ggplot(data=Brent_data, aes(x=Brent_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BrentCum_Max), 
                     breaks = seq(0, BrentCum_Max, BrentCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Brent") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Brent_figure <- ggarrange(plot_BrentNew + font("x.text", size = 8), 
                          plot_BrentCum + font("x.text", size = 8), 
                          ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Brent_cases_plot.pdf", height = 8.27, width = 11.69)
Brent_figure
dev.off()



#### Brighton_andHove plots ####
#Read in new cases per day from summarised data, format date correctly
Brighton_andHove_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Brighton_andHove))
Brighton_andHove_data <- as.data.frame(lapply(Brighton_andHove_data, rep, Brighton_andHove_data$ntimes))


# Plot number of NEW CASES per day
Brighton_andHoveNew_Sigfig <- signif(max(COVID19_by_LA$Brighton_andHove))
Brighton_andHoveNew_Factor <- 10^(floor(log10(Brighton_andHoveNew_Sigfig)))
Brighton_andHoveNew_Max <- round_any(max(COVID19_by_LA$Brighton_andHove), Brighton_andHoveNew_Factor, f=ceiling)
Brighton_andHoveNew_Breaks <- ceiling(Brighton_andHoveNew_Factor/2)

plot_Brighton_andHoveNew <- ggplot(data=Brighton_andHove_data, aes(x=Brighton_andHove_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Brighton_andHoveNew_Max), 
                     breaks = seq(0, Brighton_andHoveNew_Max, Brighton_andHoveNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Brighton andHove") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Brighton_andHoveCum_Sigfig <- signif(sum(COVID19_by_LA$Brighton_andHove))
Brighton_andHoveCum_Factor <- 10^(floor(log10(Brighton_andHoveCum_Sigfig)))
Brighton_andHoveCum_Max <- round_any(sum(COVID19_by_LA$Brighton_andHove), Brighton_andHoveCum_Factor, f=ceiling)
Brighton_andHoveCum_Breaks <- ceiling(Brighton_andHoveCum_Factor/2)

plot_Brighton_andHoveCum <- ggplot(data=Brighton_andHove_data, aes(x=Brighton_andHove_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Brighton_andHoveCum_Max), 
                     breaks = seq(0, Brighton_andHoveCum_Max, Brighton_andHoveCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Brighton andHove") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Brighton_andHove_figure <- ggarrange(plot_Brighton_andHoveNew + font("x.text", size = 8), 
                                     plot_Brighton_andHoveCum + font("x.text", size = 8), 
                                     ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Brighton_andHove_cases_plot.pdf", height = 8.27, width = 11.69)
Brighton_andHove_figure
dev.off()



#### Bristol plots ####
#Read in new cases per day from summarised data, format date correctly
Bristol_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Bristol))
Bristol_data <- as.data.frame(lapply(Bristol_data, rep, Bristol_data$ntimes))


# Plot number of NEW CASES per day
BristolNew_Sigfig <- signif(max(COVID19_by_LA$Bristol))
BristolNew_Factor <- 10^(floor(log10(BristolNew_Sigfig)))
BristolNew_Max <- round_any(max(COVID19_by_LA$Bristol), BristolNew_Factor, f=ceiling)
BristolNew_Breaks <- ceiling(BristolNew_Factor/2)

plot_BristolNew <- ggplot(data=Bristol_data, aes(x=Bristol_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BristolNew_Max), 
                     breaks = seq(0, BristolNew_Max, BristolNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Bristol") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BristolCum_Sigfig <- signif(sum(COVID19_by_LA$Bristol))
BristolCum_Factor <- 10^(floor(log10(BristolCum_Sigfig)))
BristolCum_Max <- round_any(sum(COVID19_by_LA$Bristol), BristolCum_Factor, f=ceiling)
BristolCum_Breaks <- ceiling(BristolCum_Factor/2)

plot_BristolCum <- ggplot(data=Bristol_data, aes(x=Bristol_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BristolCum_Max), 
                     breaks = seq(0, BristolCum_Max, BristolCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Bristol") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Bristol_figure <- ggarrange(plot_BristolNew + font("x.text", size = 8), 
                            plot_BristolCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Bristol_cases_plot.pdf", height = 8.27, width = 11.69)
Bristol_figure
dev.off()



#### Bromley plots ####
#Read in new cases per day from summarised data, format date correctly
Bromley_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Bromley))
Bromley_data <- as.data.frame(lapply(Bromley_data, rep, Bromley_data$ntimes))


# Plot number of NEW CASES per day
BromleyNew_Sigfig <- signif(max(COVID19_by_LA$Bromley))
BromleyNew_Factor <- 10^(floor(log10(BromleyNew_Sigfig)))
BromleyNew_Max <- round_any(max(COVID19_by_LA$Bromley), BromleyNew_Factor, f=ceiling)
BromleyNew_Breaks <- ceiling(BromleyNew_Factor/2)

plot_BromleyNew <- ggplot(data=Bromley_data, aes(x=Bromley_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BromleyNew_Max), 
                     breaks = seq(0, BromleyNew_Max, BromleyNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Bromley") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BromleyCum_Sigfig <- signif(sum(COVID19_by_LA$Bromley))
BromleyCum_Factor <- 10^(floor(log10(BromleyCum_Sigfig)))
BromleyCum_Max <- round_any(sum(COVID19_by_LA$Bromley), BromleyCum_Factor, f=ceiling)
BromleyCum_Breaks <- ceiling(BromleyCum_Factor/2)

plot_BromleyCum <- ggplot(data=Bromley_data, aes(x=Bromley_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BromleyCum_Max), 
                     breaks = seq(0, BromleyCum_Max, BromleyCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Bromley") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Bromley_figure <- ggarrange(plot_BromleyNew + font("x.text", size = 8), 
                            plot_BromleyCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Bromley_cases_plot.pdf", height = 8.27, width = 11.69)
Bromley_figure
dev.off()



#### Buckinghamshire plots ####
#Read in new cases per day from summarised data, format date correctly
Buckinghamshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Buckinghamshire))
Buckinghamshire_data <- as.data.frame(lapply(Buckinghamshire_data, rep, Buckinghamshire_data$ntimes))


# Plot number of NEW CASES per day
BuckinghamshireNew_Sigfig <- signif(max(COVID19_by_LA$Buckinghamshire))
BuckinghamshireNew_Factor <- 10^(floor(log10(BuckinghamshireNew_Sigfig)))
BuckinghamshireNew_Max <- round_any(max(COVID19_by_LA$Buckinghamshire), BuckinghamshireNew_Factor, f=ceiling)
BuckinghamshireNew_Breaks <- ceiling(BuckinghamshireNew_Factor/2)

plot_BuckinghamshireNew <- ggplot(data=Buckinghamshire_data, aes(x=Buckinghamshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BuckinghamshireNew_Max), 
                     breaks = seq(0, BuckinghamshireNew_Max, BuckinghamshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Buckinghamshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BuckinghamshireCum_Sigfig <- signif(sum(COVID19_by_LA$Buckinghamshire))
BuckinghamshireCum_Factor <- 10^(floor(log10(BuckinghamshireCum_Sigfig)))
BuckinghamshireCum_Max <- round_any(sum(COVID19_by_LA$Buckinghamshire), BuckinghamshireCum_Factor, f=ceiling)
BuckinghamshireCum_Breaks <- ceiling(BuckinghamshireCum_Factor/2)

plot_BuckinghamshireCum <- ggplot(data=Buckinghamshire_data, aes(x=Buckinghamshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BuckinghamshireCum_Max), 
                     breaks = seq(0, BuckinghamshireCum_Max, BuckinghamshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Buckinghamshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Buckinghamshire_figure <- ggarrange(plot_BuckinghamshireNew + font("x.text", size = 8), 
                                    plot_BuckinghamshireCum + font("x.text", size = 8), 
                                    ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Buckinghamshire_cases_plot.pdf", height = 8.27, width = 11.69)
Buckinghamshire_figure
dev.off()



#### Bury plots ####
#Read in new cases per day from summarised data, format date correctly
Bury_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Bury))
Bury_data <- as.data.frame(lapply(Bury_data, rep, Bury_data$ntimes))


# Plot number of NEW CASES per day
BuryNew_Sigfig <- signif(max(COVID19_by_LA$Bury))
BuryNew_Factor <- 10^(floor(log10(BuryNew_Sigfig)))
BuryNew_Max <- round_any(max(COVID19_by_LA$Bury), BuryNew_Factor, f=ceiling)
BuryNew_Breaks <- ceiling(BuryNew_Factor/2)

plot_BuryNew <- ggplot(data=Bury_data, aes(x=Bury_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BuryNew_Max), 
                     breaks = seq(0, BuryNew_Max, BuryNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Bury") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
BuryCum_Sigfig <- signif(sum(COVID19_by_LA$Bury))
BuryCum_Factor <- 10^(floor(log10(BuryCum_Sigfig)))
BuryCum_Max <- round_any(sum(COVID19_by_LA$Bury), BuryCum_Factor, f=ceiling)
BuryCum_Breaks <- ceiling(BuryCum_Factor/2)

plot_BuryCum <- ggplot(data=Bury_data, aes(x=Bury_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, BuryCum_Max), 
                     breaks = seq(0, BuryCum_Max, BuryCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Bury") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Bury_figure <- ggarrange(plot_BuryNew + font("x.text", size = 8), 
                         plot_BuryCum + font("x.text", size = 8), 
                         ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Bury_cases_plot.pdf", height = 8.27, width = 11.69)
Bury_figure
dev.off()



#### Calderdale plots ####
#Read in new cases per day from summarised data, format date correctly
Calderdale_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Calderdale))
Calderdale_data <- as.data.frame(lapply(Calderdale_data, rep, Calderdale_data$ntimes))


# Plot number of NEW CASES per day
CalderdaleNew_Sigfig <- signif(max(COVID19_by_LA$Calderdale))
CalderdaleNew_Factor <- 10^(floor(log10(CalderdaleNew_Sigfig)))
CalderdaleNew_Max <- round_any(max(COVID19_by_LA$Calderdale), CalderdaleNew_Factor, f=ceiling)
CalderdaleNew_Breaks <- ceiling(CalderdaleNew_Factor/2)

plot_CalderdaleNew <- ggplot(data=Calderdale_data, aes(x=Calderdale_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, CalderdaleNew_Max), 
                     breaks = seq(0, CalderdaleNew_Max, CalderdaleNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Calderdale") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
CalderdaleCum_Sigfig <- signif(sum(COVID19_by_LA$Calderdale))
CalderdaleCum_Factor <- 10^(floor(log10(CalderdaleCum_Sigfig)))
CalderdaleCum_Max <- round_any(sum(COVID19_by_LA$Calderdale), CalderdaleCum_Factor, f=ceiling)
CalderdaleCum_Breaks <- ceiling(CalderdaleCum_Factor/2)

plot_CalderdaleCum <- ggplot(data=Calderdale_data, aes(x=Calderdale_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, CalderdaleCum_Max), 
                     breaks = seq(0, CalderdaleCum_Max, CalderdaleCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Calderdale") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Calderdale_figure <- ggarrange(plot_CalderdaleNew + font("x.text", size = 8), 
                               plot_CalderdaleCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Calderdale_cases_plot.pdf", height = 8.27, width = 11.69)
Calderdale_figure
dev.off()



#### Cambridgeshire plots ####
#Read in new cases per day from summarised data, format date correctly
Cambridgeshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Cambridgeshire))
Cambridgeshire_data <- as.data.frame(lapply(Cambridgeshire_data, rep, Cambridgeshire_data$ntimes))


# Plot number of NEW CASES per day
CambridgeshireNew_Sigfig <- signif(max(COVID19_by_LA$Cambridgeshire))
CambridgeshireNew_Factor <- 10^(floor(log10(CambridgeshireNew_Sigfig)))
CambridgeshireNew_Max <- round_any(max(COVID19_by_LA$Cambridgeshire), CambridgeshireNew_Factor, f=ceiling)
CambridgeshireNew_Breaks <- ceiling(CambridgeshireNew_Factor/2)

plot_CambridgeshireNew <- ggplot(data=Cambridgeshire_data, aes(x=Cambridgeshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, CambridgeshireNew_Max), 
                     breaks = seq(0, CambridgeshireNew_Max, CambridgeshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Cambridgeshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
CambridgeshireCum_Sigfig <- signif(sum(COVID19_by_LA$Cambridgeshire))
CambridgeshireCum_Factor <- 10^(floor(log10(CambridgeshireCum_Sigfig)))
CambridgeshireCum_Max <- round_any(sum(COVID19_by_LA$Cambridgeshire), CambridgeshireCum_Factor, f=ceiling)
CambridgeshireCum_Breaks <- ceiling(CambridgeshireCum_Factor/2)

plot_CambridgeshireCum <- ggplot(data=Cambridgeshire_data, aes(x=Cambridgeshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, CambridgeshireCum_Max), 
                     breaks = seq(0, CambridgeshireCum_Max, CambridgeshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Cambridgeshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Cambridgeshire_figure <- ggarrange(plot_CambridgeshireNew + font("x.text", size = 8), 
                                   plot_CambridgeshireCum + font("x.text", size = 8), 
                                   ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Cambridgeshire_cases_plot.pdf", height = 8.27, width = 11.69)
Cambridgeshire_figure
dev.off()



#### Camden plots ####
#Read in new cases per day from summarised data, format date correctly
Camden_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Camden))
Camden_data <- as.data.frame(lapply(Camden_data, rep, Camden_data$ntimes))


# Plot number of NEW CASES per day
CamdenNew_Sigfig <- signif(max(COVID19_by_LA$Camden))
CamdenNew_Factor <- 10^(floor(log10(CamdenNew_Sigfig)))
CamdenNew_Max <- round_any(max(COVID19_by_LA$Camden), CamdenNew_Factor, f=ceiling)
CamdenNew_Breaks <- ceiling(CamdenNew_Factor/2)

plot_CamdenNew <- ggplot(data=Camden_data, aes(x=Camden_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, CamdenNew_Max), 
                     breaks = seq(0, CamdenNew_Max, CamdenNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Camden") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
CamdenCum_Sigfig <- signif(sum(COVID19_by_LA$Camden))
CamdenCum_Factor <- 10^(floor(log10(CamdenCum_Sigfig)))
CamdenCum_Max <- round_any(sum(COVID19_by_LA$Camden), CamdenCum_Factor, f=ceiling)
CamdenCum_Breaks <- ceiling(CamdenCum_Factor/2)

plot_CamdenCum <- ggplot(data=Camden_data, aes(x=Camden_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, CamdenCum_Max), 
                     breaks = seq(0, CamdenCum_Max, CamdenCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Camden") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Camden_figure <- ggarrange(plot_CamdenNew + font("x.text", size = 8), 
                           plot_CamdenCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Camden_cases_plot.pdf", height = 8.27, width = 11.69)
Camden_figure
dev.off()



#### Central_Bedfordshire plots ####
#Read in new cases per day from summarised data, format date correctly
Central_Bedfordshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Central_Bedfordshire))
Central_Bedfordshire_data <- as.data.frame(lapply(Central_Bedfordshire_data, rep, Central_Bedfordshire_data$ntimes))


# Plot number of NEW CASES per day
Central_BedfordshireNew_Sigfig <- signif(max(COVID19_by_LA$Central_Bedfordshire))
Central_BedfordshireNew_Factor <- 10^(floor(log10(Central_BedfordshireNew_Sigfig)))
Central_BedfordshireNew_Max <- round_any(max(COVID19_by_LA$Central_Bedfordshire), Central_BedfordshireNew_Factor, f=ceiling)
Central_BedfordshireNew_Breaks <- ceiling(Central_BedfordshireNew_Factor/2)

plot_Central_BedfordshireNew <- ggplot(data=Central_Bedfordshire_data, aes(x=Central_Bedfordshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Central_BedfordshireNew_Max), 
                     breaks = seq(0, Central_BedfordshireNew_Max, Central_BedfordshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Central Bedfordshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Central_BedfordshireCum_Sigfig <- signif(sum(COVID19_by_LA$Central_Bedfordshire))
Central_BedfordshireCum_Factor <- 10^(floor(log10(Central_BedfordshireCum_Sigfig)))
Central_BedfordshireCum_Max <- round_any(sum(COVID19_by_LA$Central_Bedfordshire), Central_BedfordshireCum_Factor, f=ceiling)
Central_BedfordshireCum_Breaks <- ceiling(Central_BedfordshireCum_Factor/2)

plot_Central_BedfordshireCum <- ggplot(data=Central_Bedfordshire_data, aes(x=Central_Bedfordshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Central_BedfordshireCum_Max), 
                     breaks = seq(0, Central_BedfordshireCum_Max, Central_BedfordshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Central Bedfordshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Central_Bedfordshire_figure <- ggarrange(plot_Central_BedfordshireNew + font("x.text", size = 8), 
                                         plot_Central_BedfordshireCum + font("x.text", size = 8), 
                                         ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Central_Bedfordshire_cases_plot.pdf", height = 8.27, width = 11.69)
Central_Bedfordshire_figure
dev.off()



#### Cheshire_East plots ####
#Read in new cases per day from summarised data, format date correctly
Cheshire_East_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Cheshire_East))
Cheshire_East_data <- as.data.frame(lapply(Cheshire_East_data, rep, Cheshire_East_data$ntimes))


# Plot number of NEW CASES per day
Cheshire_EastNew_Sigfig <- signif(max(COVID19_by_LA$Cheshire_East))
Cheshire_EastNew_Factor <- 10^(floor(log10(Cheshire_EastNew_Sigfig)))
Cheshire_EastNew_Max <- round_any(max(COVID19_by_LA$Cheshire_East), Cheshire_EastNew_Factor, f=ceiling)
Cheshire_EastNew_Breaks <- ceiling(Cheshire_EastNew_Factor/2)

plot_Cheshire_EastNew <- ggplot(data=Cheshire_East_data, aes(x=Cheshire_East_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Cheshire_EastNew_Max), 
                     breaks = seq(0, Cheshire_EastNew_Max, Cheshire_EastNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Cheshire East") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Cheshire_EastCum_Sigfig <- signif(sum(COVID19_by_LA$Cheshire_East))
Cheshire_EastCum_Factor <- 10^(floor(log10(Cheshire_EastCum_Sigfig)))
Cheshire_EastCum_Max <- round_any(sum(COVID19_by_LA$Cheshire_East), Cheshire_EastCum_Factor, f=ceiling)
Cheshire_EastCum_Breaks <- ceiling(Cheshire_EastCum_Factor/2)

plot_Cheshire_EastCum <- ggplot(data=Cheshire_East_data, aes(x=Cheshire_East_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Cheshire_EastCum_Max), 
                     breaks = seq(0, Cheshire_EastCum_Max, Cheshire_EastCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Cheshire East") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Cheshire_East_figure <- ggarrange(plot_Cheshire_EastNew + font("x.text", size = 8), 
                                  plot_Cheshire_EastCum + font("x.text", size = 8), 
                                  ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Cheshire_East_cases_plot.pdf", height = 8.27, width = 11.69)
Cheshire_East_figure
dev.off()



#### Cheshire_West_and_Chester plots ####
#Read in new cases per day from summarised data, format date correctly
Cheshire_West_and_Chester_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Cheshire_West_and_Chester))
Cheshire_West_and_Chester_data <- as.data.frame(lapply(Cheshire_West_and_Chester_data, rep, Cheshire_West_and_Chester_data$ntimes))


# Plot number of NEW CASES per day
Cheshire_West_and_ChesterNew_Sigfig <- signif(max(COVID19_by_LA$Cheshire_West_and_Chester))
Cheshire_West_and_ChesterNew_Factor <- 10^(floor(log10(Cheshire_West_and_ChesterNew_Sigfig)))
Cheshire_West_and_ChesterNew_Max <- round_any(max(COVID19_by_LA$Cheshire_West_and_Chester), Cheshire_West_and_ChesterNew_Factor, f=ceiling)
Cheshire_West_and_ChesterNew_Breaks <- ceiling(Cheshire_West_and_ChesterNew_Factor/2)

plot_Cheshire_West_and_ChesterNew <- ggplot(data=Cheshire_West_and_Chester_data, aes(x=Cheshire_West_and_Chester_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Cheshire_West_and_ChesterNew_Max), 
                     breaks = seq(0, Cheshire_West_and_ChesterNew_Max, Cheshire_West_and_ChesterNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Cheshire West and Chester") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Cheshire_West_and_ChesterCum_Sigfig <- signif(sum(COVID19_by_LA$Cheshire_West_and_Chester))
Cheshire_West_and_ChesterCum_Factor <- 10^(floor(log10(Cheshire_West_and_ChesterCum_Sigfig)))
Cheshire_West_and_ChesterCum_Max <- round_any(sum(COVID19_by_LA$Cheshire_West_and_Chester), Cheshire_West_and_ChesterCum_Factor, f=ceiling)
Cheshire_West_and_ChesterCum_Breaks <- ceiling(Cheshire_West_and_ChesterCum_Factor/2)

plot_Cheshire_West_and_ChesterCum <- ggplot(data=Cheshire_West_and_Chester_data, aes(x=Cheshire_West_and_Chester_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Cheshire_West_and_ChesterCum_Max), 
                     breaks = seq(0, Cheshire_West_and_ChesterCum_Max, Cheshire_West_and_ChesterCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Cheshire West and Chester") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Cheshire_West_and_Chester_figure <- ggarrange(plot_Cheshire_West_and_ChesterNew + font("x.text", size = 8), 
                                              plot_Cheshire_West_and_ChesterCum + font("x.text", size = 8), 
                                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Cheshire_West_and_Chester_cases_plot.pdf", height = 8.27, width = 11.69)
Cheshire_West_and_Chester_figure
dev.off()



#### Cornwall_and_Isles_of_Scilly plots ####
#Read in new cases per day from summarised data, format date correctly
Cornwall_and_Isles_of_Scilly_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Cornwall_and_Isles_of_Scilly))
Cornwall_and_Isles_of_Scilly_data <- as.data.frame(lapply(Cornwall_and_Isles_of_Scilly_data, rep, Cornwall_and_Isles_of_Scilly_data$ntimes))


# Plot number of NEW CASES per day
Cornwall_and_Isles_of_ScillyNew_Sigfig <- signif(max(COVID19_by_LA$Cornwall_and_Isles_of_Scilly))
Cornwall_and_Isles_of_ScillyNew_Factor <- 10^(floor(log10(Cornwall_and_Isles_of_ScillyNew_Sigfig)))
Cornwall_and_Isles_of_ScillyNew_Max <- round_any(max(COVID19_by_LA$Cornwall_and_Isles_of_Scilly), Cornwall_and_Isles_of_ScillyNew_Factor, f=ceiling)
Cornwall_and_Isles_of_ScillyNew_Breaks <- ceiling(Cornwall_and_Isles_of_ScillyNew_Factor/2)

plot_Cornwall_and_Isles_of_ScillyNew <- ggplot(data=Cornwall_and_Isles_of_Scilly_data, aes(x=Cornwall_and_Isles_of_Scilly_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Cornwall_and_Isles_of_ScillyNew_Max), 
                     breaks = seq(0, Cornwall_and_Isles_of_ScillyNew_Max, Cornwall_and_Isles_of_ScillyNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Cornwall and Isles of Scilly") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Cornwall_and_Isles_of_ScillyCum_Sigfig <- signif(sum(COVID19_by_LA$Cornwall_and_Isles_of_Scilly))
Cornwall_and_Isles_of_ScillyCum_Factor <- 10^(floor(log10(Cornwall_and_Isles_of_ScillyCum_Sigfig)))
Cornwall_and_Isles_of_ScillyCum_Max <- round_any(sum(COVID19_by_LA$Cornwall_and_Isles_of_Scilly), Cornwall_and_Isles_of_ScillyCum_Factor, f=ceiling)
Cornwall_and_Isles_of_ScillyCum_Breaks <- ceiling(Cornwall_and_Isles_of_ScillyCum_Factor/2)

plot_Cornwall_and_Isles_of_ScillyCum <- ggplot(data=Cornwall_and_Isles_of_Scilly_data, aes(x=Cornwall_and_Isles_of_Scilly_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Cornwall_and_Isles_of_ScillyCum_Max), 
                     breaks = seq(0, Cornwall_and_Isles_of_ScillyCum_Max, Cornwall_and_Isles_of_ScillyCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Cornwall and Isles of Scilly") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Cornwall_and_Isles_of_Scilly_figure <- ggarrange(plot_Cornwall_and_Isles_of_ScillyNew + font("x.text", size = 8), 
                                                 plot_Cornwall_and_Isles_of_ScillyCum + font("x.text", size = 8), 
                                                 ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Cornwall_and_Isles_of_Scilly_cases_plot.pdf", height = 8.27, width = 11.69)
Cornwall_and_Isles_of_Scilly_figure
dev.off()



#### County_Durham plots ####
#Read in new cases per day from summarised data, format date correctly
County_Durham_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$County_Durham))
County_Durham_data <- as.data.frame(lapply(County_Durham_data, rep, County_Durham_data$ntimes))


# Plot number of NEW CASES per day
County_DurhamNew_Sigfig <- signif(max(COVID19_by_LA$County_Durham))
County_DurhamNew_Factor <- 10^(floor(log10(County_DurhamNew_Sigfig)))
County_DurhamNew_Max <- round_any(max(COVID19_by_LA$County_Durham), County_DurhamNew_Factor, f=ceiling)
County_DurhamNew_Breaks <- ceiling(County_DurhamNew_Factor/2)

plot_County_DurhamNew <- ggplot(data=County_Durham_data, aes(x=County_Durham_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, County_DurhamNew_Max), 
                     breaks = seq(0, County_DurhamNew_Max, County_DurhamNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: County Durham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
County_DurhamCum_Sigfig <- signif(sum(COVID19_by_LA$County_Durham))
County_DurhamCum_Factor <- 10^(floor(log10(County_DurhamCum_Sigfig)))
County_DurhamCum_Max <- round_any(sum(COVID19_by_LA$County_Durham), County_DurhamCum_Factor, f=ceiling)
County_DurhamCum_Breaks <- ceiling(County_DurhamCum_Factor/2)

plot_County_DurhamCum <- ggplot(data=County_Durham_data, aes(x=County_Durham_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, County_DurhamCum_Max), 
                     breaks = seq(0, County_DurhamCum_Max, County_DurhamCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: County Durham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
County_Durham_figure <- ggarrange(plot_County_DurhamNew + font("x.text", size = 8), 
                                  plot_County_DurhamCum + font("x.text", size = 8), 
                                  ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/County_Durham_cases_plot.pdf", height = 8.27, width = 11.69)
County_Durham_figure
dev.off()



#### Coventry plots ####
#Read in new cases per day from summarised data, format date correctly
Coventry_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Coventry))
Coventry_data <- as.data.frame(lapply(Coventry_data, rep, Coventry_data$ntimes))


# Plot number of NEW CASES per day
CoventryNew_Sigfig <- signif(max(COVID19_by_LA$Coventry))
CoventryNew_Factor <- 10^(floor(log10(CoventryNew_Sigfig)))
CoventryNew_Max <- round_any(max(COVID19_by_LA$Coventry), CoventryNew_Factor, f=ceiling)
CoventryNew_Breaks <- ceiling(CoventryNew_Factor/2)

plot_CoventryNew <- ggplot(data=Coventry_data, aes(x=Coventry_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, CoventryNew_Max), 
                     breaks = seq(0, CoventryNew_Max, CoventryNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Coventry") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
CoventryCum_Sigfig <- signif(sum(COVID19_by_LA$Coventry))
CoventryCum_Factor <- 10^(floor(log10(CoventryCum_Sigfig)))
CoventryCum_Max <- round_any(sum(COVID19_by_LA$Coventry), CoventryCum_Factor, f=ceiling)
CoventryCum_Breaks <- ceiling(CoventryCum_Factor/2)

plot_CoventryCum <- ggplot(data=Coventry_data, aes(x=Coventry_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, CoventryCum_Max), 
                     breaks = seq(0, CoventryCum_Max, CoventryCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Coventry") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Coventry_figure <- ggarrange(plot_CoventryNew + font("x.text", size = 8), 
                             plot_CoventryCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Coventry_cases_plot.pdf", height = 8.27, width = 11.69)
Coventry_figure
dev.off()



#### Croydon plots ####
#Read in new cases per day from summarised data, format date correctly
Croydon_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Croydon))
Croydon_data <- as.data.frame(lapply(Croydon_data, rep, Croydon_data$ntimes))


# Plot number of NEW CASES per day
CroydonNew_Sigfig <- signif(max(COVID19_by_LA$Croydon))
CroydonNew_Factor <- 10^(floor(log10(CroydonNew_Sigfig)))
CroydonNew_Max <- round_any(max(COVID19_by_LA$Croydon), CroydonNew_Factor, f=ceiling)
CroydonNew_Breaks <- ceiling(CroydonNew_Factor/2)

plot_CroydonNew <- ggplot(data=Croydon_data, aes(x=Croydon_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, CroydonNew_Max), 
                     breaks = seq(0, CroydonNew_Max, CroydonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Croydon") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
CroydonCum_Sigfig <- signif(sum(COVID19_by_LA$Croydon))
CroydonCum_Factor <- 10^(floor(log10(CroydonCum_Sigfig)))
CroydonCum_Max <- round_any(sum(COVID19_by_LA$Croydon), CroydonCum_Factor, f=ceiling)
CroydonCum_Breaks <- ceiling(CroydonCum_Factor/2)

plot_CroydonCum <- ggplot(data=Croydon_data, aes(x=Croydon_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, CroydonCum_Max), 
                     breaks = seq(0, CroydonCum_Max, CroydonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Croydon") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Croydon_figure <- ggarrange(plot_CroydonNew + font("x.text", size = 8), 
                            plot_CroydonCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Croydon_cases_plot.pdf", height = 8.27, width = 11.69)
Croydon_figure
dev.off()



#### Cumbria plots ####
#Read in new cases per day from summarised data, format date correctly
Cumbria_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Cumbria))
Cumbria_data <- as.data.frame(lapply(Cumbria_data, rep, Cumbria_data$ntimes))


# Plot number of NEW CASES per day
CumbriaNew_Sigfig <- signif(max(COVID19_by_LA$Cumbria))
CumbriaNew_Factor <- 10^(floor(log10(CumbriaNew_Sigfig)))
CumbriaNew_Max <- round_any(max(COVID19_by_LA$Cumbria), CumbriaNew_Factor, f=ceiling)
CumbriaNew_Breaks <- ceiling(CumbriaNew_Factor/2)

plot_CumbriaNew <- ggplot(data=Cumbria_data, aes(x=Cumbria_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, CumbriaNew_Max), 
                     breaks = seq(0, CumbriaNew_Max, CumbriaNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Cumbria") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
CumbriaCum_Sigfig <- signif(sum(COVID19_by_LA$Cumbria))
CumbriaCum_Factor <- 10^(floor(log10(CumbriaCum_Sigfig)))
CumbriaCum_Max <- round_any(sum(COVID19_by_LA$Cumbria), CumbriaCum_Factor, f=ceiling)
CumbriaCum_Breaks <- ceiling(CumbriaCum_Factor/2)

plot_CumbriaCum <- ggplot(data=Cumbria_data, aes(x=Cumbria_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, CumbriaCum_Max), 
                     breaks = seq(0, CumbriaCum_Max, CumbriaCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Cumbria") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Cumbria_figure <- ggarrange(plot_CumbriaNew + font("x.text", size = 8), 
                            plot_CumbriaCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Cumbria_cases_plot.pdf", height = 8.27, width = 11.69)
Cumbria_figure
dev.off()



#### Darlington plots ####
#Read in new cases per day from summarised data, format date correctly
Darlington_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Darlington))
Darlington_data <- as.data.frame(lapply(Darlington_data, rep, Darlington_data$ntimes))


# Plot number of NEW CASES per day
DarlingtonNew_Sigfig <- signif(max(COVID19_by_LA$Darlington))
DarlingtonNew_Factor <- 10^(floor(log10(DarlingtonNew_Sigfig)))
DarlingtonNew_Max <- round_any(max(COVID19_by_LA$Darlington), DarlingtonNew_Factor, f=ceiling)
DarlingtonNew_Breaks <- ceiling(DarlingtonNew_Factor/2)

plot_DarlingtonNew <- ggplot(data=Darlington_data, aes(x=Darlington_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DarlingtonNew_Max), 
                     breaks = seq(0, DarlingtonNew_Max, DarlingtonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Darlington") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
DarlingtonCum_Sigfig <- signif(sum(COVID19_by_LA$Darlington))
DarlingtonCum_Factor <- 10^(floor(log10(DarlingtonCum_Sigfig)))
DarlingtonCum_Max <- round_any(sum(COVID19_by_LA$Darlington), DarlingtonCum_Factor, f=ceiling)
DarlingtonCum_Breaks <- ceiling(DarlingtonCum_Factor/2)

plot_DarlingtonCum <- ggplot(data=Darlington_data, aes(x=Darlington_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DarlingtonCum_Max), 
                     breaks = seq(0, DarlingtonCum_Max, DarlingtonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Darlington") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Darlington_figure <- ggarrange(plot_DarlingtonNew + font("x.text", size = 8), 
                               plot_DarlingtonCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Darlington_cases_plot.pdf", height = 8.27, width = 11.69)
Darlington_figure
dev.off()



#### Derby plots ####
#Read in new cases per day from summarised data, format date correctly
Derby_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Derby))
Derby_data <- as.data.frame(lapply(Derby_data, rep, Derby_data$ntimes))


# Plot number of NEW CASES per day
DerbyNew_Sigfig <- signif(max(COVID19_by_LA$Derby))
DerbyNew_Factor <- 10^(floor(log10(DerbyNew_Sigfig)))
DerbyNew_Max <- round_any(max(COVID19_by_LA$Derby), DerbyNew_Factor, f=ceiling)
DerbyNew_Breaks <- ceiling(DerbyNew_Factor/2)

plot_DerbyNew <- ggplot(data=Derby_data, aes(x=Derby_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DerbyNew_Max), 
                     breaks = seq(0, DerbyNew_Max, DerbyNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Derby") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
DerbyCum_Sigfig <- signif(sum(COVID19_by_LA$Derby))
DerbyCum_Factor <- 10^(floor(log10(DerbyCum_Sigfig)))
DerbyCum_Max <- round_any(sum(COVID19_by_LA$Derby), DerbyCum_Factor, f=ceiling)
DerbyCum_Breaks <- ceiling(DerbyCum_Factor/2)

plot_DerbyCum <- ggplot(data=Derby_data, aes(x=Derby_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DerbyCum_Max), 
                     breaks = seq(0, DerbyCum_Max, DerbyCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Derby") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Derby_figure <- ggarrange(plot_DerbyNew + font("x.text", size = 8), 
                          plot_DerbyCum + font("x.text", size = 8), 
                          ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Derby_cases_plot.pdf", height = 8.27, width = 11.69)
Derby_figure
dev.off()



#### Derbyshire plots ####
#Read in new cases per day from summarised data, format date correctly
Derbyshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Derbyshire))
Derbyshire_data <- as.data.frame(lapply(Derbyshire_data, rep, Derbyshire_data$ntimes))


# Plot number of NEW CASES per day
DerbyshireNew_Sigfig <- signif(max(COVID19_by_LA$Derbyshire))
DerbyshireNew_Factor <- 10^(floor(log10(DerbyshireNew_Sigfig)))
DerbyshireNew_Max <- round_any(max(COVID19_by_LA$Derbyshire), DerbyshireNew_Factor, f=ceiling)
DerbyshireNew_Breaks <- ceiling(DerbyshireNew_Factor/2)

plot_DerbyshireNew <- ggplot(data=Derbyshire_data, aes(x=Derbyshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DerbyshireNew_Max), 
                     breaks = seq(0, DerbyshireNew_Max, DerbyshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Derbyshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
DerbyshireCum_Sigfig <- signif(sum(COVID19_by_LA$Derbyshire))
DerbyshireCum_Factor <- 10^(floor(log10(DerbyshireCum_Sigfig)))
DerbyshireCum_Max <- round_any(sum(COVID19_by_LA$Derbyshire), DerbyshireCum_Factor, f=ceiling)
DerbyshireCum_Breaks <- ceiling(DerbyshireCum_Factor/2)

plot_DerbyshireCum <- ggplot(data=Derbyshire_data, aes(x=Derbyshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DerbyshireCum_Max), 
                     breaks = seq(0, DerbyshireCum_Max, DerbyshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Derbyshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Derbyshire_figure <- ggarrange(plot_DerbyshireNew + font("x.text", size = 8), 
                               plot_DerbyshireCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Derbyshire_cases_plot.pdf", height = 8.27, width = 11.69)
Derbyshire_figure
dev.off()



#### Devon plots ####
#Read in new cases per day from summarised data, format date correctly
Devon_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Devon))
Devon_data <- as.data.frame(lapply(Devon_data, rep, Devon_data$ntimes))


# Plot number of NEW CASES per day
DevonNew_Sigfig <- signif(max(COVID19_by_LA$Devon))
DevonNew_Factor <- 10^(floor(log10(DevonNew_Sigfig)))
DevonNew_Max <- round_any(max(COVID19_by_LA$Devon), DevonNew_Factor, f=ceiling)
DevonNew_Breaks <- ceiling(DevonNew_Factor/2)

plot_DevonNew <- ggplot(data=Devon_data, aes(x=Devon_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DevonNew_Max), 
                     breaks = seq(0, DevonNew_Max, DevonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Devon") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
DevonCum_Sigfig <- signif(sum(COVID19_by_LA$Devon))
DevonCum_Factor <- 10^(floor(log10(DevonCum_Sigfig)))
DevonCum_Max <- round_any(sum(COVID19_by_LA$Devon), DevonCum_Factor, f=ceiling)
DevonCum_Breaks <- ceiling(DevonCum_Factor/2)

plot_DevonCum <- ggplot(data=Devon_data, aes(x=Devon_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DevonCum_Max), 
                     breaks = seq(0, DevonCum_Max, DevonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Devon") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Devon_figure <- ggarrange(plot_DevonNew + font("x.text", size = 8), 
                          plot_DevonCum + font("x.text", size = 8), 
                          ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Devon_cases_plot.pdf", height = 8.27, width = 11.69)
Devon_figure
dev.off()



#### Doncaster plots ####
#Read in new cases per day from summarised data, format date correctly
Doncaster_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Doncaster))
Doncaster_data <- as.data.frame(lapply(Doncaster_data, rep, Doncaster_data$ntimes))


# Plot number of NEW CASES per day
DoncasterNew_Sigfig <- signif(max(COVID19_by_LA$Doncaster))
DoncasterNew_Factor <- 10^(floor(log10(DoncasterNew_Sigfig)))
DoncasterNew_Max <- round_any(max(COVID19_by_LA$Doncaster), DoncasterNew_Factor, f=ceiling)
DoncasterNew_Breaks <- ceiling(DoncasterNew_Factor/2)

plot_DoncasterNew <- ggplot(data=Doncaster_data, aes(x=Doncaster_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DoncasterNew_Max), 
                     breaks = seq(0, DoncasterNew_Max, DoncasterNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Doncaster") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
DoncasterCum_Sigfig <- signif(sum(COVID19_by_LA$Doncaster))
DoncasterCum_Factor <- 10^(floor(log10(DoncasterCum_Sigfig)))
DoncasterCum_Max <- round_any(sum(COVID19_by_LA$Doncaster), DoncasterCum_Factor, f=ceiling)
DoncasterCum_Breaks <- ceiling(DoncasterCum_Factor/2)

plot_DoncasterCum <- ggplot(data=Doncaster_data, aes(x=Doncaster_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DoncasterCum_Max), 
                     breaks = seq(0, DoncasterCum_Max, DoncasterCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Doncaster") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Doncaster_figure <- ggarrange(plot_DoncasterNew + font("x.text", size = 8), 
                              plot_DoncasterCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Doncaster_cases_plot.pdf", height = 8.27, width = 11.69)
Doncaster_figure
dev.off()



#### Dorset plots ####
#Read in new cases per day from summarised data, format date correctly
Dorset_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Dorset))
Dorset_data <- as.data.frame(lapply(Dorset_data, rep, Dorset_data$ntimes))


# Plot number of NEW CASES per day
DorsetNew_Sigfig <- signif(max(COVID19_by_LA$Dorset))
DorsetNew_Factor <- 10^(floor(log10(DorsetNew_Sigfig)))
DorsetNew_Max <- round_any(max(COVID19_by_LA$Dorset), DorsetNew_Factor, f=ceiling)
DorsetNew_Breaks <- ceiling(DorsetNew_Factor/2)

plot_DorsetNew <- ggplot(data=Dorset_data, aes(x=Dorset_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DorsetNew_Max), 
                     breaks = seq(0, DorsetNew_Max, DorsetNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Dorset") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
DorsetCum_Sigfig <- signif(sum(COVID19_by_LA$Dorset))
DorsetCum_Factor <- 10^(floor(log10(DorsetCum_Sigfig)))
DorsetCum_Max <- round_any(sum(COVID19_by_LA$Dorset), DorsetCum_Factor, f=ceiling)
DorsetCum_Breaks <- ceiling(DorsetCum_Factor/2)

plot_DorsetCum <- ggplot(data=Dorset_data, aes(x=Dorset_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DorsetCum_Max), 
                     breaks = seq(0, DorsetCum_Max, DorsetCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Dorset") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Dorset_figure <- ggarrange(plot_DorsetNew + font("x.text", size = 8), 
                           plot_DorsetCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Dorset_cases_plot.pdf", height = 8.27, width = 11.69)
Dorset_figure
dev.off()



#### Dudley plots ####
#Read in new cases per day from summarised data, format date correctly
Dudley_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Dudley))
Dudley_data <- as.data.frame(lapply(Dudley_data, rep, Dudley_data$ntimes))


# Plot number of NEW CASES per day
DudleyNew_Sigfig <- signif(max(COVID19_by_LA$Dudley))
DudleyNew_Factor <- 10^(floor(log10(DudleyNew_Sigfig)))
DudleyNew_Max <- round_any(max(COVID19_by_LA$Dudley), DudleyNew_Factor, f=ceiling)
DudleyNew_Breaks <- ceiling(DudleyNew_Factor/2)

plot_DudleyNew <- ggplot(data=Dudley_data, aes(x=Dudley_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DudleyNew_Max), 
                     breaks = seq(0, DudleyNew_Max, DudleyNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Dudley") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
DudleyCum_Sigfig <- signif(sum(COVID19_by_LA$Dudley))
DudleyCum_Factor <- 10^(floor(log10(DudleyCum_Sigfig)))
DudleyCum_Max <- round_any(sum(COVID19_by_LA$Dudley), DudleyCum_Factor, f=ceiling)
DudleyCum_Breaks <- ceiling(DudleyCum_Factor/2)

plot_DudleyCum <- ggplot(data=Dudley_data, aes(x=Dudley_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, DudleyCum_Max), 
                     breaks = seq(0, DudleyCum_Max, DudleyCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Dudley") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Dudley_figure <- ggarrange(plot_DudleyNew + font("x.text", size = 8), 
                           plot_DudleyCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Dudley_cases_plot.pdf", height = 8.27, width = 11.69)
Dudley_figure
dev.off()



#### Ealing plots ####
#Read in new cases per day from summarised data, format date correctly
Ealing_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Ealing))
Ealing_data <- as.data.frame(lapply(Ealing_data, rep, Ealing_data$ntimes))


# Plot number of NEW CASES per day
EalingNew_Sigfig <- signif(max(COVID19_by_LA$Ealing))
EalingNew_Factor <- 10^(floor(log10(EalingNew_Sigfig)))
EalingNew_Max <- round_any(max(COVID19_by_LA$Ealing), EalingNew_Factor, f=ceiling)
EalingNew_Breaks <- ceiling(EalingNew_Factor/2)

plot_EalingNew <- ggplot(data=Ealing_data, aes(x=Ealing_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, EalingNew_Max), 
                     breaks = seq(0, EalingNew_Max, EalingNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Ealing") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
EalingCum_Sigfig <- signif(sum(COVID19_by_LA$Ealing))
EalingCum_Factor <- 10^(floor(log10(EalingCum_Sigfig)))
EalingCum_Max <- round_any(sum(COVID19_by_LA$Ealing), EalingCum_Factor, f=ceiling)
EalingCum_Breaks <- ceiling(EalingCum_Factor/2)

plot_EalingCum <- ggplot(data=Ealing_data, aes(x=Ealing_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, EalingCum_Max), 
                     breaks = seq(0, EalingCum_Max, EalingCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Ealing") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Ealing_figure <- ggarrange(plot_EalingNew + font("x.text", size = 8), 
                           plot_EalingCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Ealing_cases_plot.pdf", height = 8.27, width = 11.69)
Ealing_figure
dev.off()



#### East_Riding_of_Yorkshire plots ####
#Read in new cases per day from summarised data, format date correctly
East_Riding_of_Yorkshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$East_Riding_of_Yorkshire))
East_Riding_of_Yorkshire_data <- as.data.frame(lapply(East_Riding_of_Yorkshire_data, rep, East_Riding_of_Yorkshire_data$ntimes))


# Plot number of NEW CASES per day
East_Riding_of_YorkshireNew_Sigfig <- signif(max(COVID19_by_LA$East_Riding_of_Yorkshire))
East_Riding_of_YorkshireNew_Factor <- 10^(floor(log10(East_Riding_of_YorkshireNew_Sigfig)))
East_Riding_of_YorkshireNew_Max <- round_any(max(COVID19_by_LA$East_Riding_of_Yorkshire), East_Riding_of_YorkshireNew_Factor, f=ceiling)
East_Riding_of_YorkshireNew_Breaks <- ceiling(East_Riding_of_YorkshireNew_Factor/2)

plot_East_Riding_of_YorkshireNew <- ggplot(data=East_Riding_of_Yorkshire_data, aes(x=East_Riding_of_Yorkshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, East_Riding_of_YorkshireNew_Max), 
                     breaks = seq(0, East_Riding_of_YorkshireNew_Max, East_Riding_of_YorkshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: East Riding of Yorkshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
East_Riding_of_YorkshireCum_Sigfig <- signif(sum(COVID19_by_LA$East_Riding_of_Yorkshire))
East_Riding_of_YorkshireCum_Factor <- 10^(floor(log10(East_Riding_of_YorkshireCum_Sigfig)))
East_Riding_of_YorkshireCum_Max <- round_any(sum(COVID19_by_LA$East_Riding_of_Yorkshire), East_Riding_of_YorkshireCum_Factor, f=ceiling)
East_Riding_of_YorkshireCum_Breaks <- ceiling(East_Riding_of_YorkshireCum_Factor/2)

plot_East_Riding_of_YorkshireCum <- ggplot(data=East_Riding_of_Yorkshire_data, aes(x=East_Riding_of_Yorkshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, East_Riding_of_YorkshireCum_Max), 
                     breaks = seq(0, East_Riding_of_YorkshireCum_Max, East_Riding_of_YorkshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: East Riding of Yorkshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
East_Riding_of_Yorkshire_figure <- ggarrange(plot_East_Riding_of_YorkshireNew + font("x.text", size = 8), 
                                             plot_East_Riding_of_YorkshireCum + font("x.text", size = 8), 
                                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/East_Riding_of_Yorkshire_cases_plot.pdf", height = 8.27, width = 11.69)
East_Riding_of_Yorkshire_figure
dev.off()



#### East_Sussex plots ####
#Read in new cases per day from summarised data, format date correctly
East_Sussex_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$East_Sussex))
East_Sussex_data <- as.data.frame(lapply(East_Sussex_data, rep, East_Sussex_data$ntimes))


# Plot number of NEW CASES per day
East_SussexNew_Sigfig <- signif(max(COVID19_by_LA$East_Sussex))
East_SussexNew_Factor <- 10^(floor(log10(East_SussexNew_Sigfig)))
East_SussexNew_Max <- round_any(max(COVID19_by_LA$East_Sussex), East_SussexNew_Factor, f=ceiling)
East_SussexNew_Breaks <- ceiling(East_SussexNew_Factor/2)

plot_East_SussexNew <- ggplot(data=East_Sussex_data, aes(x=East_Sussex_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, East_SussexNew_Max), 
                     breaks = seq(0, East_SussexNew_Max, East_SussexNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: East Sussex") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
East_SussexCum_Sigfig <- signif(sum(COVID19_by_LA$East_Sussex))
East_SussexCum_Factor <- 10^(floor(log10(East_SussexCum_Sigfig)))
East_SussexCum_Max <- round_any(sum(COVID19_by_LA$East_Sussex), East_SussexCum_Factor, f=ceiling)
East_SussexCum_Breaks <- ceiling(East_SussexCum_Factor/2)

plot_East_SussexCum <- ggplot(data=East_Sussex_data, aes(x=East_Sussex_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, East_SussexCum_Max), 
                     breaks = seq(0, East_SussexCum_Max, East_SussexCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: East Sussex") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
East_Sussex_figure <- ggarrange(plot_East_SussexNew + font("x.text", size = 8), 
                                plot_East_SussexCum + font("x.text", size = 8), 
                                ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/East_Sussex_cases_plot.pdf", height = 8.27, width = 11.69)
East_Sussex_figure
dev.off()



#### Enfield plots ####
#Read in new cases per day from summarised data, format date correctly
Enfield_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Enfield))
Enfield_data <- as.data.frame(lapply(Enfield_data, rep, Enfield_data$ntimes))


# Plot number of NEW CASES per day
EnfieldNew_Sigfig <- signif(max(COVID19_by_LA$Enfield))
EnfieldNew_Factor <- 10^(floor(log10(EnfieldNew_Sigfig)))
EnfieldNew_Max <- round_any(max(COVID19_by_LA$Enfield), EnfieldNew_Factor, f=ceiling)
EnfieldNew_Breaks <- ceiling(EnfieldNew_Factor/2)

plot_EnfieldNew <- ggplot(data=Enfield_data, aes(x=Enfield_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, EnfieldNew_Max), 
                     breaks = seq(0, EnfieldNew_Max, EnfieldNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Enfield") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
EnfieldCum_Sigfig <- signif(sum(COVID19_by_LA$Enfield))
EnfieldCum_Factor <- 10^(floor(log10(EnfieldCum_Sigfig)))
EnfieldCum_Max <- round_any(sum(COVID19_by_LA$Enfield), EnfieldCum_Factor, f=ceiling)
EnfieldCum_Breaks <- ceiling(EnfieldCum_Factor/2)

plot_EnfieldCum <- ggplot(data=Enfield_data, aes(x=Enfield_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, EnfieldCum_Max), 
                     breaks = seq(0, EnfieldCum_Max, EnfieldCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Enfield") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Enfield_figure <- ggarrange(plot_EnfieldNew + font("x.text", size = 8), 
                            plot_EnfieldCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Enfield_cases_plot.pdf", height = 8.27, width = 11.69)
Enfield_figure
dev.off()



#### Essex plots ####
#Read in new cases per day from summarised data, format date correctly
Essex_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Essex))
Essex_data <- as.data.frame(lapply(Essex_data, rep, Essex_data$ntimes))


# Plot number of NEW CASES per day
EssexNew_Sigfig <- signif(max(COVID19_by_LA$Essex))
EssexNew_Factor <- 10^(floor(log10(EssexNew_Sigfig)))
EssexNew_Max <- round_any(max(COVID19_by_LA$Essex), EssexNew_Factor, f=ceiling)
EssexNew_Breaks <- ceiling(EssexNew_Factor/2)

plot_EssexNew <- ggplot(data=Essex_data, aes(x=Essex_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, EssexNew_Max), 
                     breaks = seq(0, EssexNew_Max, EssexNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Essex") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
EssexCum_Sigfig <- signif(sum(COVID19_by_LA$Essex))
EssexCum_Factor <- 10^(floor(log10(EssexCum_Sigfig)))
EssexCum_Max <- round_any(sum(COVID19_by_LA$Essex), EssexCum_Factor, f=ceiling)
EssexCum_Breaks <- ceiling(EssexCum_Factor/2)

plot_EssexCum <- ggplot(data=Essex_data, aes(x=Essex_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, EssexCum_Max), 
                     breaks = seq(0, EssexCum_Max, EssexCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Essex") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Essex_figure <- ggarrange(plot_EssexNew + font("x.text", size = 8), 
                          plot_EssexCum + font("x.text", size = 8), 
                          ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Essex_cases_plot.pdf", height = 8.27, width = 11.69)
Essex_figure
dev.off()



#### Gateshead plots ####
#Read in new cases per day from summarised data, format date correctly
Gateshead_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Gateshead))
Gateshead_data <- as.data.frame(lapply(Gateshead_data, rep, Gateshead_data$ntimes))


# Plot number of NEW CASES per day
GatesheadNew_Sigfig <- signif(max(COVID19_by_LA$Gateshead))
GatesheadNew_Factor <- 10^(floor(log10(GatesheadNew_Sigfig)))
GatesheadNew_Max <- round_any(max(COVID19_by_LA$Gateshead), GatesheadNew_Factor, f=ceiling)
GatesheadNew_Breaks <- ceiling(GatesheadNew_Factor/2)

plot_GatesheadNew <- ggplot(data=Gateshead_data, aes(x=Gateshead_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, GatesheadNew_Max), 
                     breaks = seq(0, GatesheadNew_Max, GatesheadNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Gateshead") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
GatesheadCum_Sigfig <- signif(sum(COVID19_by_LA$Gateshead))
GatesheadCum_Factor <- 10^(floor(log10(GatesheadCum_Sigfig)))
GatesheadCum_Max <- round_any(sum(COVID19_by_LA$Gateshead), GatesheadCum_Factor, f=ceiling)
GatesheadCum_Breaks <- ceiling(GatesheadCum_Factor/2)

plot_GatesheadCum <- ggplot(data=Gateshead_data, aes(x=Gateshead_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, GatesheadCum_Max), 
                     breaks = seq(0, GatesheadCum_Max, GatesheadCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Gateshead") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Gateshead_figure <- ggarrange(plot_GatesheadNew + font("x.text", size = 8), 
                              plot_GatesheadCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Gateshead_cases_plot.pdf", height = 8.27, width = 11.69)
Gateshead_figure
dev.off()



#### Gloucestershire plots ####
#Read in new cases per day from summarised data, format date correctly
Gloucestershire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Gloucestershire))
Gloucestershire_data <- as.data.frame(lapply(Gloucestershire_data, rep, Gloucestershire_data$ntimes))


# Plot number of NEW CASES per day
GloucestershireNew_Sigfig <- signif(max(COVID19_by_LA$Gloucestershire))
GloucestershireNew_Factor <- 10^(floor(log10(GloucestershireNew_Sigfig)))
GloucestershireNew_Max <- round_any(max(COVID19_by_LA$Gloucestershire), GloucestershireNew_Factor, f=ceiling)
GloucestershireNew_Breaks <- ceiling(GloucestershireNew_Factor/2)

plot_GloucestershireNew <- ggplot(data=Gloucestershire_data, aes(x=Gloucestershire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, GloucestershireNew_Max), 
                     breaks = seq(0, GloucestershireNew_Max, GloucestershireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Gloucestershire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
GloucestershireCum_Sigfig <- signif(sum(COVID19_by_LA$Gloucestershire))
GloucestershireCum_Factor <- 10^(floor(log10(GloucestershireCum_Sigfig)))
GloucestershireCum_Max <- round_any(sum(COVID19_by_LA$Gloucestershire), GloucestershireCum_Factor, f=ceiling)
GloucestershireCum_Breaks <- ceiling(GloucestershireCum_Factor/2)

plot_GloucestershireCum <- ggplot(data=Gloucestershire_data, aes(x=Gloucestershire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, GloucestershireCum_Max), 
                     breaks = seq(0, GloucestershireCum_Max, GloucestershireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Gloucestershire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Gloucestershire_figure <- ggarrange(plot_GloucestershireNew + font("x.text", size = 8), 
                                    plot_GloucestershireCum + font("x.text", size = 8), 
                                    ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Gloucestershire_cases_plot.pdf", height = 8.27, width = 11.69)
Gloucestershire_figure
dev.off()



#### Greenwich plots ####
#Read in new cases per day from summarised data, format date correctly
Greenwich_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Greenwich))
Greenwich_data <- as.data.frame(lapply(Greenwich_data, rep, Greenwich_data$ntimes))


# Plot number of NEW CASES per day
GreenwichNew_Sigfig <- signif(max(COVID19_by_LA$Greenwich))
GreenwichNew_Factor <- 10^(floor(log10(GreenwichNew_Sigfig)))
GreenwichNew_Max <- round_any(max(COVID19_by_LA$Greenwich), GreenwichNew_Factor, f=ceiling)
GreenwichNew_Breaks <- ceiling(GreenwichNew_Factor/2)

plot_GreenwichNew <- ggplot(data=Greenwich_data, aes(x=Greenwich_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, GreenwichNew_Max), 
                     breaks = seq(0, GreenwichNew_Max, GreenwichNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Greenwich") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
GreenwichCum_Sigfig <- signif(sum(COVID19_by_LA$Greenwich))
GreenwichCum_Factor <- 10^(floor(log10(GreenwichCum_Sigfig)))
GreenwichCum_Max <- round_any(sum(COVID19_by_LA$Greenwich), GreenwichCum_Factor, f=ceiling)
GreenwichCum_Breaks <- ceiling(GreenwichCum_Factor/2)

plot_GreenwichCum <- ggplot(data=Greenwich_data, aes(x=Greenwich_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, GreenwichCum_Max), 
                     breaks = seq(0, GreenwichCum_Max, GreenwichCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Greenwich") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Greenwich_figure <- ggarrange(plot_GreenwichNew + font("x.text", size = 8), 
                              plot_GreenwichCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Greenwich_cases_plot.pdf", height = 8.27, width = 11.69)
Greenwich_figure
dev.off()



#### Hackney_and_City_of_London plots ####
#Read in new cases per day from summarised data, format date correctly
Hackney_and_City_of_London_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Hackney_and_City_of_London))
Hackney_and_City_of_London_data <- as.data.frame(lapply(Hackney_and_City_of_London_data, rep, Hackney_and_City_of_London_data$ntimes))


# Plot number of NEW CASES per day
Hackney_and_City_of_LondonNew_Sigfig <- signif(max(COVID19_by_LA$Hackney_and_City_of_London))
Hackney_and_City_of_LondonNew_Factor <- 10^(floor(log10(Hackney_and_City_of_LondonNew_Sigfig)))
Hackney_and_City_of_LondonNew_Max <- round_any(max(COVID19_by_LA$Hackney_and_City_of_London), Hackney_and_City_of_LondonNew_Factor, f=ceiling)
Hackney_and_City_of_LondonNew_Breaks <- ceiling(Hackney_and_City_of_LondonNew_Factor/2)

plot_Hackney_and_City_of_LondonNew <- ggplot(data=Hackney_and_City_of_London_data, aes(x=Hackney_and_City_of_London_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Hackney_and_City_of_LondonNew_Max), 
                     breaks = seq(0, Hackney_and_City_of_LondonNew_Max, Hackney_and_City_of_LondonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Hackney and City of London") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Hackney_and_City_of_LondonCum_Sigfig <- signif(sum(COVID19_by_LA$Hackney_and_City_of_London))
Hackney_and_City_of_LondonCum_Factor <- 10^(floor(log10(Hackney_and_City_of_LondonCum_Sigfig)))
Hackney_and_City_of_LondonCum_Max <- round_any(sum(COVID19_by_LA$Hackney_and_City_of_London), Hackney_and_City_of_LondonCum_Factor, f=ceiling)
Hackney_and_City_of_LondonCum_Breaks <- ceiling(Hackney_and_City_of_LondonCum_Factor/2)

plot_Hackney_and_City_of_LondonCum <- ggplot(data=Hackney_and_City_of_London_data, aes(x=Hackney_and_City_of_London_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Hackney_and_City_of_LondonCum_Max), 
                     breaks = seq(0, Hackney_and_City_of_LondonCum_Max, Hackney_and_City_of_LondonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Hackney and City of London") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Hackney_and_City_of_London_figure <- ggarrange(plot_Hackney_and_City_of_LondonNew + font("x.text", size = 8), 
                                               plot_Hackney_and_City_of_LondonCum + font("x.text", size = 8), 
                                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Hackney_and_City_of_London_cases_plot.pdf", height = 8.27, width = 11.69)
Hackney_and_City_of_London_figure
dev.off()



#### Halton plots ####
#Read in new cases per day from summarised data, format date correctly
Halton_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Halton))
Halton_data <- as.data.frame(lapply(Halton_data, rep, Halton_data$ntimes))


# Plot number of NEW CASES per day
HaltonNew_Sigfig <- signif(max(COVID19_by_LA$Halton))
HaltonNew_Factor <- 10^(floor(log10(HaltonNew_Sigfig)))
HaltonNew_Max <- round_any(max(COVID19_by_LA$Halton), HaltonNew_Factor, f=ceiling)
HaltonNew_Breaks <- ceiling(HaltonNew_Factor/2)

plot_HaltonNew <- ggplot(data=Halton_data, aes(x=Halton_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HaltonNew_Max), 
                     breaks = seq(0, HaltonNew_Max, HaltonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Halton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
HaltonCum_Sigfig <- signif(sum(COVID19_by_LA$Halton))
HaltonCum_Factor <- 10^(floor(log10(HaltonCum_Sigfig)))
HaltonCum_Max <- round_any(sum(COVID19_by_LA$Halton), HaltonCum_Factor, f=ceiling)
HaltonCum_Breaks <- ceiling(HaltonCum_Factor/2)

plot_HaltonCum <- ggplot(data=Halton_data, aes(x=Halton_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HaltonCum_Max), 
                     breaks = seq(0, HaltonCum_Max, HaltonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Halton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Halton_figure <- ggarrange(plot_HaltonNew + font("x.text", size = 8), 
                           plot_HaltonCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Halton_cases_plot.pdf", height = 8.27, width = 11.69)
Halton_figure
dev.off()



#### Hammersmith_and_Fulham plots ####
#Read in new cases per day from summarised data, format date correctly
Hammersmith_and_Fulham_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Hammersmith_and_Fulham))
Hammersmith_and_Fulham_data <- as.data.frame(lapply(Hammersmith_and_Fulham_data, rep, Hammersmith_and_Fulham_data$ntimes))


# Plot number of NEW CASES per day
Hammersmith_and_FulhamNew_Sigfig <- signif(max(COVID19_by_LA$Hammersmith_and_Fulham))
Hammersmith_and_FulhamNew_Factor <- 10^(floor(log10(Hammersmith_and_FulhamNew_Sigfig)))
Hammersmith_and_FulhamNew_Max <- round_any(max(COVID19_by_LA$Hammersmith_and_Fulham), Hammersmith_and_FulhamNew_Factor, f=ceiling)
Hammersmith_and_FulhamNew_Breaks <- ceiling(Hammersmith_and_FulhamNew_Factor/2)

plot_Hammersmith_and_FulhamNew <- ggplot(data=Hammersmith_and_Fulham_data, aes(x=Hammersmith_and_Fulham_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Hammersmith_and_FulhamNew_Max), 
                     breaks = seq(0, Hammersmith_and_FulhamNew_Max, Hammersmith_and_FulhamNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Hammersmith and Fulham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Hammersmith_and_FulhamCum_Sigfig <- signif(sum(COVID19_by_LA$Hammersmith_and_Fulham))
Hammersmith_and_FulhamCum_Factor <- 10^(floor(log10(Hammersmith_and_FulhamCum_Sigfig)))
Hammersmith_and_FulhamCum_Max <- round_any(sum(COVID19_by_LA$Hammersmith_and_Fulham), Hammersmith_and_FulhamCum_Factor, f=ceiling)
Hammersmith_and_FulhamCum_Breaks <- ceiling(Hammersmith_and_FulhamCum_Factor/2)

plot_Hammersmith_and_FulhamCum <- ggplot(data=Hammersmith_and_Fulham_data, aes(x=Hammersmith_and_Fulham_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Hammersmith_and_FulhamCum_Max), 
                     breaks = seq(0, Hammersmith_and_FulhamCum_Max, Hammersmith_and_FulhamCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Hammersmith and Fulham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Hammersmith_and_Fulham_figure <- ggarrange(plot_Hammersmith_and_FulhamNew + font("x.text", size = 8), 
                                           plot_Hammersmith_and_FulhamCum + font("x.text", size = 8), 
                                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Hammersmith_and_Fulham_cases_plot.pdf", height = 8.27, width = 11.69)
Hammersmith_and_Fulham_figure
dev.off()



#### Hampshire plots ####
#Read in new cases per day from summarised data, format date correctly
Hampshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Hampshire))
Hampshire_data <- as.data.frame(lapply(Hampshire_data, rep, Hampshire_data$ntimes))


# Plot number of NEW CASES per day
HampshireNew_Sigfig <- signif(max(COVID19_by_LA$Hampshire))
HampshireNew_Factor <- 10^(floor(log10(HampshireNew_Sigfig)))
HampshireNew_Max <- round_any(max(COVID19_by_LA$Hampshire), HampshireNew_Factor, f=ceiling)
HampshireNew_Breaks <- ceiling(HampshireNew_Factor/2)

plot_HampshireNew <- ggplot(data=Hampshire_data, aes(x=Hampshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HampshireNew_Max), 
                     breaks = seq(0, HampshireNew_Max, HampshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Hampshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
HampshireCum_Sigfig <- signif(sum(COVID19_by_LA$Hampshire))
HampshireCum_Factor <- 10^(floor(log10(HampshireCum_Sigfig)))
HampshireCum_Max <- round_any(sum(COVID19_by_LA$Hampshire), HampshireCum_Factor, f=ceiling)
HampshireCum_Breaks <- ceiling(HampshireCum_Factor/2)

plot_HampshireCum <- ggplot(data=Hampshire_data, aes(x=Hampshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HampshireCum_Max), 
                     breaks = seq(0, HampshireCum_Max, HampshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Hampshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Hampshire_figure <- ggarrange(plot_HampshireNew + font("x.text", size = 8), 
                              plot_HampshireCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Hampshire_cases_plot.pdf", height = 8.27, width = 11.69)
Hampshire_figure
dev.off()



#### Haringey plots ####
#Read in new cases per day from summarised data, format date correctly
Haringey_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Haringey))
Haringey_data <- as.data.frame(lapply(Haringey_data, rep, Haringey_data$ntimes))


# Plot number of NEW CASES per day
HaringeyNew_Sigfig <- signif(max(COVID19_by_LA$Haringey))
HaringeyNew_Factor <- 10^(floor(log10(HaringeyNew_Sigfig)))
HaringeyNew_Max <- round_any(max(COVID19_by_LA$Haringey), HaringeyNew_Factor, f=ceiling)
HaringeyNew_Breaks <- ceiling(HaringeyNew_Factor/2)

plot_HaringeyNew <- ggplot(data=Haringey_data, aes(x=Haringey_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HaringeyNew_Max), 
                     breaks = seq(0, HaringeyNew_Max, HaringeyNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Haringey") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
HaringeyCum_Sigfig <- signif(sum(COVID19_by_LA$Haringey))
HaringeyCum_Factor <- 10^(floor(log10(HaringeyCum_Sigfig)))
HaringeyCum_Max <- round_any(sum(COVID19_by_LA$Haringey), HaringeyCum_Factor, f=ceiling)
HaringeyCum_Breaks <- ceiling(HaringeyCum_Factor/2)

plot_HaringeyCum <- ggplot(data=Haringey_data, aes(x=Haringey_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HaringeyCum_Max), 
                     breaks = seq(0, HaringeyCum_Max, HaringeyCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Haringey") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Haringey_figure <- ggarrange(plot_HaringeyNew + font("x.text", size = 8), 
                             plot_HaringeyCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Haringey_cases_plot.pdf", height = 8.27, width = 11.69)
Haringey_figure
dev.off()



#### Harrow plots ####
#Read in new cases per day from summarised data, format date correctly
Harrow_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Harrow))
Harrow_data <- as.data.frame(lapply(Harrow_data, rep, Harrow_data$ntimes))


# Plot number of NEW CASES per day
HarrowNew_Sigfig <- signif(max(COVID19_by_LA$Harrow))
HarrowNew_Factor <- 10^(floor(log10(HarrowNew_Sigfig)))
HarrowNew_Max <- round_any(max(COVID19_by_LA$Harrow), HarrowNew_Factor, f=ceiling)
HarrowNew_Breaks <- ceiling(HarrowNew_Factor/2)

plot_HarrowNew <- ggplot(data=Harrow_data, aes(x=Harrow_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HarrowNew_Max), 
                     breaks = seq(0, HarrowNew_Max, HarrowNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Harrow") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
HarrowCum_Sigfig <- signif(sum(COVID19_by_LA$Harrow))
HarrowCum_Factor <- 10^(floor(log10(HarrowCum_Sigfig)))
HarrowCum_Max <- round_any(sum(COVID19_by_LA$Harrow), HarrowCum_Factor, f=ceiling)
HarrowCum_Breaks <- ceiling(HarrowCum_Factor/2)

plot_HarrowCum <- ggplot(data=Harrow_data, aes(x=Harrow_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HarrowCum_Max), 
                     breaks = seq(0, HarrowCum_Max, HarrowCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Harrow") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Harrow_figure <- ggarrange(plot_HarrowNew + font("x.text", size = 8), 
                           plot_HarrowCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Harrow_cases_plot.pdf", height = 8.27, width = 11.69)
Harrow_figure
dev.off()



#### Hartlepool plots ####
#Read in new cases per day from summarised data, format date correctly
Hartlepool_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Hartlepool))
Hartlepool_data <- as.data.frame(lapply(Hartlepool_data, rep, Hartlepool_data$ntimes))


# Plot number of NEW CASES per day
HartlepoolNew_Sigfig <- signif(max(COVID19_by_LA$Hartlepool))
HartlepoolNew_Factor <- 10^(floor(log10(HartlepoolNew_Sigfig)))
HartlepoolNew_Max <- round_any(max(COVID19_by_LA$Hartlepool), HartlepoolNew_Factor, f=ceiling)
HartlepoolNew_Breaks <- ceiling(HartlepoolNew_Factor/2)

plot_HartlepoolNew <- ggplot(data=Hartlepool_data, aes(x=Hartlepool_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HartlepoolNew_Max), 
                     breaks = seq(0, HartlepoolNew_Max, HartlepoolNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Hartlepool") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
HartlepoolCum_Sigfig <- signif(sum(COVID19_by_LA$Hartlepool))
HartlepoolCum_Factor <- 10^(floor(log10(HartlepoolCum_Sigfig)))
HartlepoolCum_Max <- round_any(sum(COVID19_by_LA$Hartlepool), HartlepoolCum_Factor, f=ceiling)
HartlepoolCum_Breaks <- ceiling(HartlepoolCum_Factor/2)

plot_HartlepoolCum <- ggplot(data=Hartlepool_data, aes(x=Hartlepool_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HartlepoolCum_Max), 
                     breaks = seq(0, HartlepoolCum_Max, HartlepoolCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Hartlepool") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Hartlepool_figure <- ggarrange(plot_HartlepoolNew + font("x.text", size = 8), 
                               plot_HartlepoolCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Hartlepool_cases_plot.pdf", height = 8.27, width = 11.69)
Hartlepool_figure
dev.off()



#### Havering plots ####
#Read in new cases per day from summarised data, format date correctly
Havering_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Havering))
Havering_data <- as.data.frame(lapply(Havering_data, rep, Havering_data$ntimes))


# Plot number of NEW CASES per day
HaveringNew_Sigfig <- signif(max(COVID19_by_LA$Havering))
HaveringNew_Factor <- 10^(floor(log10(HaveringNew_Sigfig)))
HaveringNew_Max <- round_any(max(COVID19_by_LA$Havering), HaveringNew_Factor, f=ceiling)
HaveringNew_Breaks <- ceiling(HaveringNew_Factor/2)

plot_HaveringNew <- ggplot(data=Havering_data, aes(x=Havering_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HaveringNew_Max), 
                     breaks = seq(0, HaveringNew_Max, HaveringNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Havering") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
HaveringCum_Sigfig <- signif(sum(COVID19_by_LA$Havering))
HaveringCum_Factor <- 10^(floor(log10(HaveringCum_Sigfig)))
HaveringCum_Max <- round_any(sum(COVID19_by_LA$Havering), HaveringCum_Factor, f=ceiling)
HaveringCum_Breaks <- ceiling(HaveringCum_Factor/2)

plot_HaveringCum <- ggplot(data=Havering_data, aes(x=Havering_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HaveringCum_Max), 
                     breaks = seq(0, HaveringCum_Max, HaveringCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Havering") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Havering_figure <- ggarrange(plot_HaveringNew + font("x.text", size = 8), 
                             plot_HaveringCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Havering_cases_plot.pdf", height = 8.27, width = 11.69)
Havering_figure
dev.off()



#### Herefordshire plots ####
#Read in new cases per day from summarised data, format date correctly
Herefordshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Herefordshire))
Herefordshire_data <- as.data.frame(lapply(Herefordshire_data, rep, Herefordshire_data$ntimes))


# Plot number of NEW CASES per day
HerefordshireNew_Sigfig <- signif(max(COVID19_by_LA$Herefordshire))
HerefordshireNew_Factor <- 10^(floor(log10(HerefordshireNew_Sigfig)))
HerefordshireNew_Max <- round_any(max(COVID19_by_LA$Herefordshire), HerefordshireNew_Factor, f=ceiling)
HerefordshireNew_Breaks <- ceiling(HerefordshireNew_Factor/2)

plot_HerefordshireNew <- ggplot(data=Herefordshire_data, aes(x=Herefordshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HerefordshireNew_Max), 
                     breaks = seq(0, HerefordshireNew_Max, HerefordshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Herefordshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
HerefordshireCum_Sigfig <- signif(sum(COVID19_by_LA$Herefordshire))
HerefordshireCum_Factor <- 10^(floor(log10(HerefordshireCum_Sigfig)))
HerefordshireCum_Max <- round_any(sum(COVID19_by_LA$Herefordshire), HerefordshireCum_Factor, f=ceiling)
HerefordshireCum_Breaks <- ceiling(HerefordshireCum_Factor/2)

plot_HerefordshireCum <- ggplot(data=Herefordshire_data, aes(x=Herefordshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HerefordshireCum_Max), 
                     breaks = seq(0, HerefordshireCum_Max, HerefordshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Herefordshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Herefordshire_figure <- ggarrange(plot_HerefordshireNew + font("x.text", size = 8), 
                                  plot_HerefordshireCum + font("x.text", size = 8), 
                                  ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Herefordshire_cases_plot.pdf", height = 8.27, width = 11.69)
Herefordshire_figure
dev.off()



#### Hertfordshire plots ####
#Read in new cases per day from summarised data, format date correctly
Hertfordshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Hertfordshire))
Hertfordshire_data <- as.data.frame(lapply(Hertfordshire_data, rep, Hertfordshire_data$ntimes))


# Plot number of NEW CASES per day
HertfordshireNew_Sigfig <- signif(max(COVID19_by_LA$Hertfordshire))
HertfordshireNew_Factor <- 10^(floor(log10(HertfordshireNew_Sigfig)))
HertfordshireNew_Max <- round_any(max(COVID19_by_LA$Hertfordshire), HertfordshireNew_Factor, f=ceiling)
HertfordshireNew_Breaks <- ceiling(HertfordshireNew_Factor/2)

plot_HertfordshireNew <- ggplot(data=Hertfordshire_data, aes(x=Hertfordshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HertfordshireNew_Max), 
                     breaks = seq(0, HertfordshireNew_Max, HertfordshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Hertfordshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
HertfordshireCum_Sigfig <- signif(sum(COVID19_by_LA$Hertfordshire))
HertfordshireCum_Factor <- 10^(floor(log10(HertfordshireCum_Sigfig)))
HertfordshireCum_Max <- round_any(sum(COVID19_by_LA$Hertfordshire), HertfordshireCum_Factor, f=ceiling)
HertfordshireCum_Breaks <- ceiling(HertfordshireCum_Factor/2)

plot_HertfordshireCum <- ggplot(data=Hertfordshire_data, aes(x=Hertfordshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HertfordshireCum_Max), 
                     breaks = seq(0, HertfordshireCum_Max, HertfordshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Hertfordshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Hertfordshire_figure <- ggarrange(plot_HertfordshireNew + font("x.text", size = 8), 
                                  plot_HertfordshireCum + font("x.text", size = 8), 
                                  ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Hertfordshire_cases_plot.pdf", height = 8.27, width = 11.69)
Hertfordshire_figure
dev.off()



#### Hillingdon plots ####
#Read in new cases per day from summarised data, format date correctly
Hillingdon_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Hillingdon))
Hillingdon_data <- as.data.frame(lapply(Hillingdon_data, rep, Hillingdon_data$ntimes))


# Plot number of NEW CASES per day
HillingdonNew_Sigfig <- signif(max(COVID19_by_LA$Hillingdon))
HillingdonNew_Factor <- 10^(floor(log10(HillingdonNew_Sigfig)))
HillingdonNew_Max <- round_any(max(COVID19_by_LA$Hillingdon), HillingdonNew_Factor, f=ceiling)
HillingdonNew_Breaks <- ceiling(HillingdonNew_Factor/2)

plot_HillingdonNew <- ggplot(data=Hillingdon_data, aes(x=Hillingdon_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HillingdonNew_Max), 
                     breaks = seq(0, HillingdonNew_Max, HillingdonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Hillingdon") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
HillingdonCum_Sigfig <- signif(sum(COVID19_by_LA$Hillingdon))
HillingdonCum_Factor <- 10^(floor(log10(HillingdonCum_Sigfig)))
HillingdonCum_Max <- round_any(sum(COVID19_by_LA$Hillingdon), HillingdonCum_Factor, f=ceiling)
HillingdonCum_Breaks <- ceiling(HillingdonCum_Factor/2)

plot_HillingdonCum <- ggplot(data=Hillingdon_data, aes(x=Hillingdon_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HillingdonCum_Max), 
                     breaks = seq(0, HillingdonCum_Max, HillingdonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Hillingdon") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Hillingdon_figure <- ggarrange(plot_HillingdonNew + font("x.text", size = 8), 
                               plot_HillingdonCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Hillingdon_cases_plot.pdf", height = 8.27, width = 11.69)
Hillingdon_figure
dev.off()



#### Hounslow plots ####
#Read in new cases per day from summarised data, format date correctly
Hounslow_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Hounslow))
Hounslow_data <- as.data.frame(lapply(Hounslow_data, rep, Hounslow_data$ntimes))


# Plot number of NEW CASES per day
HounslowNew_Sigfig <- signif(max(COVID19_by_LA$Hounslow))
HounslowNew_Factor <- 10^(floor(log10(HounslowNew_Sigfig)))
HounslowNew_Max <- round_any(max(COVID19_by_LA$Hounslow), HounslowNew_Factor, f=ceiling)
HounslowNew_Breaks <- ceiling(HounslowNew_Factor/2)

plot_HounslowNew <- ggplot(data=Hounslow_data, aes(x=Hounslow_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HounslowNew_Max), 
                     breaks = seq(0, HounslowNew_Max, HounslowNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Hounslow") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
HounslowCum_Sigfig <- signif(sum(COVID19_by_LA$Hounslow))
HounslowCum_Factor <- 10^(floor(log10(HounslowCum_Sigfig)))
HounslowCum_Max <- round_any(sum(COVID19_by_LA$Hounslow), HounslowCum_Factor, f=ceiling)
HounslowCum_Breaks <- ceiling(HounslowCum_Factor/2)

plot_HounslowCum <- ggplot(data=Hounslow_data, aes(x=Hounslow_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, HounslowCum_Max), 
                     breaks = seq(0, HounslowCum_Max, HounslowCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Hounslow") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Hounslow_figure <- ggarrange(plot_HounslowNew + font("x.text", size = 8), 
                             plot_HounslowCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Hounslow_cases_plot.pdf", height = 8.27, width = 11.69)
Hounslow_figure
dev.off()



#### Isle_of_Wight plots ####
#Read in new cases per day from summarised data, format date correctly
Isle_of_Wight_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Isle_of_Wight))
Isle_of_Wight_data <- as.data.frame(lapply(Isle_of_Wight_data, rep, Isle_of_Wight_data$ntimes))


# Plot number of NEW CASES per day
Isle_of_WightNew_Sigfig <- signif(max(COVID19_by_LA$Isle_of_Wight))
Isle_of_WightNew_Factor <- 10^(floor(log10(Isle_of_WightNew_Sigfig)))
Isle_of_WightNew_Max <- round_any(max(COVID19_by_LA$Isle_of_Wight), Isle_of_WightNew_Factor, f=ceiling)
Isle_of_WightNew_Breaks <- ceiling(Isle_of_WightNew_Factor/2)

plot_Isle_of_WightNew <- ggplot(data=Isle_of_Wight_data, aes(x=Isle_of_Wight_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Isle_of_WightNew_Max), 
                     breaks = seq(0, Isle_of_WightNew_Max, Isle_of_WightNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Isle of Wight") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Isle_of_WightCum_Sigfig <- signif(sum(COVID19_by_LA$Isle_of_Wight))
Isle_of_WightCum_Factor <- 10^(floor(log10(Isle_of_WightCum_Sigfig)))
Isle_of_WightCum_Max <- round_any(sum(COVID19_by_LA$Isle_of_Wight), Isle_of_WightCum_Factor, f=ceiling)
Isle_of_WightCum_Breaks <- ceiling(Isle_of_WightCum_Factor/2)

plot_Isle_of_WightCum <- ggplot(data=Isle_of_Wight_data, aes(x=Isle_of_Wight_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Isle_of_WightCum_Max), 
                     breaks = seq(0, Isle_of_WightCum_Max, Isle_of_WightCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Isle of Wight") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Isle_of_Wight_figure <- ggarrange(plot_Isle_of_WightNew + font("x.text", size = 8), 
                                  plot_Isle_of_WightCum + font("x.text", size = 8), 
                                  ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Isle_of_Wight_cases_plot.pdf", height = 8.27, width = 11.69)
Isle_of_Wight_figure
dev.off()



#### Islington plots ####
#Read in new cases per day from summarised data, format date correctly
Islington_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Islington))
Islington_data <- as.data.frame(lapply(Islington_data, rep, Islington_data$ntimes))


# Plot number of NEW CASES per day
IslingtonNew_Sigfig <- signif(max(COVID19_by_LA$Islington))
IslingtonNew_Factor <- 10^(floor(log10(IslingtonNew_Sigfig)))
IslingtonNew_Max <- round_any(max(COVID19_by_LA$Islington), IslingtonNew_Factor, f=ceiling)
IslingtonNew_Breaks <- ceiling(IslingtonNew_Factor/2)

plot_IslingtonNew <- ggplot(data=Islington_data, aes(x=Islington_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, IslingtonNew_Max), 
                     breaks = seq(0, IslingtonNew_Max, IslingtonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Islington") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
IslingtonCum_Sigfig <- signif(sum(COVID19_by_LA$Islington))
IslingtonCum_Factor <- 10^(floor(log10(IslingtonCum_Sigfig)))
IslingtonCum_Max <- round_any(sum(COVID19_by_LA$Islington), IslingtonCum_Factor, f=ceiling)
IslingtonCum_Breaks <- ceiling(IslingtonCum_Factor/2)

plot_IslingtonCum <- ggplot(data=Islington_data, aes(x=Islington_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, IslingtonCum_Max), 
                     breaks = seq(0, IslingtonCum_Max, IslingtonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Islington") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Islington_figure <- ggarrange(plot_IslingtonNew + font("x.text", size = 8), 
                              plot_IslingtonCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Islington_cases_plot.pdf", height = 8.27, width = 11.69)
Islington_figure
dev.off()



#### Kensington_and_Chelsea plots ####
#Read in new cases per day from summarised data, format date correctly
Kensington_and_Chelsea_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Kensington_and_Chelsea))
Kensington_and_Chelsea_data <- as.data.frame(lapply(Kensington_and_Chelsea_data, rep, Kensington_and_Chelsea_data$ntimes))


# Plot number of NEW CASES per day
Kensington_and_ChelseaNew_Sigfig <- signif(max(COVID19_by_LA$Kensington_and_Chelsea))
Kensington_and_ChelseaNew_Factor <- 10^(floor(log10(Kensington_and_ChelseaNew_Sigfig)))
Kensington_and_ChelseaNew_Max <- round_any(max(COVID19_by_LA$Kensington_and_Chelsea), Kensington_and_ChelseaNew_Factor, f=ceiling)
Kensington_and_ChelseaNew_Breaks <- ceiling(Kensington_and_ChelseaNew_Factor/2)

plot_Kensington_and_ChelseaNew <- ggplot(data=Kensington_and_Chelsea_data, aes(x=Kensington_and_Chelsea_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Kensington_and_ChelseaNew_Max), 
                     breaks = seq(0, Kensington_and_ChelseaNew_Max, Kensington_and_ChelseaNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Kensington and Chelsea") +
  theme_minimal() +
  theme(plot.title = element_text(size=11, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Kensington_and_ChelseaCum_Sigfig <- signif(sum(COVID19_by_LA$Kensington_and_Chelsea))
Kensington_and_ChelseaCum_Factor <- 10^(floor(log10(Kensington_and_ChelseaCum_Sigfig)))
Kensington_and_ChelseaCum_Max <- round_any(sum(COVID19_by_LA$Kensington_and_Chelsea), Kensington_and_ChelseaCum_Factor, f=ceiling)
Kensington_and_ChelseaCum_Breaks <- ceiling(Kensington_and_ChelseaCum_Factor/2)

plot_Kensington_and_ChelseaCum <- ggplot(data=Kensington_and_Chelsea_data, aes(x=Kensington_and_Chelsea_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Kensington_and_ChelseaCum_Max), 
                     breaks = seq(0, Kensington_and_ChelseaCum_Max, Kensington_and_ChelseaCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Kensington and Chelsea") +
  theme_minimal() +
  theme(plot.title = element_text(size=11, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Kensington_and_Chelsea_figure <- ggarrange(plot_Kensington_and_ChelseaNew + font("x.text", size = 8), 
                                           plot_Kensington_and_ChelseaCum + font("x.text", size = 8), 
                                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Kensington_and_Chelsea_cases_plot.pdf", height = 8.27, width = 11.69)
Kensington_and_Chelsea_figure
dev.off()



#### Kent plots ####
#Read in new cases per day from summarised data, format date correctly
Kent_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Kent))
Kent_data <- as.data.frame(lapply(Kent_data, rep, Kent_data$ntimes))


# Plot number of NEW CASES per day
KentNew_Sigfig <- signif(max(COVID19_by_LA$Kent))
KentNew_Factor <- 10^(floor(log10(KentNew_Sigfig)))
KentNew_Max <- round_any(max(COVID19_by_LA$Kent), KentNew_Factor, f=ceiling)
KentNew_Breaks <- ceiling(KentNew_Factor/2)

plot_KentNew <- ggplot(data=Kent_data, aes(x=Kent_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, KentNew_Max), 
                     breaks = seq(0, KentNew_Max, KentNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Kent") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
KentCum_Sigfig <- signif(sum(COVID19_by_LA$Kent))
KentCum_Factor <- 10^(floor(log10(KentCum_Sigfig)))
KentCum_Max <- round_any(sum(COVID19_by_LA$Kent), KentCum_Factor, f=ceiling)
KentCum_Breaks <- ceiling(KentCum_Factor/2)

plot_KentCum <- ggplot(data=Kent_data, aes(x=Kent_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, KentCum_Max), 
                     breaks = seq(0, KentCum_Max, KentCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Kent") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Kent_figure <- ggarrange(plot_KentNew + font("x.text", size = 8), 
                         plot_KentCum + font("x.text", size = 8), 
                         ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Kent_cases_plot.pdf", height = 8.27, width = 11.69)
Kent_figure
dev.off()



#### Kingston_upon_Hull plots ####
#Read in new cases per day from summarised data, format date correctly
Kingston_upon_Hull_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Kingston_upon_Hull))
Kingston_upon_Hull_data <- as.data.frame(lapply(Kingston_upon_Hull_data, rep, Kingston_upon_Hull_data$ntimes))


# Plot number of NEW CASES per day
Kingston_upon_HullNew_Sigfig <- signif(max(COVID19_by_LA$Kingston_upon_Hull))
Kingston_upon_HullNew_Factor <- 10^(floor(log10(Kingston_upon_HullNew_Sigfig)))
Kingston_upon_HullNew_Max <- round_any(max(COVID19_by_LA$Kingston_upon_Hull), Kingston_upon_HullNew_Factor, f=ceiling)
Kingston_upon_HullNew_Breaks <- ceiling(Kingston_upon_HullNew_Factor/2)

plot_Kingston_upon_HullNew <- ggplot(data=Kingston_upon_Hull_data, aes(x=Kingston_upon_Hull_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Kingston_upon_HullNew_Max), 
                     breaks = seq(0, Kingston_upon_HullNew_Max, Kingston_upon_HullNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Kingston upon Hull") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Kingston_upon_HullCum_Sigfig <- signif(sum(COVID19_by_LA$Kingston_upon_Hull))
Kingston_upon_HullCum_Factor <- 10^(floor(log10(Kingston_upon_HullCum_Sigfig)))
Kingston_upon_HullCum_Max <- round_any(sum(COVID19_by_LA$Kingston_upon_Hull), Kingston_upon_HullCum_Factor, f=ceiling)
Kingston_upon_HullCum_Breaks <- ceiling(Kingston_upon_HullCum_Factor/2)

plot_Kingston_upon_HullCum <- ggplot(data=Kingston_upon_Hull_data, aes(x=Kingston_upon_Hull_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Kingston_upon_HullCum_Max), 
                     breaks = seq(0, Kingston_upon_HullCum_Max, Kingston_upon_HullCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Kingston upon Hull") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Kingston_upon_Hull_figure <- ggarrange(plot_Kingston_upon_HullNew + font("x.text", size = 8), 
                                       plot_Kingston_upon_HullCum + font("x.text", size = 8), 
                                       ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Kingston_upon_Hull_cases_plot.pdf", height = 8.27, width = 11.69)
Kingston_upon_Hull_figure
dev.off()



#### Kingston_upon_Thames plots ####
#Read in new cases per day from summarised data, format date correctly
Kingston_upon_Thames_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Kingston_upon_Thames))
Kingston_upon_Thames_data <- as.data.frame(lapply(Kingston_upon_Thames_data, rep, Kingston_upon_Thames_data$ntimes))


# Plot number of NEW CASES per day
Kingston_upon_ThamesNew_Sigfig <- signif(max(COVID19_by_LA$Kingston_upon_Thames))
Kingston_upon_ThamesNew_Factor <- 10^(floor(log10(Kingston_upon_ThamesNew_Sigfig)))
Kingston_upon_ThamesNew_Max <- round_any(max(COVID19_by_LA$Kingston_upon_Thames), Kingston_upon_ThamesNew_Factor, f=ceiling)
Kingston_upon_ThamesNew_Breaks <- ceiling(Kingston_upon_ThamesNew_Factor/2)

plot_Kingston_upon_ThamesNew <- ggplot(data=Kingston_upon_Thames_data, aes(x=Kingston_upon_Thames_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Kingston_upon_ThamesNew_Max), 
                     breaks = seq(0, Kingston_upon_ThamesNew_Max, Kingston_upon_ThamesNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Kingston upon Thames") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Kingston_upon_ThamesCum_Sigfig <- signif(sum(COVID19_by_LA$Kingston_upon_Thames))
Kingston_upon_ThamesCum_Factor <- 10^(floor(log10(Kingston_upon_ThamesCum_Sigfig)))
Kingston_upon_ThamesCum_Max <- round_any(sum(COVID19_by_LA$Kingston_upon_Thames), Kingston_upon_ThamesCum_Factor, f=ceiling)
Kingston_upon_ThamesCum_Breaks <- ceiling(Kingston_upon_ThamesCum_Factor/2)

plot_Kingston_upon_ThamesCum <- ggplot(data=Kingston_upon_Thames_data, aes(x=Kingston_upon_Thames_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Kingston_upon_ThamesCum_Max), 
                     breaks = seq(0, Kingston_upon_ThamesCum_Max, Kingston_upon_ThamesCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Kingston upon Thames") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Kingston_upon_Thames_figure <- ggarrange(plot_Kingston_upon_ThamesNew + font("x.text", size = 8), 
                                         plot_Kingston_upon_ThamesCum + font("x.text", size = 8), 
                                         ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Kingston_upon_Thames_cases_plot.pdf", height = 8.27, width = 11.69)
Kingston_upon_Thames_figure
dev.off()



#### Kirklees plots ####
#Read in new cases per day from summarised data, format date correctly
Kirklees_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Kirklees))
Kirklees_data <- as.data.frame(lapply(Kirklees_data, rep, Kirklees_data$ntimes))


# Plot number of NEW CASES per day
KirkleesNew_Sigfig <- signif(max(COVID19_by_LA$Kirklees))
KirkleesNew_Factor <- 10^(floor(log10(KirkleesNew_Sigfig)))
KirkleesNew_Max <- round_any(max(COVID19_by_LA$Kirklees), KirkleesNew_Factor, f=ceiling)
KirkleesNew_Breaks <- ceiling(KirkleesNew_Factor/2)

plot_KirkleesNew <- ggplot(data=Kirklees_data, aes(x=Kirklees_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, KirkleesNew_Max), 
                     breaks = seq(0, KirkleesNew_Max, KirkleesNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Kirklees") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
KirkleesCum_Sigfig <- signif(sum(COVID19_by_LA$Kirklees))
KirkleesCum_Factor <- 10^(floor(log10(KirkleesCum_Sigfig)))
KirkleesCum_Max <- round_any(sum(COVID19_by_LA$Kirklees), KirkleesCum_Factor, f=ceiling)
KirkleesCum_Breaks <- ceiling(KirkleesCum_Factor/2)

plot_KirkleesCum <- ggplot(data=Kirklees_data, aes(x=Kirklees_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, KirkleesCum_Max), 
                     breaks = seq(0, KirkleesCum_Max, KirkleesCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Kirklees") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Kirklees_figure <- ggarrange(plot_KirkleesNew + font("x.text", size = 8), 
                             plot_KirkleesCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Kirklees_cases_plot.pdf", height = 8.27, width = 11.69)
Kirklees_figure
dev.off()



#### Knowsley plots ####
#Read in new cases per day from summarised data, format date correctly
Knowsley_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Knowsley))
Knowsley_data <- as.data.frame(lapply(Knowsley_data, rep, Knowsley_data$ntimes))


# Plot number of NEW CASES per day
KnowsleyNew_Sigfig <- signif(max(COVID19_by_LA$Knowsley))
KnowsleyNew_Factor <- 10^(floor(log10(KnowsleyNew_Sigfig)))
KnowsleyNew_Max <- round_any(max(COVID19_by_LA$Knowsley), KnowsleyNew_Factor, f=ceiling)
KnowsleyNew_Breaks <- ceiling(KnowsleyNew_Factor/2)

plot_KnowsleyNew <- ggplot(data=Knowsley_data, aes(x=Knowsley_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, KnowsleyNew_Max), 
                     breaks = seq(0, KnowsleyNew_Max, KnowsleyNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Knowsley") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
KnowsleyCum_Sigfig <- signif(sum(COVID19_by_LA$Knowsley))
KnowsleyCum_Factor <- 10^(floor(log10(KnowsleyCum_Sigfig)))
KnowsleyCum_Max <- round_any(sum(COVID19_by_LA$Knowsley), KnowsleyCum_Factor, f=ceiling)
KnowsleyCum_Breaks <- ceiling(KnowsleyCum_Factor/2)

plot_KnowsleyCum <- ggplot(data=Knowsley_data, aes(x=Knowsley_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, KnowsleyCum_Max), 
                     breaks = seq(0, KnowsleyCum_Max, KnowsleyCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Knowsley") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Knowsley_figure <- ggarrange(plot_KnowsleyNew + font("x.text", size = 8), 
                             plot_KnowsleyCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Knowsley_cases_plot.pdf", height = 8.27, width = 11.69)
Knowsley_figure
dev.off()



#### Lambeth plots ####
#Read in new cases per day from summarised data, format date correctly
Lambeth_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Lambeth))
Lambeth_data <- as.data.frame(lapply(Lambeth_data, rep, Lambeth_data$ntimes))


# Plot number of NEW CASES per day
LambethNew_Sigfig <- signif(max(COVID19_by_LA$Lambeth))
LambethNew_Factor <- 10^(floor(log10(LambethNew_Sigfig)))
LambethNew_Max <- round_any(max(COVID19_by_LA$Lambeth), LambethNew_Factor, f=ceiling)
LambethNew_Breaks <- ceiling(LambethNew_Factor/2)

plot_LambethNew <- ggplot(data=Lambeth_data, aes(x=Lambeth_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LambethNew_Max), 
                     breaks = seq(0, LambethNew_Max, LambethNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Lambeth") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
LambethCum_Sigfig <- signif(sum(COVID19_by_LA$Lambeth))
LambethCum_Factor <- 10^(floor(log10(LambethCum_Sigfig)))
LambethCum_Max <- round_any(sum(COVID19_by_LA$Lambeth), LambethCum_Factor, f=ceiling)
LambethCum_Breaks <- ceiling(LambethCum_Factor/2)

plot_LambethCum <- ggplot(data=Lambeth_data, aes(x=Lambeth_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LambethCum_Max), 
                     breaks = seq(0, LambethCum_Max, LambethCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Lambeth") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Lambeth_figure <- ggarrange(plot_LambethNew + font("x.text", size = 8), 
                            plot_LambethCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Lambeth_cases_plot.pdf", height = 8.27, width = 11.69)
Lambeth_figure
dev.off()



#### Lancashire plots ####
#Read in new cases per day from summarised data, format date correctly
Lancashire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Lancashire))
Lancashire_data <- as.data.frame(lapply(Lancashire_data, rep, Lancashire_data$ntimes))


# Plot number of NEW CASES per day
LancashireNew_Sigfig <- signif(max(COVID19_by_LA$Lancashire))
LancashireNew_Factor <- 10^(floor(log10(LancashireNew_Sigfig)))
LancashireNew_Max <- round_any(max(COVID19_by_LA$Lancashire), LancashireNew_Factor, f=ceiling)
LancashireNew_Breaks <- ceiling(LancashireNew_Factor/2)

plot_LancashireNew <- ggplot(data=Lancashire_data, aes(x=Lancashire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LancashireNew_Max), 
                     breaks = seq(0, LancashireNew_Max, LancashireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Lancashire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
LancashireCum_Sigfig <- signif(sum(COVID19_by_LA$Lancashire))
LancashireCum_Factor <- 10^(floor(log10(LancashireCum_Sigfig)))
LancashireCum_Max <- round_any(sum(COVID19_by_LA$Lancashire), LancashireCum_Factor, f=ceiling)
LancashireCum_Breaks <- ceiling(LancashireCum_Factor/2)

plot_LancashireCum <- ggplot(data=Lancashire_data, aes(x=Lancashire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LancashireCum_Max), 
                     breaks = seq(0, LancashireCum_Max, LancashireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Lancashire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Lancashire_figure <- ggarrange(plot_LancashireNew + font("x.text", size = 8), 
                               plot_LancashireCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Lancashire_cases_plot.pdf", height = 8.27, width = 11.69)
Lancashire_figure
dev.off()



#### Leeds plots ####
#Read in new cases per day from summarised data, format date correctly
Leeds_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Leeds))
Leeds_data <- as.data.frame(lapply(Leeds_data, rep, Leeds_data$ntimes))


# Plot number of NEW CASES per day
LeedsNew_Sigfig <- signif(max(COVID19_by_LA$Leeds))
LeedsNew_Factor <- 10^(floor(log10(LeedsNew_Sigfig)))
LeedsNew_Max <- round_any(max(COVID19_by_LA$Leeds), LeedsNew_Factor, f=ceiling)
LeedsNew_Breaks <- ceiling(LeedsNew_Factor/2)

plot_LeedsNew <- ggplot(data=Leeds_data, aes(x=Leeds_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LeedsNew_Max), 
                     breaks = seq(0, LeedsNew_Max, LeedsNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Leeds") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
LeedsCum_Sigfig <- signif(sum(COVID19_by_LA$Leeds))
LeedsCum_Factor <- 10^(floor(log10(LeedsCum_Sigfig)))
LeedsCum_Max <- round_any(sum(COVID19_by_LA$Leeds), LeedsCum_Factor, f=ceiling)
LeedsCum_Breaks <- ceiling(LeedsCum_Factor/2)

plot_LeedsCum <- ggplot(data=Leeds_data, aes(x=Leeds_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LeedsCum_Max), 
                     breaks = seq(0, LeedsCum_Max, LeedsCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Leeds") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Leeds_figure <- ggarrange(plot_LeedsNew + font("x.text", size = 8), 
                          plot_LeedsCum + font("x.text", size = 8), 
                          ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Leeds_cases_plot.pdf", height = 8.27, width = 11.69)
Leeds_figure
dev.off()



#### Leicester plots ####
#Read in new cases per day from summarised data, format date correctly
Leicester_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Leicester))
Leicester_data <- as.data.frame(lapply(Leicester_data, rep, Leicester_data$ntimes))


# Plot number of NEW CASES per day
LeicesterNew_Sigfig <- signif(max(COVID19_by_LA$Leicester))
LeicesterNew_Factor <- 10^(floor(log10(LeicesterNew_Sigfig)))
LeicesterNew_Max <- round_any(max(COVID19_by_LA$Leicester), LeicesterNew_Factor, f=ceiling)
LeicesterNew_Breaks <- ceiling(LeicesterNew_Factor/2)

plot_LeicesterNew <- ggplot(data=Leicester_data, aes(x=Leicester_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LeicesterNew_Max), 
                     breaks = seq(0, LeicesterNew_Max, LeicesterNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Leicester") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
LeicesterCum_Sigfig <- signif(sum(COVID19_by_LA$Leicester))
LeicesterCum_Factor <- 10^(floor(log10(LeicesterCum_Sigfig)))
LeicesterCum_Max <- round_any(sum(COVID19_by_LA$Leicester), LeicesterCum_Factor, f=ceiling)
LeicesterCum_Breaks <- ceiling(LeicesterCum_Factor/2)

plot_LeicesterCum <- ggplot(data=Leicester_data, aes(x=Leicester_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LeicesterCum_Max), 
                     breaks = seq(0, LeicesterCum_Max, LeicesterCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Leicester") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Leicester_figure <- ggarrange(plot_LeicesterNew + font("x.text", size = 8), 
                              plot_LeicesterCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Leicester_cases_plot.pdf", height = 8.27, width = 11.69)
Leicester_figure
dev.off()



#### Leicestershire plots ####
#Read in new cases per day from summarised data, format date correctly
Leicestershire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Leicestershire))
Leicestershire_data <- as.data.frame(lapply(Leicestershire_data, rep, Leicestershire_data$ntimes))


# Plot number of NEW CASES per day
LeicestershireNew_Sigfig <- signif(max(COVID19_by_LA$Leicestershire))
LeicestershireNew_Factor <- 10^(floor(log10(LeicestershireNew_Sigfig)))
LeicestershireNew_Max <- round_any(max(COVID19_by_LA$Leicestershire), LeicestershireNew_Factor, f=ceiling)
LeicestershireNew_Breaks <- ceiling(LeicestershireNew_Factor/2)

plot_LeicestershireNew <- ggplot(data=Leicestershire_data, aes(x=Leicestershire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LeicestershireNew_Max), 
                     breaks = seq(0, LeicestershireNew_Max, LeicestershireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Leicestershire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
LeicestershireCum_Sigfig <- signif(sum(COVID19_by_LA$Leicestershire))
LeicestershireCum_Factor <- 10^(floor(log10(LeicestershireCum_Sigfig)))
LeicestershireCum_Max <- round_any(sum(COVID19_by_LA$Leicestershire), LeicestershireCum_Factor, f=ceiling)
LeicestershireCum_Breaks <- ceiling(LeicestershireCum_Factor/2)

plot_LeicestershireCum <- ggplot(data=Leicestershire_data, aes(x=Leicestershire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LeicestershireCum_Max), 
                     breaks = seq(0, LeicestershireCum_Max, LeicestershireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Leicestershire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Leicestershire_figure <- ggarrange(plot_LeicestershireNew + font("x.text", size = 8), 
                                   plot_LeicestershireCum + font("x.text", size = 8), 
                                   ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Leicestershire_cases_plot.pdf", height = 8.27, width = 11.69)
Leicestershire_figure
dev.off()



#### Lewisham plots ####
#Read in new cases per day from summarised data, format date correctly
Lewisham_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Lewisham))
Lewisham_data <- as.data.frame(lapply(Lewisham_data, rep, Lewisham_data$ntimes))


# Plot number of NEW CASES per day
LewishamNew_Sigfig <- signif(max(COVID19_by_LA$Lewisham))
LewishamNew_Factor <- 10^(floor(log10(LewishamNew_Sigfig)))
LewishamNew_Max <- round_any(max(COVID19_by_LA$Lewisham), LewishamNew_Factor, f=ceiling)
LewishamNew_Breaks <- ceiling(LewishamNew_Factor/2)

plot_LewishamNew <- ggplot(data=Lewisham_data, aes(x=Lewisham_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LewishamNew_Max), 
                     breaks = seq(0, LewishamNew_Max, LewishamNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Lewisham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
LewishamCum_Sigfig <- signif(sum(COVID19_by_LA$Lewisham))
LewishamCum_Factor <- 10^(floor(log10(LewishamCum_Sigfig)))
LewishamCum_Max <- round_any(sum(COVID19_by_LA$Lewisham), LewishamCum_Factor, f=ceiling)
LewishamCum_Breaks <- ceiling(LewishamCum_Factor/2)

plot_LewishamCum <- ggplot(data=Lewisham_data, aes(x=Lewisham_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LewishamCum_Max), 
                     breaks = seq(0, LewishamCum_Max, LewishamCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Lewisham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Lewisham_figure <- ggarrange(plot_LewishamNew + font("x.text", size = 8), 
                             plot_LewishamCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Lewisham_cases_plot.pdf", height = 8.27, width = 11.69)
Lewisham_figure
dev.off()



#### Lincolnshire plots ####
#Read in new cases per day from summarised data, format date correctly
Lincolnshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Lincolnshire))
Lincolnshire_data <- as.data.frame(lapply(Lincolnshire_data, rep, Lincolnshire_data$ntimes))


# Plot number of NEW CASES per day
LincolnshireNew_Sigfig <- signif(max(COVID19_by_LA$Lincolnshire))
LincolnshireNew_Factor <- 10^(floor(log10(LincolnshireNew_Sigfig)))
LincolnshireNew_Max <- round_any(max(COVID19_by_LA$Lincolnshire), LincolnshireNew_Factor, f=ceiling)
LincolnshireNew_Breaks <- ceiling(LincolnshireNew_Factor/2)

plot_LincolnshireNew <- ggplot(data=Lincolnshire_data, aes(x=Lincolnshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LincolnshireNew_Max), 
                     breaks = seq(0, LincolnshireNew_Max, LincolnshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Lincolnshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
LincolnshireCum_Sigfig <- signif(sum(COVID19_by_LA$Lincolnshire))
LincolnshireCum_Factor <- 10^(floor(log10(LincolnshireCum_Sigfig)))
LincolnshireCum_Max <- round_any(sum(COVID19_by_LA$Lincolnshire), LincolnshireCum_Factor, f=ceiling)
LincolnshireCum_Breaks <- ceiling(LincolnshireCum_Factor/2)

plot_LincolnshireCum <- ggplot(data=Lincolnshire_data, aes(x=Lincolnshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LincolnshireCum_Max), 
                     breaks = seq(0, LincolnshireCum_Max, LincolnshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Lincolnshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Lincolnshire_figure <- ggarrange(plot_LincolnshireNew + font("x.text", size = 8), 
                                 plot_LincolnshireCum + font("x.text", size = 8), 
                                 ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Lincolnshire_cases_plot.pdf", height = 8.27, width = 11.69)
Lincolnshire_figure
dev.off()



#### Liverpool plots ####
#Read in new cases per day from summarised data, format date correctly
Liverpool_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Liverpool))
Liverpool_data <- as.data.frame(lapply(Liverpool_data, rep, Liverpool_data$ntimes))


# Plot number of NEW CASES per day
LiverpoolNew_Sigfig <- signif(max(COVID19_by_LA$Liverpool))
LiverpoolNew_Factor <- 10^(floor(log10(LiverpoolNew_Sigfig)))
LiverpoolNew_Max <- round_any(max(COVID19_by_LA$Liverpool), LiverpoolNew_Factor, f=ceiling)
LiverpoolNew_Breaks <- ceiling(LiverpoolNew_Factor/2)

plot_LiverpoolNew <- ggplot(data=Liverpool_data, aes(x=Liverpool_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LiverpoolNew_Max), 
                     breaks = seq(0, LiverpoolNew_Max, LiverpoolNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Liverpool") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
LiverpoolCum_Sigfig <- signif(sum(COVID19_by_LA$Liverpool))
LiverpoolCum_Factor <- 10^(floor(log10(LiverpoolCum_Sigfig)))
LiverpoolCum_Max <- round_any(sum(COVID19_by_LA$Liverpool), LiverpoolCum_Factor, f=ceiling)
LiverpoolCum_Breaks <- ceiling(LiverpoolCum_Factor/2)

plot_LiverpoolCum <- ggplot(data=Liverpool_data, aes(x=Liverpool_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LiverpoolCum_Max), 
                     breaks = seq(0, LiverpoolCum_Max, LiverpoolCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Liverpool") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Liverpool_figure <- ggarrange(plot_LiverpoolNew + font("x.text", size = 8), 
                              plot_LiverpoolCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Liverpool_cases_plot.pdf", height = 8.27, width = 11.69)
Liverpool_figure
dev.off()



#### Luton plots ####
#Read in new cases per day from summarised data, format date correctly
Luton_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Luton))
Luton_data <- as.data.frame(lapply(Luton_data, rep, Luton_data$ntimes))


# Plot number of NEW CASES per day
LutonNew_Sigfig <- signif(max(COVID19_by_LA$Luton))
LutonNew_Factor <- 10^(floor(log10(LutonNew_Sigfig)))
LutonNew_Max <- round_any(max(COVID19_by_LA$Luton), LutonNew_Factor, f=ceiling)
LutonNew_Breaks <- ceiling(LutonNew_Factor/2)

plot_LutonNew <- ggplot(data=Luton_data, aes(x=Luton_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LutonNew_Max), 
                     breaks = seq(0, LutonNew_Max, LutonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Luton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
LutonCum_Sigfig <- signif(sum(COVID19_by_LA$Luton))
LutonCum_Factor <- 10^(floor(log10(LutonCum_Sigfig)))
LutonCum_Max <- round_any(sum(COVID19_by_LA$Luton), LutonCum_Factor, f=ceiling)
LutonCum_Breaks <- ceiling(LutonCum_Factor/2)

plot_LutonCum <- ggplot(data=Luton_data, aes(x=Luton_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, LutonCum_Max), 
                     breaks = seq(0, LutonCum_Max, LutonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Luton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Luton_figure <- ggarrange(plot_LutonNew + font("x.text", size = 8), 
                          plot_LutonCum + font("x.text", size = 8), 
                          ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Luton_cases_plot.pdf", height = 8.27, width = 11.69)
Luton_figure
dev.off()



#### Manchester plots ####
#Read in new cases per day from summarised data, format date correctly
Manchester_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Manchester))
Manchester_data <- as.data.frame(lapply(Manchester_data, rep, Manchester_data$ntimes))


# Plot number of NEW CASES per day
ManchesterNew_Sigfig <- signif(max(COVID19_by_LA$Manchester))
ManchesterNew_Factor <- 10^(floor(log10(ManchesterNew_Sigfig)))
ManchesterNew_Max <- round_any(max(COVID19_by_LA$Manchester), ManchesterNew_Factor, f=ceiling)
ManchesterNew_Breaks <- ceiling(ManchesterNew_Factor/2)

plot_ManchesterNew <- ggplot(data=Manchester_data, aes(x=Manchester_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, ManchesterNew_Max), 
                     breaks = seq(0, ManchesterNew_Max, ManchesterNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Manchester") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
ManchesterCum_Sigfig <- signif(sum(COVID19_by_LA$Manchester))
ManchesterCum_Factor <- 10^(floor(log10(ManchesterCum_Sigfig)))
ManchesterCum_Max <- round_any(sum(COVID19_by_LA$Manchester), ManchesterCum_Factor, f=ceiling)
ManchesterCum_Breaks <- ceiling(ManchesterCum_Factor/2)

plot_ManchesterCum <- ggplot(data=Manchester_data, aes(x=Manchester_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, ManchesterCum_Max), 
                     breaks = seq(0, ManchesterCum_Max, ManchesterCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Manchester") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Manchester_figure <- ggarrange(plot_ManchesterNew + font("x.text", size = 8), 
                               plot_ManchesterCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Manchester_cases_plot.pdf", height = 8.27, width = 11.69)
Manchester_figure
dev.off()



#### Medway plots ####
#Read in new cases per day from summarised data, format date correctly
Medway_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Medway))
Medway_data <- as.data.frame(lapply(Medway_data, rep, Medway_data$ntimes))


# Plot number of NEW CASES per day
MedwayNew_Sigfig <- signif(max(COVID19_by_LA$Medway))
MedwayNew_Factor <- 10^(floor(log10(MedwayNew_Sigfig)))
MedwayNew_Max <- round_any(max(COVID19_by_LA$Medway), MedwayNew_Factor, f=ceiling)
MedwayNew_Breaks <- ceiling(MedwayNew_Factor/2)

plot_MedwayNew <- ggplot(data=Medway_data, aes(x=Medway_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, MedwayNew_Max), 
                     breaks = seq(0, MedwayNew_Max, MedwayNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Medway") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
MedwayCum_Sigfig <- signif(sum(COVID19_by_LA$Medway))
MedwayCum_Factor <- 10^(floor(log10(MedwayCum_Sigfig)))
MedwayCum_Max <- round_any(sum(COVID19_by_LA$Medway), MedwayCum_Factor, f=ceiling)
MedwayCum_Breaks <- ceiling(MedwayCum_Factor/2)

plot_MedwayCum <- ggplot(data=Medway_data, aes(x=Medway_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, MedwayCum_Max), 
                     breaks = seq(0, MedwayCum_Max, MedwayCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Medway") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Medway_figure <- ggarrange(plot_MedwayNew + font("x.text", size = 8), 
                           plot_MedwayCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Medway_cases_plot.pdf", height = 8.27, width = 11.69)
Medway_figure
dev.off()



#### Merton plots ####
#Read in new cases per day from summarised data, format date correctly
Merton_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Merton))
Merton_data <- as.data.frame(lapply(Merton_data, rep, Merton_data$ntimes))


# Plot number of NEW CASES per day
MertonNew_Sigfig <- signif(max(COVID19_by_LA$Merton))
MertonNew_Factor <- 10^(floor(log10(MertonNew_Sigfig)))
MertonNew_Max <- round_any(max(COVID19_by_LA$Merton), MertonNew_Factor, f=ceiling)
MertonNew_Breaks <- ceiling(MertonNew_Factor/2)

plot_MertonNew <- ggplot(data=Merton_data, aes(x=Merton_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, MertonNew_Max), 
                     breaks = seq(0, MertonNew_Max, MertonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Merton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
MertonCum_Sigfig <- signif(sum(COVID19_by_LA$Merton))
MertonCum_Factor <- 10^(floor(log10(MertonCum_Sigfig)))
MertonCum_Max <- round_any(sum(COVID19_by_LA$Merton), MertonCum_Factor, f=ceiling)
MertonCum_Breaks <- ceiling(MertonCum_Factor/2)

plot_MertonCum <- ggplot(data=Merton_data, aes(x=Merton_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, MertonCum_Max), 
                     breaks = seq(0, MertonCum_Max, MertonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Merton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Merton_figure <- ggarrange(plot_MertonNew + font("x.text", size = 8), 
                           plot_MertonCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Merton_cases_plot.pdf", height = 8.27, width = 11.69)
Merton_figure
dev.off()



#### Middlesbrough plots ####
#Read in new cases per day from summarised data, format date correctly
Middlesbrough_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Middlesbrough))
Middlesbrough_data <- as.data.frame(lapply(Middlesbrough_data, rep, Middlesbrough_data$ntimes))


# Plot number of NEW CASES per day
MiddlesbroughNew_Sigfig <- signif(max(COVID19_by_LA$Middlesbrough))
MiddlesbroughNew_Factor <- 10^(floor(log10(MiddlesbroughNew_Sigfig)))
MiddlesbroughNew_Max <- round_any(max(COVID19_by_LA$Middlesbrough), MiddlesbroughNew_Factor, f=ceiling)
MiddlesbroughNew_Breaks <- ceiling(MiddlesbroughNew_Factor/2)

plot_MiddlesbroughNew <- ggplot(data=Middlesbrough_data, aes(x=Middlesbrough_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, MiddlesbroughNew_Max), 
                     breaks = seq(0, MiddlesbroughNew_Max, MiddlesbroughNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Middlesbrough") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
MiddlesbroughCum_Sigfig <- signif(sum(COVID19_by_LA$Middlesbrough))
MiddlesbroughCum_Factor <- 10^(floor(log10(MiddlesbroughCum_Sigfig)))
MiddlesbroughCum_Max <- round_any(sum(COVID19_by_LA$Middlesbrough), MiddlesbroughCum_Factor, f=ceiling)
MiddlesbroughCum_Breaks <- ceiling(MiddlesbroughCum_Factor/2)

plot_MiddlesbroughCum <- ggplot(data=Middlesbrough_data, aes(x=Middlesbrough_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, MiddlesbroughCum_Max), 
                     breaks = seq(0, MiddlesbroughCum_Max, MiddlesbroughCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Middlesbrough") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Middlesbrough_figure <- ggarrange(plot_MiddlesbroughNew + font("x.text", size = 8), 
                                  plot_MiddlesbroughCum + font("x.text", size = 8), 
                                  ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Middlesbrough_cases_plot.pdf", height = 8.27, width = 11.69)
Middlesbrough_figure
dev.off()



#### Milton_Keynes plots ####
#Read in new cases per day from summarised data, format date correctly
Milton_Keynes_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Milton_Keynes))
Milton_Keynes_data <- as.data.frame(lapply(Milton_Keynes_data, rep, Milton_Keynes_data$ntimes))


# Plot number of NEW CASES per day
Milton_KeynesNew_Sigfig <- signif(max(COVID19_by_LA$Milton_Keynes))
Milton_KeynesNew_Factor <- 10^(floor(log10(Milton_KeynesNew_Sigfig)))
Milton_KeynesNew_Max <- round_any(max(COVID19_by_LA$Milton_Keynes), Milton_KeynesNew_Factor, f=ceiling)
Milton_KeynesNew_Breaks <- ceiling(Milton_KeynesNew_Factor/2)

plot_Milton_KeynesNew <- ggplot(data=Milton_Keynes_data, aes(x=Milton_Keynes_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Milton_KeynesNew_Max), 
                     breaks = seq(0, Milton_KeynesNew_Max, Milton_KeynesNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Milton Keynes") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Milton_KeynesCum_Sigfig <- signif(sum(COVID19_by_LA$Milton_Keynes))
Milton_KeynesCum_Factor <- 10^(floor(log10(Milton_KeynesCum_Sigfig)))
Milton_KeynesCum_Max <- round_any(sum(COVID19_by_LA$Milton_Keynes), Milton_KeynesCum_Factor, f=ceiling)
Milton_KeynesCum_Breaks <- ceiling(Milton_KeynesCum_Factor/2)

plot_Milton_KeynesCum <- ggplot(data=Milton_Keynes_data, aes(x=Milton_Keynes_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Milton_KeynesCum_Max), 
                     breaks = seq(0, Milton_KeynesCum_Max, Milton_KeynesCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Milton Keynes") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Milton_Keynes_figure <- ggarrange(plot_Milton_KeynesNew + font("x.text", size = 8), 
                                  plot_Milton_KeynesCum + font("x.text", size = 8), 
                                  ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Milton_Keynes_cases_plot.pdf", height = 8.27, width = 11.69)
Milton_Keynes_figure
dev.off()



#### Newcastle_upon_Tyne plots ####
#Read in new cases per day from summarised data, format date correctly
Newcastle_upon_Tyne_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Newcastle_upon_Tyne))
Newcastle_upon_Tyne_data <- as.data.frame(lapply(Newcastle_upon_Tyne_data, rep, Newcastle_upon_Tyne_data$ntimes))


# Plot number of NEW CASES per day
Newcastle_upon_TyneNew_Sigfig <- signif(max(COVID19_by_LA$Newcastle_upon_Tyne))
Newcastle_upon_TyneNew_Factor <- 10^(floor(log10(Newcastle_upon_TyneNew_Sigfig)))
Newcastle_upon_TyneNew_Max <- round_any(max(COVID19_by_LA$Newcastle_upon_Tyne), Newcastle_upon_TyneNew_Factor, f=ceiling)
Newcastle_upon_TyneNew_Breaks <- ceiling(Newcastle_upon_TyneNew_Factor/2)

plot_Newcastle_upon_TyneNew <- ggplot(data=Newcastle_upon_Tyne_data, aes(x=Newcastle_upon_Tyne_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Newcastle_upon_TyneNew_Max), 
                     breaks = seq(0, Newcastle_upon_TyneNew_Max, Newcastle_upon_TyneNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Newcastle upon Tyne") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Newcastle_upon_TyneCum_Sigfig <- signif(sum(COVID19_by_LA$Newcastle_upon_Tyne))
Newcastle_upon_TyneCum_Factor <- 10^(floor(log10(Newcastle_upon_TyneCum_Sigfig)))
Newcastle_upon_TyneCum_Max <- round_any(sum(COVID19_by_LA$Newcastle_upon_Tyne), Newcastle_upon_TyneCum_Factor, f=ceiling)
Newcastle_upon_TyneCum_Breaks <- ceiling(Newcastle_upon_TyneCum_Factor/2)

plot_Newcastle_upon_TyneCum <- ggplot(data=Newcastle_upon_Tyne_data, aes(x=Newcastle_upon_Tyne_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Newcastle_upon_TyneCum_Max), 
                     breaks = seq(0, Newcastle_upon_TyneCum_Max, Newcastle_upon_TyneCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Newcastle upon Tyne") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Newcastle_upon_Tyne_figure <- ggarrange(plot_Newcastle_upon_TyneNew + font("x.text", size = 8), 
                                        plot_Newcastle_upon_TyneCum + font("x.text", size = 8), 
                                        ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Newcastle_upon_Tyne_cases_plot.pdf", height = 8.27, width = 11.69)
Newcastle_upon_Tyne_figure
dev.off()



#### Newham plots ####
#Read in new cases per day from summarised data, format date correctly
Newham_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Newham))
Newham_data <- as.data.frame(lapply(Newham_data, rep, Newham_data$ntimes))


# Plot number of NEW CASES per day
NewhamNew_Sigfig <- signif(max(COVID19_by_LA$Newham))
NewhamNew_Factor <- 10^(floor(log10(NewhamNew_Sigfig)))
NewhamNew_Max <- round_any(max(COVID19_by_LA$Newham), NewhamNew_Factor, f=ceiling)
NewhamNew_Breaks <- ceiling(NewhamNew_Factor/2)

plot_NewhamNew <- ggplot(data=Newham_data, aes(x=Newham_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, NewhamNew_Max), 
                     breaks = seq(0, NewhamNew_Max, NewhamNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Newham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
NewhamCum_Sigfig <- signif(sum(COVID19_by_LA$Newham))
NewhamCum_Factor <- 10^(floor(log10(NewhamCum_Sigfig)))
NewhamCum_Max <- round_any(sum(COVID19_by_LA$Newham), NewhamCum_Factor, f=ceiling)
NewhamCum_Breaks <- ceiling(NewhamCum_Factor/2)

plot_NewhamCum <- ggplot(data=Newham_data, aes(x=Newham_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, NewhamCum_Max), 
                     breaks = seq(0, NewhamCum_Max, NewhamCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Newham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Newham_figure <- ggarrange(plot_NewhamNew + font("x.text", size = 8), 
                           plot_NewhamCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Newham_cases_plot.pdf", height = 8.27, width = 11.69)
Newham_figure
dev.off()



#### Norfolk plots ####
#Read in new cases per day from summarised data, format date correctly
Norfolk_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Norfolk))
Norfolk_data <- as.data.frame(lapply(Norfolk_data, rep, Norfolk_data$ntimes))


# Plot number of NEW CASES per day
NorfolkNew_Sigfig <- signif(max(COVID19_by_LA$Norfolk))
NorfolkNew_Factor <- 10^(floor(log10(NorfolkNew_Sigfig)))
NorfolkNew_Max <- round_any(max(COVID19_by_LA$Norfolk), NorfolkNew_Factor, f=ceiling)
NorfolkNew_Breaks <- ceiling(NorfolkNew_Factor/2)

plot_NorfolkNew <- ggplot(data=Norfolk_data, aes(x=Norfolk_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, NorfolkNew_Max), 
                     breaks = seq(0, NorfolkNew_Max, NorfolkNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Norfolk") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
NorfolkCum_Sigfig <- signif(sum(COVID19_by_LA$Norfolk))
NorfolkCum_Factor <- 10^(floor(log10(NorfolkCum_Sigfig)))
NorfolkCum_Max <- round_any(sum(COVID19_by_LA$Norfolk), NorfolkCum_Factor, f=ceiling)
NorfolkCum_Breaks <- ceiling(NorfolkCum_Factor/2)

plot_NorfolkCum <- ggplot(data=Norfolk_data, aes(x=Norfolk_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, NorfolkCum_Max), 
                     breaks = seq(0, NorfolkCum_Max, NorfolkCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Norfolk") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Norfolk_figure <- ggarrange(plot_NorfolkNew + font("x.text", size = 8), 
                            plot_NorfolkCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Norfolk_cases_plot.pdf", height = 8.27, width = 11.69)
Norfolk_figure
dev.off()



#### North_East_Lincolnshire plots ####
#Read in new cases per day from summarised data, format date correctly
North_East_Lincolnshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$North_East_Lincolnshire))
North_East_Lincolnshire_data <- as.data.frame(lapply(North_East_Lincolnshire_data, rep, North_East_Lincolnshire_data$ntimes))


# Plot number of NEW CASES per day
North_East_LincolnshireNew_Sigfig <- signif(max(COVID19_by_LA$North_East_Lincolnshire))
North_East_LincolnshireNew_Factor <- 10^(floor(log10(North_East_LincolnshireNew_Sigfig)))
North_East_LincolnshireNew_Max <- round_any(max(COVID19_by_LA$North_East_Lincolnshire), North_East_LincolnshireNew_Factor, f=ceiling)
North_East_LincolnshireNew_Breaks <- ceiling(North_East_LincolnshireNew_Factor/2)

plot_North_East_LincolnshireNew <- ggplot(data=North_East_Lincolnshire_data, aes(x=North_East_Lincolnshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, North_East_LincolnshireNew_Max), 
                     breaks = seq(0, North_East_LincolnshireNew_Max, North_East_LincolnshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: North East Lincolnshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
North_East_LincolnshireCum_Sigfig <- signif(sum(COVID19_by_LA$North_East_Lincolnshire))
North_East_LincolnshireCum_Factor <- 10^(floor(log10(North_East_LincolnshireCum_Sigfig)))
North_East_LincolnshireCum_Max <- round_any(sum(COVID19_by_LA$North_East_Lincolnshire), North_East_LincolnshireCum_Factor, f=ceiling)
North_East_LincolnshireCum_Breaks <- ceiling(North_East_LincolnshireCum_Factor/2)

plot_North_East_LincolnshireCum <- ggplot(data=North_East_Lincolnshire_data, aes(x=North_East_Lincolnshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, North_East_LincolnshireCum_Max), 
                     breaks = seq(0, North_East_LincolnshireCum_Max, North_East_LincolnshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: North East Lincolnshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
North_East_Lincolnshire_figure <- ggarrange(plot_North_East_LincolnshireNew + font("x.text", size = 8), 
                                            plot_North_East_LincolnshireCum + font("x.text", size = 8), 
                                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/North_East_Lincolnshire_cases_plot.pdf", height = 8.27, width = 11.69)
North_East_Lincolnshire_figure
dev.off()



#### North_Lincolnshire plots ####
#Read in new cases per day from summarised data, format date correctly
North_Lincolnshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$North_Lincolnshire))
North_Lincolnshire_data <- as.data.frame(lapply(North_Lincolnshire_data, rep, North_Lincolnshire_data$ntimes))


# Plot number of NEW CASES per day
North_LincolnshireNew_Sigfig <- signif(max(COVID19_by_LA$North_Lincolnshire))
North_LincolnshireNew_Factor <- 10^(floor(log10(North_LincolnshireNew_Sigfig)))
North_LincolnshireNew_Max <- round_any(max(COVID19_by_LA$North_Lincolnshire), North_LincolnshireNew_Factor, f=ceiling)
North_LincolnshireNew_Breaks <- ceiling(North_LincolnshireNew_Factor/2)

plot_North_LincolnshireNew <- ggplot(data=North_Lincolnshire_data, aes(x=North_Lincolnshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, North_LincolnshireNew_Max), 
                     breaks = seq(0, North_LincolnshireNew_Max, North_LincolnshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: North Lincolnshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
North_LincolnshireCum_Sigfig <- signif(sum(COVID19_by_LA$North_Lincolnshire))
North_LincolnshireCum_Factor <- 10^(floor(log10(North_LincolnshireCum_Sigfig)))
North_LincolnshireCum_Max <- round_any(sum(COVID19_by_LA$North_Lincolnshire), North_LincolnshireCum_Factor, f=ceiling)
North_LincolnshireCum_Breaks <- ceiling(North_LincolnshireCum_Factor/2)

plot_North_LincolnshireCum <- ggplot(data=North_Lincolnshire_data, aes(x=North_Lincolnshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, North_LincolnshireCum_Max), 
                     breaks = seq(0, North_LincolnshireCum_Max, North_LincolnshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: North Lincolnshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
North_Lincolnshire_figure <- ggarrange(plot_North_LincolnshireNew + font("x.text", size = 8), 
                                       plot_North_LincolnshireCum + font("x.text", size = 8), 
                                       ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/North_Lincolnshire_cases_plot.pdf", height = 8.27, width = 11.69)
North_Lincolnshire_figure
dev.off()



#### North_Somerset plots ####
#Read in new cases per day from summarised data, format date correctly
North_Somerset_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$North_Somerset))
North_Somerset_data <- as.data.frame(lapply(North_Somerset_data, rep, North_Somerset_data$ntimes))


# Plot number of NEW CASES per day
North_SomersetNew_Sigfig <- signif(max(COVID19_by_LA$North_Somerset))
North_SomersetNew_Factor <- 10^(floor(log10(North_SomersetNew_Sigfig)))
North_SomersetNew_Max <- round_any(max(COVID19_by_LA$North_Somerset), North_SomersetNew_Factor, f=ceiling)
North_SomersetNew_Breaks <- ceiling(North_SomersetNew_Factor/2)

plot_North_SomersetNew <- ggplot(data=North_Somerset_data, aes(x=North_Somerset_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, North_SomersetNew_Max), 
                     breaks = seq(0, North_SomersetNew_Max, North_SomersetNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: North Somerset") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
North_SomersetCum_Sigfig <- signif(sum(COVID19_by_LA$North_Somerset))
North_SomersetCum_Factor <- 10^(floor(log10(North_SomersetCum_Sigfig)))
North_SomersetCum_Max <- round_any(sum(COVID19_by_LA$North_Somerset), North_SomersetCum_Factor, f=ceiling)
North_SomersetCum_Breaks <- ceiling(North_SomersetCum_Factor/2)

plot_North_SomersetCum <- ggplot(data=North_Somerset_data, aes(x=North_Somerset_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, North_SomersetCum_Max), 
                     breaks = seq(0, North_SomersetCum_Max, North_SomersetCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: North Somerset") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
North_Somerset_figure <- ggarrange(plot_North_SomersetNew + font("x.text", size = 8), 
                                   plot_North_SomersetCum + font("x.text", size = 8), 
                                   ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/North_Somerset_cases_plot.pdf", height = 8.27, width = 11.69)
North_Somerset_figure
dev.off()



#### North_Tyneside plots ####
#Read in new cases per day from summarised data, format date correctly
North_Tyneside_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$North_Tyneside))
North_Tyneside_data <- as.data.frame(lapply(North_Tyneside_data, rep, North_Tyneside_data$ntimes))


# Plot number of NEW CASES per day
North_TynesideNew_Sigfig <- signif(max(COVID19_by_LA$North_Tyneside))
North_TynesideNew_Factor <- 10^(floor(log10(North_TynesideNew_Sigfig)))
North_TynesideNew_Max <- round_any(max(COVID19_by_LA$North_Tyneside), North_TynesideNew_Factor, f=ceiling)
North_TynesideNew_Breaks <- ceiling(North_TynesideNew_Factor/2)

plot_North_TynesideNew <- ggplot(data=North_Tyneside_data, aes(x=North_Tyneside_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, North_TynesideNew_Max), 
                     breaks = seq(0, North_TynesideNew_Max, North_TynesideNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: North Tyneside") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
North_TynesideCum_Sigfig <- signif(sum(COVID19_by_LA$North_Tyneside))
North_TynesideCum_Factor <- 10^(floor(log10(North_TynesideCum_Sigfig)))
North_TynesideCum_Max <- round_any(sum(COVID19_by_LA$North_Tyneside), North_TynesideCum_Factor, f=ceiling)
North_TynesideCum_Breaks <- ceiling(North_TynesideCum_Factor/2)

plot_North_TynesideCum <- ggplot(data=North_Tyneside_data, aes(x=North_Tyneside_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, North_TynesideCum_Max), 
                     breaks = seq(0, North_TynesideCum_Max, North_TynesideCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: North Tyneside") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
North_Tyneside_figure <- ggarrange(plot_North_TynesideNew + font("x.text", size = 8), 
                                   plot_North_TynesideCum + font("x.text", size = 8), 
                                   ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/North_Tyneside_cases_plot.pdf", height = 8.27, width = 11.69)
North_Tyneside_figure
dev.off()



#### North_Yorkshire plots ####
#Read in new cases per day from summarised data, format date correctly
North_Yorkshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$North_Yorkshire))
North_Yorkshire_data <- as.data.frame(lapply(North_Yorkshire_data, rep, North_Yorkshire_data$ntimes))


# Plot number of NEW CASES per day
North_YorkshireNew_Sigfig <- signif(max(COVID19_by_LA$North_Yorkshire))
North_YorkshireNew_Factor <- 10^(floor(log10(North_YorkshireNew_Sigfig)))
North_YorkshireNew_Max <- round_any(max(COVID19_by_LA$North_Yorkshire), North_YorkshireNew_Factor, f=ceiling)
North_YorkshireNew_Breaks <- ceiling(North_YorkshireNew_Factor/2)

plot_North_YorkshireNew <- ggplot(data=North_Yorkshire_data, aes(x=North_Yorkshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, North_YorkshireNew_Max), 
                     breaks = seq(0, North_YorkshireNew_Max, North_YorkshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: North Yorkshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
North_YorkshireCum_Sigfig <- signif(sum(COVID19_by_LA$North_Yorkshire))
North_YorkshireCum_Factor <- 10^(floor(log10(North_YorkshireCum_Sigfig)))
North_YorkshireCum_Max <- round_any(sum(COVID19_by_LA$North_Yorkshire), North_YorkshireCum_Factor, f=ceiling)
North_YorkshireCum_Breaks <- ceiling(North_YorkshireCum_Factor/2)

plot_North_YorkshireCum <- ggplot(data=North_Yorkshire_data, aes(x=North_Yorkshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, North_YorkshireCum_Max), 
                     breaks = seq(0, North_YorkshireCum_Max, North_YorkshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: North Yorkshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
North_Yorkshire_figure <- ggarrange(plot_North_YorkshireNew + font("x.text", size = 8), 
                                    plot_North_YorkshireCum + font("x.text", size = 8), 
                                    ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/North_Yorkshire_cases_plot.pdf", height = 8.27, width = 11.69)
North_Yorkshire_figure
dev.off()



#### Northamptonshire plots ####
#Read in new cases per day from summarised data, format date correctly
Northamptonshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Northamptonshire))
Northamptonshire_data <- as.data.frame(lapply(Northamptonshire_data, rep, Northamptonshire_data$ntimes))


# Plot number of NEW CASES per day
NorthamptonshireNew_Sigfig <- signif(max(COVID19_by_LA$Northamptonshire))
NorthamptonshireNew_Factor <- 10^(floor(log10(NorthamptonshireNew_Sigfig)))
NorthamptonshireNew_Max <- round_any(max(COVID19_by_LA$Northamptonshire), NorthamptonshireNew_Factor, f=ceiling)
NorthamptonshireNew_Breaks <- ceiling(NorthamptonshireNew_Factor/2)

plot_NorthamptonshireNew <- ggplot(data=Northamptonshire_data, aes(x=Northamptonshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, NorthamptonshireNew_Max), 
                     breaks = seq(0, NorthamptonshireNew_Max, NorthamptonshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Northamptonshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
NorthamptonshireCum_Sigfig <- signif(sum(COVID19_by_LA$Northamptonshire))
NorthamptonshireCum_Factor <- 10^(floor(log10(NorthamptonshireCum_Sigfig)))
NorthamptonshireCum_Max <- round_any(sum(COVID19_by_LA$Northamptonshire), NorthamptonshireCum_Factor, f=ceiling)
NorthamptonshireCum_Breaks <- ceiling(NorthamptonshireCum_Factor/2)

plot_NorthamptonshireCum <- ggplot(data=Northamptonshire_data, aes(x=Northamptonshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, NorthamptonshireCum_Max), 
                     breaks = seq(0, NorthamptonshireCum_Max, NorthamptonshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Northamptonshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Northamptonshire_figure <- ggarrange(plot_NorthamptonshireNew + font("x.text", size = 8), 
                                     plot_NorthamptonshireCum + font("x.text", size = 8), 
                                     ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Northamptonshire_cases_plot.pdf", height = 8.27, width = 11.69)
Northamptonshire_figure
dev.off()



#### Northumberland plots ####
#Read in new cases per day from summarised data, format date correctly
Northumberland_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Northumberland))
Northumberland_data <- as.data.frame(lapply(Northumberland_data, rep, Northumberland_data$ntimes))


# Plot number of NEW CASES per day
NorthumberlandNew_Sigfig <- signif(max(COVID19_by_LA$Northumberland))
NorthumberlandNew_Factor <- 10^(floor(log10(NorthumberlandNew_Sigfig)))
NorthumberlandNew_Max <- round_any(max(COVID19_by_LA$Northumberland), NorthumberlandNew_Factor, f=ceiling)
NorthumberlandNew_Breaks <- ceiling(NorthumberlandNew_Factor/2)

plot_NorthumberlandNew <- ggplot(data=Northumberland_data, aes(x=Northumberland_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, NorthumberlandNew_Max), 
                     breaks = seq(0, NorthumberlandNew_Max, NorthumberlandNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Northumberland") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
NorthumberlandCum_Sigfig <- signif(sum(COVID19_by_LA$Northumberland))
NorthumberlandCum_Factor <- 10^(floor(log10(NorthumberlandCum_Sigfig)))
NorthumberlandCum_Max <- round_any(sum(COVID19_by_LA$Northumberland), NorthumberlandCum_Factor, f=ceiling)
NorthumberlandCum_Breaks <- ceiling(NorthumberlandCum_Factor/2)

plot_NorthumberlandCum <- ggplot(data=Northumberland_data, aes(x=Northumberland_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, NorthumberlandCum_Max), 
                     breaks = seq(0, NorthumberlandCum_Max, NorthumberlandCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Northumberland") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Northumberland_figure <- ggarrange(plot_NorthumberlandNew + font("x.text", size = 8), 
                                   plot_NorthumberlandCum + font("x.text", size = 8), 
                                   ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Northumberland_cases_plot.pdf", height = 8.27, width = 11.69)
Northumberland_figure
dev.off()



#### Nottingham plots ####
#Read in new cases per day from summarised data, format date correctly
Nottingham_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Nottingham))
Nottingham_data <- as.data.frame(lapply(Nottingham_data, rep, Nottingham_data$ntimes))


# Plot number of NEW CASES per day
NottinghamNew_Sigfig <- signif(max(COVID19_by_LA$Nottingham))
NottinghamNew_Factor <- 10^(floor(log10(NottinghamNew_Sigfig)))
NottinghamNew_Max <- round_any(max(COVID19_by_LA$Nottingham), NottinghamNew_Factor, f=ceiling)
NottinghamNew_Breaks <- ceiling(NottinghamNew_Factor/2)

plot_NottinghamNew <- ggplot(data=Nottingham_data, aes(x=Nottingham_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, NottinghamNew_Max), 
                     breaks = seq(0, NottinghamNew_Max, NottinghamNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Nottingham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
NottinghamCum_Sigfig <- signif(sum(COVID19_by_LA$Nottingham))
NottinghamCum_Factor <- 10^(floor(log10(NottinghamCum_Sigfig)))
NottinghamCum_Max <- round_any(sum(COVID19_by_LA$Nottingham), NottinghamCum_Factor, f=ceiling)
NottinghamCum_Breaks <- ceiling(NottinghamCum_Factor/2)

plot_NottinghamCum <- ggplot(data=Nottingham_data, aes(x=Nottingham_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, NottinghamCum_Max), 
                     breaks = seq(0, NottinghamCum_Max, NottinghamCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Nottingham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Nottingham_figure <- ggarrange(plot_NottinghamNew + font("x.text", size = 8), 
                               plot_NottinghamCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Nottingham_cases_plot.pdf", height = 8.27, width = 11.69)
Nottingham_figure
dev.off()



#### Nottinghamshire plots ####
#Read in new cases per day from summarised data, format date correctly
Nottinghamshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Nottinghamshire))
Nottinghamshire_data <- as.data.frame(lapply(Nottinghamshire_data, rep, Nottinghamshire_data$ntimes))


# Plot number of NEW CASES per day
NottinghamshireNew_Sigfig <- signif(max(COVID19_by_LA$Nottinghamshire))
NottinghamshireNew_Factor <- 10^(floor(log10(NottinghamshireNew_Sigfig)))
NottinghamshireNew_Max <- round_any(max(COVID19_by_LA$Nottinghamshire), NottinghamshireNew_Factor, f=ceiling)
NottinghamshireNew_Breaks <- ceiling(NottinghamshireNew_Factor/2)

plot_NottinghamshireNew <- ggplot(data=Nottinghamshire_data, aes(x=Nottinghamshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, NottinghamshireNew_Max), 
                     breaks = seq(0, NottinghamshireNew_Max, NottinghamshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Nottinghamshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
NottinghamshireCum_Sigfig <- signif(sum(COVID19_by_LA$Nottinghamshire))
NottinghamshireCum_Factor <- 10^(floor(log10(NottinghamshireCum_Sigfig)))
NottinghamshireCum_Max <- round_any(sum(COVID19_by_LA$Nottinghamshire), NottinghamshireCum_Factor, f=ceiling)
NottinghamshireCum_Breaks <- ceiling(NottinghamshireCum_Factor/2)

plot_NottinghamshireCum <- ggplot(data=Nottinghamshire_data, aes(x=Nottinghamshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, NottinghamshireCum_Max), 
                     breaks = seq(0, NottinghamshireCum_Max, NottinghamshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Nottinghamshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Nottinghamshire_figure <- ggarrange(plot_NottinghamshireNew + font("x.text", size = 8), 
                                    plot_NottinghamshireCum + font("x.text", size = 8), 
                                    ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Nottinghamshire_cases_plot.pdf", height = 8.27, width = 11.69)
Nottinghamshire_figure
dev.off()



#### Oldham plots ####
#Read in new cases per day from summarised data, format date correctly
Oldham_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Oldham))
Oldham_data <- as.data.frame(lapply(Oldham_data, rep, Oldham_data$ntimes))


# Plot number of NEW CASES per day
OldhamNew_Sigfig <- signif(max(COVID19_by_LA$Oldham))
OldhamNew_Factor <- 10^(floor(log10(OldhamNew_Sigfig)))
OldhamNew_Max <- round_any(max(COVID19_by_LA$Oldham), OldhamNew_Factor, f=ceiling)
OldhamNew_Breaks <- ceiling(OldhamNew_Factor/2)

plot_OldhamNew <- ggplot(data=Oldham_data, aes(x=Oldham_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, OldhamNew_Max), 
                     breaks = seq(0, OldhamNew_Max, OldhamNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Oldham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
OldhamCum_Sigfig <- signif(sum(COVID19_by_LA$Oldham))
OldhamCum_Factor <- 10^(floor(log10(OldhamCum_Sigfig)))
OldhamCum_Max <- round_any(sum(COVID19_by_LA$Oldham), OldhamCum_Factor, f=ceiling)
OldhamCum_Breaks <- ceiling(OldhamCum_Factor/2)

plot_OldhamCum <- ggplot(data=Oldham_data, aes(x=Oldham_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, OldhamCum_Max), 
                     breaks = seq(0, OldhamCum_Max, OldhamCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Oldham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Oldham_figure <- ggarrange(plot_OldhamNew + font("x.text", size = 8), 
                           plot_OldhamCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Oldham_cases_plot.pdf", height = 8.27, width = 11.69)
Oldham_figure
dev.off()



#### Oxfordshire plots ####
#Read in new cases per day from summarised data, format date correctly
Oxfordshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Oxfordshire))
Oxfordshire_data <- as.data.frame(lapply(Oxfordshire_data, rep, Oxfordshire_data$ntimes))


# Plot number of NEW CASES per day
OxfordshireNew_Sigfig <- signif(max(COVID19_by_LA$Oxfordshire))
OxfordshireNew_Factor <- 10^(floor(log10(OxfordshireNew_Sigfig)))
OxfordshireNew_Max <- round_any(max(COVID19_by_LA$Oxfordshire), OxfordshireNew_Factor, f=ceiling)
OxfordshireNew_Breaks <- ceiling(OxfordshireNew_Factor/2)

plot_OxfordshireNew <- ggplot(data=Oxfordshire_data, aes(x=Oxfordshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, OxfordshireNew_Max), 
                     breaks = seq(0, OxfordshireNew_Max, OxfordshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Oxfordshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
OxfordshireCum_Sigfig <- signif(sum(COVID19_by_LA$Oxfordshire))
OxfordshireCum_Factor <- 10^(floor(log10(OxfordshireCum_Sigfig)))
OxfordshireCum_Max <- round_any(sum(COVID19_by_LA$Oxfordshire), OxfordshireCum_Factor, f=ceiling)
OxfordshireCum_Breaks <- ceiling(OxfordshireCum_Factor/2)

plot_OxfordshireCum <- ggplot(data=Oxfordshire_data, aes(x=Oxfordshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, OxfordshireCum_Max), 
                     breaks = seq(0, OxfordshireCum_Max, OxfordshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Oxfordshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Oxfordshire_figure <- ggarrange(plot_OxfordshireNew + font("x.text", size = 8), 
                                plot_OxfordshireCum + font("x.text", size = 8), 
                                ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Oxfordshire_cases_plot.pdf", height = 8.27, width = 11.69)
Oxfordshire_figure
dev.off()



#### Peterborough plots ####
#Read in new cases per day from summarised data, format date correctly
Peterborough_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Peterborough))
Peterborough_data <- as.data.frame(lapply(Peterborough_data, rep, Peterborough_data$ntimes))


# Plot number of NEW CASES per day
PeterboroughNew_Sigfig <- signif(max(COVID19_by_LA$Peterborough))
PeterboroughNew_Factor <- 10^(floor(log10(PeterboroughNew_Sigfig)))
PeterboroughNew_Max <- round_any(max(COVID19_by_LA$Peterborough), PeterboroughNew_Factor, f=ceiling)
PeterboroughNew_Breaks <- ceiling(PeterboroughNew_Factor/2)

plot_PeterboroughNew <- ggplot(data=Peterborough_data, aes(x=Peterborough_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, PeterboroughNew_Max), 
                     breaks = seq(0, PeterboroughNew_Max, PeterboroughNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Peterborough") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
PeterboroughCum_Sigfig <- signif(sum(COVID19_by_LA$Peterborough))
PeterboroughCum_Factor <- 10^(floor(log10(PeterboroughCum_Sigfig)))
PeterboroughCum_Max <- round_any(sum(COVID19_by_LA$Peterborough), PeterboroughCum_Factor, f=ceiling)
PeterboroughCum_Breaks <- ceiling(PeterboroughCum_Factor/2)

plot_PeterboroughCum <- ggplot(data=Peterborough_data, aes(x=Peterborough_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, PeterboroughCum_Max), 
                     breaks = seq(0, PeterboroughCum_Max, PeterboroughCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Peterborough") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Peterborough_figure <- ggarrange(plot_PeterboroughNew + font("x.text", size = 8), 
                                 plot_PeterboroughCum + font("x.text", size = 8), 
                                 ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Peterborough_cases_plot.pdf", height = 8.27, width = 11.69)
Peterborough_figure
dev.off()



#### Plymouth plots ####
#Read in new cases per day from summarised data, format date correctly
Plymouth_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Plymouth))
Plymouth_data <- as.data.frame(lapply(Plymouth_data, rep, Plymouth_data$ntimes))


# Plot number of NEW CASES per day
PlymouthNew_Sigfig <- signif(max(COVID19_by_LA$Plymouth))
PlymouthNew_Factor <- 10^(floor(log10(PlymouthNew_Sigfig)))
PlymouthNew_Max <- round_any(max(COVID19_by_LA$Plymouth), PlymouthNew_Factor, f=ceiling)
PlymouthNew_Breaks <- ceiling(PlymouthNew_Factor/2)

plot_PlymouthNew <- ggplot(data=Plymouth_data, aes(x=Plymouth_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, PlymouthNew_Max), 
                     breaks = seq(0, PlymouthNew_Max, PlymouthNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Plymouth") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
PlymouthCum_Sigfig <- signif(sum(COVID19_by_LA$Plymouth))
PlymouthCum_Factor <- 10^(floor(log10(PlymouthCum_Sigfig)))
PlymouthCum_Max <- round_any(sum(COVID19_by_LA$Plymouth), PlymouthCum_Factor, f=ceiling)
PlymouthCum_Breaks <- ceiling(PlymouthCum_Factor/2)

plot_PlymouthCum <- ggplot(data=Plymouth_data, aes(x=Plymouth_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, PlymouthCum_Max), 
                     breaks = seq(0, PlymouthCum_Max, PlymouthCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Plymouth") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Plymouth_figure <- ggarrange(plot_PlymouthNew + font("x.text", size = 8), 
                             plot_PlymouthCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Plymouth_cases_plot.pdf", height = 8.27, width = 11.69)
Plymouth_figure
dev.off()



#### Portsmouth plots ####
#Read in new cases per day from summarised data, format date correctly
Portsmouth_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Portsmouth))
Portsmouth_data <- as.data.frame(lapply(Portsmouth_data, rep, Portsmouth_data$ntimes))


# Plot number of NEW CASES per day
PortsmouthNew_Sigfig <- signif(max(COVID19_by_LA$Portsmouth))
PortsmouthNew_Factor <- 10^(floor(log10(PortsmouthNew_Sigfig)))
PortsmouthNew_Max <- round_any(max(COVID19_by_LA$Portsmouth), PortsmouthNew_Factor, f=ceiling)
PortsmouthNew_Breaks <- ceiling(PortsmouthNew_Factor/2)

plot_PortsmouthNew <- ggplot(data=Portsmouth_data, aes(x=Portsmouth_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, PortsmouthNew_Max), 
                     breaks = seq(0, PortsmouthNew_Max, PortsmouthNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Portsmouth") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
PortsmouthCum_Sigfig <- signif(sum(COVID19_by_LA$Portsmouth))
PortsmouthCum_Factor <- 10^(floor(log10(PortsmouthCum_Sigfig)))
PortsmouthCum_Max <- round_any(sum(COVID19_by_LA$Portsmouth), PortsmouthCum_Factor, f=ceiling)
PortsmouthCum_Breaks <- ceiling(PortsmouthCum_Factor/2)

plot_PortsmouthCum <- ggplot(data=Portsmouth_data, aes(x=Portsmouth_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, PortsmouthCum_Max), 
                     breaks = seq(0, PortsmouthCum_Max, PortsmouthCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Portsmouth") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Portsmouth_figure <- ggarrange(plot_PortsmouthNew + font("x.text", size = 8), 
                               plot_PortsmouthCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Portsmouth_cases_plot.pdf", height = 8.27, width = 11.69)
Portsmouth_figure
dev.off()



#### Reading plots ####
#Read in new cases per day from summarised data, format date correctly
Reading_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Reading))
Reading_data <- as.data.frame(lapply(Reading_data, rep, Reading_data$ntimes))


# Plot number of NEW CASES per day
ReadingNew_Sigfig <- signif(max(COVID19_by_LA$Reading))
ReadingNew_Factor <- 10^(floor(log10(ReadingNew_Sigfig)))
ReadingNew_Max <- round_any(max(COVID19_by_LA$Reading), ReadingNew_Factor, f=ceiling)
ReadingNew_Breaks <- ceiling(ReadingNew_Factor/2)

plot_ReadingNew <- ggplot(data=Reading_data, aes(x=Reading_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, ReadingNew_Max), 
                     breaks = seq(0, ReadingNew_Max, ReadingNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Reading") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
ReadingCum_Sigfig <- signif(sum(COVID19_by_LA$Reading))
ReadingCum_Factor <- 10^(floor(log10(ReadingCum_Sigfig)))
ReadingCum_Max <- round_any(sum(COVID19_by_LA$Reading), ReadingCum_Factor, f=ceiling)
ReadingCum_Breaks <- ceiling(ReadingCum_Factor/2)

plot_ReadingCum <- ggplot(data=Reading_data, aes(x=Reading_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, ReadingCum_Max), 
                     breaks = seq(0, ReadingCum_Max, ReadingCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Reading") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Reading_figure <- ggarrange(plot_ReadingNew + font("x.text", size = 8), 
                            plot_ReadingCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Reading_cases_plot.pdf", height = 8.27, width = 11.69)
Reading_figure
dev.off()



#### Redbridge plots ####
#Read in new cases per day from summarised data, format date correctly
Redbridge_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Redbridge))
Redbridge_data <- as.data.frame(lapply(Redbridge_data, rep, Redbridge_data$ntimes))


# Plot number of NEW CASES per day
RedbridgeNew_Sigfig <- signif(max(COVID19_by_LA$Redbridge))
RedbridgeNew_Factor <- 10^(floor(log10(RedbridgeNew_Sigfig)))
RedbridgeNew_Max <- round_any(max(COVID19_by_LA$Redbridge), RedbridgeNew_Factor, f=ceiling)
RedbridgeNew_Breaks <- ceiling(RedbridgeNew_Factor/2)

plot_RedbridgeNew <- ggplot(data=Redbridge_data, aes(x=Redbridge_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, RedbridgeNew_Max), 
                     breaks = seq(0, RedbridgeNew_Max, RedbridgeNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Redbridge") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
RedbridgeCum_Sigfig <- signif(sum(COVID19_by_LA$Redbridge))
RedbridgeCum_Factor <- 10^(floor(log10(RedbridgeCum_Sigfig)))
RedbridgeCum_Max <- round_any(sum(COVID19_by_LA$Redbridge), RedbridgeCum_Factor, f=ceiling)
RedbridgeCum_Breaks <- ceiling(RedbridgeCum_Factor/2)

plot_RedbridgeCum <- ggplot(data=Redbridge_data, aes(x=Redbridge_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, RedbridgeCum_Max), 
                     breaks = seq(0, RedbridgeCum_Max, RedbridgeCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Redbridge") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Redbridge_figure <- ggarrange(plot_RedbridgeNew + font("x.text", size = 8), 
                              plot_RedbridgeCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Redbridge_cases_plot.pdf", height = 8.27, width = 11.69)
Redbridge_figure
dev.off()



#### Redcar_and_Cleveland plots ####
#Read in new cases per day from summarised data, format date correctly
Redcar_and_Cleveland_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Redcar_and_Cleveland))
Redcar_and_Cleveland_data <- as.data.frame(lapply(Redcar_and_Cleveland_data, rep, Redcar_and_Cleveland_data$ntimes))


# Plot number of NEW CASES per day
Redcar_and_ClevelandNew_Sigfig <- signif(max(COVID19_by_LA$Redcar_and_Cleveland))
Redcar_and_ClevelandNew_Factor <- 10^(floor(log10(Redcar_and_ClevelandNew_Sigfig)))
Redcar_and_ClevelandNew_Max <- round_any(max(COVID19_by_LA$Redcar_and_Cleveland), Redcar_and_ClevelandNew_Factor, f=ceiling)
Redcar_and_ClevelandNew_Breaks <- ceiling(Redcar_and_ClevelandNew_Factor/2)

plot_Redcar_and_ClevelandNew <- ggplot(data=Redcar_and_Cleveland_data, aes(x=Redcar_and_Cleveland_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Redcar_and_ClevelandNew_Max), 
                     breaks = seq(0, Redcar_and_ClevelandNew_Max, Redcar_and_ClevelandNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Redcar and Cleveland") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Redcar_and_ClevelandCum_Sigfig <- signif(sum(COVID19_by_LA$Redcar_and_Cleveland))
Redcar_and_ClevelandCum_Factor <- 10^(floor(log10(Redcar_and_ClevelandCum_Sigfig)))
Redcar_and_ClevelandCum_Max <- round_any(sum(COVID19_by_LA$Redcar_and_Cleveland), Redcar_and_ClevelandCum_Factor, f=ceiling)
Redcar_and_ClevelandCum_Breaks <- ceiling(Redcar_and_ClevelandCum_Factor/2)

plot_Redcar_and_ClevelandCum <- ggplot(data=Redcar_and_Cleveland_data, aes(x=Redcar_and_Cleveland_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Redcar_and_ClevelandCum_Max), 
                     breaks = seq(0, Redcar_and_ClevelandCum_Max, Redcar_and_ClevelandCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Redcar and Cleveland") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Redcar_and_Cleveland_figure <- ggarrange(plot_Redcar_and_ClevelandNew + font("x.text", size = 8), 
                                         plot_Redcar_and_ClevelandCum + font("x.text", size = 8), 
                                         ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Redcar_and_Cleveland_cases_plot.pdf", height = 8.27, width = 11.69)
Redcar_and_Cleveland_figure
dev.off()



#### Richmond_upon_Thames plots ####
#Read in new cases per day from summarised data, format date correctly
Richmond_upon_Thames_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Richmond_upon_Thames))
Richmond_upon_Thames_data <- as.data.frame(lapply(Richmond_upon_Thames_data, rep, Richmond_upon_Thames_data$ntimes))


# Plot number of NEW CASES per day
Richmond_upon_ThamesNew_Sigfig <- signif(max(COVID19_by_LA$Richmond_upon_Thames))
Richmond_upon_ThamesNew_Factor <- 10^(floor(log10(Richmond_upon_ThamesNew_Sigfig)))
Richmond_upon_ThamesNew_Max <- round_any(max(COVID19_by_LA$Richmond_upon_Thames), Richmond_upon_ThamesNew_Factor, f=ceiling)
Richmond_upon_ThamesNew_Breaks <- ceiling(Richmond_upon_ThamesNew_Factor/2)

plot_Richmond_upon_ThamesNew <- ggplot(data=Richmond_upon_Thames_data, aes(x=Richmond_upon_Thames_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Richmond_upon_ThamesNew_Max), 
                     breaks = seq(0, Richmond_upon_ThamesNew_Max, Richmond_upon_ThamesNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Richmond upon Thames") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Richmond_upon_ThamesCum_Sigfig <- signif(sum(COVID19_by_LA$Richmond_upon_Thames))
Richmond_upon_ThamesCum_Factor <- 10^(floor(log10(Richmond_upon_ThamesCum_Sigfig)))
Richmond_upon_ThamesCum_Max <- round_any(sum(COVID19_by_LA$Richmond_upon_Thames), Richmond_upon_ThamesCum_Factor, f=ceiling)
Richmond_upon_ThamesCum_Breaks <- ceiling(Richmond_upon_ThamesCum_Factor/2)

plot_Richmond_upon_ThamesCum <- ggplot(data=Richmond_upon_Thames_data, aes(x=Richmond_upon_Thames_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Richmond_upon_ThamesCum_Max), 
                     breaks = seq(0, Richmond_upon_ThamesCum_Max, Richmond_upon_ThamesCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Richmond upon Thames") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Richmond_upon_Thames_figure <- ggarrange(plot_Richmond_upon_ThamesNew + font("x.text", size = 8), 
                                         plot_Richmond_upon_ThamesCum + font("x.text", size = 8), 
                                         ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Richmond_upon_Thames_cases_plot.pdf", height = 8.27, width = 11.69)
Richmond_upon_Thames_figure
dev.off()



#### Rochdale plots ####
#Read in new cases per day from summarised data, format date correctly
Rochdale_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Rochdale))
Rochdale_data <- as.data.frame(lapply(Rochdale_data, rep, Rochdale_data$ntimes))


# Plot number of NEW CASES per day
RochdaleNew_Sigfig <- signif(max(COVID19_by_LA$Rochdale))
RochdaleNew_Factor <- 10^(floor(log10(RochdaleNew_Sigfig)))
RochdaleNew_Max <- round_any(max(COVID19_by_LA$Rochdale), RochdaleNew_Factor, f=ceiling)
RochdaleNew_Breaks <- ceiling(RochdaleNew_Factor/2)

plot_RochdaleNew <- ggplot(data=Rochdale_data, aes(x=Rochdale_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, RochdaleNew_Max), 
                     breaks = seq(0, RochdaleNew_Max, RochdaleNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Rochdale") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
RochdaleCum_Sigfig <- signif(sum(COVID19_by_LA$Rochdale))
RochdaleCum_Factor <- 10^(floor(log10(RochdaleCum_Sigfig)))
RochdaleCum_Max <- round_any(sum(COVID19_by_LA$Rochdale), RochdaleCum_Factor, f=ceiling)
RochdaleCum_Breaks <- ceiling(RochdaleCum_Factor/2)

plot_RochdaleCum <- ggplot(data=Rochdale_data, aes(x=Rochdale_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, RochdaleCum_Max), 
                     breaks = seq(0, RochdaleCum_Max, RochdaleCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Rochdale") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Rochdale_figure <- ggarrange(plot_RochdaleNew + font("x.text", size = 8), 
                             plot_RochdaleCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Rochdale_cases_plot.pdf", height = 8.27, width = 11.69)
Rochdale_figure
dev.off()



#### Rotherham plots ####
#Read in new cases per day from summarised data, format date correctly
Rotherham_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Rotherham))
Rotherham_data <- as.data.frame(lapply(Rotherham_data, rep, Rotherham_data$ntimes))


# Plot number of NEW CASES per day
RotherhamNew_Sigfig <- signif(max(COVID19_by_LA$Rotherham))
RotherhamNew_Factor <- 10^(floor(log10(RotherhamNew_Sigfig)))
RotherhamNew_Max <- round_any(max(COVID19_by_LA$Rotherham), RotherhamNew_Factor, f=ceiling)
RotherhamNew_Breaks <- ceiling(RotherhamNew_Factor/2)

plot_RotherhamNew <- ggplot(data=Rotherham_data, aes(x=Rotherham_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, RotherhamNew_Max), 
                     breaks = seq(0, RotherhamNew_Max, RotherhamNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Rotherham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
RotherhamCum_Sigfig <- signif(sum(COVID19_by_LA$Rotherham))
RotherhamCum_Factor <- 10^(floor(log10(RotherhamCum_Sigfig)))
RotherhamCum_Max <- round_any(sum(COVID19_by_LA$Rotherham), RotherhamCum_Factor, f=ceiling)
RotherhamCum_Breaks <- ceiling(RotherhamCum_Factor/2)

plot_RotherhamCum <- ggplot(data=Rotherham_data, aes(x=Rotherham_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, RotherhamCum_Max), 
                     breaks = seq(0, RotherhamCum_Max, RotherhamCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Rotherham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Rotherham_figure <- ggarrange(plot_RotherhamNew + font("x.text", size = 8), 
                              plot_RotherhamCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Rotherham_cases_plot.pdf", height = 8.27, width = 11.69)
Rotherham_figure
dev.off()



#### Rutland plots ####
#Read in new cases per day from summarised data, format date correctly
Rutland_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Rutland))
Rutland_data <- as.data.frame(lapply(Rutland_data, rep, Rutland_data$ntimes))


# Plot number of NEW CASES per day
RutlandNew_Sigfig <- signif(max(COVID19_by_LA$Rutland))
RutlandNew_Factor <- 10^(floor(log10(RutlandNew_Sigfig)))
RutlandNew_Max <- round_any(max(COVID19_by_LA$Rutland), RutlandNew_Factor, f=ceiling)
RutlandNew_Breaks <- ceiling(RutlandNew_Factor/2)

plot_RutlandNew <- ggplot(data=Rutland_data, aes(x=Rutland_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, RutlandNew_Max), 
                     breaks = seq(0, RutlandNew_Max, RutlandNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Rutland") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
RutlandCum_Sigfig <- signif(sum(COVID19_by_LA$Rutland))
RutlandCum_Factor <- 10^(floor(log10(RutlandCum_Sigfig)))
RutlandCum_Max <- round_any(sum(COVID19_by_LA$Rutland), RutlandCum_Factor, f=ceiling)
RutlandCum_Breaks <- ceiling(RutlandCum_Factor/2)

plot_RutlandCum <- ggplot(data=Rutland_data, aes(x=Rutland_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, RutlandCum_Max), 
                     breaks = seq(0, RutlandCum_Max, RutlandCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Rutland") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Rutland_figure <- ggarrange(plot_RutlandNew + font("x.text", size = 8), 
                            plot_RutlandCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Rutland_cases_plot.pdf", height = 8.27, width = 11.69)
Rutland_figure
dev.off()



#### Salford plots ####
#Read in new cases per day from summarised data, format date correctly
Salford_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Salford))
Salford_data <- as.data.frame(lapply(Salford_data, rep, Salford_data$ntimes))


# Plot number of NEW CASES per day
SalfordNew_Sigfig <- signif(max(COVID19_by_LA$Salford))
SalfordNew_Factor <- 10^(floor(log10(SalfordNew_Sigfig)))
SalfordNew_Max <- round_any(max(COVID19_by_LA$Salford), SalfordNew_Factor, f=ceiling)
SalfordNew_Breaks <- ceiling(SalfordNew_Factor/2)

plot_SalfordNew <- ggplot(data=Salford_data, aes(x=Salford_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SalfordNew_Max), 
                     breaks = seq(0, SalfordNew_Max, SalfordNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Salford") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SalfordCum_Sigfig <- signif(sum(COVID19_by_LA$Salford))
SalfordCum_Factor <- 10^(floor(log10(SalfordCum_Sigfig)))
SalfordCum_Max <- round_any(sum(COVID19_by_LA$Salford), SalfordCum_Factor, f=ceiling)
SalfordCum_Breaks <- ceiling(SalfordCum_Factor/2)

plot_SalfordCum <- ggplot(data=Salford_data, aes(x=Salford_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SalfordCum_Max), 
                     breaks = seq(0, SalfordCum_Max, SalfordCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Salford") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Salford_figure <- ggarrange(plot_SalfordNew + font("x.text", size = 8), 
                            plot_SalfordCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Salford_cases_plot.pdf", height = 8.27, width = 11.69)
Salford_figure
dev.off()



#### Sandwell plots ####
#Read in new cases per day from summarised data, format date correctly
Sandwell_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Sandwell))
Sandwell_data <- as.data.frame(lapply(Sandwell_data, rep, Sandwell_data$ntimes))


# Plot number of NEW CASES per day
SandwellNew_Sigfig <- signif(max(COVID19_by_LA$Sandwell))
SandwellNew_Factor <- 10^(floor(log10(SandwellNew_Sigfig)))
SandwellNew_Max <- round_any(max(COVID19_by_LA$Sandwell), SandwellNew_Factor, f=ceiling)
SandwellNew_Breaks <- ceiling(SandwellNew_Factor/2)

plot_SandwellNew <- ggplot(data=Sandwell_data, aes(x=Sandwell_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SandwellNew_Max), 
                     breaks = seq(0, SandwellNew_Max, SandwellNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Sandwell") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SandwellCum_Sigfig <- signif(sum(COVID19_by_LA$Sandwell))
SandwellCum_Factor <- 10^(floor(log10(SandwellCum_Sigfig)))
SandwellCum_Max <- round_any(sum(COVID19_by_LA$Sandwell), SandwellCum_Factor, f=ceiling)
SandwellCum_Breaks <- ceiling(SandwellCum_Factor/2)

plot_SandwellCum <- ggplot(data=Sandwell_data, aes(x=Sandwell_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SandwellCum_Max), 
                     breaks = seq(0, SandwellCum_Max, SandwellCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Sandwell") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Sandwell_figure <- ggarrange(plot_SandwellNew + font("x.text", size = 8), 
                             plot_SandwellCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Sandwell_cases_plot.pdf", height = 8.27, width = 11.69)
Sandwell_figure
dev.off()



#### Sefton plots ####
#Read in new cases per day from summarised data, format date correctly
Sefton_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Sefton))
Sefton_data <- as.data.frame(lapply(Sefton_data, rep, Sefton_data$ntimes))


# Plot number of NEW CASES per day
SeftonNew_Sigfig <- signif(max(COVID19_by_LA$Sefton))
SeftonNew_Factor <- 10^(floor(log10(SeftonNew_Sigfig)))
SeftonNew_Max <- round_any(max(COVID19_by_LA$Sefton), SeftonNew_Factor, f=ceiling)
SeftonNew_Breaks <- ceiling(SeftonNew_Factor/2)

plot_SeftonNew <- ggplot(data=Sefton_data, aes(x=Sefton_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SeftonNew_Max), 
                     breaks = seq(0, SeftonNew_Max, SeftonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Sefton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SeftonCum_Sigfig <- signif(sum(COVID19_by_LA$Sefton))
SeftonCum_Factor <- 10^(floor(log10(SeftonCum_Sigfig)))
SeftonCum_Max <- round_any(sum(COVID19_by_LA$Sefton), SeftonCum_Factor, f=ceiling)
SeftonCum_Breaks <- ceiling(SeftonCum_Factor/2)

plot_SeftonCum <- ggplot(data=Sefton_data, aes(x=Sefton_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SeftonCum_Max), 
                     breaks = seq(0, SeftonCum_Max, SeftonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Sefton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Sefton_figure <- ggarrange(plot_SeftonNew + font("x.text", size = 8), 
                           plot_SeftonCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Sefton_cases_plot.pdf", height = 8.27, width = 11.69)
Sefton_figure
dev.off()



#### Sheffield plots ####
#Read in new cases per day from summarised data, format date correctly
Sheffield_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Sheffield))
Sheffield_data <- as.data.frame(lapply(Sheffield_data, rep, Sheffield_data$ntimes))


# Plot number of NEW CASES per day
SheffieldNew_Sigfig <- signif(max(COVID19_by_LA$Sheffield))
SheffieldNew_Factor <- 10^(floor(log10(SheffieldNew_Sigfig)))
SheffieldNew_Max <- round_any(max(COVID19_by_LA$Sheffield), SheffieldNew_Factor, f=ceiling)
SheffieldNew_Breaks <- ceiling(SheffieldNew_Factor/2)

plot_SheffieldNew <- ggplot(data=Sheffield_data, aes(x=Sheffield_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SheffieldNew_Max), 
                     breaks = seq(0, SheffieldNew_Max, SheffieldNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Sheffield") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SheffieldCum_Sigfig <- signif(sum(COVID19_by_LA$Sheffield))
SheffieldCum_Factor <- 10^(floor(log10(SheffieldCum_Sigfig)))
SheffieldCum_Max <- round_any(sum(COVID19_by_LA$Sheffield), SheffieldCum_Factor, f=ceiling)
SheffieldCum_Breaks <- ceiling(SheffieldCum_Factor/2)

plot_SheffieldCum <- ggplot(data=Sheffield_data, aes(x=Sheffield_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SheffieldCum_Max), 
                     breaks = seq(0, SheffieldCum_Max, SheffieldCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Sheffield") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Sheffield_figure <- ggarrange(plot_SheffieldNew + font("x.text", size = 8), 
                              plot_SheffieldCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Sheffield_cases_plot.pdf", height = 8.27, width = 11.69)
Sheffield_figure
dev.off()



#### Shropshire plots ####
#Read in new cases per day from summarised data, format date correctly
Shropshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Shropshire))
Shropshire_data <- as.data.frame(lapply(Shropshire_data, rep, Shropshire_data$ntimes))


# Plot number of NEW CASES per day
ShropshireNew_Sigfig <- signif(max(COVID19_by_LA$Shropshire))
ShropshireNew_Factor <- 10^(floor(log10(ShropshireNew_Sigfig)))
ShropshireNew_Max <- round_any(max(COVID19_by_LA$Shropshire), ShropshireNew_Factor, f=ceiling)
ShropshireNew_Breaks <- ceiling(ShropshireNew_Factor/2)

plot_ShropshireNew <- ggplot(data=Shropshire_data, aes(x=Shropshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, ShropshireNew_Max), 
                     breaks = seq(0, ShropshireNew_Max, ShropshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Shropshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
ShropshireCum_Sigfig <- signif(sum(COVID19_by_LA$Shropshire))
ShropshireCum_Factor <- 10^(floor(log10(ShropshireCum_Sigfig)))
ShropshireCum_Max <- round_any(sum(COVID19_by_LA$Shropshire), ShropshireCum_Factor, f=ceiling)
ShropshireCum_Breaks <- ceiling(ShropshireCum_Factor/2)

plot_ShropshireCum <- ggplot(data=Shropshire_data, aes(x=Shropshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, ShropshireCum_Max), 
                     breaks = seq(0, ShropshireCum_Max, ShropshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Shropshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Shropshire_figure <- ggarrange(plot_ShropshireNew + font("x.text", size = 8), 
                               plot_ShropshireCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Shropshire_cases_plot.pdf", height = 8.27, width = 11.69)
Shropshire_figure
dev.off()



#### Slough plots ####
#Read in new cases per day from summarised data, format date correctly
Slough_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Slough))
Slough_data <- as.data.frame(lapply(Slough_data, rep, Slough_data$ntimes))


# Plot number of NEW CASES per day
SloughNew_Sigfig <- signif(max(COVID19_by_LA$Slough))
SloughNew_Factor <- 10^(floor(log10(SloughNew_Sigfig)))
SloughNew_Max <- round_any(max(COVID19_by_LA$Slough), SloughNew_Factor, f=ceiling)
SloughNew_Breaks <- ceiling(SloughNew_Factor/2)

plot_SloughNew <- ggplot(data=Slough_data, aes(x=Slough_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SloughNew_Max), 
                     breaks = seq(0, SloughNew_Max, SloughNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Slough") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SloughCum_Sigfig <- signif(sum(COVID19_by_LA$Slough))
SloughCum_Factor <- 10^(floor(log10(SloughCum_Sigfig)))
SloughCum_Max <- round_any(sum(COVID19_by_LA$Slough), SloughCum_Factor, f=ceiling)
SloughCum_Breaks <- ceiling(SloughCum_Factor/2)

plot_SloughCum <- ggplot(data=Slough_data, aes(x=Slough_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SloughCum_Max), 
                     breaks = seq(0, SloughCum_Max, SloughCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Slough") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Slough_figure <- ggarrange(plot_SloughNew + font("x.text", size = 8), 
                           plot_SloughCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Slough_cases_plot.pdf", height = 8.27, width = 11.69)
Slough_figure
dev.off()



#### Solihull plots ####
#Read in new cases per day from summarised data, format date correctly
Solihull_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Solihull))
Solihull_data <- as.data.frame(lapply(Solihull_data, rep, Solihull_data$ntimes))


# Plot number of NEW CASES per day
SolihullNew_Sigfig <- signif(max(COVID19_by_LA$Solihull))
SolihullNew_Factor <- 10^(floor(log10(SolihullNew_Sigfig)))
SolihullNew_Max <- round_any(max(COVID19_by_LA$Solihull), SolihullNew_Factor, f=ceiling)
SolihullNew_Breaks <- ceiling(SolihullNew_Factor/2)

plot_SolihullNew <- ggplot(data=Solihull_data, aes(x=Solihull_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SolihullNew_Max), 
                     breaks = seq(0, SolihullNew_Max, SolihullNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Solihull") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SolihullCum_Sigfig <- signif(sum(COVID19_by_LA$Solihull))
SolihullCum_Factor <- 10^(floor(log10(SolihullCum_Sigfig)))
SolihullCum_Max <- round_any(sum(COVID19_by_LA$Solihull), SolihullCum_Factor, f=ceiling)
SolihullCum_Breaks <- ceiling(SolihullCum_Factor/2)

plot_SolihullCum <- ggplot(data=Solihull_data, aes(x=Solihull_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SolihullCum_Max), 
                     breaks = seq(0, SolihullCum_Max, SolihullCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Solihull") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Solihull_figure <- ggarrange(plot_SolihullNew + font("x.text", size = 8), 
                             plot_SolihullCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Solihull_cases_plot.pdf", height = 8.27, width = 11.69)
Solihull_figure
dev.off()



#### Somerset plots ####
#Read in new cases per day from summarised data, format date correctly
Somerset_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Somerset))
Somerset_data <- as.data.frame(lapply(Somerset_data, rep, Somerset_data$ntimes))


# Plot number of NEW CASES per day
SomersetNew_Sigfig <- signif(max(COVID19_by_LA$Somerset))
SomersetNew_Factor <- 10^(floor(log10(SomersetNew_Sigfig)))
SomersetNew_Max <- round_any(max(COVID19_by_LA$Somerset), SomersetNew_Factor, f=ceiling)
SomersetNew_Breaks <- ceiling(SomersetNew_Factor/2)

plot_SomersetNew <- ggplot(data=Somerset_data, aes(x=Somerset_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SomersetNew_Max), 
                     breaks = seq(0, SomersetNew_Max, SomersetNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Somerset") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SomersetCum_Sigfig <- signif(sum(COVID19_by_LA$Somerset))
SomersetCum_Factor <- 10^(floor(log10(SomersetCum_Sigfig)))
SomersetCum_Max <- round_any(sum(COVID19_by_LA$Somerset), SomersetCum_Factor, f=ceiling)
SomersetCum_Breaks <- ceiling(SomersetCum_Factor/2)

plot_SomersetCum <- ggplot(data=Somerset_data, aes(x=Somerset_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SomersetCum_Max), 
                     breaks = seq(0, SomersetCum_Max, SomersetCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Somerset") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Somerset_figure <- ggarrange(plot_SomersetNew + font("x.text", size = 8), 
                             plot_SomersetCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Somerset_cases_plot.pdf", height = 8.27, width = 11.69)
Somerset_figure
dev.off()



#### South_Gloucestershire plots ####
#Read in new cases per day from summarised data, format date correctly
South_Gloucestershire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$South_Gloucestershire))
South_Gloucestershire_data <- as.data.frame(lapply(South_Gloucestershire_data, rep, South_Gloucestershire_data$ntimes))


# Plot number of NEW CASES per day
South_GloucestershireNew_Sigfig <- signif(max(COVID19_by_LA$South_Gloucestershire))
South_GloucestershireNew_Factor <- 10^(floor(log10(South_GloucestershireNew_Sigfig)))
South_GloucestershireNew_Max <- round_any(max(COVID19_by_LA$South_Gloucestershire), South_GloucestershireNew_Factor, f=ceiling)
South_GloucestershireNew_Breaks <- ceiling(South_GloucestershireNew_Factor/2)

plot_South_GloucestershireNew <- ggplot(data=South_Gloucestershire_data, aes(x=South_Gloucestershire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, South_GloucestershireNew_Max), 
                     breaks = seq(0, South_GloucestershireNew_Max, South_GloucestershireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: South Gloucestershire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
South_GloucestershireCum_Sigfig <- signif(sum(COVID19_by_LA$South_Gloucestershire))
South_GloucestershireCum_Factor <- 10^(floor(log10(South_GloucestershireCum_Sigfig)))
South_GloucestershireCum_Max <- round_any(sum(COVID19_by_LA$South_Gloucestershire), South_GloucestershireCum_Factor, f=ceiling)
South_GloucestershireCum_Breaks <- ceiling(South_GloucestershireCum_Factor/2)

plot_South_GloucestershireCum <- ggplot(data=South_Gloucestershire_data, aes(x=South_Gloucestershire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, South_GloucestershireCum_Max), 
                     breaks = seq(0, South_GloucestershireCum_Max, South_GloucestershireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: South Gloucestershire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
South_Gloucestershire_figure <- ggarrange(plot_South_GloucestershireNew + font("x.text", size = 8), 
                                          plot_South_GloucestershireCum + font("x.text", size = 8), 
                                          ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/South_Gloucestershire_cases_plot.pdf", height = 8.27, width = 11.69)
South_Gloucestershire_figure
dev.off()



#### South_Tyneside plots ####
#Read in new cases per day from summarised data, format date correctly
South_Tyneside_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$South_Tyneside))
South_Tyneside_data <- as.data.frame(lapply(South_Tyneside_data, rep, South_Tyneside_data$ntimes))


# Plot number of NEW CASES per day
South_TynesideNew_Sigfig <- signif(max(COVID19_by_LA$South_Tyneside))
South_TynesideNew_Factor <- 10^(floor(log10(South_TynesideNew_Sigfig)))
South_TynesideNew_Max <- round_any(max(COVID19_by_LA$South_Tyneside), South_TynesideNew_Factor, f=ceiling)
South_TynesideNew_Breaks <- ceiling(South_TynesideNew_Factor/2)

plot_South_TynesideNew <- ggplot(data=South_Tyneside_data, aes(x=South_Tyneside_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, South_TynesideNew_Max), 
                     breaks = seq(0, South_TynesideNew_Max, South_TynesideNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: South Tyneside") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
South_TynesideCum_Sigfig <- signif(sum(COVID19_by_LA$South_Tyneside))
South_TynesideCum_Factor <- 10^(floor(log10(South_TynesideCum_Sigfig)))
South_TynesideCum_Max <- round_any(sum(COVID19_by_LA$South_Tyneside), South_TynesideCum_Factor, f=ceiling)
South_TynesideCum_Breaks <- ceiling(South_TynesideCum_Factor/2)

plot_South_TynesideCum <- ggplot(data=South_Tyneside_data, aes(x=South_Tyneside_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, South_TynesideCum_Max), 
                     breaks = seq(0, South_TynesideCum_Max, South_TynesideCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: South Tyneside") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
South_Tyneside_figure <- ggarrange(plot_South_TynesideNew + font("x.text", size = 8), 
                                   plot_South_TynesideCum + font("x.text", size = 8), 
                                   ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/South_Tyneside_cases_plot.pdf", height = 8.27, width = 11.69)
South_Tyneside_figure
dev.off()



#### Southampton plots ####
#Read in new cases per day from summarised data, format date correctly
Southampton_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Southampton))
Southampton_data <- as.data.frame(lapply(Southampton_data, rep, Southampton_data$ntimes))


# Plot number of NEW CASES per day
SouthamptonNew_Sigfig <- signif(max(COVID19_by_LA$Southampton))
SouthamptonNew_Factor <- 10^(floor(log10(SouthamptonNew_Sigfig)))
SouthamptonNew_Max <- round_any(max(COVID19_by_LA$Southampton), SouthamptonNew_Factor, f=ceiling)
SouthamptonNew_Breaks <- ceiling(SouthamptonNew_Factor/2)

plot_SouthamptonNew <- ggplot(data=Southampton_data, aes(x=Southampton_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SouthamptonNew_Max), 
                     breaks = seq(0, SouthamptonNew_Max, SouthamptonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Southampton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SouthamptonCum_Sigfig <- signif(sum(COVID19_by_LA$Southampton))
SouthamptonCum_Factor <- 10^(floor(log10(SouthamptonCum_Sigfig)))
SouthamptonCum_Max <- round_any(sum(COVID19_by_LA$Southampton), SouthamptonCum_Factor, f=ceiling)
SouthamptonCum_Breaks <- ceiling(SouthamptonCum_Factor/2)

plot_SouthamptonCum <- ggplot(data=Southampton_data, aes(x=Southampton_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SouthamptonCum_Max), 
                     breaks = seq(0, SouthamptonCum_Max, SouthamptonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Southampton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Southampton_figure <- ggarrange(plot_SouthamptonNew + font("x.text", size = 8), 
                                plot_SouthamptonCum + font("x.text", size = 8), 
                                ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Southampton_cases_plot.pdf", height = 8.27, width = 11.69)
Southampton_figure
dev.off()



#### Southend-on-Sea plots ####
#Read in new cases per day from summarised data, format date correctly
Southend-on-Sea_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Southend-on-Sea))
Southend-on-Sea_data <- as.data.frame(lapply(Southend-on-Sea_data, rep, Southend-on-Sea_data$ntimes))


# Plot number of NEW CASES per day
Southend-on-SeaNew_Sigfig <- signif(max(COVID19_by_LA$Southend-on-Sea))
Southend-on-SeaNew_Factor <- 10^(floor(log10(Southend-on-SeaNew_Sigfig)))
Southend-on-SeaNew_Max <- round_any(max(COVID19_by_LA$Southend-on-Sea), Southend-on-SeaNew_Factor, f=ceiling)
Southend-on-SeaNew_Breaks <- ceiling(Southend-on-SeaNew_Factor/2)

plot_Southend-on-SeaNew <- ggplot(data=Southend-on-Sea_data, aes(x=Southend-on-Sea_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Southend-on-SeaNew_Max), 
                     breaks = seq(0, Southend-on-SeaNew_Max, Southend-on-SeaNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Southend-on-Sea") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Southend-on-SeaCum_Sigfig <- signif(sum(COVID19_by_LA$Southend-on-Sea))
Southend-on-SeaCum_Factor <- 10^(floor(log10(Southend-on-SeaCum_Sigfig)))
Southend-on-SeaCum_Max <- round_any(sum(COVID19_by_LA$Southend-on-Sea), Southend-on-SeaCum_Factor, f=ceiling)
Southend-on-SeaCum_Breaks <- ceiling(Southend-on-SeaCum_Factor/2)

plot_Southend-on-SeaCum <- ggplot(data=Southend-on-Sea_data, aes(x=Southend-on-Sea_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Southend-on-SeaCum_Max), 
                     breaks = seq(0, Southend-on-SeaCum_Max, Southend-on-SeaCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Southend-on-Sea") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Southend-on-Sea_figure <- ggarrange(plot_Southend-on-SeaNew + font("x.text", size = 8), 
                                    plot_Southend-on-SeaCum + font("x.text", size = 8), 
                                    ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Southend-on-Sea_cases_plot.pdf", height = 8.27, width = 11.69)
Southend-on-Sea_figure
dev.off()



#### Southwark plots ####
#Read in new cases per day from summarised data, format date correctly
Southwark_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Southwark))
Southwark_data <- as.data.frame(lapply(Southwark_data, rep, Southwark_data$ntimes))


# Plot number of NEW CASES per day
SouthwarkNew_Sigfig <- signif(max(COVID19_by_LA$Southwark))
SouthwarkNew_Factor <- 10^(floor(log10(SouthwarkNew_Sigfig)))
SouthwarkNew_Max <- round_any(max(COVID19_by_LA$Southwark), SouthwarkNew_Factor, f=ceiling)
SouthwarkNew_Breaks <- ceiling(SouthwarkNew_Factor/2)

plot_SouthwarkNew <- ggplot(data=Southwark_data, aes(x=Southwark_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SouthwarkNew_Max), 
                     breaks = seq(0, SouthwarkNew_Max, SouthwarkNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Southwark") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SouthwarkCum_Sigfig <- signif(sum(COVID19_by_LA$Southwark))
SouthwarkCum_Factor <- 10^(floor(log10(SouthwarkCum_Sigfig)))
SouthwarkCum_Max <- round_any(sum(COVID19_by_LA$Southwark), SouthwarkCum_Factor, f=ceiling)
SouthwarkCum_Breaks <- ceiling(SouthwarkCum_Factor/2)

plot_SouthwarkCum <- ggplot(data=Southwark_data, aes(x=Southwark_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SouthwarkCum_Max), 
                     breaks = seq(0, SouthwarkCum_Max, SouthwarkCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Southwark") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Southwark_figure <- ggarrange(plot_SouthwarkNew + font("x.text", size = 8), 
                              plot_SouthwarkCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Southwark_cases_plot.pdf", height = 8.27, width = 11.69)
Southwark_figure
dev.off()



#### St_Helens plots ####
#Read in new cases per day from summarised data, format date correctly
St_Helens_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$St_Helens))
St_Helens_data <- as.data.frame(lapply(St_Helens_data, rep, St_Helens_data$ntimes))


# Plot number of NEW CASES per day
St_HelensNew_Sigfig <- signif(max(COVID19_by_LA$St_Helens))
St_HelensNew_Factor <- 10^(floor(log10(St_HelensNew_Sigfig)))
St_HelensNew_Max <- round_any(max(COVID19_by_LA$St_Helens), St_HelensNew_Factor, f=ceiling)
St_HelensNew_Breaks <- ceiling(St_HelensNew_Factor/2)

plot_St_HelensNew <- ggplot(data=St_Helens_data, aes(x=St_Helens_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, St_HelensNew_Max), 
                     breaks = seq(0, St_HelensNew_Max, St_HelensNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: St Helens") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
St_HelensCum_Sigfig <- signif(sum(COVID19_by_LA$St_Helens))
St_HelensCum_Factor <- 10^(floor(log10(St_HelensCum_Sigfig)))
St_HelensCum_Max <- round_any(sum(COVID19_by_LA$St_Helens), St_HelensCum_Factor, f=ceiling)
St_HelensCum_Breaks <- ceiling(St_HelensCum_Factor/2)

plot_St_HelensCum <- ggplot(data=St_Helens_data, aes(x=St_Helens_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, St_HelensCum_Max), 
                     breaks = seq(0, St_HelensCum_Max, St_HelensCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: St Helens") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
St_Helens_figure <- ggarrange(plot_St_HelensNew + font("x.text", size = 8), 
                              plot_St_HelensCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/St_Helens_cases_plot.pdf", height = 8.27, width = 11.69)
St_Helens_figure
dev.off()



#### Staffordshire plots ####
#Read in new cases per day from summarised data, format date correctly
Staffordshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Staffordshire))
Staffordshire_data <- as.data.frame(lapply(Staffordshire_data, rep, Staffordshire_data$ntimes))


# Plot number of NEW CASES per day
StaffordshireNew_Sigfig <- signif(max(COVID19_by_LA$Staffordshire))
StaffordshireNew_Factor <- 10^(floor(log10(StaffordshireNew_Sigfig)))
StaffordshireNew_Max <- round_any(max(COVID19_by_LA$Staffordshire), StaffordshireNew_Factor, f=ceiling)
StaffordshireNew_Breaks <- ceiling(StaffordshireNew_Factor/2)

plot_StaffordshireNew <- ggplot(data=Staffordshire_data, aes(x=Staffordshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, StaffordshireNew_Max), 
                     breaks = seq(0, StaffordshireNew_Max, StaffordshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Staffordshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
StaffordshireCum_Sigfig <- signif(sum(COVID19_by_LA$Staffordshire))
StaffordshireCum_Factor <- 10^(floor(log10(StaffordshireCum_Sigfig)))
StaffordshireCum_Max <- round_any(sum(COVID19_by_LA$Staffordshire), StaffordshireCum_Factor, f=ceiling)
StaffordshireCum_Breaks <- ceiling(StaffordshireCum_Factor/2)

plot_StaffordshireCum <- ggplot(data=Staffordshire_data, aes(x=Staffordshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, StaffordshireCum_Max), 
                     breaks = seq(0, StaffordshireCum_Max, StaffordshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Staffordshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Staffordshire_figure <- ggarrange(plot_StaffordshireNew + font("x.text", size = 8), 
                                  plot_StaffordshireCum + font("x.text", size = 8), 
                                  ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Staffordshire_cases_plot.pdf", height = 8.27, width = 11.69)
Staffordshire_figure
dev.off()



#### Stockport plots ####
#Read in new cases per day from summarised data, format date correctly
Stockport_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Stockport))
Stockport_data <- as.data.frame(lapply(Stockport_data, rep, Stockport_data$ntimes))


# Plot number of NEW CASES per day
StockportNew_Sigfig <- signif(max(COVID19_by_LA$Stockport))
StockportNew_Factor <- 10^(floor(log10(StockportNew_Sigfig)))
StockportNew_Max <- round_any(max(COVID19_by_LA$Stockport), StockportNew_Factor, f=ceiling)
StockportNew_Breaks <- ceiling(StockportNew_Factor/2)

plot_StockportNew <- ggplot(data=Stockport_data, aes(x=Stockport_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, StockportNew_Max), 
                     breaks = seq(0, StockportNew_Max, StockportNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Stockport") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
StockportCum_Sigfig <- signif(sum(COVID19_by_LA$Stockport))
StockportCum_Factor <- 10^(floor(log10(StockportCum_Sigfig)))
StockportCum_Max <- round_any(sum(COVID19_by_LA$Stockport), StockportCum_Factor, f=ceiling)
StockportCum_Breaks <- ceiling(StockportCum_Factor/2)

plot_StockportCum <- ggplot(data=Stockport_data, aes(x=Stockport_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, StockportCum_Max), 
                     breaks = seq(0, StockportCum_Max, StockportCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Stockport") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Stockport_figure <- ggarrange(plot_StockportNew + font("x.text", size = 8), 
                              plot_StockportCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Stockport_cases_plot.pdf", height = 8.27, width = 11.69)
Stockport_figure
dev.off()



#### Stockton-on-Tees plots ####
#Read in new cases per day from summarised data, format date correctly
Stockton-on-Tees_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Stockton-on-Tees))
Stockton-on-Tees_data <- as.data.frame(lapply(Stockton-on-Tees_data, rep, Stockton-on-Tees_data$ntimes))


# Plot number of NEW CASES per day
Stockton-on-TeesNew_Sigfig <- signif(max(COVID19_by_LA$Stockton-on-Tees))
Stockton-on-TeesNew_Factor <- 10^(floor(log10(Stockton-on-TeesNew_Sigfig)))
Stockton-on-TeesNew_Max <- round_any(max(COVID19_by_LA$Stockton-on-Tees), Stockton-on-TeesNew_Factor, f=ceiling)
Stockton-on-TeesNew_Breaks <- ceiling(Stockton-on-TeesNew_Factor/2)

plot_Stockton-on-TeesNew <- ggplot(data=Stockton-on-Tees_data, aes(x=Stockton-on-Tees_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Stockton-on-TeesNew_Max), 
                     breaks = seq(0, Stockton-on-TeesNew_Max, Stockton-on-TeesNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Stockton-on-Tees") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Stockton-on-TeesCum_Sigfig <- signif(sum(COVID19_by_LA$Stockton-on-Tees))
Stockton-on-TeesCum_Factor <- 10^(floor(log10(Stockton-on-TeesCum_Sigfig)))
Stockton-on-TeesCum_Max <- round_any(sum(COVID19_by_LA$Stockton-on-Tees), Stockton-on-TeesCum_Factor, f=ceiling)
Stockton-on-TeesCum_Breaks <- ceiling(Stockton-on-TeesCum_Factor/2)

plot_Stockton-on-TeesCum <- ggplot(data=Stockton-on-Tees_data, aes(x=Stockton-on-Tees_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Stockton-on-TeesCum_Max), 
                     breaks = seq(0, Stockton-on-TeesCum_Max, Stockton-on-TeesCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Stockton-on-Tees") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Stockton-on-Tees_figure <- ggarrange(plot_Stockton-on-TeesNew + font("x.text", size = 8), 
                                     plot_Stockton-on-TeesCum + font("x.text", size = 8), 
                                     ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Stockton-on-Tees_cases_plot.pdf", height = 8.27, width = 11.69)
Stockton-on-Tees_figure
dev.off()



#### Stoke-on-Trent plots ####
#Read in new cases per day from summarised data, format date correctly
Stoke-on-Trent_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Stoke-on-Trent))
Stoke-on-Trent_data <- as.data.frame(lapply(Stoke-on-Trent_data, rep, Stoke-on-Trent_data$ntimes))


# Plot number of NEW CASES per day
Stoke-on-TrentNew_Sigfig <- signif(max(COVID19_by_LA$Stoke-on-Trent))
Stoke-on-TrentNew_Factor <- 10^(floor(log10(Stoke-on-TrentNew_Sigfig)))
Stoke-on-TrentNew_Max <- round_any(max(COVID19_by_LA$Stoke-on-Trent), Stoke-on-TrentNew_Factor, f=ceiling)
Stoke-on-TrentNew_Breaks <- ceiling(Stoke-on-TrentNew_Factor/2)

plot_Stoke-on-TrentNew <- ggplot(data=Stoke-on-Trent_data, aes(x=Stoke-on-Trent_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Stoke-on-TrentNew_Max), 
                     breaks = seq(0, Stoke-on-TrentNew_Max, Stoke-on-TrentNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Stoke-on-Trent") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Stoke-on-TrentCum_Sigfig <- signif(sum(COVID19_by_LA$Stoke-on-Trent))
Stoke-on-TrentCum_Factor <- 10^(floor(log10(Stoke-on-TrentCum_Sigfig)))
Stoke-on-TrentCum_Max <- round_any(sum(COVID19_by_LA$Stoke-on-Trent), Stoke-on-TrentCum_Factor, f=ceiling)
Stoke-on-TrentCum_Breaks <- ceiling(Stoke-on-TrentCum_Factor/2)

plot_Stoke-on-TrentCum <- ggplot(data=Stoke-on-Trent_data, aes(x=Stoke-on-Trent_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Stoke-on-TrentCum_Max), 
                     breaks = seq(0, Stoke-on-TrentCum_Max, Stoke-on-TrentCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Stoke-on-Trent") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Stoke-on-Trent_figure <- ggarrange(plot_Stoke-on-TrentNew + font("x.text", size = 8), 
                                   plot_Stoke-on-TrentCum + font("x.text", size = 8), 
                                   ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Stoke-on-Trent_cases_plot.pdf", height = 8.27, width = 11.69)
Stoke-on-Trent_figure
dev.off()



#### Suffolk plots ####
#Read in new cases per day from summarised data, format date correctly
Suffolk_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Suffolk))
Suffolk_data <- as.data.frame(lapply(Suffolk_data, rep, Suffolk_data$ntimes))


# Plot number of NEW CASES per day
SuffolkNew_Sigfig <- signif(max(COVID19_by_LA$Suffolk))
SuffolkNew_Factor <- 10^(floor(log10(SuffolkNew_Sigfig)))
SuffolkNew_Max <- round_any(max(COVID19_by_LA$Suffolk), SuffolkNew_Factor, f=ceiling)
SuffolkNew_Breaks <- ceiling(SuffolkNew_Factor/2)

plot_SuffolkNew <- ggplot(data=Suffolk_data, aes(x=Suffolk_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SuffolkNew_Max), 
                     breaks = seq(0, SuffolkNew_Max, SuffolkNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Suffolk") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SuffolkCum_Sigfig <- signif(sum(COVID19_by_LA$Suffolk))
SuffolkCum_Factor <- 10^(floor(log10(SuffolkCum_Sigfig)))
SuffolkCum_Max <- round_any(sum(COVID19_by_LA$Suffolk), SuffolkCum_Factor, f=ceiling)
SuffolkCum_Breaks <- ceiling(SuffolkCum_Factor/2)

plot_SuffolkCum <- ggplot(data=Suffolk_data, aes(x=Suffolk_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SuffolkCum_Max), 
                     breaks = seq(0, SuffolkCum_Max, SuffolkCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Suffolk") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Suffolk_figure <- ggarrange(plot_SuffolkNew + font("x.text", size = 8), 
                            plot_SuffolkCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Suffolk_cases_plot.pdf", height = 8.27, width = 11.69)
Suffolk_figure
dev.off()



#### Sunderland plots ####
#Read in new cases per day from summarised data, format date correctly
Sunderland_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Sunderland))
Sunderland_data <- as.data.frame(lapply(Sunderland_data, rep, Sunderland_data$ntimes))


# Plot number of NEW CASES per day
SunderlandNew_Sigfig <- signif(max(COVID19_by_LA$Sunderland))
SunderlandNew_Factor <- 10^(floor(log10(SunderlandNew_Sigfig)))
SunderlandNew_Max <- round_any(max(COVID19_by_LA$Sunderland), SunderlandNew_Factor, f=ceiling)
SunderlandNew_Breaks <- ceiling(SunderlandNew_Factor/2)

plot_SunderlandNew <- ggplot(data=Sunderland_data, aes(x=Sunderland_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SunderlandNew_Max), 
                     breaks = seq(0, SunderlandNew_Max, SunderlandNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Sunderland") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SunderlandCum_Sigfig <- signif(sum(COVID19_by_LA$Sunderland))
SunderlandCum_Factor <- 10^(floor(log10(SunderlandCum_Sigfig)))
SunderlandCum_Max <- round_any(sum(COVID19_by_LA$Sunderland), SunderlandCum_Factor, f=ceiling)
SunderlandCum_Breaks <- ceiling(SunderlandCum_Factor/2)

plot_SunderlandCum <- ggplot(data=Sunderland_data, aes(x=Sunderland_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SunderlandCum_Max), 
                     breaks = seq(0, SunderlandCum_Max, SunderlandCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Sunderland") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Sunderland_figure <- ggarrange(plot_SunderlandNew + font("x.text", size = 8), 
                               plot_SunderlandCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Sunderland_cases_plot.pdf", height = 8.27, width = 11.69)
Sunderland_figure
dev.off()



#### Surrey plots ####
#Read in new cases per day from summarised data, format date correctly
Surrey_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Surrey))
Surrey_data <- as.data.frame(lapply(Surrey_data, rep, Surrey_data$ntimes))


# Plot number of NEW CASES per day
SurreyNew_Sigfig <- signif(max(COVID19_by_LA$Surrey))
SurreyNew_Factor <- 10^(floor(log10(SurreyNew_Sigfig)))
SurreyNew_Max <- round_any(max(COVID19_by_LA$Surrey), SurreyNew_Factor, f=ceiling)
SurreyNew_Breaks <- ceiling(SurreyNew_Factor/2)

plot_SurreyNew <- ggplot(data=Surrey_data, aes(x=Surrey_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SurreyNew_Max), 
                     breaks = seq(0, SurreyNew_Max, SurreyNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Surrey") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SurreyCum_Sigfig <- signif(sum(COVID19_by_LA$Surrey))
SurreyCum_Factor <- 10^(floor(log10(SurreyCum_Sigfig)))
SurreyCum_Max <- round_any(sum(COVID19_by_LA$Surrey), SurreyCum_Factor, f=ceiling)
SurreyCum_Breaks <- ceiling(SurreyCum_Factor/2)

plot_SurreyCum <- ggplot(data=Surrey_data, aes(x=Surrey_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SurreyCum_Max), 
                     breaks = seq(0, SurreyCum_Max, SurreyCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Surrey") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Surrey_figure <- ggarrange(plot_SurreyNew + font("x.text", size = 8), 
                           plot_SurreyCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Surrey_cases_plot.pdf", height = 8.27, width = 11.69)
Surrey_figure
dev.off()



#### Sutton plots ####
#Read in new cases per day from summarised data, format date correctly
Sutton_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Sutton))
Sutton_data <- as.data.frame(lapply(Sutton_data, rep, Sutton_data$ntimes))


# Plot number of NEW CASES per day
SuttonNew_Sigfig <- signif(max(COVID19_by_LA$Sutton))
SuttonNew_Factor <- 10^(floor(log10(SuttonNew_Sigfig)))
SuttonNew_Max <- round_any(max(COVID19_by_LA$Sutton), SuttonNew_Factor, f=ceiling)
SuttonNew_Breaks <- ceiling(SuttonNew_Factor/2)

plot_SuttonNew <- ggplot(data=Sutton_data, aes(x=Sutton_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SuttonNew_Max), 
                     breaks = seq(0, SuttonNew_Max, SuttonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Sutton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SuttonCum_Sigfig <- signif(sum(COVID19_by_LA$Sutton))
SuttonCum_Factor <- 10^(floor(log10(SuttonCum_Sigfig)))
SuttonCum_Max <- round_any(sum(COVID19_by_LA$Sutton), SuttonCum_Factor, f=ceiling)
SuttonCum_Breaks <- ceiling(SuttonCum_Factor/2)

plot_SuttonCum <- ggplot(data=Sutton_data, aes(x=Sutton_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SuttonCum_Max), 
                     breaks = seq(0, SuttonCum_Max, SuttonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Sutton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Sutton_figure <- ggarrange(plot_SuttonNew + font("x.text", size = 8), 
                           plot_SuttonCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Sutton_cases_plot.pdf", height = 8.27, width = 11.69)
Sutton_figure
dev.off()



#### Swindon plots ####
#Read in new cases per day from summarised data, format date correctly
Swindon_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Swindon))
Swindon_data <- as.data.frame(lapply(Swindon_data, rep, Swindon_data$ntimes))


# Plot number of NEW CASES per day
SwindonNew_Sigfig <- signif(max(COVID19_by_LA$Swindon))
SwindonNew_Factor <- 10^(floor(log10(SwindonNew_Sigfig)))
SwindonNew_Max <- round_any(max(COVID19_by_LA$Swindon), SwindonNew_Factor, f=ceiling)
SwindonNew_Breaks <- ceiling(SwindonNew_Factor/2)

plot_SwindonNew <- ggplot(data=Swindon_data, aes(x=Swindon_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SwindonNew_Max), 
                     breaks = seq(0, SwindonNew_Max, SwindonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Swindon") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
SwindonCum_Sigfig <- signif(sum(COVID19_by_LA$Swindon))
SwindonCum_Factor <- 10^(floor(log10(SwindonCum_Sigfig)))
SwindonCum_Max <- round_any(sum(COVID19_by_LA$Swindon), SwindonCum_Factor, f=ceiling)
SwindonCum_Breaks <- ceiling(SwindonCum_Factor/2)

plot_SwindonCum <- ggplot(data=Swindon_data, aes(x=Swindon_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, SwindonCum_Max), 
                     breaks = seq(0, SwindonCum_Max, SwindonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Swindon") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Swindon_figure <- ggarrange(plot_SwindonNew + font("x.text", size = 8), 
                            plot_SwindonCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Swindon_cases_plot.pdf", height = 8.27, width = 11.69)
Swindon_figure
dev.off()



#### Tameside plots ####
#Read in new cases per day from summarised data, format date correctly
Tameside_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Tameside))
Tameside_data <- as.data.frame(lapply(Tameside_data, rep, Tameside_data$ntimes))


# Plot number of NEW CASES per day
TamesideNew_Sigfig <- signif(max(COVID19_by_LA$Tameside))
TamesideNew_Factor <- 10^(floor(log10(TamesideNew_Sigfig)))
TamesideNew_Max <- round_any(max(COVID19_by_LA$Tameside), TamesideNew_Factor, f=ceiling)
TamesideNew_Breaks <- ceiling(TamesideNew_Factor/2)

plot_TamesideNew <- ggplot(data=Tameside_data, aes(x=Tameside_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, TamesideNew_Max), 
                     breaks = seq(0, TamesideNew_Max, TamesideNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Tameside") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
TamesideCum_Sigfig <- signif(sum(COVID19_by_LA$Tameside))
TamesideCum_Factor <- 10^(floor(log10(TamesideCum_Sigfig)))
TamesideCum_Max <- round_any(sum(COVID19_by_LA$Tameside), TamesideCum_Factor, f=ceiling)
TamesideCum_Breaks <- ceiling(TamesideCum_Factor/2)

plot_TamesideCum <- ggplot(data=Tameside_data, aes(x=Tameside_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, TamesideCum_Max), 
                     breaks = seq(0, TamesideCum_Max, TamesideCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Tameside") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Tameside_figure <- ggarrange(plot_TamesideNew + font("x.text", size = 8), 
                             plot_TamesideCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Tameside_cases_plot.pdf", height = 8.27, width = 11.69)
Tameside_figure
dev.off()



#### Telford_and_Wrekin plots ####
#Read in new cases per day from summarised data, format date correctly
Telford_and_Wrekin_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Telford_and_Wrekin))
Telford_and_Wrekin_data <- as.data.frame(lapply(Telford_and_Wrekin_data, rep, Telford_and_Wrekin_data$ntimes))


# Plot number of NEW CASES per day
Telford_and_WrekinNew_Sigfig <- signif(max(COVID19_by_LA$Telford_and_Wrekin))
Telford_and_WrekinNew_Factor <- 10^(floor(log10(Telford_and_WrekinNew_Sigfig)))
Telford_and_WrekinNew_Max <- round_any(max(COVID19_by_LA$Telford_and_Wrekin), Telford_and_WrekinNew_Factor, f=ceiling)
Telford_and_WrekinNew_Breaks <- ceiling(Telford_and_WrekinNew_Factor/2)

plot_Telford_and_WrekinNew <- ggplot(data=Telford_and_Wrekin_data, aes(x=Telford_and_Wrekin_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Telford_and_WrekinNew_Max), 
                     breaks = seq(0, Telford_and_WrekinNew_Max, Telford_and_WrekinNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Telford and Wrekin") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Telford_and_WrekinCum_Sigfig <- signif(sum(COVID19_by_LA$Telford_and_Wrekin))
Telford_and_WrekinCum_Factor <- 10^(floor(log10(Telford_and_WrekinCum_Sigfig)))
Telford_and_WrekinCum_Max <- round_any(sum(COVID19_by_LA$Telford_and_Wrekin), Telford_and_WrekinCum_Factor, f=ceiling)
Telford_and_WrekinCum_Breaks <- ceiling(Telford_and_WrekinCum_Factor/2)

plot_Telford_and_WrekinCum <- ggplot(data=Telford_and_Wrekin_data, aes(x=Telford_and_Wrekin_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Telford_and_WrekinCum_Max), 
                     breaks = seq(0, Telford_and_WrekinCum_Max, Telford_and_WrekinCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Telford and Wrekin") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Telford_and_Wrekin_figure <- ggarrange(plot_Telford_and_WrekinNew + font("x.text", size = 8), 
                                       plot_Telford_and_WrekinCum + font("x.text", size = 8), 
                                       ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Telford_and_Wrekin_cases_plot.pdf", height = 8.27, width = 11.69)
Telford_and_Wrekin_figure
dev.off()



#### Thurrock plots ####
#Read in new cases per day from summarised data, format date correctly
Thurrock_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Thurrock))
Thurrock_data <- as.data.frame(lapply(Thurrock_data, rep, Thurrock_data$ntimes))


# Plot number of NEW CASES per day
ThurrockNew_Sigfig <- signif(max(COVID19_by_LA$Thurrock))
ThurrockNew_Factor <- 10^(floor(log10(ThurrockNew_Sigfig)))
ThurrockNew_Max <- round_any(max(COVID19_by_LA$Thurrock), ThurrockNew_Factor, f=ceiling)
ThurrockNew_Breaks <- ceiling(ThurrockNew_Factor/2)

plot_ThurrockNew <- ggplot(data=Thurrock_data, aes(x=Thurrock_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, ThurrockNew_Max), 
                     breaks = seq(0, ThurrockNew_Max, ThurrockNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Thurrock") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
ThurrockCum_Sigfig <- signif(sum(COVID19_by_LA$Thurrock))
ThurrockCum_Factor <- 10^(floor(log10(ThurrockCum_Sigfig)))
ThurrockCum_Max <- round_any(sum(COVID19_by_LA$Thurrock), ThurrockCum_Factor, f=ceiling)
ThurrockCum_Breaks <- ceiling(ThurrockCum_Factor/2)

plot_ThurrockCum <- ggplot(data=Thurrock_data, aes(x=Thurrock_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, ThurrockCum_Max), 
                     breaks = seq(0, ThurrockCum_Max, ThurrockCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Thurrock") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Thurrock_figure <- ggarrange(plot_ThurrockNew + font("x.text", size = 8), 
                             plot_ThurrockCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Thurrock_cases_plot.pdf", height = 8.27, width = 11.69)
Thurrock_figure
dev.off()



#### Torbay plots ####
#Read in new cases per day from summarised data, format date correctly
Torbay_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Torbay))
Torbay_data <- as.data.frame(lapply(Torbay_data, rep, Torbay_data$ntimes))


# Plot number of NEW CASES per day
TorbayNew_Sigfig <- signif(max(COVID19_by_LA$Torbay))
TorbayNew_Factor <- 10^(floor(log10(TorbayNew_Sigfig)))
TorbayNew_Max <- round_any(max(COVID19_by_LA$Torbay), TorbayNew_Factor, f=ceiling)
TorbayNew_Breaks <- ceiling(TorbayNew_Factor/2)

plot_TorbayNew <- ggplot(data=Torbay_data, aes(x=Torbay_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, TorbayNew_Max), 
                     breaks = seq(0, TorbayNew_Max, TorbayNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Torbay") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
TorbayCum_Sigfig <- signif(sum(COVID19_by_LA$Torbay))
TorbayCum_Factor <- 10^(floor(log10(TorbayCum_Sigfig)))
TorbayCum_Max <- round_any(sum(COVID19_by_LA$Torbay), TorbayCum_Factor, f=ceiling)
TorbayCum_Breaks <- ceiling(TorbayCum_Factor/2)

plot_TorbayCum <- ggplot(data=Torbay_data, aes(x=Torbay_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, TorbayCum_Max), 
                     breaks = seq(0, TorbayCum_Max, TorbayCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Torbay") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Torbay_figure <- ggarrange(plot_TorbayNew + font("x.text", size = 8), 
                           plot_TorbayCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Torbay_cases_plot.pdf", height = 8.27, width = 11.69)
Torbay_figure
dev.off()



#### Tower_Hamlets plots ####
#Read in new cases per day from summarised data, format date correctly
Tower_Hamlets_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Tower_Hamlets))
Tower_Hamlets_data <- as.data.frame(lapply(Tower_Hamlets_data, rep, Tower_Hamlets_data$ntimes))


# Plot number of NEW CASES per day
Tower_HamletsNew_Sigfig <- signif(max(COVID19_by_LA$Tower_Hamlets))
Tower_HamletsNew_Factor <- 10^(floor(log10(Tower_HamletsNew_Sigfig)))
Tower_HamletsNew_Max <- round_any(max(COVID19_by_LA$Tower_Hamlets), Tower_HamletsNew_Factor, f=ceiling)
Tower_HamletsNew_Breaks <- ceiling(Tower_HamletsNew_Factor/2)

plot_Tower_HamletsNew <- ggplot(data=Tower_Hamlets_data, aes(x=Tower_Hamlets_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Tower_HamletsNew_Max), 
                     breaks = seq(0, Tower_HamletsNew_Max, Tower_HamletsNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Tower Hamlets") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Tower_HamletsCum_Sigfig <- signif(sum(COVID19_by_LA$Tower_Hamlets))
Tower_HamletsCum_Factor <- 10^(floor(log10(Tower_HamletsCum_Sigfig)))
Tower_HamletsCum_Max <- round_any(sum(COVID19_by_LA$Tower_Hamlets), Tower_HamletsCum_Factor, f=ceiling)
Tower_HamletsCum_Breaks <- ceiling(Tower_HamletsCum_Factor/2)

plot_Tower_HamletsCum <- ggplot(data=Tower_Hamlets_data, aes(x=Tower_Hamlets_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Tower_HamletsCum_Max), 
                     breaks = seq(0, Tower_HamletsCum_Max, Tower_HamletsCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Tower Hamlets") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Tower_Hamlets_figure <- ggarrange(plot_Tower_HamletsNew + font("x.text", size = 8), 
                                  plot_Tower_HamletsCum + font("x.text", size = 8), 
                                  ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Tower_Hamlets_cases_plot.pdf", height = 8.27, width = 11.69)
Tower_Hamlets_figure
dev.off()



#### Trafford plots ####
#Read in new cases per day from summarised data, format date correctly
Trafford_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Trafford))
Trafford_data <- as.data.frame(lapply(Trafford_data, rep, Trafford_data$ntimes))


# Plot number of NEW CASES per day
TraffordNew_Sigfig <- signif(max(COVID19_by_LA$Trafford))
TraffordNew_Factor <- 10^(floor(log10(TraffordNew_Sigfig)))
TraffordNew_Max <- round_any(max(COVID19_by_LA$Trafford), TraffordNew_Factor, f=ceiling)
TraffordNew_Breaks <- ceiling(TraffordNew_Factor/2)

plot_TraffordNew <- ggplot(data=Trafford_data, aes(x=Trafford_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, TraffordNew_Max), 
                     breaks = seq(0, TraffordNew_Max, TraffordNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Trafford") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
TraffordCum_Sigfig <- signif(sum(COVID19_by_LA$Trafford))
TraffordCum_Factor <- 10^(floor(log10(TraffordCum_Sigfig)))
TraffordCum_Max <- round_any(sum(COVID19_by_LA$Trafford), TraffordCum_Factor, f=ceiling)
TraffordCum_Breaks <- ceiling(TraffordCum_Factor/2)

plot_TraffordCum <- ggplot(data=Trafford_data, aes(x=Trafford_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, TraffordCum_Max), 
                     breaks = seq(0, TraffordCum_Max, TraffordCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Trafford") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Trafford_figure <- ggarrange(plot_TraffordNew + font("x.text", size = 8), 
                             plot_TraffordCum + font("x.text", size = 8), 
                             ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Trafford_cases_plot.pdf", height = 8.27, width = 11.69)
Trafford_figure
dev.off()



#### Wakefield plots ####
#Read in new cases per day from summarised data, format date correctly
Wakefield_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Wakefield))
Wakefield_data <- as.data.frame(lapply(Wakefield_data, rep, Wakefield_data$ntimes))


# Plot number of NEW CASES per day
WakefieldNew_Sigfig <- signif(max(COVID19_by_LA$Wakefield))
WakefieldNew_Factor <- 10^(floor(log10(WakefieldNew_Sigfig)))
WakefieldNew_Max <- round_any(max(COVID19_by_LA$Wakefield), WakefieldNew_Factor, f=ceiling)
WakefieldNew_Breaks <- ceiling(WakefieldNew_Factor/2)

plot_WakefieldNew <- ggplot(data=Wakefield_data, aes(x=Wakefield_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WakefieldNew_Max), 
                     breaks = seq(0, WakefieldNew_Max, WakefieldNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Wakefield") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
WakefieldCum_Sigfig <- signif(sum(COVID19_by_LA$Wakefield))
WakefieldCum_Factor <- 10^(floor(log10(WakefieldCum_Sigfig)))
WakefieldCum_Max <- round_any(sum(COVID19_by_LA$Wakefield), WakefieldCum_Factor, f=ceiling)
WakefieldCum_Breaks <- ceiling(WakefieldCum_Factor/2)

plot_WakefieldCum <- ggplot(data=Wakefield_data, aes(x=Wakefield_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WakefieldCum_Max), 
                     breaks = seq(0, WakefieldCum_Max, WakefieldCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Wakefield") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Wakefield_figure <- ggarrange(plot_WakefieldNew + font("x.text", size = 8), 
                              plot_WakefieldCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Wakefield_cases_plot.pdf", height = 8.27, width = 11.69)
Wakefield_figure
dev.off()



#### Walsall plots ####
#Read in new cases per day from summarised data, format date correctly
Walsall_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Walsall))
Walsall_data <- as.data.frame(lapply(Walsall_data, rep, Walsall_data$ntimes))


# Plot number of NEW CASES per day
WalsallNew_Sigfig <- signif(max(COVID19_by_LA$Walsall))
WalsallNew_Factor <- 10^(floor(log10(WalsallNew_Sigfig)))
WalsallNew_Max <- round_any(max(COVID19_by_LA$Walsall), WalsallNew_Factor, f=ceiling)
WalsallNew_Breaks <- ceiling(WalsallNew_Factor/2)

plot_WalsallNew <- ggplot(data=Walsall_data, aes(x=Walsall_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WalsallNew_Max), 
                     breaks = seq(0, WalsallNew_Max, WalsallNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Walsall") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
WalsallCum_Sigfig <- signif(sum(COVID19_by_LA$Walsall))
WalsallCum_Factor <- 10^(floor(log10(WalsallCum_Sigfig)))
WalsallCum_Max <- round_any(sum(COVID19_by_LA$Walsall), WalsallCum_Factor, f=ceiling)
WalsallCum_Breaks <- ceiling(WalsallCum_Factor/2)

plot_WalsallCum <- ggplot(data=Walsall_data, aes(x=Walsall_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WalsallCum_Max), 
                     breaks = seq(0, WalsallCum_Max, WalsallCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Walsall") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Walsall_figure <- ggarrange(plot_WalsallNew + font("x.text", size = 8), 
                            plot_WalsallCum + font("x.text", size = 8), 
                            ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Walsall_cases_plot.pdf", height = 8.27, width = 11.69)
Walsall_figure
dev.off()



#### Waltham_Forest plots ####
#Read in new cases per day from summarised data, format date correctly
Waltham_Forest_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Waltham_Forest))
Waltham_Forest_data <- as.data.frame(lapply(Waltham_Forest_data, rep, Waltham_Forest_data$ntimes))


# Plot number of NEW CASES per day
Waltham_ForestNew_Sigfig <- signif(max(COVID19_by_LA$Waltham_Forest))
Waltham_ForestNew_Factor <- 10^(floor(log10(Waltham_ForestNew_Sigfig)))
Waltham_ForestNew_Max <- round_any(max(COVID19_by_LA$Waltham_Forest), Waltham_ForestNew_Factor, f=ceiling)
Waltham_ForestNew_Breaks <- ceiling(Waltham_ForestNew_Factor/2)

plot_Waltham_ForestNew <- ggplot(data=Waltham_Forest_data, aes(x=Waltham_Forest_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Waltham_ForestNew_Max), 
                     breaks = seq(0, Waltham_ForestNew_Max, Waltham_ForestNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Waltham Forest") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Waltham_ForestCum_Sigfig <- signif(sum(COVID19_by_LA$Waltham_Forest))
Waltham_ForestCum_Factor <- 10^(floor(log10(Waltham_ForestCum_Sigfig)))
Waltham_ForestCum_Max <- round_any(sum(COVID19_by_LA$Waltham_Forest), Waltham_ForestCum_Factor, f=ceiling)
Waltham_ForestCum_Breaks <- ceiling(Waltham_ForestCum_Factor/2)

plot_Waltham_ForestCum <- ggplot(data=Waltham_Forest_data, aes(x=Waltham_Forest_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Waltham_ForestCum_Max), 
                     breaks = seq(0, Waltham_ForestCum_Max, Waltham_ForestCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Waltham Forest") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Waltham_Forest_figure <- ggarrange(plot_Waltham_ForestNew + font("x.text", size = 8), 
                                   plot_Waltham_ForestCum + font("x.text", size = 8), 
                                   ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Waltham_Forest_cases_plot.pdf", height = 8.27, width = 11.69)
Waltham_Forest_figure
dev.off()



#### Wandsworth plots ####
#Read in new cases per day from summarised data, format date correctly
Wandsworth_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Wandsworth))
Wandsworth_data <- as.data.frame(lapply(Wandsworth_data, rep, Wandsworth_data$ntimes))


# Plot number of NEW CASES per day
WandsworthNew_Sigfig <- signif(max(COVID19_by_LA$Wandsworth))
WandsworthNew_Factor <- 10^(floor(log10(WandsworthNew_Sigfig)))
WandsworthNew_Max <- round_any(max(COVID19_by_LA$Wandsworth), WandsworthNew_Factor, f=ceiling)
WandsworthNew_Breaks <- ceiling(WandsworthNew_Factor/2)

plot_WandsworthNew <- ggplot(data=Wandsworth_data, aes(x=Wandsworth_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WandsworthNew_Max), 
                     breaks = seq(0, WandsworthNew_Max, WandsworthNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Wandsworth") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
WandsworthCum_Sigfig <- signif(sum(COVID19_by_LA$Wandsworth))
WandsworthCum_Factor <- 10^(floor(log10(WandsworthCum_Sigfig)))
WandsworthCum_Max <- round_any(sum(COVID19_by_LA$Wandsworth), WandsworthCum_Factor, f=ceiling)
WandsworthCum_Breaks <- ceiling(WandsworthCum_Factor/2)

plot_WandsworthCum <- ggplot(data=Wandsworth_data, aes(x=Wandsworth_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WandsworthCum_Max), 
                     breaks = seq(0, WandsworthCum_Max, WandsworthCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Wandsworth") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Wandsworth_figure <- ggarrange(plot_WandsworthNew + font("x.text", size = 8), 
                               plot_WandsworthCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Wandsworth_cases_plot.pdf", height = 8.27, width = 11.69)
Wandsworth_figure
dev.off()



#### Warrington plots ####
#Read in new cases per day from summarised data, format date correctly
Warrington_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Warrington))
Warrington_data <- as.data.frame(lapply(Warrington_data, rep, Warrington_data$ntimes))


# Plot number of NEW CASES per day
WarringtonNew_Sigfig <- signif(max(COVID19_by_LA$Warrington))
WarringtonNew_Factor <- 10^(floor(log10(WarringtonNew_Sigfig)))
WarringtonNew_Max <- round_any(max(COVID19_by_LA$Warrington), WarringtonNew_Factor, f=ceiling)
WarringtonNew_Breaks <- ceiling(WarringtonNew_Factor/2)

plot_WarringtonNew <- ggplot(data=Warrington_data, aes(x=Warrington_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WarringtonNew_Max), 
                     breaks = seq(0, WarringtonNew_Max, WarringtonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Warrington") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
WarringtonCum_Sigfig <- signif(sum(COVID19_by_LA$Warrington))
WarringtonCum_Factor <- 10^(floor(log10(WarringtonCum_Sigfig)))
WarringtonCum_Max <- round_any(sum(COVID19_by_LA$Warrington), WarringtonCum_Factor, f=ceiling)
WarringtonCum_Breaks <- ceiling(WarringtonCum_Factor/2)

plot_WarringtonCum <- ggplot(data=Warrington_data, aes(x=Warrington_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WarringtonCum_Max), 
                     breaks = seq(0, WarringtonCum_Max, WarringtonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Warrington") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Warrington_figure <- ggarrange(plot_WarringtonNew + font("x.text", size = 8), 
                               plot_WarringtonCum + font("x.text", size = 8), 
                               ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Warrington_cases_plot.pdf", height = 8.27, width = 11.69)
Warrington_figure
dev.off()



#### Warwickshire plots ####
#Read in new cases per day from summarised data, format date correctly
Warwickshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Warwickshire))
Warwickshire_data <- as.data.frame(lapply(Warwickshire_data, rep, Warwickshire_data$ntimes))


# Plot number of NEW CASES per day
WarwickshireNew_Sigfig <- signif(max(COVID19_by_LA$Warwickshire))
WarwickshireNew_Factor <- 10^(floor(log10(WarwickshireNew_Sigfig)))
WarwickshireNew_Max <- round_any(max(COVID19_by_LA$Warwickshire), WarwickshireNew_Factor, f=ceiling)
WarwickshireNew_Breaks <- ceiling(WarwickshireNew_Factor/2)

plot_WarwickshireNew <- ggplot(data=Warwickshire_data, aes(x=Warwickshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WarwickshireNew_Max), 
                     breaks = seq(0, WarwickshireNew_Max, WarwickshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Warwickshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
WarwickshireCum_Sigfig <- signif(sum(COVID19_by_LA$Warwickshire))
WarwickshireCum_Factor <- 10^(floor(log10(WarwickshireCum_Sigfig)))
WarwickshireCum_Max <- round_any(sum(COVID19_by_LA$Warwickshire), WarwickshireCum_Factor, f=ceiling)
WarwickshireCum_Breaks <- ceiling(WarwickshireCum_Factor/2)

plot_WarwickshireCum <- ggplot(data=Warwickshire_data, aes(x=Warwickshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WarwickshireCum_Max), 
                     breaks = seq(0, WarwickshireCum_Max, WarwickshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Warwickshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Warwickshire_figure <- ggarrange(plot_WarwickshireNew + font("x.text", size = 8), 
                                 plot_WarwickshireCum + font("x.text", size = 8), 
                                 ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Warwickshire_cases_plot.pdf", height = 8.27, width = 11.69)
Warwickshire_figure
dev.off()



#### West_Berkshire plots ####
#Read in new cases per day from summarised data, format date correctly
West_Berkshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$West_Berkshire))
West_Berkshire_data <- as.data.frame(lapply(West_Berkshire_data, rep, West_Berkshire_data$ntimes))


# Plot number of NEW CASES per day
West_BerkshireNew_Sigfig <- signif(max(COVID19_by_LA$West_Berkshire))
West_BerkshireNew_Factor <- 10^(floor(log10(West_BerkshireNew_Sigfig)))
West_BerkshireNew_Max <- round_any(max(COVID19_by_LA$West_Berkshire), West_BerkshireNew_Factor, f=ceiling)
West_BerkshireNew_Breaks <- ceiling(West_BerkshireNew_Factor/2)

plot_West_BerkshireNew <- ggplot(data=West_Berkshire_data, aes(x=West_Berkshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, West_BerkshireNew_Max), 
                     breaks = seq(0, West_BerkshireNew_Max, West_BerkshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: West Berkshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
West_BerkshireCum_Sigfig <- signif(sum(COVID19_by_LA$West_Berkshire))
West_BerkshireCum_Factor <- 10^(floor(log10(West_BerkshireCum_Sigfig)))
West_BerkshireCum_Max <- round_any(sum(COVID19_by_LA$West_Berkshire), West_BerkshireCum_Factor, f=ceiling)
West_BerkshireCum_Breaks <- ceiling(West_BerkshireCum_Factor/2)

plot_West_BerkshireCum <- ggplot(data=West_Berkshire_data, aes(x=West_Berkshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, West_BerkshireCum_Max), 
                     breaks = seq(0, West_BerkshireCum_Max, West_BerkshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: West Berkshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
West_Berkshire_figure <- ggarrange(plot_West_BerkshireNew + font("x.text", size = 8), 
                                   plot_West_BerkshireCum + font("x.text", size = 8), 
                                   ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/West_Berkshire_cases_plot.pdf", height = 8.27, width = 11.69)
West_Berkshire_figure
dev.off()



#### West_Sussex plots ####
#Read in new cases per day from summarised data, format date correctly
West_Sussex_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$West_Sussex))
West_Sussex_data <- as.data.frame(lapply(West_Sussex_data, rep, West_Sussex_data$ntimes))


# Plot number of NEW CASES per day
West_SussexNew_Sigfig <- signif(max(COVID19_by_LA$West_Sussex))
West_SussexNew_Factor <- 10^(floor(log10(West_SussexNew_Sigfig)))
West_SussexNew_Max <- round_any(max(COVID19_by_LA$West_Sussex), West_SussexNew_Factor, f=ceiling)
West_SussexNew_Breaks <- ceiling(West_SussexNew_Factor/2)

plot_West_SussexNew <- ggplot(data=West_Sussex_data, aes(x=West_Sussex_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, West_SussexNew_Max), 
                     breaks = seq(0, West_SussexNew_Max, West_SussexNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: West Sussex") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
West_SussexCum_Sigfig <- signif(sum(COVID19_by_LA$West_Sussex))
West_SussexCum_Factor <- 10^(floor(log10(West_SussexCum_Sigfig)))
West_SussexCum_Max <- round_any(sum(COVID19_by_LA$West_Sussex), West_SussexCum_Factor, f=ceiling)
West_SussexCum_Breaks <- ceiling(West_SussexCum_Factor/2)

plot_West_SussexCum <- ggplot(data=West_Sussex_data, aes(x=West_Sussex_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, West_SussexCum_Max), 
                     breaks = seq(0, West_SussexCum_Max, West_SussexCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: West Sussex") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
West_Sussex_figure <- ggarrange(plot_West_SussexNew + font("x.text", size = 8), 
                                plot_West_SussexCum + font("x.text", size = 8), 
                                ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/West_Sussex_cases_plot.pdf", height = 8.27, width = 11.69)
West_Sussex_figure
dev.off()



#### Westminster plots ####
#Read in new cases per day from summarised data, format date correctly
Westminster_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Westminster))
Westminster_data <- as.data.frame(lapply(Westminster_data, rep, Westminster_data$ntimes))


# Plot number of NEW CASES per day
WestminsterNew_Sigfig <- signif(max(COVID19_by_LA$Westminster))
WestminsterNew_Factor <- 10^(floor(log10(WestminsterNew_Sigfig)))
WestminsterNew_Max <- round_any(max(COVID19_by_LA$Westminster), WestminsterNew_Factor, f=ceiling)
WestminsterNew_Breaks <- ceiling(WestminsterNew_Factor/2)

plot_WestminsterNew <- ggplot(data=Westminster_data, aes(x=Westminster_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WestminsterNew_Max), 
                     breaks = seq(0, WestminsterNew_Max, WestminsterNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Westminster") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
WestminsterCum_Sigfig <- signif(sum(COVID19_by_LA$Westminster))
WestminsterCum_Factor <- 10^(floor(log10(WestminsterCum_Sigfig)))
WestminsterCum_Max <- round_any(sum(COVID19_by_LA$Westminster), WestminsterCum_Factor, f=ceiling)
WestminsterCum_Breaks <- ceiling(WestminsterCum_Factor/2)

plot_WestminsterCum <- ggplot(data=Westminster_data, aes(x=Westminster_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WestminsterCum_Max), 
                     breaks = seq(0, WestminsterCum_Max, WestminsterCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Westminster") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Westminster_figure <- ggarrange(plot_WestminsterNew + font("x.text", size = 8), 
                                plot_WestminsterCum + font("x.text", size = 8), 
                                ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Westminster_cases_plot.pdf", height = 8.27, width = 11.69)
Westminster_figure
dev.off()



#### Wigan plots ####
#Read in new cases per day from summarised data, format date correctly
Wigan_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Wigan))
Wigan_data <- as.data.frame(lapply(Wigan_data, rep, Wigan_data$ntimes))


# Plot number of NEW CASES per day
WiganNew_Sigfig <- signif(max(COVID19_by_LA$Wigan))
WiganNew_Factor <- 10^(floor(log10(WiganNew_Sigfig)))
WiganNew_Max <- round_any(max(COVID19_by_LA$Wigan), WiganNew_Factor, f=ceiling)
WiganNew_Breaks <- ceiling(WiganNew_Factor/2)

plot_WiganNew <- ggplot(data=Wigan_data, aes(x=Wigan_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WiganNew_Max), 
                     breaks = seq(0, WiganNew_Max, WiganNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Wigan") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
WiganCum_Sigfig <- signif(sum(COVID19_by_LA$Wigan))
WiganCum_Factor <- 10^(floor(log10(WiganCum_Sigfig)))
WiganCum_Max <- round_any(sum(COVID19_by_LA$Wigan), WiganCum_Factor, f=ceiling)
WiganCum_Breaks <- ceiling(WiganCum_Factor/2)

plot_WiganCum <- ggplot(data=Wigan_data, aes(x=Wigan_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WiganCum_Max), 
                     breaks = seq(0, WiganCum_Max, WiganCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Wigan") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Wigan_figure <- ggarrange(plot_WiganNew + font("x.text", size = 8), 
                          plot_WiganCum + font("x.text", size = 8), 
                          ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Wigan_cases_plot.pdf", height = 8.27, width = 11.69)
Wigan_figure
dev.off()



#### Wiltshire plots ####
#Read in new cases per day from summarised data, format date correctly
Wiltshire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Wiltshire))
Wiltshire_data <- as.data.frame(lapply(Wiltshire_data, rep, Wiltshire_data$ntimes))


# Plot number of NEW CASES per day
WiltshireNew_Sigfig <- signif(max(COVID19_by_LA$Wiltshire))
WiltshireNew_Factor <- 10^(floor(log10(WiltshireNew_Sigfig)))
WiltshireNew_Max <- round_any(max(COVID19_by_LA$Wiltshire), WiltshireNew_Factor, f=ceiling)
WiltshireNew_Breaks <- ceiling(WiltshireNew_Factor/2)

plot_WiltshireNew <- ggplot(data=Wiltshire_data, aes(x=Wiltshire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WiltshireNew_Max), 
                     breaks = seq(0, WiltshireNew_Max, WiltshireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Wiltshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
WiltshireCum_Sigfig <- signif(sum(COVID19_by_LA$Wiltshire))
WiltshireCum_Factor <- 10^(floor(log10(WiltshireCum_Sigfig)))
WiltshireCum_Max <- round_any(sum(COVID19_by_LA$Wiltshire), WiltshireCum_Factor, f=ceiling)
WiltshireCum_Breaks <- ceiling(WiltshireCum_Factor/2)

plot_WiltshireCum <- ggplot(data=Wiltshire_data, aes(x=Wiltshire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WiltshireCum_Max), 
                     breaks = seq(0, WiltshireCum_Max, WiltshireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Wiltshire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Wiltshire_figure <- ggarrange(plot_WiltshireNew + font("x.text", size = 8), 
                              plot_WiltshireCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Wiltshire_cases_plot.pdf", height = 8.27, width = 11.69)
Wiltshire_figure
dev.off()



#### Windsor_and_Maidenhead plots ####
#Read in new cases per day from summarised data, format date correctly
Windsor_and_Maidenhead_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Windsor_and_Maidenhead))
Windsor_and_Maidenhead_data <- as.data.frame(lapply(Windsor_and_Maidenhead_data, rep, Windsor_and_Maidenhead_data$ntimes))


# Plot number of NEW CASES per day
Windsor_and_MaidenheadNew_Sigfig <- signif(max(COVID19_by_LA$Windsor_and_Maidenhead))
Windsor_and_MaidenheadNew_Factor <- 10^(floor(log10(Windsor_and_MaidenheadNew_Sigfig)))
Windsor_and_MaidenheadNew_Max <- round_any(max(COVID19_by_LA$Windsor_and_Maidenhead), Windsor_and_MaidenheadNew_Factor, f=ceiling)
Windsor_and_MaidenheadNew_Breaks <- ceiling(Windsor_and_MaidenheadNew_Factor/2)

plot_Windsor_and_MaidenheadNew <- ggplot(data=Windsor_and_Maidenhead_data, aes(x=Windsor_and_Maidenhead_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Windsor_and_MaidenheadNew_Max), 
                     breaks = seq(0, Windsor_and_MaidenheadNew_Max, Windsor_and_MaidenheadNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Windsor and Maidenhead") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
Windsor_and_MaidenheadCum_Sigfig <- signif(sum(COVID19_by_LA$Windsor_and_Maidenhead))
Windsor_and_MaidenheadCum_Factor <- 10^(floor(log10(Windsor_and_MaidenheadCum_Sigfig)))
Windsor_and_MaidenheadCum_Max <- round_any(sum(COVID19_by_LA$Windsor_and_Maidenhead), Windsor_and_MaidenheadCum_Factor, f=ceiling)
Windsor_and_MaidenheadCum_Breaks <- ceiling(Windsor_and_MaidenheadCum_Factor/2)

plot_Windsor_and_MaidenheadCum <- ggplot(data=Windsor_and_Maidenhead_data, aes(x=Windsor_and_Maidenhead_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, Windsor_and_MaidenheadCum_Max), 
                     breaks = seq(0, Windsor_and_MaidenheadCum_Max, Windsor_and_MaidenheadCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Windsor and Maidenhead") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Windsor_and_Maidenhead_figure <- ggarrange(plot_Windsor_and_MaidenheadNew + font("x.text", size = 8), 
                                           plot_Windsor_and_MaidenheadCum + font("x.text", size = 8), 
                                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Windsor_and_Maidenhead_cases_plot.pdf", height = 8.27, width = 11.69)
Windsor_and_Maidenhead_figure
dev.off()



#### Wirral plots ####
#Read in new cases per day from summarised data, format date correctly
Wirral_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Wirral))
Wirral_data <- as.data.frame(lapply(Wirral_data, rep, Wirral_data$ntimes))


# Plot number of NEW CASES per day
WirralNew_Sigfig <- signif(max(COVID19_by_LA$Wirral))
WirralNew_Factor <- 10^(floor(log10(WirralNew_Sigfig)))
WirralNew_Max <- round_any(max(COVID19_by_LA$Wirral), WirralNew_Factor, f=ceiling)
WirralNew_Breaks <- ceiling(WirralNew_Factor/2)

plot_WirralNew <- ggplot(data=Wirral_data, aes(x=Wirral_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WirralNew_Max), 
                     breaks = seq(0, WirralNew_Max, WirralNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Wirral") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
WirralCum_Sigfig <- signif(sum(COVID19_by_LA$Wirral))
WirralCum_Factor <- 10^(floor(log10(WirralCum_Sigfig)))
WirralCum_Max <- round_any(sum(COVID19_by_LA$Wirral), WirralCum_Factor, f=ceiling)
WirralCum_Breaks <- ceiling(WirralCum_Factor/2)

plot_WirralCum <- ggplot(data=Wirral_data, aes(x=Wirral_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WirralCum_Max), 
                     breaks = seq(0, WirralCum_Max, WirralCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Wirral") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Wirral_figure <- ggarrange(plot_WirralNew + font("x.text", size = 8), 
                           plot_WirralCum + font("x.text", size = 8), 
                           ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Wirral_cases_plot.pdf", height = 8.27, width = 11.69)
Wirral_figure
dev.off()



#### Wokingham plots ####
#Read in new cases per day from summarised data, format date correctly
Wokingham_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Wokingham))
Wokingham_data <- as.data.frame(lapply(Wokingham_data, rep, Wokingham_data$ntimes))


# Plot number of NEW CASES per day
WokinghamNew_Sigfig <- signif(max(COVID19_by_LA$Wokingham))
WokinghamNew_Factor <- 10^(floor(log10(WokinghamNew_Sigfig)))
WokinghamNew_Max <- round_any(max(COVID19_by_LA$Wokingham), WokinghamNew_Factor, f=ceiling)
WokinghamNew_Breaks <- ceiling(WokinghamNew_Factor/2)

plot_WokinghamNew <- ggplot(data=Wokingham_data, aes(x=Wokingham_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WokinghamNew_Max), 
                     breaks = seq(0, WokinghamNew_Max, WokinghamNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Wokingham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
WokinghamCum_Sigfig <- signif(sum(COVID19_by_LA$Wokingham))
WokinghamCum_Factor <- 10^(floor(log10(WokinghamCum_Sigfig)))
WokinghamCum_Max <- round_any(sum(COVID19_by_LA$Wokingham), WokinghamCum_Factor, f=ceiling)
WokinghamCum_Breaks <- ceiling(WokinghamCum_Factor/2)

plot_WokinghamCum <- ggplot(data=Wokingham_data, aes(x=Wokingham_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WokinghamCum_Max), 
                     breaks = seq(0, WokinghamCum_Max, WokinghamCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Wokingham") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Wokingham_figure <- ggarrange(plot_WokinghamNew + font("x.text", size = 8), 
                              plot_WokinghamCum + font("x.text", size = 8), 
                              ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Wokingham_cases_plot.pdf", height = 8.27, width = 11.69)
Wokingham_figure
dev.off()



#### Wolverhampton plots ####
#Read in new cases per day from summarised data, format date correctly
Wolverhampton_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Wolverhampton))
Wolverhampton_data <- as.data.frame(lapply(Wolverhampton_data, rep, Wolverhampton_data$ntimes))


# Plot number of NEW CASES per day
WolverhamptonNew_Sigfig <- signif(max(COVID19_by_LA$Wolverhampton))
WolverhamptonNew_Factor <- 10^(floor(log10(WolverhamptonNew_Sigfig)))
WolverhamptonNew_Max <- round_any(max(COVID19_by_LA$Wolverhampton), WolverhamptonNew_Factor, f=ceiling)
WolverhamptonNew_Breaks <- ceiling(WolverhamptonNew_Factor/2)

plot_WolverhamptonNew <- ggplot(data=Wolverhampton_data, aes(x=Wolverhampton_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WolverhamptonNew_Max), 
                     breaks = seq(0, WolverhamptonNew_Max, WolverhamptonNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Wolverhampton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
WolverhamptonCum_Sigfig <- signif(sum(COVID19_by_LA$Wolverhampton))
WolverhamptonCum_Factor <- 10^(floor(log10(WolverhamptonCum_Sigfig)))
WolverhamptonCum_Max <- round_any(sum(COVID19_by_LA$Wolverhampton), WolverhamptonCum_Factor, f=ceiling)
WolverhamptonCum_Breaks <- ceiling(WolverhamptonCum_Factor/2)

plot_WolverhamptonCum <- ggplot(data=Wolverhampton_data, aes(x=Wolverhampton_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WolverhamptonCum_Max), 
                     breaks = seq(0, WolverhamptonCum_Max, WolverhamptonCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Wolverhampton") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Wolverhampton_figure <- ggarrange(plot_WolverhamptonNew + font("x.text", size = 8), 
                                  plot_WolverhamptonCum + font("x.text", size = 8), 
                                  ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Wolverhampton_cases_plot.pdf", height = 8.27, width = 11.69)
Wolverhampton_figure
dev.off()



#### Worcestershire plots ####
#Read in new cases per day from summarised data, format date correctly
Worcestershire_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$Worcestershire))
Worcestershire_data <- as.data.frame(lapply(Worcestershire_data, rep, Worcestershire_data$ntimes))


# Plot number of NEW CASES per day
WorcestershireNew_Sigfig <- signif(max(COVID19_by_LA$Worcestershire))
WorcestershireNew_Factor <- 10^(floor(log10(WorcestershireNew_Sigfig)))
WorcestershireNew_Max <- round_any(max(COVID19_by_LA$Worcestershire), WorcestershireNew_Factor, f=ceiling)
WorcestershireNew_Breaks <- ceiling(WorcestershireNew_Factor/2)

plot_WorcestershireNew <- ggplot(data=Worcestershire_data, aes(x=Worcestershire_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WorcestershireNew_Max), 
                     breaks = seq(0, WorcestershireNew_Max, WorcestershireNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: Worcestershire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
WorcestershireCum_Sigfig <- signif(sum(COVID19_by_LA$Worcestershire))
WorcestershireCum_Factor <- 10^(floor(log10(WorcestershireCum_Sigfig)))
WorcestershireCum_Max <- round_any(sum(COVID19_by_LA$Worcestershire), WorcestershireCum_Factor, f=ceiling)
WorcestershireCum_Breaks <- ceiling(WorcestershireCum_Factor/2)

plot_WorcestershireCum <- ggplot(data=Worcestershire_data, aes(x=Worcestershire_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, WorcestershireCum_Max), 
                     breaks = seq(0, WorcestershireCum_Max, WorcestershireCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: Worcestershire") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
Worcestershire_figure <- ggarrange(plot_WorcestershireNew + font("x.text", size = 8), 
                                   plot_WorcestershireCum + font("x.text", size = 8), 
                                   ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/Worcestershire_cases_plot.pdf", height = 8.27, width = 11.69)
Worcestershire_figure
dev.off()



#### York plots ####
#Read in new cases per day from summarised data, format date correctly
York_data <- data.frame(Date=c(COVID19_by_LA$Date), ntimes=c(COVID19_by_LA$York))
York_data <- as.data.frame(lapply(York_data, rep, York_data$ntimes))


# Plot number of NEW CASES per day
YorkNew_Sigfig <- signif(max(COVID19_by_LA$York))
YorkNew_Factor <- 10^(floor(log10(YorkNew_Sigfig)))
YorkNew_Max <- round_any(max(COVID19_by_LA$York), YorkNew_Factor, f=ceiling)
YorkNew_Breaks <- ceiling(YorkNew_Factor/2)

plot_YorkNew <- ggplot(data=York_data, aes(x=York_data$Date)) +
  geom_histogram(binwidth=1, boundary = 1, colour="black", fill= "#E5E1EE") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, YorkNew_Max), 
                     breaks = seq(0, YorkNew_Max, YorkNew_Breaks),
                     expand = c(0, 0)) +
  ylab("Frequency") + xlab("Date") +
  ggtitle("New cases per day: York") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Plot CUMULATIVE CASES per day
YorkCum_Sigfig <- signif(sum(COVID19_by_LA$York))
YorkCum_Factor <- 10^(floor(log10(YorkCum_Sigfig)))
YorkCum_Max <- round_any(sum(COVID19_by_LA$York), YorkCum_Factor, f=ceiling)
YorkCum_Breaks <- ceiling(YorkCum_Factor/2)

plot_YorkCum <- ggplot(data=York_data, aes(x=York_data$Date)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth=1, 
                 boundary = 1, colour="black", fill= "#DFFDFF") +
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
               limits = c(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date)),
               breaks = seq(min(COVID19_by_LA$Date)-1, max(COVID19_by_LA$Date), 1)) +
  scale_y_continuous(limits = c(0, YorkCum_Max), 
                     breaks = seq(0, YorkCum_Max, YorkCum_Breaks), expand = c(0, 0)) +
  ylab("Cumulative frequency") + xlab("Date") +
  ggtitle("Cumulative cases: York") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())


# Generate figures
York_figure <- ggarrange(plot_YorkNew + font("x.text", size = 8), 
                         plot_YorkCum + font("x.text", size = 8), 
                         ncol = 1, nrow = 2, align = "hv")

pdf("Plot per local authority/York_cases_plot.pdf", height = 8.27, width = 11.69)
York_figure
dev.off()

#### Summary plot- 4 LA with highest cases 11/03/2020 #####

LA_figure <- ggarrange(plot_DevonCum + font("x.text", size = 8), 
                        plot_HampshireCum + font("x.text", size = 8), 
                        plot_HertfordshireCum + font("x.text", size = 8), 
                        plot_Kensington_and_ChelseaCum + font("x.text", size = 8), 
                        ncol = 2, nrow = 2, align = "hv")

Sum_figure <- annotate_figure(LA_figure,
                              top = text_grob("Local authorities with highest total cases, 11/03/2020", face = "bold", size = 14))

pdf("Plot per local authority/Highest_LA_plot.pdf", height = 8.27, width = 11.69)
Sum_figure
dev.off()

