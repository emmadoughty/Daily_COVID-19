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
COVID19_by_LA <- read_csv("COVID19_by_LA.csv")

COVID19_by_LA$Date <- as.Date(COVID19_by_LA$Date, format = "%d/%m/%y")
COVID19_by_LA <- COVID19_by_LA[order(COVID19_by_LA$Date),]



#### Barnet plots ####
# Read in new cases per day from summarised data, format date correctly
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
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
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
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
        panel.grid.minor = element_blank())

Barnet_figure <- ggarrange(plot_BarnetNew + font("x.text", size = 6), 
                    plot_CumCases + font("x.text", size = 6), 
                    ncol = 1, nrow = 2, align = "hv")

pdf("Barnet_cases_plot.pdf", height = 8.27, width = 11.69)
Barnet_figure
dev.off()

# For transposed data (columns= date, rows=locations), ateempting (failed) to produce stacked histogram
# Mar7th_by_LA <- data.frame(Date="07/03/20", ntimes=c(COVID19_by_LA$`07/03/2020`), location=c(COVID19_by_LA$Location))
# Mar7th_by_LA <- as.data.frame(lapply(Mar7th_by_LA, rep, Mar7th_by_LA$ntimes))
# 
# Mar8th_by_LA <- data.frame(Date="08/03/20", ntimes=c(COVID19_by_LA$`08/03/2020`), location=c(COVID19_by_LA$Location))
# Mar8th_by_LA <- as.data.frame(lapply(Mar8th_by_LA, rep, Mar8th_by_LA$ntimes))
# 
# Mar9th_by_LA <- data.frame(Date="09/03/20", ntimes=c(COVID19_by_LA$`09/03/2020`), location=c(COVID19_by_LA$Location))
# Mar9th_by_LA <- as.data.frame(lapply(Mar9th_by_LA, rep, Mar9th_by_LA$ntimes))
# 
# Mar10th_by_LA <- data.frame(Date="10/03/20", ntimes=c(COVID19_by_LA$`10/03/2020`), location=c(COVID19_by_LA$Location))
# Mar10th_by_LA <- as.data.frame(lapply(Mar10th_by_LA, rep, Mar10th_by_LA$ntimes))
# 
# Cases_by_LA <- rbind(Mar7th_by_LA, Mar8th_by_LA, Mar9th_by_LA, Mar10th_by_LA)
# 
# Cases_by_LA$Date <- as.Date(Cases_by_LA$Date, format = "%d/%m/%y")
# Cases_by_LA <- Cases_by_LA[order(Cases_by_LA$Date),]
# 
# 
# plot_LA <- ggplot(data=Cases_by_LA, aes(fill= Cases_by_LA$location, x=Cases_by_LA$Date)) +
#   geom_histogram(aes(y = cumsum(..count..)), binwidth=1, boundary = 1, 
#                  colour="black") +
#   scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
#                limits = c(min(Cases_by_LA$Date), max(Cases_by_LA$Date)),
#                breaks = seq(min(Cases_by_LA$Date), max(Cases_by_LA$Date), 1)) +
#   scale_y_continuous(limits = c(0,400), 
#                      breaks = seq(0, 400, 20),
#                      expand = c(0, 0)) +
#   ylab("Frequency") + xlab("Date") +
#   theme_minimal() +
#   theme(axis.line = element_line(colour = "black"),
#         axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
#         panel.grid.minor = element_blank())


