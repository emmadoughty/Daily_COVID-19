## Exploration of UK COVID-19 data: Proportion of positive tests
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

#### Load data ####
COVID19_by_day <- read_csv("Data/COVID19_by_day.csv")
COVID19_by_day$Date <- as.Date(COVID19_by_day$Date, format = "%d/%m/%y")
COVID19_by_day <- COVID19_by_day[order(COVID19_by_day$Date),]

df_NewPos <- data.frame(Date = COVID19_by_day$Date,
                          NewCases = COVID19_by_day$NewCases,
                          CumCases = COVID19_by_day$CumCases,
                          NewTests = COVID19_by_day$NewTests,
                          CumTests = COVID19_by_day$CumTests,
                          TestStat = "Per day",
                          StatValue = (100 * (COVID19_by_day$NewCases / COVID19_by_day$NewTests)),
                          stringsAsFactors = FALSE)


df_CumPos <- data.frame(Date = COVID19_by_day$Date,
                        NewCases = COVID19_by_day$NewCases,
                        CumCases = COVID19_by_day$CumCases,
                        NewTests = COVID19_by_day$NewTests,
                        CumTests = COVID19_by_day$CumTests,
                        TestStat = "Cumulatively",
                        StatValue = (100 * (COVID19_by_day$CumCases / COVID19_by_day$CumTests)),
                        stringsAsFactors = FALSE)

df_PosTests <- rbind(df_NewPos, df_CumPos)

PosTests_Factor <- 10^(floor(log10(signif(max(df_PosTests$StatValue)))))
PosTests_Breaks <- ceiling(PosTests_Factor/2)

plot_PosTests <- ggplot(data=df_PosTests, aes(x=Date, y=df_PosTests$StatValue, group=TestStat)) +
  geom_line(aes(color=TestStat))+
  geom_point()+
  scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 1), 
               limits = c(min(df_PosTests$Date), max(df_PosTests$Date)),
               breaks = seq(min(df_PosTests$Date), max(df_PosTests$Date), 1)) +
  scale_y_continuous(limits = c(0, 33), 
                     breaks = seq(0, 33, 3),
                     expand = c(0, 0)) +
  ylab("Positive cases (%)") + xlab("") +
  ggtitle("% postive tests") +
  theme_minimal() +
  theme(plot.title = element_text(size=13, face="bold"),
        legend.title = element_blank(),
        legend.position = "bottom", 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1, size = 7), 
        panel.grid.minor = element_blank())

ggsave("PosTests_plot.png")

# 
# 
# 
# df_TestNeg <- data.frame(Date = COVID19_by_day$Date,
#                        TestRes = "Negative",
#                        StatValue = COVID19_by_day$NewTests - COVID19_by_day$NewCases,
#                        stringsAsFactors = FALSE)
# df_TestNeg <- as.data.frame(lapply(df_TestNeg, rep, df_TestNeg$StatValue))
# 
# df_TestPos <- data.frame(Date = COVID19_by_day$Date,
#                       TestRes = "Positive",
#                       StatValue = COVID19_by_day$NewCases,
#                       stringsAsFactors = FALSE)
# df_TestPos <- as.data.frame(lapply(df_TestPos, rep, df_TestPos$StatValue))
# 
# df_TestRes <- rbind(df_TestNeg, df_TestPos)    
# 
# TestRes_Sigfig <- signif(max(df_TestRes$StatValue))
# TestRes_Factor <- 10^(floor(log10(TestRes_Sigfig)))
# TestRes_Max <- round_any(max(df_TestRes$StatValue), TestRes_Factor, f=ceiling)
# TestRes_Breaks <- ceiling(TestRes_Factor/2)
# 
# plot_TestRes <- ggplot(data=df_TestRes, aes(x=df_TestRes$Date, fill=df_TestRes$TestRes)) +
#   geom_histogram(binwidth=1, boundary = 1, colour="black") +
#   scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
#                limits = c(min(df_TestRes$Date)-1, max(df_TestRes$Date)),
#                breaks = seq(min(df_TestRes$Date)-1, max(df_TestRes$Date), 1)) +
#   scale_y_continuous(limits = c(0, TestRes_Max), 
#                      breaks = seq(0, TestRes_Max, TestRes_Breaks),
#                      expand = c(0, 0)) +
#   ylab("Frequency") + xlab("Date") +
#   ggtitle("UK SARS-CoV2 tests per day") +
#   theme_minimal() +
#   theme(plot.title = element_text(size=13, face="bold"),
#         legend.title = element_blank(),
#         legend.position = "bottom", 
#         axis.line = element_line(colour = "black"),
#         axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
#         panel.grid.minor = element_blank())
# 
# 
# TestPos_Sigfig <- signif(max(df_TestPos$StatValue))
# TestPos_Factor <- 10^(floor(log10(TestPos_Sigfig)))
# TestPos_Max <- round_any(max(df_TestPos$StatValue), TestPos_Factor, f=ceiling)
# TestPos_Breaks <- ceiling(TestPos_Factor/2)
# 
# plot_TestPos <- ggplot(data=df_TestPos, aes(x=df_TestPos$Date, fill=df_TestPos$TestPos)) +
#   geom_histogram(binwidth=1, boundary = 1, colour="black") +
#   scale_x_date(labels = date_format("%d/%m/%y"), expand = c(0, 0), 
#                limits = c(min(df_TestPos$Date)-1, max(df_TestPos$Date)),
#                breaks = seq(min(df_TestPos$Date)-1, max(df_TestPos$Date), 1)) +
#   scale_y_continuous(limits = c(0, TestPos_Max), 
#                      breaks = seq(0, TestPos_Max, TestPos_Breaks),
#                      expand = c(0, 0)) +
#   ylab("Frequency") + xlab("Date") +
#   ggtitle("UK SARS-CoV2 tests per day") +
#   theme_minimal() +
#   theme(plot.title = element_text(size=13, face="bold"),
#         legend.title = element_blank(),
#         legend.position = "bottom", 
#         axis.line = element_line(colour = "black"),
#         axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1), 
#         panel.grid.minor = element_blank())
# 
# 
# 
# 
