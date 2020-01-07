library(tidyverse)
library(outliers)
library(ggplot2)
library(cowplot)
library(magrittr)
library(multipanelfigure)
library(stats)
library(RODBC)
library(lubridate)
library(reshape)
library(ggpubr)


# WEEKLY AVERAGES------------------------------------------------------------------
# the average time spent per week by each team on the different ticket types 

ALL_weekly_avg <- ALL %>% group_by(Project,Year, Week, Team) %>% summarise(total = sum(TIMESPENT, na.rm = TRUE)) %>%
  group_by(Project, Year, Team) %>% summarise(avg = round(median(total), digits = 2))

ALL_weekly_avg <- split(ALL_weekly_avg, ALL_weekly_avg$Team)


for(e in names(ALL_weekly_avg)){
  
  ALL_weekly_avg[[e]] <- cast(ALL_weekly_avg[[e]], Year + Team ~ Project)
  ALL_weekly_avg[[e]][nrow(ALL_weekly_avg[[e]]) + 1,] <- c("Average per Week", "-",
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[1],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[2],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[3],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[4],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[5],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[6],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[7],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[8],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[9],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[10],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[11],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[12],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[13],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[14],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[15],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[16],
                                                           round(colMeans(ALL_weekly_avg[[e]], na.rm = TRUE), digits = 2)[17])
  }


# Weekly Percentages ------------------------------------------------------
# the avergae percentage of each week spent by each team on the different ticket types

ALL_weekly_perc <- ALL %>% group_by(Team, Project,Year, Week) %>% summarise(total = sum(TIMESPENT, na.rm = TRUE)) %>%
  group_by(Project, Year, Team) %>% summarise(perc = median(total)) %>% group_by(Year,Team) %>%
  mutate(perc1 = round(perc/sum(perc) * 100, digits = 0))

ALL_weekly_perc <- split(ALL_weekly_perc, ALL_weekly_perc$Team)


for(e in names(ALL_weekly_perc)){
  
  ALL_weekly_perc[[e]] <- cast(ALL_weekly_perc[[e]], Year + Team ~ Project)
  ALL_weekly_perc[[e]][nrow(ALL_weekly_perc[[e]]) + 1,] <- c("Average per Week", "-",
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[1],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[2],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[3],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[4],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[5],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[6],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[7],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[8],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[9],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[10],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[11],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[12],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[13],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[14],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[15],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[16],
                                                             colMeans(ALL_weekly_perc[[e]],na.rm = TRUE)[17])
  
  ALL_weekly_perc[[e]][1:nrow(ALL_weekly_perc[[e]]),c(3:ncol(ALL_weekly_perc[[e]]))] <- sapply(ALL_weekly_perc[[e]][1:nrow(ALL_weekly_perc[[e]]),
                                                                                           c(3:ncol(ALL_weekly_perc[[e]]))],paste, '%', sep = "" )
  }







# 
# x <- ALL_weekly_avg[grepl("2015", ALL_weekly_avg$Year) & grepl("TS", ALL_weekly_avg$Team) & 
#                       grepl("OP", ALL_weekly_avg$Project),]
# 
# # shapiro.test(x$total)
# # ggqqplot(x$total)
# hist(x$total)
# median(x$total)
# mean(x$total)