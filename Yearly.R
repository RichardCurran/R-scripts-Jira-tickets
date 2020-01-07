library(tidyverse)
library(outliers)
library(ggplot2)
library(cowplot)
library(magrittr)
library(stats)
library(RODBC)
library(lubridate)
library(reshape)
library(ggpubr)

ALL <- procs[!grepl("TD|VD|EXP", procs$Project),]

ALL <- ALL %>% group_by(ID, Team, Project,Year, Quarter, Month, Week) %>% summarise(TIMESPENT = sum(TIMESPENT))

# totals ------------------------------------------------------------------

ALL_totals <- ALL %>% group_by(Project,Year, Team) %>% summarise(total = round(sum(TIMESPENT, na.rm = TRUE), digits = 0))
ALL_totals <- split(ALL_totals, ALL_totals$Team)

for(e in names(ALL_totals)){
  
  ALL_totals[[e]] <- cast(ALL_totals[[e]], Year + Team ~ Project)
  ALL_totals[[e]][nrow(ALL_totals[[e]]) + 1,] <- c("Average per year", "-",
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[1],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[2],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[3],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[4],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[5],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[6],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[7],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[8],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[9],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[10],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[11],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[12],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[13],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[14],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[15],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[16],
                                                   colMeans(ALL_totals[[e]], na.rm = TRUE)[17])
  
  }


# percentages -------------------------------------------------------------


ALL_perc <- ALL %>% group_by(Team,Year, Project) %>%
  summarise(total = sum(TIMESPENT, na.rm = TRUE))  %>% group_by(Year, Team) %>%
  mutate(perc = round(total/sum(total) * 100, digits = 0))

ALL_perc <- split(ALL_perc, ALL_perc$Team)


for(e in names(ALL_perc)){
  
  ALL_perc[[e]] <- cast(ALL_perc[[e]], Year + Team ~ Project)
  ALL_perc[[e]][nrow(ALL_perc[[e]]) + 1,] <- c("Average per year", "-",
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[1],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[2],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[3],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[4],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[5],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[6],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[7],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[8],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[9],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[10],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[11],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[12],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[13],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[14],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[15],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[16],
                                               round(colMeans(ALL_perc[[e]], na.rm = TRUE),digits = 0)[17])
  
  ALL_perc[[e]][1:nrow(ALL_perc[[e]]),c(3:ncol(ALL_perc[[e]]))] <- sapply(ALL_perc[[e]][1:nrow(ALL_perc[[e]]),
                                                                      c(3:ncol(ALL_perc[[e]]))],paste, '%', sep = "" )
  }


# Average hours spent per ticket ------------------------------------------

ALL_avg <- ALL %>% group_by(ID, Project, Team, Year) %>% summarise(x = sum(TIMESPENT)) %>% 
  group_by(Year, Team, Project) %>%
  summarise(Avg. = round(median(x), digits = 2))
  
ALL_avg <- split(ALL_avg, ALL_avg$Team)

for(e in names(ALL_avg)){
  
  ALL_avg[[e]] <- cast(ALL_avg[[e]], Year + Team ~ Project)
  ALL_avg[[e]][nrow(ALL_avg[[e]]) + 1,] <- c("Average per year", "-",
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[1],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[2],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[3],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[4],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[5],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[6],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[7],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[8],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[9],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[10],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[11],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[12],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[13],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[14],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[15],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[16],
                                             round(colMeans(ALL_avg[[e]], na.rm = TRUE), digits = 2)[17])
                                             
                                                  
                                             
  
  }



# rough work --------------------------------------------------------------




y <- ALL %>% group_by(ID, Project, Team, Year) %>% summarise(x = sum(TIMESPENT)) %>% group_by(Year, Team, Project) %>%
  summarise(x = median(x))

y <- y[grepl("BA", y$Team) & grepl("2016", y$Year) & grepl("OP", y$Project),] 

median(y$x)

hist(y$x)
y <- split(ALL, ALL$Team)
y <- y[["BA"]][grepl("2018", y[["BA"]]$Year),]
y <- split(y, y$Project)
y <- y[["CID"]]
hist(y$TIMESPENT)

shapiro.test(y$TIMESPENT)

y <- ALL %>% group_by(Team, Year, Project) %>% summarise(x = sum(TIMESPENT), avg = median(TIMESPENT)) %>% group_by(Year) %>% 
  mutate(perc = round(x/sum(x)*100, digits = 0))

