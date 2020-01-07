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

ALL <- procs[!grepl("TD|VD|EXP", procs$Project),]

# 
# 
# colnames(ALL)[10] <- 'month2'
# ALL$Month <- month(ALL$CREATED, label = TRUE)


# TOTALS------------------------------------------------------------------

#_____________________________________________ MONTHLY _____________________________________________


ALL_monthly_totals <- ALL %>% group_by(Project,Year,Month,  Team) %>% summarise(total = round(sum(TIMESPENT, na.rm = TRUE), digits = 1))
ALL_monthly_totals <- split(ALL_monthly_totals, list(ALL_monthly_totals$Year, ALL_monthly_totals$Team))

for(e in names(ALL_monthly_totals)){
  
  ALL_monthly_totals[[e]] <- cast(ALL_monthly_totals[[e]], Year + Month +Team ~ Project)
  ALL_monthly_totals[[e]][nrow(ALL_monthly_totals[[e]]) + 1,] <- c("Average per Month", "-",'-',
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[1], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[2], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[3], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[4], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[5], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[6], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[7], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[8], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[9], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[10], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[11], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[12], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[13], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[14], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[15], digits = 2),
                                                   round(colMeans(ALL_monthly_totals[[e]], na.rm = TRUE)[16], digits = 2))
  # 
  # tail(ALL_monthly_totals[[e]][4:ncol(ALL_monthly_totals[[e]])],-12) <- round(tail(ALL_monthly_totals[[e]][4:ncol(ALL_monthly_totals[[e]])],-12),
  #                                                    digits = 2)
}



ALL_quarterly_totals <- ALL %>% group_by(Project,Year,Quarter,Team) %>% summarise(total = round(sum(TIMESPENT, na.rm = TRUE), digits = 1))
ALL_quarterly_totals <- split(ALL_quarterly_totals, list(ALL_quarterly_totals$Year, ALL_quarterly_totals$Team))

for(e in names(ALL_quarterly_totals)){
  
  ALL_quarterly_totals[[e]] <- cast(ALL_quarterly_totals[[e]], Year + Quarter +Team ~ Project)
  ALL_quarterly_totals[[e]][nrow(ALL_quarterly_totals[[e]]) + 1,] <- c("Average per Quarter", "-",'-',
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[1], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[2], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[3], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[4], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[5], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[6], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[7], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[8], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[9], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[10], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[11], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[12], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[13], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[14], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[15], digits = 2),
                                                                       round(colMeans(ALL_quarterly_totals[[e]], na.rm = TRUE)[16], digits = 2))
  # 
  #   tail(ALL_quarterly_totals[[e]][4:ncol(ALL_quarterly_totals[[e]])],-12) <- round(tail(ALL_quarterly_totals[[e]][4:ncol(ALL_quarterly_totals[[e]])],-12),
  #                                                      digits = 2)
}

# AVERAGES ----------------------------------------------------------------


# _____________________________________________Monthly averages _____________________________________________

ALL_monthly_avg <- procs[!grepl("VD|TD|EXP", procs$Project),]

ALL_monthly_avg <- ALL_monthly_avg %>% group_by(ID, Team, Project, CIDRAS, Year, Quarter, Month, Week) %>% summarise(x = sum(TIMESPENT)) %>%
  group_by(Team, Project, Year, Month) %>% summarise(timespent = sum(x)) %>% group_by(Team, Project, Year) %>%
  summarise(avg = round(mean(timespent), digits = 1))



ALL_monthly_avg <- split(ALL_monthly_avg, ALL_monthly_avg$Team)

for(e in names(ALL_monthly_avg)){
  
  ALL_monthly_avg[[e]] <- cast(ALL_monthly_avg[[e]], Year + Team  ~ Project)
  ALL_monthly_avg[[e]][nrow(ALL_monthly_avg[[e]]) + 1,] <- c("Average per Month", "-",
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[1],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[2],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[3],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[4],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[5],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[6],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[7],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[8],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[9],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[10],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[11],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[12],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[13],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[14],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[15],
                                 round(colMeans(ALL_monthly_avg[[e]], na.rm = TRUE),digits = 2)[16])
}


# _____________________________________________Average monthly percentages _____________________________________________


ALL_monthly_perc <- ALL %>% group_by(Team, Project,Year, Week) %>% summarise(total = sum(TIMESPENT, na.rm = TRUE)) %>%
  group_by(Project, Year, Team) %>% summarise(perc = median(total)) %>% group_by(Year,Team) %>%
  mutate(perc1 = round(perc/sum(perc) * 100, digits = 0))

ALL_monthly_perc <- split(ALL_monthly_perc, ALL_monthly_perc$Team)


for(e in names(ALL_monthly_perc)){
  
  ALL_monthly_perc[[e]] <- cast(ALL_monthly_perc[[e]], Year + Team ~ Project)
  ALL_monthly_perc[[e]][nrow(ALL_monthly_perc[[e]]) + 1,] <- c("Average per Week", "-",
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[1],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[2],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[3],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[4],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[5],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[6],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[7],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[8],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[9],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[10],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[11],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[12],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[13],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[14],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[15],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[16],
                                                             colMeans(ALL_monthly_perc[[e]],na.rm = TRUE)[17])
  
  ALL_monthly_perc[[e]][1:nrow(ALL_monthly_perc[[e]]),c(3:ncol(ALL_monthly_perc[[e]]))] <- sapply(ALL_monthly_perc[[e]][1:nrow(ALL_monthly_perc[[e]]),
                                                                                                                    c(3:ncol(ALL_monthly_perc[[e]]))],paste, '%', sep = "" )
}


# _____________________________________________Quarterly avg _____________________________________________

ALL_quarterly_avg <- procs

ALL_quarterly_avg <- ALL_quarterly_avg %>% group_by(ID, Team, Project, CIDRAS, Year, Quarter, Month, Week) %>% summarise(x = sum(TIMESPENT)) %>%
  group_by(Team, Project, Year, Quarter) %>% summarise(timespent = sum(x)) %>% group_by(Team, Project, Year) %>%
  summarise(avg = round(mean(timespent), digits = 1))
ALL_quarterly_avg <- ALL_quarterly_avg[!grepl("EX|TD|VD", ALL_quarterly_avg$Project),]

ALL_quarterly_avg <- split(ALL_quarterly_avg, ALL_quarterly_avg$Team)

for(e in names(ALL_quarterly_avg)){
  
  ALL_quarterly_avg[[e]] <- cast(ALL_quarterly_avg[[e]], Year + Team  ~ Project)
  ALL_quarterly_avg[[e]][nrow(ALL_quarterly_avg[[e]]) + 1,] <- c("Average per Quarter", "-",
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[1],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[2],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[3],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[4],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[5],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[6],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[7],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[8],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[9],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[10],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[11],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[12],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[13],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[14],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[15],
                                                                 round(colMeans(ALL_quarterly_avg[[e]], na.rm = TRUE),digits = 2)[16])
}


# _____________________________________________Quarterly average percentages _____________________________________________

ALL_quarterly_perc <- ALL %>% group_by(Team, Project,Year, Week) %>% summarise(total = sum(TIMESPENT, na.rm = TRUE)) %>%
  group_by(Project, Year, Team) %>% summarise(perc = median(total)) %>% group_by(Year,Team) %>%
  mutate(perc1 = round(perc/sum(perc) * 100, digits = 0))

ALL_quarterly_perc <- split(ALL_quarterly_perc, ALL_quarterly_perc$Team)


for(e in names(ALL_quarterly_perc)){
  
  ALL_quarterly_perc[[e]] <- cast(ALL_quarterly_perc[[e]], Year + Team ~ Project)
  ALL_quarterly_perc[[e]][nrow(ALL_quarterly_perc[[e]]) + 1,] <- c("Average per Week", "-",
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[1],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[2],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[3],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[4],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[5],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[6],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[7],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[8],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[9],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[10],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[11],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[12],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[13],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[14],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[15],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[16],
                                                                   colMeans(ALL_quarterly_perc[[e]],na.rm = TRUE)[17])
  
  ALL_quarterly_perc[[e]][1:nrow(ALL_quarterly_perc[[e]]),c(3:ncol(ALL_quarterly_perc[[e]]))] <- sapply(ALL_quarterly_perc[[e]][1:nrow(ALL_quarterly_perc[[e]]),
                                                                                                                                c(3:ncol(ALL_quarterly_perc[[e]]))],paste, '%', sep = "" )
}


# rough work --------------------------------------------------------------


# checking the normality of UAT Dev 2018 dataframe variable.
# arbirtary choice to check for normality
shapiro.test(as.numeric(ALL_monthly_totals[["2018.Dev"]]$UAT[]))
shapiro.test(as.numeric(ALL_monthly_totals[["2018.Dev"]][1:nrow(ALL_monthly_totals[["2018.Dev"]])-1, ncol(ALL_monthly_totals[["2018.Dev"]])]))
x <- as.numeric(ALL_monthly_totals[["2018.Dev"]][1:nrow(ALL_monthly_totals[["2018.Dev"]])-1, ncol(ALL_monthly_totals[["2018.Dev"]])])
hist(x)
median(as.numeric(ALL_monthly_totals[["2018.Dev"]][1:nrow(ALL_monthly_totals[["2018.Dev"]])-1, ncol(ALL_monthly_totals[["2018.Dev"]])]))
mean(as.numeric(ALL_monthly_totals[["2018.Dev"]][1:nrow(ALL_monthly_totals[["2018.Dev"]])-1, ncol(ALL_monthly_totals[["2018.Dev"]])]))
ggqqplot(as.numeric(ALL_monthly_totals[["2018.Dev"]][1:nrow(ALL_monthly_totals[["2018.Dev"]])-1, ncol(ALL_monthly_totals[["2018.Dev"]])]))

sort(as.numeric(ALL_monthly_totals[["2018.Dev"]][1:nrow(ALL_monthly_totals[["2018.Dev"]])-1, ncol(ALL_monthly_totals[["2018.Dev"]])]))

x <- ALL_monthly_totals[["2018.Dev"]]
x[1:nrow(x)-1, ncol(x)]

## checking to see how many developers are in the main dataframe in 2018 
# there are hours missing; should be around 3000 hours per month for 25 developers. 
# there is ~2000 per month
setwd("C:/Users/rcurran.GARTANTECH/Documents/KPI_Novemeber")
y <- read.csv("team_members.csv", stringsAsFactors =  FALSE) 
colnames(y)[1] <- 'AUTHOR'
x <- subset(df, df$Year == 2018)
x <- left_join(x, y, by = 'AUTHOR') 
x <- unique(x[c("AUTHOR", "Team")])
nrow(subset(x, x$Team == "Dev"))

x <- procs

length(unique(x$Project))

x_projects <- function(proj_d,proj_u) {
  
  x$Project[grepl(paste(proj_d), x$Project)] <<- paste(proj_u)
  
}

x_projects(proj_d = paste('CID'), proj_u = 'CID')
x_projects(proj_d = paste('RAS'), proj_u = 'RAS')
x_projects(proj_d = paste('VR|TR'), proj_u = 'Rollout')
x_projects(proj_d = paste('VU|TU'), proj_u = 'UAT')
x_projects(proj_d = paste('OP'), proj_u = 'OP')
x_projects(proj_d = paste('FS'), proj_u = 'FSi')

x <- x %>% group_by(ID, Team, Project, CIDRAS, Year, Quarter, Month, Week) %>% summarise(x = sum(TIMESPENT)) %>%
  group_by(Team, Project, Year, Month) %>% summarise(timespent = sum(x)) %>% group_by(Team, Project, Year) %>%
  summarise(avg = round(mean(timespent), digits = 1))
x <- x[!grepl("EX|TD|VD", x$Project),]
 
sum(procs[procs$Team == "Dev" & procs$Project == "RTL - RAS" & procs$Year == "2017",]$TIMESPENT)
