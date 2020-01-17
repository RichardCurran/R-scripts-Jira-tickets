library(tidyverse)
library(outliers)
library(ggplot2)
library(cowplot)
library(magrittr)
library(multipanelfigure)
library(stats)
library(reshape)

KPIS_Processes <- list(ALL, ALL_totals, ALL_avg, ALL_perc, ALL_quarterly_totals, ALL_quarterly_avg, ALL_quarterly_perc,
                       ALL_monthly_totals, ALL_monthly_avg, ALL_monthly_perc, ALL_weekly_avg, ALL_weekly_perc)

names(KPIS_Processes) <- c("ALL", "ALL_totals", "ALL_avg", "ALL_perc", "ALL_quarterly_totals", "ALL_quarterly_avg", "ALL_quarterly_perc",
                           "ALL_monthly_totals", "ALL_monthly_avg", "ALL_monthly_perc", "ALL_weekly_avg", "ALL_weekly_perc")

library(reshape)


# this is df of all developers time logged 
x <- procs[grepl("Dev", procs$Team),]
x <- x[!grepl("VD|TD|EXP", x$Project),]
## run the following 4 lines to get avg. developers per Live application
x <- procs[grepl("Dev", procs$Team),]
x <- x[!grepl("VD|TD|EXP|OP|Inter|SP", x$Project),]
x$Project[grepl("AV", x$Project)] <- "Availability"
x$Project[grepl("PT", x$Project)] <- "Payroll"
x$Project[grepl("RT", x$Project)] <- "Roster"

## total employess per module per year----------------
dev_year <- x %>% 
  group_by(Project, Year) %>% 
  summarise("Total Devs" = length(unique(AUTHOR))) %>%
  group_by(Project, Year) %>% 
  summarise(avg_dev = round(mean(`Total Devs`), digits =0))
dev_year <- cast(dev_year, Year ~ Project)


## avg dev. per module per week----------------
dev_week <- x %>% 
group_by(Project, Year, Week) %>% 
  summarise("Total Devs" = length(unique(AUTHOR))) %>%
  group_by(Project, Year) %>% 
  summarise(avg_dev = round(mean(`Total Devs`), digits = 0))
dev_week <- cast(dev_week, Year ~ Project)
write.csv(dev_week, "dev_week.csv", row.names = FALSE)
## avg dev. per module per month 
dev_month <- x %>% 
  group_by(Project, Year,  Month) %>% 
  summarise("Total Devs" = length(unique(AUTHOR))) %>%
  group_by(Project, Year) %>% 
  summarise(avg_dev = round(mean(`Total Devs`), digits = 0))
dev_month <- cast(dev_month, Year ~ Project)


## avg dev. per module per quarter 
dev_quarter <- x %>% 
  group_by(Project, Year, Quarter) %>% 
  summarise("Total Devs" = length(unique(AUTHOR))) %>%
  group_by(Project, Year) %>% 
  summarise(avg_dev = round(mean(`Total Devs`), digits = 0))
dev_quarter <- cast(dev_quarter, Year ~ Project)

dev_all_apps <- list("dev_week" = dev_week, "dev_month" = dev_month, "dev_quarter" = dev_quarter, 
                     "dev_year" = dev_year)

dev_live_apps <- list("dev_week" = dev_week, "dev_month" = dev_month, "dev_quarter" = dev_quarter, 
                     "dev_year" = dev_year)

rm(dev_month, dev_quarter, dev_week, dev_year)


y <- procs[grepl("RTL - RAS", procs$Project)  & procs$Year == 2018 & procs$Team == "Dev",] %>%
  summarise(x = length(unique(AUTHOR)))


