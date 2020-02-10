library(tidyverse)
library(outliers)
library(ggplot2)
library(cowplot)
library(magrittr)
library(multipanelfigure)
library(stats)
library(reshape)

# KPIS_Processes <- list(ALL, ALL_totals, ALL_avg, ALL_perc, ALL_quarterly_totals, ALL_quarterly_avg, ALL_quarterly_perc,
#                        ALL_monthly_totals, ALL_monthly_avg, ALL_monthly_perc, ALL_weekly_avg, ALL_weekly_perc)
# 
# names(KPIS_Processes) <- c("ALL", "ALL_totals", "ALL_avg", "ALL_perc", "ALL_quarterly_totals", "ALL_quarterly_avg", "ALL_quarterly_perc",
#                            "ALL_monthly_totals", "ALL_monthly_avg", "ALL_monthly_perc", "ALL_weekly_avg", "ALL_weekly_perc")
# 
# rm(ALL, ALL_totals, ALL_avg, ALL_perc, ALL_quarterly_totals, ALL_quarterly_avg, ALL_quarterly_perc,
#     ALL_monthly_totals, ALL_monthly_avg, ALL_monthly_perc, ALL_weekly_avg, ALL_weekly_perc)


## rough work
y <- procs[grepl("2018", procs$Year) & grepl("3", procs$Quarter) & grepl("QA", procs$Team),] %>% group_by(Project) %>%
  summarise(x = length(unique(AUTHOR)))
unique(y$AUTHOR)
table(x$Project, x$AUTHOR)


x <- x %>% group_by(Project, Quarter) %>% summarise(x = sum(TIMESPENT))
mean(x$x)


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

## avg dev. per module per month ----------------
dev_month <- x %>% 
  group_by(Project, Year,  Month) %>% 
  summarise("Total Devs" = length(unique(AUTHOR))) %>%
  group_by(Project, Year) %>% 
  summarise(avg_dev = round(mean(`Total Devs`), digits = 0))
dev_month <- cast(dev_month, Year ~ Project)


## avg dev. per module per quarter ----------------
dev_quarter <- x %>% 
  group_by(Project, Year, Quarter) %>% 
  summarise("Total Devs" = length(unique(AUTHOR))) %>%
  group_by(Project, Year) %>% 
  summarise(avg_dev = round(mean(`Total Devs`), digits = 0))
dev_quarter <- cast(dev_quarter, Year ~ Project)


## list of each dev. dataframe

dev_all_apps <- list("Totals dev per year" = dev_year,"avg. dev per quarter" = dev_quarter, "avg. dev per month" = dev_month,
                     "avg.dev per week" = dev_week)

dev_live_apps <- list( "Totals dev per year" = dev_year,"avg. dev per quarter" = dev_quarter, "avg. dev per month" = dev_month,
                       "avg.dev per week" = dev_week)

rm(dev_month, dev_quarter, dev_week, dev_year)


# Devs per Quarter: total devs per quarter, yer and project ---------------
#dev & qa per quarter
dev_quarter <- function(a){
  x <- procs[grepl(paste(a), procs$Team) & !grepl("VD|TD|EXP", procs$Project),]
  dev_quarter <- x %>% 
    group_by(Year, Quarter, Project,Team) %>% 
    summarise("Total" = length(unique(AUTHOR)))
  
  x <- split(dev_quarter, dev_quarter$Year)
  for(e in names(x)){
    x[[e]] <- cast(x[[e]], Year + Quarter ~ Project)
  }
  
  assign(paste(a), x, envir = .GlobalEnv)
  

  
}

#dev & qa per ticket type per quarter
team_total <- function (a, b){
  
  x <- procs[grepl(paste(a), procs$Team) & !grepl("VD|TD|EXP", procs$Project),]
  dev_quarter <- x %>% 
    group_by(Year, Quarter, Team) %>% 
    summarise("Total" = length(unique(AUTHOR)))
  dev_quarter <- cast(dev_quarter, Quarter ~ Year)
  
  assign(paste(b), dev_quarter, envir = .GlobalEnv)
  write.csv(dev_quarter, paste(b, ".csv"))
}

#excel function
library(xlsx)
wb <- createWorkbook()
sheetone <- createSheet(wb,"Dev")
sheet1 <- createSheet(wb,"QA")
currRow <- 1
xl_sheet <- function(a, p){
  
  for(i in 1:length(a)){
    
    cs <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border(position=c("BOTTOM", "LEFT",
                                                                    "TOP", "RIGHT"))
    
    addDataFrame(a[[i]],
                 sheet= sheetone,
                 startRow=currRow,
                 row.names=FALSE,
                 colnamesStyle = cs, rownamesStyle = cs, colStyle=cs)
    
    
    currRow <- currRow + nrow(a[[i]]) + 4 
  }
  
  currRow <- 1
  for(i in 1:length(p)){
    
    cs <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border(position=c("BOTTOM", "LEFT",
                                                                    "TOP", "RIGHT"))
    
    addDataFrame(p[[i]],
                 sheet= sheet1,
                 startRow=currRow,
                 row.names=FALSE, 
                 colnamesStyle=cs)
    
    
    currRow <- currRow + nrow(p[[i]]) + 4 
  }
}

##number of QA and Devs that logged time each project for each quarter of each year
dev_quarter(a = "Dev")
dev_quarter(a = "QA")

##number of QA and Devs that logged time for each quarter of each year
team_total(a = "Dev", b = "Dev_count")
team_total(a = "QA", b = "QA_count")

QA <-c(QA_count = list(QA_count), QA)
Dev <- c(Dev_count = list(Dev_count),Dev)

xl_sheet(a = Dev, p = QA)
saveWorkbook(wb, file = "Dev_QA_quarterly.xlsx")
