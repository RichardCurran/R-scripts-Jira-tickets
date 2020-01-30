library(tidyverse)
library(outliers)
library(ggplot2)
library(cowplot)
library(magrittr)
library(multipanelfigure)
library(stats)
library(RODBC)
library(lubridate)
library(xlsx)
library(ggpubr)

## This script is for clening the timesheet table and the DFT with logged time table.
# these tables include the author of each logged time on each ticket
# differs from the other clean as the DFT table was created using RICHARD_issues (no AUTHOR)
# procs_dft has the AUTHOR for each time logged on each ticket - more granular data


# TIMESHEET df ------------------------------------------------------------

con <- odbcConnect(dsn = "JIRA_RC")
df <- sqlQuery(con, "select * from richard_test_timesheet1")

#x <- sqlQuery(con, "select * from RICHARD_SPRINT_ROSTER")
#procs_dft <- sqlQuery(con, "select * from richard_DFT_final_procs")

# DF initial clean -----
table(df$Project)
df_backup <- df
df <- df_backup

df <- df[!grepl("Duplicat", df$Resolution) & !grepl("Duplicat", df$label),]
#!grepl("SP", df$Project),] 

df[c(2,3,4,5,6,7,10)] <- sapply(df[c(2,3,4,5,6,7,10)], as.character)

# splitting craeted column
df$Year <- year(df$CREATED)
df$Quarter <- quarter(df$CREATED)
df$Month <- month(df$CREATED, label = TRUE)
df$Week <- week(df$CREATED)

#only want last 5 years

df <- df[df$Year > 2014,]


# rounding timespent to nearest minute
df$timeworked <- as.numeric(df$timeworked)
df$TIMESPENT <- round(df$timeworked/3600, digits = 3)

# sorting into RAS and CID
procs_cidras_fn <- function(c_r, cr){
  
  df$CIDRAS <<- as.character(df$CIDRAS)
  df$CIDRAS[grepl(c_r, df$CIDRAS)] <<- paste(cr)
  
}

procs_cidras_fn(c_r = 'CID', cr = paste('CID'))
procs_cidras_fn(c_r = 'RAS', cr = paste('RAS'))
df$CIDRAS[grepl("RTR|PTR|AVR", df$Project)] <- NA

#sorting gartan internal ras project name
df$Project[df$Client == 'GARTAN' & df$CIDRAS == 'RAS' |df$Client == 'GARTAN' & df$CIDRAS == 'CID'] <- 'Internal'

# removing NA values from project and timespent; the two main variables
df <- df[!is.na(df$Project) & !is.na(df$TIMESPENT),]

# DFT with time logged ---------------------------------------------------
# DFT initial clean: changign client UAT issues to UAT project and removing duplicates
# check ID = 72291 and ID = 44488 for confirmation ( before and after execution)

destination  <- sqlQuery(con, "select * from richard_DFT_DESTINATION1")
destination[c(2,3)] <- sapply(destination[c(2,3)], as.character)

sores <-  sqlQuery(con, "select * from richard_DFT_SOURCE1")
sores[c(2,3)] <- sapply(sores[c(2,3)], as.character)

procs_dft_uat <- function(proj_d, proj_u){
  
  destination$Parent[grepl(paste(proj_d), destination$Parent) & grepl("UAT issue", destination$ParentType)] <<- paste(proj_u)
  sores$Parent[grepl(paste(proj_d), sores$Parent) & grepl("UAT issue", sores$ParentType)] <<- paste(proj_u)
  df$Project[grepl(paste(proj_d), df$Project) & grepl("UAT issue", df$Type)] <<- paste(proj_u)
  
}

procs_dft_uat(proj_d = paste('RTD'), proj_u = 'RTU')
procs_dft_uat(proj_d = paste('AVD'), proj_u = 'AVU')
procs_dft_uat(proj_d = paste('PTD'), proj_u = 'PTU')

procs_dft <- rbind(sores,destination)
procs_dft <- procs_dft %>% select(ParentID, AUTHOR ,Parent, ParentType, CIDRAS, CREATED, timeworked)
colnames(procs_dft) <- c("ID", "AUTHOR", "Project", "Type", "CIDRAS", "CREATED","TIMESPENT")
procs_dft[c(2,3,4,5)] <-sapply(procs_dft[c(2,3,4,5)], as.character)

# all dft issues with time logged but no parent ticket
DFT_anomalies <- procs_dft[grepl("TD|VD", procs_dft$Project) & !grepl("UAT issu", procs_dft$Type) & !duplicated(procs_dft$CREATED),]  %>% 
  group_by(ID, Project, AUTHOR, CREATED) %>% summarise(TOTAL = sum(TIMESPENT)) %>% group_by(ID, Project, CREATED) %>% summarise(x = sum(TOTAL))

procs_dft <- procs_dft[!grepl("VD|TD", procs_dft$Project),]
procs_dft <-  procs_dft[!duplicated(procs_dft$CREATED),]


# DFT with timelogged clean and transform -----------------------------------------------------
# adding year, month, quarter columns to DFT
procs_dft$Year <- year(procs_dft$CREATED)
procs_dft$Quarter <- quarter(procs_dft$CREATED)
procs_dft$Month <- month(procs_dft$CREATED, label = TRUE)
procs_dft$Week <- week(procs_dft$CREATED)

# from 2015 - present and converting time to hours
procs_dft <- procs_dft[procs_dft$Year > 2014,]
procs_dft$TIMESPENT <- round(procs_dft$TIMESPENT/3600, digits = 3)

# simplifying CIDRAS column
procs_dft$CIDRAS[grepl('CID', procs_dft$CIDRAS)] <- 'CID'
procs_dft$CIDRAS[grepl('RAS', procs_dft$CIDRAS)] <- 'RAS'

# IInternal issues in dft 
procs_dft$Project[procs_dft$Client == 'GARTAN' &
                    procs_dft$CIDRAS == 'RAS' | procs_dft$Client == 'GARTAN' &
                    procs_dft$CIDRAS == 'CID'] <- 'Internal'

# removing NA values from project and timespent
procs_dft <- procs_dft[!is.na(procs_dft$Project) & !is.na(procs_dft$TIMESPENT),]




# PROCS dataframe ---------------------------------------------------------
# informed that any american DFTs with time logged were RTR issues 
df$Project[grepl("dashi|bay|openh|platt", df$Client) & grepl("RTD", df$Project)] <- "RTR"

colnames(df)[2] <- 'ID'
colnames(df)[3] <- 'AUTHOR'

df <- df %>% select(ID, AUTHOR, Project,CIDRAS, Type, CREATED, TIMESPENT, 
                    Year, Quarter, Month, Week)
procs_dft <- procs_dft %>% select(ID, AUTHOR, Project,  CIDRAS, Type, CREATED, TIMESPENT, 
                                  Year, Quarter, Month, Week)

procs <- rbind(df, procs_dft)

## procs dataframe contains all the corrected DFT issues with time logged; the time logged was transferred to the parent ticket
# need to remove all DFT issues to ensure no duplicates
# procs is the final, cleaned df that contains the correct time logged for each ticket in Jira for each specified Project

# Team names ------------------------------------------------------------
# Team names are not in Jira DB and so hav to hardcode them from a .csv file
getwd()
team <- read.csv("C:/Users/rcurran.GARTANTECH/Documents/Processes-2020/team_members.csv", stringsAsFactors =  FALSE) 
colnames(team)[1] <- 'AUTHOR'

procs <- left_join(procs, team, by = 'AUTHOR')
procs <- procs[!is.na(procs$Team),]
unique(procs[c("AUTHOR", "Team")])


# Procs final clean & transform -------------------------------------------------------
# the final clean & transform: slicing projct names and removing invalid tickets

procs <- procs  %>% select(ID, Project, CIDRAS, Type, CREATED, TIMESPENT, 
                           Year, Quarter, Month, Week, AUTHOR, Team)

procs$Project<- substr(procs$Project, 0, 3)
procs$Project[grepl("SP", procs$Project)] <- 'SP'
procs$Project[grepl("In", procs$Project)] <- 'Internal'
procs$Project[grepl("OP", procs$Project)] <- 'OP'

procs$CIDRAS[!grepl("RTL|PTL|AVL|Intern", procs$Project)] <- NA

procs <- procs[!grepl("56324|43073|56190|43070", procs$ID),] %>% 
  mutate(Project = ifelse(!is.na(CIDRAS), paste(Project, '-', CIDRAS), Project))

rm(df, sores, destination, team)
procs <- procs[!grepl("2020", procs$Year) & !grepl("VD|TD", procs$Project),]


# saving the clean dataframe as external .csv file
write.csv(procs, file = 'procs.csv', row.names = FALSE)

# High-level list of dataframes: does not contain information on each team.
# only contains information about each module
# This is for projects (not as detailed as above)


# removing team and author columns from procs dataframe
ALL <- procs[!grepl("TD|VD|EXP", procs$Project),]


# defining functions for MODULE time logged -------
time_list <- function(a, ...){
  
  df  <- ALL %>% group_by_(...) %>%
    summarise(total =  round(sum(TIMESPENT, na.rm = TRUE), digits = 1))
  
  df_a <- split(df, df$Year)
  
  assign(paste(a), df_a,envir = .GlobalEnv)
}

time_df <- function(df_list, col, row_name, list_name){
  
  x <- df_list
  
  for(e in names(df_list)){
    
    x[[e]] <- cast(x[[e]], col)
    x[[e]][nrow(x[[e]]) + 1,] <- c(paste(row_name), "-",'-',
                                   round(colMeans(x[[e]], na.rm = TRUE)[1], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[2], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[3], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[4], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[5], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[6], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[7], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[8], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[9], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[10], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[11], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[12], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[13], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[14], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[15], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[16], digits = 2))
    #
    # tail(x[[e]][4:ncol(x[[e]])],-12) <- round(tail(x[[e]][4:ncol(x[[e]])],-12),
    #                                                    digits = 2)
  }
  
  assign(paste(list_name), x, envir = .GlobalEnv)
  
  
}



# HIGH-LEvEL: QUARTERLY ---------------------------------------------------------------
time_list(a = "high_quarterly", "Project", "Year", "Quarter")
time_df(df_list = high_quarterly,
        col = Year + Quarter ~ Project, 
        row_name = "Avg. per Q", 
        list_name = "high_quarterly")


# HIGH-LEvEL: MONTHLY ------------------------------------------------------------------


time_list(a = "high_monthly", "Project", "Year", "Month")
time_df(df_list = high_monthly, col = Year + Month ~ Project, row_name = "Avg. per Month", list_name = "high_monthly")


# HIGH-LEvEL: Weekly ------------------------------------------------------------------

high_weekly  <- ALL %>% group_by(Project, Year, Week) %>% summarise(total =  sum(TIMESPENT, na.rm = TRUE)) %>%
  group_by(Project, Year) %>% summarise(avg =  round(median(total), digits = 1))

high_weekly <- split(high_weekly, high_weekly$Year)

for( e in names(high_weekly)){
  high_weekly[[e]] <- cast(high_weekly[[e]], Year ~ Project)
  
}



# HIGH-LEvEL: Avg. time spent per ticket ----------------------------------------------

high_avg_ticket <-ALL %>% group_by(ID, Project, Year) %>% 
  summarise(x = sum(TIMESPENT)) %>% 
  group_by(Year, Project) %>%
  summarise(Avg. = round(median(x), digits = 2))

high_avg_ticket <- cast(high_avg_ticket, Year ~ Project)




# List of all High Level dataframes ---------------------------------------
MODULES_time_logged <- list("Quarterly" = high_quarterly, "Monthly" = high_monthly, "Weekly avg." = high_weekly, "Avg. per ticket" = high_avg_ticket)

rm(high_weekly, high_monthly, high_quarterly, high_avg_ticket)  



# Functions & dataframe for TEAM time logged ---------------------------------------------------

# similar to the previous  time_ function but split and number of rows are different
ALL <- procs[!grepl("TD|VD|EXP", procs$Project),]

# defining functions  for total calc.
time_list <- function(a, ...){
  
  df  <- ALL %>% group_by_(...) %>% summarise(total =  round(sum(TIMESPENT, na.rm = TRUE), digits = 1))
  df_a <- split(df, list(df$Year,df$Team))
  assign(paste(a), df_a,envir = .GlobalEnv)
  
}

time_df <- function(df_list, col, row_name, list_name){
  x <- df_list
  for(e in names(df_list)){
    
    x[[e]] <- cast(x[[e]], col)
    x[[e]][nrow(x[[e]]) + 1,] <- c(paste(row_name), "-",'-',
                                   round(colMeans(x[[e]], na.rm = TRUE)[1], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[2], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[3], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[4], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[5], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[6], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[7], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[8], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[9], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[10], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[11], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[12], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[13], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[14], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[15], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[16], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[17], digits = 2))
    #
    # tail(x[[e]][4:ncol(x[[e]])],-12) <- round(tail(x[[e]][4:ncol(x[[e]])],-12),
    #                                                    digits = 2)
  }
  
  assign(paste(list_name), x, envir = .GlobalEnv)
  
  
}

# defininig functions for avg. calc.

time_list_avg <- function(a, col){
  
  df  <- ALL %>% group_by_("Team", "Project", "Year", col) %>% 
    summarise(total =  sum(TIMESPENT)) %>%
    group_by(Team, Project, Year) %>%
    summarise(avg = round(mean(total), digits = 1))
  
  df_a <- split(df, df$Team)
  assign(paste(a), df_a,envir = .GlobalEnv)
  
}

time_df_avg <- function(df_list, col, row_name, list_name){
  x <- df_list
  for(e in names(df_list)){
    
    x[[e]] <- cast(x[[e]], col)
    x[[e]][nrow(x[[e]]) + 1,] <- c(paste(row_name), "-",
                                   round(colMeans(x[[e]], na.rm = TRUE)[1], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[2], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[3], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[4], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[5], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[6], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[7], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[8], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[9], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[10], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[11], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[12], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[13], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[14], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[15], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[16], digits = 2),
                                   round(colMeans(x[[e]], na.rm = TRUE)[17], digits = 2))
    #
    # tail(x[[e]][4:ncol(x[[e]])],-12) <- round(tail(x[[e]][4:ncol(x[[e]])],-12),
    #                                                    digits = 2)
  }
  
  assign(paste(list_name), x, envir = .GlobalEnv)
  
  
}

# LOW-LEVEL: TOTALS------------------------------------------------------------------

#_____________________________________________ Yearly TOTALS ________________________________________________________________________________________

Low_yearly_total  <- ALL %>% group_by(Project, Team, Year) %>% summarise(total =  round(sum(TIMESPENT, na.rm = TRUE), digits = 1))
Low_yearly_total <- split(Low_yearly_total, Low_yearly_total$Team)
time_df_avg(df_list = Low_yearly_total, col = Year + Team ~ Project, row_name = "Avg. per year", list_name = "Low_yearly_total")


#_____________________________________________ MONTHLY TOTALS________________________________________________________________________________________


time_list(a = "Low_monthly_total", "Project", "Team", "Year", "Month")
time_df(df_list = Low_monthly_total, col = Year + Month +Team ~ Project, row_name = "Avg. per Month", list_name = "Low_monthly_total")

## ___________________________________________QUARTERYL TOTALS______________________________________________________________________________________

time_list(a = "Low_quarterly_total", "Project", "Team", "Year", "Quarter")
time_df(df_list = Low_quarterly_total, col = Year + Quarter +Team ~ Project, row_name = "Avg. per Q", list_name = "Low_quarterly_total")




# LOW-LEVEL:  AVERAGES ----------------------------------------------------------------



# _____________________________________________Monthly averages _____________________________________________

time_list_avg(a = "low_monthly_average", col = "Month")
time_df_avg(df_list = low_monthly_average, col = Year + Team  ~ Project, row_name = "Avg. per month",list_name = "low_monthly_average")


# _____________________________________________Quarterly averages _____________________________________________

time_list_avg(a = "low_Q_average", col = "Quarter")
time_df_avg(df_list = low_Q_average, col = Year + Team  ~ Project, row_name = "Avg. per Q",list_name = "low_Q_average")


# _____________________________________________Weekly averages _____________________________________________ 

# different pipeline for weekly average: need to use median instead of mean due to distribution (not normal)
low_weekly_average  <- ALL %>% group_by_("Team", "Project", "Year", "Week") %>% 
  summarise(total =  sum(TIMESPENT)) %>%
  group_by(Team, Project, Year) %>%
  summarise(avg = round(median(total), digits = 1))
low_weekly_average <- split(low_weekly_average, low_weekly_average$Team)
time_df_avg(df_list = low_weekly_average, col = Year + Team  ~ Project, row_name = "Avg. per week",list_name = "low_weekly_average")



# _____________________________________________Average time per ticket each year _____________________________________________ 
low_avg_ticket <-ALL %>% group_by(ID, Project, Team, Year) %>% 
  summarise(x = sum(TIMESPENT)) %>% 
  group_by(Year, Team, Project) %>%
  summarise(Avg. = round(median(x), digits = 2))

low_avg_ticket <- split(low_avg_ticket, low_avg_ticket$Team)
time_df_avg(df_list = low_avg_ticket, col = Year + Team ~ Project, row_name = "Avg. per ticket", list_name =  "low_avg_ticket")




# List of all Low Level Dataframes ----------------------------------------


TEAMS_time_logged <- list(Low_yearly_total, Low_quarterly_total, Low_monthly_total, low_Q_average,  low_monthly_average,
                          low_weekly_average, low_avg_ticket)
names(TEAMS_time_logged) <- c("Total per year", "Total per quarter",
                              "Total per month", "Avg. per quarter", "Avg. per month",
                              "Avg. per week", "Avg per ticket")

rm(Low_yearly_total, Low_quarterly_total, Low_monthly_total, low_Q_average,  low_monthly_average,
   low_weekly_average, low_avg_ticket)
rm(procs_detailed,TEAMS_time_ogged, KPIS_procsess)


# WRITING ALL DATAFRAMES TO EXCEL -----------------------------------------


wb <- createWorkbook()
sheetone <- createSheet(wb,"Total per year")
sheet1 <- createSheet(wb,"Total per Q")
sheet2 <- createSheet(wb,"Total per month")
sheet3 <- createSheet(wb, "Avg. per quarter")
sheet4 <- createSheet(wb, "Avg. per month")
sheet5 <- createSheet(wb, "Avg. per week")
sheet6 <- createSheet(wb, "Avg. per ticket")
#sheet7 <- createSheet(wb, "Q percentage")

currRow <- 1
xl_sheet <- function(a, p, w,perc, month, monthperc,  q){
  
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
  
  currRow <- 1
  for(i in 1:length(w)){
    
    cs <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border(position=c("BOTTOM", "LEFT",
                                                                    "TOP", "RIGHT"))
    
    addDataFrame(w[[i]],
                 sheet= sheet2,
                 startRow=currRow,
                 row.names=FALSE,
                 colnamesStyle=cs)
    
    currRow <- currRow + nrow(w[[i]]) + 4
    
  }
  #
  currRow <- 1
  for(i in 1:length(perc)){
    
    cs <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border(position=c("BOTTOM", "LEFT",
                                                                    "TOP", "RIGHT"))
    
    addDataFrame(perc[[i]],
                 sheet= sheet3,
                 startRow=currRow,
                 row.names=FALSE,
                 colnamesStyle=cs)
    
    
    currRow <- currRow + nrow(perc[[i]]) + 4
  }
  currRow <- 1
  for(i in 1:length(month)){
    
    cs <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border(position=c("BOTTOM", "LEFT",
                                                                    "TOP", "RIGHT"))
    
    addDataFrame(month[[i]],
                 sheet= sheet4,
                 startRow=currRow,
                 row.names=FALSE,
                 colnamesStyle=cs)
    
    
    currRow <- currRow + nrow(month[[i]]) + 4
  }
  
  currRow <- 1
  for(i in 1:length(monthperc)){
    
    cs <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border(position=c("BOTTOM", "LEFT",
                                                                    "TOP", "RIGHT"))
    
    addDataFrame(monthperc[[i]],
                 sheet= sheet5,
                 startRow=currRow,
                 row.names=FALSE,
                 colnamesStyle=cs)
    
    
    currRow <- currRow + nrow(monthperc[[i]]) + 4
  }
  currRow <- 1
  for(i in 1:length(q)){
    
    cs <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border(position=c("BOTTOM", "LEFT",
                                                                    "TOP", "RIGHT"))
    
    addDataFrame(q[[i]],
                 sheet= sheet6,
                 startRow=currRow,
                 row.names=FALSE,
                 colnamesStyle=cs)
    
    
    currRow <- currRow + nrow(q[[i]]) + 4
  }
  
  
}

xl_sheet(a= TEAMS_time_logged$`Total per year` , p = TEAMS_time_logged$`Total per quarter`,
         w =  TEAMS_time_logged$`Total per month`, perc = TEAMS_time_logged$`Avg. per quarter`, 
         month = TEAMS_time_logged$`Avg. per month`, 
         monthperc =  TEAMS_time_logged$`Avg. per week`, q = TEAMS_time_logged$`Avg per ticket`)
saveWorkbook(wb, file = "TEAM_time_logged.xlsx")


## Save workbook to excel file 
saveWorkbook(wb, file = "ALL.xlsx")
write.csv(DFT_anomalies, file = "DFT_anomalies.csv", row.names = FALSE)


# Module Time Logged ---------------------------------------------------------------

wb <- createWorkbook()
sheetone <- createSheet(wb,"Quarterly Totals")
sheet1 <- createSheet(wb,"Monthly Totals")
sheet2 <- createSheet(wb,"Weekly avg.")
sheet3 <- createSheet(wb, "Avg. per ticket")


currRow <- 1
xl_sheet <- function(a, p, w,perc ){
  
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
  
  currRow <- 1
  for(i in 1:length(w)){
    
    cs <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border(position=c("BOTTOM", "LEFT",
                                                                    "TOP", "RIGHT"))
    
    addDataFrame(w[[i]],
                 sheet= sheet2,
                 startRow=currRow,
                 row.names=FALSE,
                 colnamesStyle=cs)
    
    currRow <- currRow + nrow(w[[i]]) + 4
    
  }
  #
  currRow <- 1
  cs <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border(position=c("BOTTOM", "LEFT",
                                                                  "TOP", "RIGHT"))
  addDataFrame(perc,
               sheet= sheet3,
               startRow=currRow,
               row.names=FALSE,
               colnamesStyle=cs)
  #currRow <- currRow + nrow(perc) + 4
  
}

xl_sheet(a = MODULES_time_logged$Quarterly, p = MODULES_time_logged$Monthly,
         w = MODULES_time_logged$`Weekly avg.`,perc = MODULES_time_logged$`Avg. per ticket`)
saveWorkbook(wb, file = "MODULE_time_logged.xlsx")

rm(sheet1, sheet2, sheet3, sheet4, sheet5, sheet6, sheetone,wb)


