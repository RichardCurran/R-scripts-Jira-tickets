library(tidyverse)
library(outliers)
library(ggplot2)
library(cowplot)
library(magrittr)
library(multipanelfigure)
library(stats)
library(RODBC)
library(lubridate)

## This script is for clening the timesheet table and the DFT with logged time table.
# these tables include the author of each logged time on each ticket
# differs from the other clean as the DFT table was created using RICHARD_issues (no AUTHOR)
# procs_dft has the AUTHOR for each time logged on each ticket - more granular data


# TIMESHEET df ------------------------------------------------------------

con <- odbcConnect(dsn = "JIRA_RC")
df <- sqlQuery(con, "select * from richard_test_timesheet1")
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


# DFT with time loggedclean and transform -----------------------------------------------------
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


