library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(scales)
library(DataCombine)
library(chron)
library(naniar)
library(GmAMisc)
library(outliers)
library(EnvStats)
library(zoo)
library(expss)
library(gtable)
library(gtools)
library(kableExtra)
library(tidyr)
library(devtools)
library(sheetr)
library(wesanderson)
library(httr)
library(anytime)

## This script is for cleaning the timesheet table and the DFT with logged time table.
# these tables include the author of each logged time on each ticket
# differs from the other clean as the DFT table was created using RICHARD_issues (no AUTHOR)
# procs_dft has the AUTHOR for each time logged on each ticket - more granular data

# TIMESHEET dataframe------------------------------------------------------------
# only using ID, timeworked, created, project and type columns; can redo and include more if needs be.
# Could add team and see how accurate each process is at  planning time (left join with team.csv ~ unsure of name)

con <- odbcConnect(dsn = "JIRA_RC")
df <- sqlQuery(con, "select * from richard_test_timesheet1")
df_backup <- df

df <- df_backup
table(df$Status)
str(df)
df[c(3,4,5,6,11,12)] <- sapply(df[c(3,4,5,6,11,12)], as.character)

# DF clean
df <- df[!grepl("Duplicat", df$Resolution) & !grepl("Duplicat", df$label),] %>%
  select(issueid, Project, Type, Resolution, CREATED, timeworked, Status)%>%
  group_by(issueid, Project, Type, Status) %>% 
  summarise(TIMESPENT = sum(timeworked)) %>% 
  filter(!is.na(Project))


# Planned time dataframe --------------------------------------------------


# all planned time from 24/10/2019 - end of year
setwd("C:/Users/rcurran.GARTANTECH/Documents/Processes-2020/PlannedTime_input/")
csv_files <- list.files(pattern = ".csv",
                        recursive = TRUE,
                        full.names = TRUE)

new_pt <- do.call(rbind, lapply(csv_files,
                                         FUN = function(amalgamate){
                                           read.csv(amalgamate, stringsAsFactors = FALSE)
                                           }))
new_pt$Plan.Date <- as.POSIXct(new_pt$Plan.Date,format="%Y-%m-%d")

# Getting the older planned time (. - 24/10/2019)
setwd("C:/Users/rcurran.GARTANTECH/Documents/Processes-2020")
planned_time <- read.csv("planned_time.csv", stringsAsFactors =  FALSE)
planned_time$Plan.Date<- as.POSIXct(planned_time$Plan.Date,format="%Y-%m-%d")
colnames(planned_time) <- colnames(new_pt)

# joining the two planned time datframes
planned_time <- rbind(new_pt, planned_time)  
colnames(planned_time)[1] <- "Project"
sum(duplicated(planned_time$Project))

planned_time <- planned_time %>% group_by()
# PLANNED_TIME ------------------------------------------------------------

# new Planned-time dataframe: Planned time and the actual time spent
planned_time <- inner_join(df, planned_time, by = "Project")
planned_time <- planned_time %>% 
  mutate(TIMESPENT = round(TIMESPENT/ 3600,  digits = 3),
         Hours = round(Hours, digits = 3)) %>% 
  select(issueid, Project, Type, Status, Plan.Date, Planned = Hours, TIMESPENT)

planned_time$Project <- substr(planned_time$Project, 0, 3) # remove key number, just have module code
planned_time$Difference <- planned_time$Planned - planned_time$TIMESPENT # difference to get estimate value
planned_time$Difference1 <- abs(planned_time$Planned - planned_time$TIMESPENT) # abs(difference) column for wiggle room

# adding new time based columns
planned_time$Year <- year(planned_time$Plan.Date)
planned_time$Month <- month(planned_time$Plan.Date, label =  TRUE)
planned_time$Quarter <- quarter(planned_time$Plan.Date)

#adding percentage difference. Formula for precentage difference between two numbers is used
planned_time$PercentDiffernce <- ""
planned_time$PercentDiffernce <- round((planned_time$Planned-planned_time$TIMESPENT)/
                                            ((planned_time$Planned+planned_time$TIMESPENT)/2)*100,digits = 0)
planned_time$PercentDiffernce <- abs(planned_time$PercentDiffernce) #absolute value

# add estimate column
planned_time$Estimate <- ""
planned_time$Estimate[planned_time$Difference < -0.1] <- "Under"
planned_time$Estimate[planned_time$Difference > 0.1] <- "Over"

## adding some wiggle room to the estimate value
# any issues with less than 10% bewtween planned and timespent is labelled "Good"
# the hours are taken into account; thresholds can be seen below
planned_time$Estimate[planned_time$PercentDiffernce < 10.5] <- "Good"
planned_time <- planned_time %>% mutate(Estimate = ifelse(Difference1 == 0.0|
                                                            Planned < 10 & Planned > 0.0 & Difference1 < 0.25|
                                                            Planned < 20 & Planned > 10 & Difference1 < 0.5|
                                                            Planned < 30 & Planned > 20 & Difference1 < 1.0|
                                                            Planned < 40 & Planned > 30 & Difference1  < 1.5|
                                                            Planned < 50 & Planned > 40 & Difference1  < 2.5|
                                                            Planned > 50 & Difference1 < 5,
                                                          "Good", Estimate))
planned_time <- planned_time %>% select(ID = issueid, Project, Type, Status, Plan.Date, Year, Month, Quarter,
                                        Planned, TIMESPENT, Difference, PercentDiffernce, Estimate)

rm(new_pt)
