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

con <- odbcConnect(dsn = "JIRA_RC")
# richard_test_timesheet1 from database -----------------------------------

df <- sqlQuery(con, "select * from richard_test_timesheet1")
df_backup <- df
df <- df_backup

df[c(3,4,5,6,11,12)] <- sapply(df[c(3,4,5,6,11,12)], as.character)

# DF clean
df <- df[!grepl("Duplicat", df$Resolution) & !grepl("Duplicat", df$label),] %>%
  select(issueid, Project, Type, Resolution, CREATED, timeworked, Status)%>%
  group_by(issueid, Project, Type, Status) %>% 
  summarise(TIMESPENT = sum(timeworked)) %>% 
  filter(!is.na(Project))


## HERE TOMORROE BOSS
#  PLANNED_ALLOCATION table from database ---------------------------------
plan_test <- sqlQuery(con, " select  PLAN_ITEM_ID, COMMITMENT, planned_date = CREATED  from AO_2D3BEA_PLAN_ALLOCATION 
                      where SCOPE_ID = 10070 or SCOPE_ID = 10071 or SCOPE_ID = 10072 ")
plan_test <- plan_test %>% group_by(PLAN_ITEM_ID) %>% summarise(planned_time = sum(COMMITMENT)*7.5)
colnames(plan_test)[1] <-  "issueid"
plan_test <- inner_join(plan_test, df, by = 'issueid')
plan_test$TIMESPENT <- x$TIMESPENT * 3600

plan_test$Project <- substr(plan_test$Project, 0, 3) # remove key number, just have module code
plan_test$Difference <- plan_test$planned_time - plan_test$TIMESPENT # difference to get estimate value
plan_test$Difference1 <- abs(plan_test$planned_time - plan_test$TIMESPENT) # abs(difference) column for wiggle room

# adding new time based columns
plan_test$Year <- year(plan_test$Plan.Date)
plan_test$Month <- month(plan_test$Plan.Date, label =  TRUE)
plan_test$Quarter <- quarter(plan_test$Plan.Date)

#adding percentage difference. Formula for precentage difference between two numbers is used
plan_test$PercentDiffernce <- ""
plan_test$PercentDiffernce <- round((plan_test$Planned-plan_test$TIMESPENT)/
                                         ((plan_test$Planned+plan_test$TIMESPENT)/2)*100,digits = 0)
plan_test$PercentDiffernce <- abs(plan_test$PercentDiffernce) #absolute value

# add estimate column
plan_test$Estimate <- ""
plan_test$Estimate[plan_test$Difference < -0.1] <- "Under"
plan_test$Estimate[plan_test$Difference > 0.1] <- "Over"

## adding some wiggle room to the estimate value
# any issues with less than 10% bewtween planned and timespent is labelled "Good"
# the hours are taken into account; thresholds can be seen below
plan_test$Estimate[plan_test$PercentDiffernce < 10.5] <- "Good"
plan_test <- plan_test %>% mutate(Estimate = ifelse(Difference1 == 0.0|
                                                            Planned < 10 & Planned > 0.0 & Difference1 < 0.25|
                                                            Planned < 20 & Planned > 10 & Difference1 < 0.5|
                                                            Planned < 30 & Planned > 20 & Difference1 < 1.0|
                                                            Planned < 40 & Planned > 30 & Difference1  < 1.5|
                                                            Planned < 50 & Planned > 40 & Difference1  < 2.5|
                                                            Planned > 50 & Difference1 < 5,
                                                          "Good", Estimate))
plan_test <- plan_test %>% select(ID = issueid, Project, Type, Status, Plan.Date, Year, Month, Quarter,
                                        Planned, TIMESPENT, Difference, PercentDiffernce, Estimate)