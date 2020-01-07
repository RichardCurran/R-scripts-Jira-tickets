library(tidyverse)
library(outliers)
library(ggplot2)
library(cowplot)
library(magrittr)
library(multipanelfigure)
library(stats)

# PROCS dataframe ---------------------------------------------------------

df$Project[grepl("dashi|bay|openh|platt", df$Client) & grepl("RTD", df$Project)] <- "RTR"

colnames(df)[2] <- 'ID'
colnames(df)[3] <- 'AUTHOR'

df <- df %>% select(ID, AUTHOR, Project,CIDRAS, Type, CREATED, TIMESPENT, 
                    Year, Quarter, Month, Week)
procs_dft <- procs_dft %>% select(ID, AUTHOR, Project,  CIDRAS, Type, CREATED, TIMESPENT, 
                                  Year, Quarter, Month, Week)

procs <- rbind(df, procs_dft)


# Team names ------------------------------------------------------------
team <- read.csv("team_members.csv", stringsAsFactors =  FALSE) 
colnames(team)[1] <- 'AUTHOR'

procs <- left_join(procs, team, by = 'AUTHOR')
procs <- procs[!is.na(procs$Team),]
unique(procs[c("AUTHOR", "Team")])


# Procs final clean -------------------------------------------------------
procs <- procs  %>% select(ID, Team, Project, CIDRAS, Type, CREATED, TIMESPENT, 
                           Year, Quarter, Month, Week)
procs$Project<- substr(procs$Project, 0, 3)
procs$Project[grepl("SP", procs$Project)] <- 'SP'
procs$Project[grepl("In", procs$Project)] <- 'Internal'
procs$Project[grepl("OP", procs$Project)] <- 'OP'
procs$CIDRAS[!grepl("RTL|PTL|AVL", procs$Project)] <- NA

procs <- procs[!grepl("56324|43073|56190|43070", procs$ID),] %>% 
  mutate(Project = ifelse(!is.na(CIDRAS), paste(Project, '-', CIDRAS), Project))

rm(df, sores, destination, team)
procs <- procs[!grepl("2020", procs$Year),]

write.csv(procs, file = 'procs.csv', row.names = FALSE)

# sum(df[df$ID == 40883,]$TIMESPENT)
# sum(procs_dft[procs_dft$ID == 40883,]$TIMESPENT)


#  rough work -------------------------------------------------------------

#dft issues in procs that have no parent -- needs to be finished (these are the anomalies)
x <- procs[grepl("VD|TD|EXP", procs$Project),]%>% group_by(ID) %>% summarise(x = sum(TIMESPENT))

x <- procs[grepl("RTL - RAS", procs$Project) & grepl("Dev", procs$Team) & grepl("2019", procs$Year),] %>% 
  group_by(Week) %>% summarise(x = sum(TIMESPENT))
median(x$x)
procs$
mean(x$x)
sum(x$TIMESPENT)
y <- x[grepl("72940", x$ID),]
sum(y$TIMESPENT)
