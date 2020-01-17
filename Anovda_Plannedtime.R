library(tseries)
library(forecast)
library(ggplot2)
library(lubridate)
library(dplyr)

PT <- planned_time[!grepl("Good", planned_time$Estimate),]
PT$Plan.Date <- floor_date(PT$Plan.Date, unit = 'month')
head(PT$Plan.Date)
head(PT, 5)

PT$Plan.Date = as.Date(PT$Plan.Date)
PT$Project <- as.factor(PT$Project)

PT <- PT[grepl("Und", PT$Estimate),] %>% group_by(Project,Estimate, Plan.Date) %>% summarise(PD = median(PercentDiffernce))
with(PT, tapply(PD, Project, mean))
attach(PT)
aov_model <- aov(PT$PD ~ PT$Project)
model.tables(aov_model, type = 'effects')

plotmeans(PT$PD ~ PT$Project, xlab="Project", ylab="PD",
          main="Mean Plot\nwith 95% CI")

summary(aov_model)
plot(TukeyHSD(aov_model))
rm(aov_model, AOV_model, arima_model, auto_arima_model, auto_arima_PT)

accuracy(auto_arima_PT)
plot(forecast(auto_arima_PT, 3))
