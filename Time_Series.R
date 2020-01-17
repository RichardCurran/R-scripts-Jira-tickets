#### Time series for underestimated issues: AVL, PTL, RTL

library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)

PT <- planned_time[!grepl("Good", planned_time$Estimate),]
PT$Plan.Date <- as.Date(PT$Plan.Date)
PT$Project <- as.factor(PT$Project)
PT$Plan.Date <- floor_date(PT$Plan.Date, unit = 'month')

#check for normality in each project
with(PT, tapply(PercentDiffernce, Project, shapiro.test))

PT <- PT[grepl("Ove", PT$Estimate) & grepl("AV", PT$Project) & !grepl("2013|2014", PT$Plan.Date),] %>%
  group_by(Project,Estimate, Plan.Date) %>% 
  summarise(PD = median(PercentDiffernce))

PT <- slice(PT, 1:(n()-3))
## Perc.Difference UNDERESTIMATED tickets time series -----------------

#finding the start and end dates
min(PT$Plan.Date)
max(PT$Plan.Date)

PT_ts <- ts(PT$PD, start = c(2015, 5), end = c(2019, 12), frequency = 12)
PT_ts

# Additive if amplitude remains constant 
# multiplicative if amplitude increases
plot(PT_ts)

## Damps random fluctuations to extract the trend (removes noise)
# replaces the datapoints with the mean of the value 
# and the datapoint before and after it ( centered moving average)
# can see if additive/multplicative
plot(ma(x = PT_ts, order = 10), main  =  "AVL, Over issues")

## check for stationarity: p < 0.05 means the TS is stationary. This means there are less instabilities and
# modeling a satble series with constant properties involves less uncertainty
# if non-stationary: the planned time perc.diff changes through time, levels changes etc.
adf.test(PT_ts)
ndiffs(PT_ts)


# log of the TS to get additive model 
x <- log(PT_ts)
plot(x)
plot(PT_ts)
x <- exp(x)
plot(x)
#if multiplicative
x <- PT_ts

# extract seasonal component
seasonal_decomposition <- stl(x, s.window = "period")
plot(seasonal_decomposition$time.series)

# examine the seasonal affects 
# 
converted_PT <- exp(seasonal_decomposition$time.series)
plot(converted_PT)

# seasonal component; checking for stationarity 
season_adj <- seasadj(seasonal_decomposition)
seasonplot(season_adj, 12, col = rainbow(12), year.labels = TRUE)
adf.test(season_adj)

Acf(season_adj)
Pacf(season_adj)

# auto-arima
x <- auto.arima(season_adj)
x

#check MAPE
accuracy(x)
qqnorm(x$residuals)
qqline(x$residuals)

# P > 0.05 then the model is a good fit
Box.test(x$residuals, type = 'Ljung-Box')


plot(forecast(x, 12))
x <- data.frame(forecast(x, 12))
x
log(x$Point.Forecast)

## need to take the log of forecast and possibly the MAPE value to get forecasted Perc.DIff

