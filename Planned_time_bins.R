x <- planned_time
x$Planned <- as.numeric(x$Planned)
x$Bin <- ''

x$Bin[x$Planned < 5.000 & x$Planned > 0.0] <- '0-5 hrs'
x$Bin[x$Planned > 5.0 & x$Planned < 10.00] <- '5-10 hrs'
x$Bin[x$Planned < 15.0 & x$Planned > 10.0] <- '10-15 hrs'
x$Bin[x$Planned < 20.0 & x$Planned > 15.0] <- '15-20 hrs'
x$Bin[x$Planned < 25.0 & x$Planned > 20.0] <- '20-25 hrs'
x$Bin[x$Planned < 30.0 & x$Planned > 25.0] <- '25-30 hrs'
x$Bin[x$Planned < 40.0 & x$Planned > 30.0] <- '30-40 hrs'
x$Bin[x$Planned < 50.0 & x$Planned > 40.0] <- '40-50 hrs'
x$Bin[x$Planned > 50.0] <- '50+ hrs'
x$Bin[x$Planned == ''] <- NA
y <- x[is.na(x$Bin),]


# % of each Bin thats good, over and under
round(prop.table(table(x$Bin, x$Estimate),1)*100,digits = 0)

