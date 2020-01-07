library(xlsx)
library(ggpubr)

wb <- createWorkbook()
sheetone <- createSheet(wb,"Yearly Totals")
sheet1 <- createSheet(wb,"Yearly Percentages")
sheet2 <- createSheet(wb,"Weekly averages")
sheet3 <- createSheet(wb, "weekly percentage")
sheet4 <- createSheet(wb, "monthly avg")
sheet5 <- createSheet(wb, "monthly percentage")
sheet6 <- createSheet(wb, "Q avg")
sheet7 <- createSheet(wb, "Q percentage")

currRow <- 1
xl_sheet <- function(a, p, w,perc, month, monthperc,  q, qperc){
  
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
  
  for(i in 1:length(qperc)){
    
    cs <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border(position=c("BOTTOM", "LEFT",
                                                                    "TOP", "RIGHT"))
    
    addDataFrame(qperc[[i]],
                 sheet= sheet7,
                 startRow=currRow,
                 row.names=FALSE,
                 colnamesStyle=cs)
    
    
    currRow <- currRow + nrow(qperc[[i]]) + 4
  }
  #

}

xl_sheet(a= ALL_totals , p = ALL_perc, w =  ALL_weekly_avg, perc = ALL_weekly_perc, month = ALL_monthly_avg,
        monthperc =  ALL_monthly_perc, q = ALL_quarterly_avg, qperc = ALL_quarterly_perc)

xl_sheet(a =  avl_monthly, p = pay_monthly, r = rost_monthly, inter = internal_monthly, sheet_name = sheet2)
xl_sheet(a =  avl_quarterly, p = pay_quarterly, r = rost_quarterly, inter = internal_quarterly, sheet_name = sheet1)

## Save workbook to excel file 
saveWorkbook(wb, file = "ALL.xlsx")

write.csv(DFT_anomalies, file = "DFT_anomalies.csv", row.names = FALSE)

y <- planned_time
y[,c(8,9)] <- round(y[,c(8,9)], digits = 1)
