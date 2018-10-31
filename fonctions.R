findKey <- function(x) {
  result <- NULL
  for (i in 1:3) {
    if (!is.na(match(x , park_themes[[i]]))) {
      result <- names(park_themes)[i]
    }
  }
  return(result)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

diffTime <- function(date1,date2) {
  return(abs(as.numeric(difftime(date2, date1), units="days")))
}

getPlotAtt <- function(nameAttInput,dateInput1,dateInput2) {
  dfPlot <- dfPlotInit
  dfPlot <- dfPlot[dfPlot$name == toString(nameAttInput),]
  diffTime <- diffTime(dateInput1,dateInput2)
  if(diffTime >= 0 & diffTime < 1) {
    # date1 <- as.POSIXct(paste(format(dateInput1,format="%Y-%m-%d")," 00:00:00"),tz="GMT")
    # date2 <- as.POSIXct(paste(format(dateInput2,format="%Y-%m-%d")," 23:59:59"),tz="GMT")
    dfPlot <- with(dfPlot, dfPlot[(dfPlot$lastUpdate >= as.POSIXct(paste(format(dateInput1,format="%Y-%m-%d")," 00:00:00"),tz="GMT") & 
                                     dfPlot$lastUpdate <= as.POSIXct(paste(format(dateInput2,format="%Y-%m-%d")," 23:59:59"),tz="GMT")),])
    date1 <- min(dfPlot$lastUpdate)
    date2 <- max(dfPlot$lastUpdate)
    byVar <- "1 hours"
    formatPlot <- "%H:%m"
    periodVar <- hours(1)
  }
  else if(diffTime >= 1 & diffTime < 8) {
    dfPlot$lastUpdate <- as.POSIXct(strptime(paste(format(dfPlot$lastUpdate,"%Y-%m-%d"),"12:00:00", sep=" "), "%Y-%m-%d %H:%M"))
    dfPlot <- aggregate(waitTime ~ lastUpdate, dfPlot , mean)
    date1 <- as.POSIXct(strptime(paste(format(dateInput1,"%Y-%m-%d"),"12:00:00", sep=" "), "%Y-%m-%d %H:%M"))
    date2 <- as.POSIXct(strptime(paste(format(dateInput2,"%Y-%m-%d"),"12:00:00", sep=" "), "%Y-%m-%d %H:%M"))
    dfPlot <- with(dfPlot, dfPlot[(dfPlot$lastUpdate >= date1 & df$lastUpdate <= date2), ])
    byVar <- "1 days"
    formatPlot <- "%a, %d"
    periodVar <- days(1)
  }
  else if(diffTime >= 7 & diffTime < 31) {
    dfPlot$lastUpdate <- as.POSIXct(strptime(paste(format(dfPlot$lastUpdate,"%Y-%m-%d"),"12:00:00", sep=" "), "%Y-%m-%d %H:%M"))
    dfPlot <- aggregate(waitTime ~ lastUpdate, dfPlot , mean)
    date1 <- as.POSIXct(strptime(paste(format(dateInput1,"%Y-%m-%d"),"12:00:00", sep=" "), "%Y-%m-%d %H:%M"))
    date2 <- as.POSIXct(strptime(paste(format(dateInput2,"%Y-%m-%d"),"12:00:00", sep=" "), "%Y-%m-%d %H:%M"))
    dfPlot <- with(dfPlot, dfPlot[(dfPlot$lastUpdate >= date1 & df$lastUpdate <= date2), ])
    byVar <- "4 days"
    formatPlot <- "%m-%d"
    periodVar <- days(1)
  }
  else if(diffTime >=31 & diffTime < 365) {
    dfPlot$lastUpdate <- as.POSIXct(strptime(paste(format(dfPlot$lastUpdate,"%Y-%m"),"-15 12:00:00", sep=""), "%Y-%m-%d %H:%M"))
    dfPlot <- aggregate(waitTime ~ lastUpdate, dfPlot , mean)
    date1 <- as.POSIXct(strptime(paste(format(dateInput1,"%Y-%m"),"-15 12:00:00", sep=""), "%Y-%m-%d %H:%M"))
    date2 <- as.POSIXct(strptime(paste(format(dateInput2,"%Y-%m"),"-15 12:00:00", sep=""), "%Y-%m-%d %H:%M"))
    dfPlot <- with(dfPlot, dfPlot[(dfPlot$lastUpdate >= date1 & df$lastUpdate <= date2), ])
    byVar <- "1 months"
    formatPlot <- "%b-%y"
    periodVar <- months(1)
  }
  else if(diffTime >=365) {
    dfPlot$lastUpdate <- as.POSIXct(strptime(paste(format(dfPlot$lastUpdate,"%Y"),"-07-02 12:00:00", sep=""), "%Y-%m-%d %H:%M"))
    dfPlot <- aggregate(waitTime ~ lastUpdate, dfPlot , mean)
    date1 <- as.POSIXct(strptime(paste(format(dateInput1,"%Y"),"-07-02 12:00:00", sep=""), "%Y-%m-%d %H:%M"))
    date2 <- as.POSIXct(strptime(paste(format(dateInput2,"%Y"),"-07-02 12:00:00", sep=""), "%Y-%m-%d %H:%M"))
    dfPlot <- with(dfPlot, dfPlot[(dfPlot$lastUpdate >= date1 & df$lastUpdate <= date2), ])
    byVar <- "1 years"
    formatPlot <- "%Y"
    periodVar <- months(6)
  }
  return(list("dfPlot"=dfPlot, "date1"=date1, "date2"=date2, "byVar"=byVar, "formatPlot"=formatPlot, "periodVar"=periodVar))
}

getKPI <- function(nameAttInput,dateInput1,dateInput2) {
  dfStats <- dfStatsInit
  dfStats <- dfStats[dfStats$name == toString(nameAttInput),]
  date1 <- as.POSIXct(paste(dateInput1,"00:00:00",sep=" "),tz="GMT")
  date2 <- as.POSIXct(paste(dateInput2,"23:59:59",sep=" "),tz="GMT")
  meanVar <- mean(with(dfStats, dfStats[(dfStats$lastUpdate >= date1 & dfStats$lastUpdate <= date2), ][["waitTime"]]))
  medVar <- median(with(dfStats, dfStats[(dfStats$lastUpdate >= date1 & dfStats$lastUpdate <= date2), ][["waitTime"]]))
  modeVar <- Mode(with(dfStats, dfStats[(dfStats$lastUpdate >= date1 & dfStats$lastUpdate <= date2), ][["waitTime"]]))
  minVar <- min(with(dfStats, dfStats[(dfStats$lastUpdate >= date1 & dfStats$lastUpdate <= date2), ][["waitTime"]]))
  maxVar <- max(with(dfStats, dfStats[(dfStats$lastUpdate >= date1 & dfStats$lastUpdate <= date2), ][["waitTime"]]))
  return(list("mean"=round(meanVar,1), "median"=round(medVar,1), "max"=maxVar, "min"=minVar, "mode"=round(modeVar,1)))
}
  
  
  
  
  