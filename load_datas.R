df_init <- read.csv(file="/Users/Julien/Documents/Stages & Alternance/THALES/DataDisney/wait_times.csv", header=TRUE, sep=',')
dfCoordInit <- read.csv(file="/Users/Julien/Documents/Stages & Alternance/THALES/DataDisney/ShinyApp/CoOrlando.csv", header=TRUE, sep=';')

df <- df_init
df$lastUpdate <- as.POSIXct(as.numeric(df$lastUpdate), origin = '1970-01-01', tz = 'GMT')

dfPlotInit <- df
dfPlotInit$waitTime[dfPlotInit$waitTime==0] <- -1
dfPlotInit <- dfPlotInit[!dfPlotInit$waitTime==-999,]

dfRealTimeAgg <- aggregate(lastUpdate ~ name, df, max)
dfRealTime <- merge(df, dfRealTimeAgg, by=c("name","lastUpdate"))

dfStatsInit <- df
dfStatsInit <- dfStatsInit[!dfStatsInit$waitTime==-999,]

dfOcc <- df
dfOcc$nbOcc <- 1
dfOcc$lastUpdate <- as.POSIXct(strptime(paste(format(dfOcc$lastUpdate,"%Y-%m"),"-15 12:00:00", sep=""), "%Y-%m-%d %H:%M"))
dfOcc <- aggregate(nbOcc ~ lastUpdate+name,dfOcc, sum)

park_themes <- list(
  "By Theme Park" = list("All","Magic Kingdom","Epcot","Disney\'s Hollywood Studios","Disney\'s Animal Kingdom"),
  "Magic Kingdom" = unique(df$name[str_match(df$id,"(\\w*)_")[,2] == "MagicKingdom"]),
  "Epcot" = unique(df$name[str_match(df$id,"(\\w*)_")[,2] == "Epcot"]),
  "Disney\'s Hollywood Studios" = unique(df$name[str_match(df$id,"(\\w*)_")[,2] == "DisneysHollywoodStudios"]),
  "Disney\'s Animal Kingdom" = unique(df$name[str_match(df$id,"(\\w*)_")[,2] == "DisneysAnimalKingdom"])
)

test <- head(dfPlotInit,50)
test_min <- aggregate(waitTime ~ name, test, min)
test_max <- aggregate(waitTime ~ name, test, max)
test_max <- head(test_max[order(test_max$waitTime),],3)

dfCoord <- dfCoordInit
dfCoord <- merge(df, dfCoord, by=c("name","name"))
dfCoord$latitude <- as.numeric(dfCoord$latitude)
dfCoord$longitude <- as.numeric(dfCoord$longitude)

dfCoord$legend <- paste(dfCoord$name,as.character(dfCoord$waitTime), sep = "\n","minutes d'attente")

dfCoordAgg <- aggregate(lastUpdate ~ name+latitude+longitude, dfCoord, max, na.rm=TRUE)
dfCoordAgg <- merge(dfCoordAgg,dfCoord,by=c("lastUpdate","name","latitude","longitude"))

# df$waitTime[df$waitTime == -999] <- 0
# df <- with(df, df[(df$lastUpdate >= "2018-01-02 00:00:00" & df$lastUpdate <= "2018-01-02 23:59:59"), ])

# times <- c()
# for (i in 0:23){
#   if (i<10){
#     times <- c(times,c(paste(paste("0",toString(i), sep=""),":00", sep="")))    
#   }
#   else{
#     times <- c(times,c(paste(toString(i),":00", sep="")))
#   }
# }

# temps1 <- as.POSIXct("2012-03-13")
# temps2 <- as.POSIXct("2011-02-13")
# abs(as.numeric(difftime(temps1, temps2), units="auto"))
# test <- df[df$name == "Dinosaur",]
# print(as.numeric(difftime("2015-12-05", "2015-12-06"), units="auto"))

