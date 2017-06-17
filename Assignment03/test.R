library(plyr)
library(dplyr)

startYear <- 2014
endYear <- 2015
rank <- 5

touristData <- read.csv("./data/tours.csv", stringsAsFactors = FALSE)
mydata1 <- touristData[which (touristData$Year >= startYear & touristData$Year <= endYear), ]

groupColumns = c("Country")
dataColumns = c("Tourists")
mydata2 <- ddply(mydata1, groupColumns, function(x) colSums(x[dataColumns]))
mydata3 <- mydata2[order(mydata2$Tourists, decreasing = TRUE),]
finalData <- mydata3[1:rank,]
finalData

statsData <- read.csv("./data/tourist.csv", stringsAsFactors = FALSE)
i<-startYear
keepCols <- c()
newCols <- c()
for (i in startYear:endYear){
     col <- paste("X",i, sep = '')
     rcol <- paste("Yr",i)
     keepCols <- c(keepCols,col)
     newCols <- c(newCols,rcol)
}
zoneFilter <- c("Western Europe", "Eastern Europe")

newStats <- na.omit(subset(statsData, Zone %in% zoneFilter))     
newStats <- newStats[,(names(newStats) %in% keepCols)]
colnames(newStats) <- newCols

DF <- data.frame(
     x=1:10,
     y=10:1,
     z=rep(5,10),
     a=11:20
)