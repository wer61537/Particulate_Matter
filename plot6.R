# Q6
# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (fips ==
# "06037"). Which city has seen greater changes over time in motor vehicle
# emissions?


#remove all objects just to be safe
rm(list = ls(all = TRUE))

#get needed libraries
library(ggplot2)

# Read data files
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
zipfile <- "FNEI_data.zip"
datafile1<-"summarySCC_PM25.rds"
datafile2<- "Source_Classification_Code.rds"
fullpath1 <-paste("data/",datafile1,sep="")
fullpath2<-paste("data/",datafile2,sep="")

### download the file, if necessary
if (!file.exists(fullpath1) || !file.exists(fullpath2)){
  download.file(sourceURL, zipfile, mode="wb")
  ### unzip the file into local directory
  unzip(zipfile,exdir="./data")
}

### read in the dataset, if necessary
if (!exists("PM25") || !exists("SCC") ){
  cat("Reading in the data files ... ")
  PM25 <- readRDS(fullpath1)
  SCC <- readRDS(fullpath2)
  cat("All data loaded!")
  PM25$fYear <- as.factor(PM25$year)
} 


# Get Baltimore and LA emissions from motor vehicle sources
#str(PM25)
#emissions <-PM25[PM25$fips %in% c("24510", "06037") & (PM25$type=="ON-ROAD"), ]
#str(emissions)

#sum by year and fips
#emissions.agg <- aggregate(Emissions ~ year +fips , data=emissions, FUN=sum)
#emissions.agg$Location <- ifelse(emissions.agg$fips =="24510",c("Baltimore City, MD"),c("Los Angeles County, CA"))

PM25.mobile <- PM25[PM25$SCC %in% SCC[grep("Mobile", SCC$EI.Sector), 1] & PM25$fips %in% c("24510", "06037") & (PM25$type=="ON-ROAD"), ]
PM25.mobile$Location <- ifelse(PM25.mobile$fips =="24510",c("Baltimore City, MD"),c("Los Angeles County, CA"))
head(PM25.mobile)

SCC.mobile <- SCC[, c(1, 4)]
head(SCC.mobile)
emissions.mobile <- merge( PM25.mobile,SCC.mobile, by.x = "SCC", by.y = "SCC") 
str(emissions.mobile)
head(emissions.mobile)

# sum by year,location and Sector
emissions.agg <- aggregate(Emissions ~ year + EI.Sector + Location, data=emissions.mobile, FUN=sum)
#rename EI.Sector
head(emissions.agg)

# Comparative Plot
png("plot6.png", height=480, width=680)
p<-ggplot(emissions.agg, aes(x=factor(year), y=Emissions, fill=EI.Sector)) +
  geom_bar(stat="identity") + 
  facet_grid(.~Location)+
  ylab(expression("Total PM"[2.5]*" Emissions (tons)")) +
  xlab("Year") +
  ggtitle(expression("\n\nComparison of Motor Vehicle Total PM"[2.5]*" Emissions in Baltimore and Los Angeles"))+
  theme(plot.title = element_text(  color="#666666", face="bold", size=12, hjust=0)) +
  theme(axis.title = element_text( color="#666666", face="bold", size=14)) 
print(p)

dev.off()