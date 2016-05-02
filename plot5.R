# Q5
# How have emissions from motor vehicle sources changed from 1999–2008 in
# Baltimore City?

#remove all objects just to be safe
rm(list = ls(all = TRUE))

#get needed library
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
  PM25$year <- as.factor(PM25$year)
} 

# Get Baltimore from motor vehicle sources
str(PM25)
emissions <-PM25[PM25$fips %in% c("24510") & (PM25$type=="ON-ROAD"), ]
str(emissions)

PM25.mobile <- PM25[PM25$SCC %in% SCC[grep("Mobile", SCC$EI.Sector), 1] & PM25$fips %in% c("24510") & (PM25$type=="ON-ROAD"), ]
PM25.mobile$Location <- "Baltimore City, MD"
head(PM25.mobile)

SCC.mobile <- SCC[, c(1, 4)]
head(SCC.mobile)
emissions.mobile <- merge( PM25.mobile,SCC.mobile, by.x = "SCC", by.y = "SCC") 
str(emissions.mobile)
head(emissions.mobile)

# sum by year and Sector
emissions.agg <- aggregate(Emissions ~ year + EI.Sector, data=emissions.mobile, FUN=sum)
head(emissions.agg)

# plot
png("plot5.png", height=480, width=680)
p<-ggplot(emissions.agg, aes(x=factor(year), y=Emissions, fill=EI.Sector)) +
  geom_bar(stat="identity") +
  xlab("year") +
  ylab(expression("Total PM"[2.5]*" Emissions (tons)")) +
  ggtitle(expression("Total PM"[2.5]*" Baltimore City, MD Vehicle Related Emissions")) +
  theme(plot.title = element_text(  color="#666666", face="bold", size=16)) +
  theme(axis.title = element_text( color="#666666", face="bold", size=16))
print(p)
dev.off()
