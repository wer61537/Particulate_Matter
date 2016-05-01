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

# Get Baltimore and LA emissions from motor vehicle sources
str(PM25)
emissions <-PM25[PM25$fips %in% c("24510") & (PM25$type=="ON-ROAD"), ]
str(emissions)

#sum by year and fips
emissions.agg <- aggregate(Emissions ~ year , data=emissions, FUN=sum)
emissions.agg$Location <- c("Baltimore City, MD")

str(emissions.agg)
emissions.agg


# Plot
png("plot5.png")
p<-ggplot(emissions.agg, aes(x=factor(year), y=Emissions)) +
  geom_bar(stat="identity") + 
  ylab("Total Emissions (tons)") + 
  xlab("Year") +
  ggtitle("Emissions from motor vehicle sources in Baltimore City") +
  theme(plot.title = element_text(  color="#666666", face="bold", size=16, hjust=0)) +
  theme(axis.title = element_text( color="#666666", face="bold", size=16)) 
print(p)
dev.off()

# plot
library(ggplot2)
png("plot5.png")
ggplot(bmore.emissions.aggr, aes(x=factor(year), y=Emissions)) +
  geom_bar(stat="identity") +
  xlab("year") +
  ylab(expression("total PM"[2.5]*" emissions")) +
  ggtitle("Emissions from motor vehicle sources in Baltimore City")
dev.off()