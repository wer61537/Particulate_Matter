#Q2
# Have total emissions from PM2.5 decreased in Baltimore City, Maryland from
# 1999 to 2008?


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
emissions <-PM25[PM25$fips %in% c("24510"), ]
str(emissions)

#sum by year
emissions.agg <- aggregate(Emissions ~ year, data=emissions, FUN=sum)
emissions.agg$Location <- c("Baltimore City, MD")
str(emissions.agg)
emissions.agg

png('plot2.png')
barplot(height=emissions.agg$Emissions,
        names.arg=emissions.agg$year,
        xlab="Year", ylab=expression('Total PM'[2]*' Emissions (tons)'),
        main=expression('Baltimore City, Maryland Total PM'[2]*' Emissions By Year'))
dev.off()