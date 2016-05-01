# Q3
# Of the four types of sources indicated by the type (point, nonpoint, onroad,
# nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999–2008 for Baltimore City? Which have seen increases in
# emissions from 1999–2008?


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

# Get Baltimore emissions 
str(PM25)
emissions <-PM25[PM25$fips %in% c("24510"), ]
str(emissions)

#sum by year and type
emissions.agg <- aggregate(Emissions ~ year + type , data=emissions, FUN=sum)
emissions.agg$Location <- c("Baltimore City, MD")

str(emissions.agg)

png("plot3.png", height=480, width=680)
p<-ggplot(emissions.agg, aes(x=factor(year), y=Emissions, fill=type)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ type) +
  ylab(expression("Total PM"[2.5]*" Emissions (tons)")) +
  xlab("Year") +
  theme(plot.title = element_text(  color="#666666", face="bold", size=16)) +
  theme(axis.title = element_text( color="#666666", face="bold", size=16)) +
  ggtitle(expression("Baltimore City PM"[2.5]*" Emissions by Source"))
print(p)
dev.off()