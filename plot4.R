# Q4
# Across the United States, how have emissions from coal combustion-related
# sources changed from 1999 to 2008?



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

# Find coal combustion-related sources in the SCC data
PM25.coal <- PM25[PM25$SCC %in% SCC[grep("Coal", SCC$EI.Sector), 1], ]
SCC.coal <- SCC[, c(1, 4)]
emissions.coal <- merge(PM25.coal, SCC.coal, by.x = "SCC", by.y = "SCC")[, c(4, 6, 7)]
head(emissions.coal)

# sum by year and Sector
emissions.agg <- aggregate(Emissions ~ year + EI.Sector, data=emissions.coal, FUN=sum)
head(emissions.agg)

# plot
png("plot4.png", height=480, width=680)
p<-ggplot(emissions.agg, aes(x=factor(year), y=Emissions, fill=EI.Sector)) +
  geom_bar(stat="identity") +
  xlab("year") +
  ylab(expression("Total PM"[2.5]*" Emissions (tons)")) +
  ggtitle(expression("Total PM"[2.5]*" Coal Combustion Related Emissions")) +
  theme(plot.title = element_text(  color="#666666", face="bold", size=16)) +
  theme(axis.title = element_text( color="#666666", face="bold", size=16))
print(p)
dev.off()
