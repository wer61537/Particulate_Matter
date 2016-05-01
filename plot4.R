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
coal <- grepl("Fuel Comb.*Coal", SCC$EI.Sector)
coal.sources <- SCC[coal,]

# Find emissions from coal combustion-related sources
emissions <- PM25[(PM25$SCC %in% coal.sources$SCC), ]

# sum by year
emissions.yearly <- aggregate(Emissions ~ year, data=emissions, FUN=sum)

# plot
library(ggplot2)
png("plot4.png")
p<-ggplot(emissions.yearly, aes(x=factor(year), y=Emissions)) +
  geom_bar(stat="identity") +
  xlab("year") +
  ylab(expression("Total PM"[2.5]*" Emissions (tons)")) +
  ggtitle(expression("Total PM"[2.5]*" Coal Combustion Related Emissions")) +
  theme(plot.title = element_text(  color="#666666", face="bold", size=16)) +
  theme(axis.title = element_text( color="#666666", face="bold", size=16))
print(p)
dev.off()