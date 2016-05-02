#Q1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all [emission] sources 
# for each of the years 1999, 2002, 2005, and 2008.

#remove all objects just to be safe
rm(list = ls(all = TRUE))


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

str(PM25)

#sum by year
emissions.agg <- aggregate(Emissions ~ year, data=PM25, FUN=sum)

str(emissions.agg)
emissions.agg

#do an lr to get a line and coefficients
fit<-lm(Emissions ~ year, data=emissions.agg)

#
summary(fit)
intcp <- format(coef(fit)[1],digits=4) 
slp <-  format(coef(fit)[2],digits=4)
Rsq <-format(summary(fit)$r.squared, digits=4)

## roundcoefficients for better output
cf <- round(coef(fit), 2) 

## build equation label
eq <- paste0("Total PM  = ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " * Year ",
             ifelse(sign(cf[2])==1,"\nSlope of regression is positive 20 emissions are increasing.","\nSlope of regression is negative so emissions are decreasing.")
             )
eq             

png("plot1.png", height=480, width=680)
plot(Emissions ~ year, data = emissions.agg, pch=2, col=topo.colors(8), xlab="Year",
     ylab=expression("Total PM"[2.5]*" Emissions (tons)"),
     main=expression("Total PM"[2.5]*" Emissions From All US Sources")
)

#add regression line and equation
abline(fit,lty="dashed", col='red')
legend("top", bty="n", eq)

#test slope, if negative then decrease
message(ifelse(sign(cf[2])==1,"Slope of regression is positive to emissions are increasing.","Slope of regression is negative to emissions are decreasing."))
dev.off()
