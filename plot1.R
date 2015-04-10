###################
## load data set ##
###################

# download
if(!file.exists("./exdata_data_NEI_data.zip")){
  
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(url,"./exdata_data_NEI_data.zip")
  
  # save downloading date
  file <- file("./downldate.txt")
  writeLines(date(), file)
  close(file)
}

#  unzip (check if data already in the directory)
if(!sum("summarySCC_PM25.rds"== dir()|"Source_Classification_Code.rds"== dir())==2){
  unzip("./exdata_data_NEI_data.zip",exdir=".")
}

# reading the data set (check if already read)
if(!exists("NEI")|!exists("SCC")){
  # National Emission Inventory
  NEI <- readRDS("summarySCC_PM25.rds")
  # Source Classification Code
  SCC <- readRDS("Source_Classification_Code.rds")
}

# Question 1 :
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

# sum emission by year and convert to kilotons
pm_year <- aggregate(NEI$Emissions, by=list(Category=NEI$year), FUN=sum)
pm_year$x <- round(pm_year$x/1000,1)

# calc evolution rate (absolute value)
evo <- (pm_year$x[4]-pm_year$x[1])/pm_year$x[1]*100
evo <- as.character(round(sqrt(evo^2),1))

##############
## plotting ##
##############
# save it to a PNG file
png(filename = "plot1.png")

# barplot (font.main and cex.main are options for the title)
barplot(pm_year$x, names.arg=pm_year$Category, font.main = 2, cex.main = 1.5,
        main=expression(paste('Total PM',''[2.5],' emissions in the US from 1999 to 2008')), 
        ylab= expression(paste('PM', ''[2.5], ' emissions (kilotons)')), xlab="Year", col="red")

# add annotation (under title, complete the title which answers the question)
mtext(paste("decreased",evo,"%"), side = 3, line = 0, outer = FALSE, cex=1.5)

#close file
dev.off()

# remove intermediary variables
rm(list=ls()[!(ls()=="NEI"|ls()=="SCC")])