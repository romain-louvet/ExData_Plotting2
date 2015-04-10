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


# Question 2 :
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# select balimore city
baltimore <- NEI[NEI$fips=="24510",]

# sum emission by year
baltimore_sum <- aggregate(baltimore$Emissions, by=list(Category=baltimore$year), FUN=sum)

# calc evo rate (absolute value)
evo <- (baltimore_sum$x[4]-baltimore_sum$x[1])/baltimore_sum$x[1]*100
evo <- as.character(round(sqrt(evo^2),1))

##############
## plotting ##
##############
# save it to a PNG file
png(filename = "plot2.png")

# same as plot1.R:
# barplot 
barplot(baltimore_sum$x, names.arg=baltimore_sum$Category, font.main = 2, cex.main = 1.5,
        main=expression(paste('Total PM',''[2.5],' emissions in Baltimore City (MD)')), 
        ylab= expression(paste('PM', ''[2.5], ' emissions (tons)')), xlab="Year", col="red")

# annotation
mtext(paste("from 1999 to 2008 decreased",evo,"%"), side = 3, line = 0, outer = FALSE, cex=1.5)

#close file
dev.off()

# remove intermediary variables
rm(list=ls()[!(ls()=="NEI"|ls()=="SCC")])