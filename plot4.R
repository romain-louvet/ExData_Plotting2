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

# Question 4 :
# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?

# select coal combustion-related sources (not case sensitive)
# result from selection from EI.Sector:
#[1] Fuel Comb - Electric Generation - Coal     
#[2] Fuel Comb - Industrial Boilers, ICEs - Coal
#[3] Fuel Comb - Comm/Institutional - Coal
coal <- SCC[grepl("coal",SCC$EI.Sector, ignore.case=TRUE),]

# select corresponding rows in NEI
coal_pm <- merge(x=NEI, y=coal, by='SCC', all = FALSE)

# calc sum by year, in kilotons
coal_sum <- aggregate(coal_pm$Emissions, by=list(Category=coal_pm$year), FUN=sum)
coal_sum$x <- round(coal_sum$x/1000,1)

# calc evo rate (absolute value)
evo <- (coal_sum$x[4]-coal_sum$x[1])/coal_sum$x[1]*100
evo <- as.character(round(sqrt(evo^2),1))

##############
## plotting ##
##############
# save it to a PNG file
png(filename = "plot4.png")

# baseplot: line with points
plot(coal_sum$Category,coal_sum$x, type="b", axes = FALSE, col="red", lwd=2, cex.main=1.5,
     main=expression(paste('PM',''[2.5],' emissions from coal combustion related sources')), 
     ylab= expression(paste('PM', ''[2.5], ' emissions (kilotons)')), xlab="Year")

mtext(paste(" in the US from 1999 to 2008 decreased",evo,"%"), side = 3, line = 0, outer = FALSE, cex=1.5)

axis(side=1, at=coal_sum$Category)
axis(side =2)
box()

# add labels to points (since only 4 points, no need for a function)
text(x=coal_sum$Category[1]+0.1,y=coal_sum$x[1]+3,labels=coal_sum$x[1], pos=4, col ="red", font=2)
text(x=coal_sum$Category[2],y=coal_sum$x[2],labels=coal_sum$x[2], pos=1, col ="red", font=2)
text(x=coal_sum$Category[3],y=coal_sum$x[3],labels=coal_sum$x[3], pos=3, col ="red", font=2)
text(x=coal_sum$Category[4],y=coal_sum$x[4],labels=coal_sum$x[4], pos=2, col ="red", font=2)

#close file
dev.off()

# remove intermediary variables
rm(list=ls()[!(ls()=="NEI"|ls()=="SCC")])