###############
## libraries ##
###############
# check if already installed, else ask to install it

packs <- c("ggplot2")

for(pack in packs){
  answer <- 0
  if(sum(installed.packages()==pack,na.rm=TRUE)==0){
    while(answer!="n" & answer!="y"){
      print(paste(pack, "package is not installed. Do you agree to install it? (type y/n)"))
      answer<-readline()
    }
    if(answer=="y"){install.packages(pkgs = as.character(pack))} 
  }    
}

library(ggplot2)

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

# Question 6 :
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor 
# vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

###
# copy to console from here
###

###
# select Baltimore
###
baltimore <- NEI[NEI$fips=="24510",]

# select motor vehicle sources (not case sensitive)
# result from selection from EI.Sector:
#[1] Mobile - On-Road Gasoline Light Duty Vehicles
#[2] Mobile - On-Road Gasoline Heavy Duty Vehicles
#[3] Mobile - On-Road Diesel Light Duty Vehicles  
#[4] Mobile - On-Road Diesel Heavy Duty Vehicles 
vehicle <- SCC[grepl("vehicle",SCC$EI.Sector, ignore.case=TRUE),]

# select corresponding rows in NEI/baltimore
vehicle_pm <- merge(x=baltimore, y=vehicle, by='SCC', all = FALSE)

# add new variable: fuel type
vehicle_pm$fuel.type <- ifelse(grepl("Diesel",vehicle_pm$EI.Sector,ignore.case=TRUE),
                               "Diesel","Gasoline")
# calc sum by year
vehicle_pm <- aggregate(vehicle_pm$Emissions, by=list(Category=vehicle_pm$year,vehicle_pm$fuel.type), FUN=sum)

# calc evo rate (vector as a result: 1 = Diesel, 2 = Gasoline, 3 = Total)
# (absolute values)
evo <- (sum(vehicle_pm$x[vehicle_pm$Category==2008])-
          sum(vehicle_pm$x[vehicle_pm$Category==1999]))/
  sum(vehicle_pm$x[vehicle_pm$Category==1999])*100
evo <- as.character(round(sqrt(evo^2),1))

# add new variable : city name (baltimore)
vehicle_pm$city <- "Baltimore City (MD)" 

###
# select LA
###

# same thing as for Baltimore

LA <- NEI[NEI$fips=="06037",]

# select corresponding rows in NEI/baltimore
vehicle_pm1 <- merge(x=LA, y=vehicle, by='SCC', all = FALSE)

vehicle_pm1$fuel.type <- ifelse(grepl("Diesel",vehicle_pm1$EI.Sector,ignore.case=TRUE),
                               "Diesel","Gasoline")
vehicle_pm1 <- aggregate(vehicle_pm1$Emissions, by=list(Category=vehicle_pm1$year,vehicle_pm1$fuel.type), FUN=sum)

# calc evo rate
evo1 <- (sum(vehicle_pm1$x[vehicle_pm1$Category==2008])-
          sum(vehicle_pm1$x[vehicle_pm1$Category==1999]))/
  sum(vehicle_pm1$x[vehicle_pm1$Category==1999])*100
evo1 <- as.character(round(sqrt(evo1^2),1))

# baltimore
vehicle_pm1$city <- "Los Angeles County (CA)"

vehicle_pm <- rbind(vehicle_pm,vehicle_pm1)

##############
## plotting ##
##############
# save it to a PNG file (doesn't work the first time as a script, need to be
# pasted into the console)
png(filename = "plot6.png")

# qplot (ggplot easy barplot)
# facet_grid is the option used for plot each city side by side
qplot(y=x, x=Category,  data=vehicle_pm, geom="bar", 
      stat="identity", fill = Group.2)+facet_grid(~city)+
  # axis and title
  labs(x=paste("Year\n\nBaltimore City total emissions decreased",evo,"%","\nLos Angeles County total emissions increased",evo1,"%"), y=expression('PM'[2.5]*" emissions (tons)"), 
       title="Motor Vehicles PM2.5 emissions in Baltimore City (MD)\nand Los Angeles County (CA) from 1999 to 2008 by fuel types") +
  # title options
  theme(plot.title = element_text(size=15, face="bold", vjust=2)) +
  # x axis scale option
  scale_x_continuous(breaks = unique(vehicle_pm$Category)) +
  # legend options
  scale_fill_manual(name="Fuel types",
                     values=c("#999966", "#663300"), 
                     breaks=c("Gasoline","Diesel"),
                     labels=c("Gasoline","Diesel"))


#close file
dev.off()

# remove intermediary variables
rm(list=ls()[!(ls()=="NEI"|ls()=="SCC")])