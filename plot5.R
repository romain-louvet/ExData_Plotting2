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

###
# copy from here
###

# Question 5 :
# How have emissions from motor vehicle sources changed from 1999–2008 
# in Baltimore City?

# select balimore city
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

# add totals (since only four years, I didn't use a function)
total99 <- data.frame(Category=1999,Group.2="Total",x=sum(vehicle_pm[vehicle_pm$Category==1999,]$x))
total02 <- data.frame(Category=2002,Group.2="Total",x=sum(vehicle_pm[vehicle_pm$Category==2002,]$x))
total05 <- data.frame(Category=2005,Group.2="Total",x=sum(vehicle_pm[vehicle_pm$Category==2005,]$x))
total08 <- data.frame(Category=2008,Group.2="Total",x=sum(vehicle_pm[vehicle_pm$Category==2008,]$x))
vehicle_pm <- rbind(vehicle_pm,total99)
vehicle_pm <- rbind(vehicle_pm,total02)
vehicle_pm <- rbind(vehicle_pm,total05)
vehicle_pm <- rbind(vehicle_pm,total08)

# calc evo rate (vector as a result: 1 = Diesel, 2 = Gasoline, 3 = Total)
# (absolute values)
evo <- (vehicle_pm$x[vehicle_pm$Category==2008]-
          vehicle_pm$x[vehicle_pm$Category==1999])/
  vehicle_pm$x[vehicle_pm$Category==1999]*100
evo <- as.character(round(sqrt(evo^2),1))

##############
## plotting ##
##############
# save it to a PNG file (doesn't work the first time as a script, need to be
# pasted into the console)
png(filename = "plot5.png")

# ggplot, lines, color by group (fuel types and total)
# this plot can tell how emissions changed over time with
# precision of the fuel type
ggplot(vehicle_pm, aes(x = Category, y = x, color = Group.2)) +
  # axis and title
  labs(x="Year", y=expression('PM'[2.5]*" emissions (tons)"), 
       title="Motor Vehicle PM2.5 emissions in Baltimore City (MD)\nfrom 1999 to 2008 by fuel types") +
  # title options
  theme(plot.title = element_text(size=15, face="bold", vjust=2)) +
  # x axis scale option
  scale_x_continuous(breaks = unique(vehicle_pm$Category)) +
  # add line
  geom_line(stat = "identity", fun.y = "sum", size=1) +
  # add points
  geom_point(stat = "identity", fun.y = "sum", size=3) +
  # legend options
  scale_color_manual(name="Fuel types",
                    values=c("#999966", "#663300","#000000"), 
                    breaks=c("Gasoline","Diesel","Total"),
                    labels=c("Gasoline","Diesel","Both")) +
  # annotations
  annotate("text", x = 2002, y = 45, label = paste("Gasoline decreased",evo[2],"%"),color="#663300") +
  annotate("text", x = 2005, y = 90, label = paste("Diesel decreased",evo[1],"%"),color="#999966") +
  annotate("text", x = 2005, y = 150, label = paste("Total decreased",evo[3],"%"),color="#000000")

#close file
dev.off()

# remove intermediary variables
rm(list=ls()[!(ls()=="NEI"|ls()=="SCC")])