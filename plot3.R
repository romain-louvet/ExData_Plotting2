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

# Question 3 :
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make 
# a plot answer this question.

###
# COPY to console from here
###

# select balimore city
baltimore <- NEI[NEI$fips=="24510",]

##############
## plotting ##
##############
# save it to a PNG file (works only if paste in R console, not if used as a script)
png(filename='plot3.png')

# line + points with ggplot
# aes = aestetic
ggplot(baltimore, aes(x = year, y = Emissions, color = type)) +
  # add to base plot, lines (summary for each color type)
  geom_line(stat = "summary", fun.y = "sum", size=1) +
  # add labels
  labs(x="Year", y=expression('PM'[2.5]*" emissions (tons)"), 
       title='Total PM2.5 Emissions in Baltimore City (MD)\nfrom 1999 to 2008 per sources types') +
  # title options
  theme(plot.title = element_text(size=15, face="bold", vjust=2)) +
  # legend
  scale_color_manual(name="Types of sources",
                     values=c("#40FF00", "#58D3F7","#003399","#FE2E2E"), 
                     breaks=c("NON-ROAD", "NONPOINT", "ON-ROAD","POINT"),
                     labels=c("Non-road", "Non-point", "On-road", "Point")) +
  # add points
  geom_point(stat = "summary", fun.y = "sum",size=4) +
  # x axis option
  scale_x_continuous(breaks=unique(baltimore$year)) +
  # annotation
  annotate("text", x = 2004, y = 2000, label = "every sources type decreased,\nexcept for point")

#close file
dev.off()

# remove intermediary variables
rm(list=ls()[!(ls()=="NEI"|ls()=="SCC")])