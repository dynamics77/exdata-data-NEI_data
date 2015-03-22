#4. Across the United States, how have emissions 
#from coal combustion-related sources changed from 1999–2008?

#usage: source("plot4.R")
#output: plot4.png

#clear workspace
rm(list=ls())

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(gridExtra)

#make sure requried packages are installed and loaded
if(!require(dplyr) | !require(ggplot2) | !require(tidyr) | !require(magrittr) | !require(gridExtra)){
  stop('The required packages not installed')
}

#check if the data files are located in the current working directory
if (!file.exists("summarySCC_PM25.rds")){
  stop("Error, data file summarySCC_PM25.rds not found in the current directory")}

if (!file.exists("Source_Classification_Code.rds")){
  stop("Error, data file Source_Classification_Code.rds not found in the current directory")}

#Read two data files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset coal combustion emissions for all US
#Select "coal" SCC codes from EI. Sector.
#use these SCC codes to select coal Emissions data 
#summarize data. group by year and compute total emissions.
#In addition to just computing total emissions. I have decided to 
#also plot emissions data using boxplots and combined two plots
#into one via viewport (See below). 
#I hope you will agree that plotting more data provides better picture of
#how coal combustion related emissions have changes in US over years.

#make boxplot of coal emissions data
#check how median value has changed.

p1<-select(SCC,SCC, EI.Sector)%>%
  filter(grepl("Coal",EI.Sector))%$%
  NEI[NEI$SCC %in% .$SCC,]%>%
  group_by(year)%>%
  transmute(.,Emissions=ifelse(Emissions==0,1,Emissions))%>%  
    ggplot(data=.,aes((as.character(year)),Emissions)) +
    geom_point() +
    geom_boxplot(outlier.colour = "hotpink") +
    scale_y_log10() +
    geom_jitter(position = position_jitter(width = 0.05, height = 0), alpha = 1/4) +
    xlab("Year") +  
    ylab("Emissions (in tons) on log scale") +
    theme_bw() +
    ggtitle("Emissions from coal combustion-related sources in US")

#Now plot total emissions from coal combustion related sources in US

p2<-select(SCC,SCC, EI.Sector)%>%
    filter(grepl("Coal",EI.Sector))%$%
    NEI[NEI$SCC %in% .$SCC,]%>%
    group_by(year)%>%
    summarise(total=sum(Emissions))%>%
      ggplot(data= .,aes(as.character(year),total)) +
      geom_bar(stat="identity") + 
      guides(fill=FALSE) +
      theme_bw() + 
      xlab("Year") +
      ylab("Total emissions (in tons)")
 
# finally arrange and save the plots
png(file="plot4.png",w=1800,h=1800, res=300)
 grid.newpage()
 v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
 v2<-viewport(width = 0.4, height = 0.4, x = 0.4, y = 0.30) #plot area for the inset map
 print(p1,vp=v1) 
 print(p2,vp=v2)
dev.off()

