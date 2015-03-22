#1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?

#usage: source("plot1.R")
#output: plot1.png

#clear workspace
rm(list=ls())

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)

#make sure requried packages are installed and loaded
if(!require(dplyr) | !require(ggplot2) | !require(tidyr) | !require(magrittr)){
  stop('The required packages not installed')
}

#make sure files are located in your current working directory.
#check if the data files are located in the current working directory
if (!file.exists("summarySCC_PM25.rds")){
  stop("Error, data file summarySCC_PM25.rds not found in the current directory")}

if (!file.exists("Source_Classification_Code.rds")){
  stop("Error, data file Source_Classification_Code.rds not found in the current directory")}

#read data files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


#group by year and then compute total emissions from all sources
#Note the use %T>% operator and with for chaining.
 
png("plot1.png")
NEI%>%
  group_by(year)%>%
  summarize(total=sum(Emissions))%T>%
    with(barplot(total,names=year,
    xlab=("Year"),
    ylab=("Total PM2.5 Emissions (in tons)"),
    main="Total PM2.5 emissions in US over years ",col="red"))
dev.off()