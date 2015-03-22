#5 How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
#usage: source("plot5.R")
#output: plot5.png

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

#check if the data files are located in the current working directory
if (!file.exists("summarySCC_PM25.rds")){
  stop("Error, data file summarySCC_PM25.rds not found in the current directory")}

if (!file.exists("Source_Classification_Code.rds")){
  stop("Error, data file Source_Classification_Code.rds not found in the current directory")}

#read data files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Extract SCC codes for on-road vehicles emissions from EI.Sector
df.vh<-select(SCC,SCC,EI.Sector)%>%
       filter(grepl("Vehicles",EI.Sector))

#Extract all Baltomore city emissions data 
#Then use SCC codes in "df.vh" to extract Baltimore city vehicle emissions data
#Both subsetting data and plotting done via chanining in code below.
#

ggsave("plot5.png")
 NEI%>%
  filter(fips == "24510")%>%
  .[.$SCC %in% df.vh$SCC,]%>%
  group_by(year)%>%
  summarise(BAL=sum(Emissions))%>%
    ggplot(data = .,aes(as.character(year),BAL))+
    geom_bar(stat="identity")+
    theme_bw() + 
    xlab("Year")+
    ylab("Total emissions (in tons)")+
    ggtitle("Emissions from motor vehicle sources in Baltimore city")
dev.off()
