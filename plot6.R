#6. Compare emissions from motor vehicle sources in Baltimore City 
#with emissions from motor vehicle sources in Los Angeles County

#usage: source("plot6.R")
#output: plot6.png

#clear the workspace
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

#Read two data files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Extract SCC codes for on road vehicle emissions 
df.vh<-select(SCC,SCC,EI.Sector)%>%
       filter(grepl("Vehicles",EI.Sector))

#Subset Baltimore city vehicle emissions data based on SCC codes in df.vh
#Summarize data and compute total emissions.
#note the use use magrittr place holder (.)

df.bal<-NEI%>%
       filter(fips == "24510")%>%
      .[.$SCC%in% df.vh$SCC,]%>%
       group_by(year)%>%
       summarise(BAL=sum(Emissions))

#Similarly, subset LA county on road vehicle emissions data
#Summarize data and compute total emissions.
df.la<-NEI%>%
      filter(fips =="06037")%>%
      .[.$SCC%in% df.vh$SCC,]%>%
       group_by(year)%>%
       summarise(LA=sum(Emissions))

#Combine Baltimore city (df.la) and LA county (df.la) data in one data frame (bind_cols)
#Plot la vs baltimore city vehicles emissions.
#Notice the use of y-log transform to “spread out” data 
#code below uses chaining in dplyr and magrittr packages
#Approach below saves retyping and avoids need for saving intermediate variables.

ggsave("plot6.png")
 bind_cols(df.bal,df.la[2])%>%
 gather(Place,total,-year)%>%
   ggplot(data= .,aes(year,total)) +
     geom_line(aes(color=Place),size=1.2) +
     geom_point(aes(col=Place),size=4) +
     scale_y_log10() +
     xlab("Year") +
     ylab("Total emissions from Vehicles") +
     ggtitle("Vehicular emissions BAL vs LA on Log scale") +
     theme_bw()
dev.off()
