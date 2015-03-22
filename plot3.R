#3. Of the four types of sources indicated by the type 
#(point, nonpoint, onroad, nonroad) variable, which of these four sources have 
#seen decreases in emissions from 1999–2008 for Baltimore City?

#usage: source("plot3.R")
#output: plot3.png

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

#Instead of plotting summaries I am using boxplots with the emissions data overlayed.
#I believe plotting more of the actual data allows identfication of outliers,
#relationships, correlations, etc.

#Note use of Y-log scale that allows to spread out data.
# However, 0 values of Emission on the logaritmic rescaling of y-axis provides "-Inf"
# To solve this problem replace all "0"-values to 
# "1" values. Note that this is reasonable because log(1)=0!
# In addition means are shown in the diamond shape on the box plot using stat_summary()
#Note: Outliers shown in "hotpink" color
#geom_jitter used to see overlapped data points

ggsave("plot3.png")
NEI%>%
  filter(fips == "24510")%>%
  group_by(year,type)%>%
  transmute(.,Emissions=ifelse(Emissions==0,1,Emissions))%>%
     ggplot(data = .,aes(as.character(year),Emissions))+
     geom_boxplot(outlier.colour = "hotpink",aes(fill=type)) + 
     scale_y_log10() +
     facet_wrap(~type,scale="free_y")+
     geom_point(alpha=0.2) +
     geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4) +
     stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white") +
     ylab("Emissions (in tons)") +
     xlab("Year") + 
     ggtitle("Emissions by type in Baltimore City over years")+
     guides(fill=FALSE) +
     guides(color=FALSE) +
     theme_bw()
dev.off()
