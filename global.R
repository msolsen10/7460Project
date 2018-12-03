library(shiny)
library(ggplot2)
library(leaflet)
library(magrittr)
library(gridExtra)
library(knitr)
library(tinytex)
library(rmarkdown)
library(plot3D)

newbuoydata<-read.csv('data/newformattedbuoydata.csv')
newbuoydata$DateTime <- as.Date(newbuoydata$DateTime,format="%m/%d/%Y")
colnames(newbuoydata)[12] = "Phycocyanin-RFU"
#newbuoydata$Latitude <- as.numeric(levels(newbuoydata$Latitude))[newbuoydata$Latitude]
#newbuoydata$Longitude <- as.numeric(levels(newbuoydata$Longitude))[newbuoydata$Longitude]

str(newbuoydata)

