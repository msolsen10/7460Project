library(shiny)
library(ggplot2)
library(leaflet)
library(magrittr)
library(gridExtra)
library(knitr)
library(tinytex)
library(rmarkdown)

newbuoydata<-read.csv('data/newFormattedBuoyData.csv')
newbuoydata$DateTime <- as.Date(newbuoydata$DateTime,format="%m/%d/%Y")
colnames(newbuoydata)[12] = "Phycocyanin-RFU"

