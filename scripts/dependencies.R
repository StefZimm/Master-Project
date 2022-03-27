required_packages <- c(
  "checkpoint"
)

# install missing packages

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}

rm(new.packages)

library(checkpoint)
checkpoint(snapshotDate ='2022-03-27')
library(weanderson)
library(raster)
library(sf)
library(rgeos) 
library(dplyr)
library(tools)
library(stringr)
library(ggplot2)
library(plotly) 
library(wesanderson) 
library(randomcoloR) 
library(tidyverse)
library(dplyr)
library(randomcoloR)
