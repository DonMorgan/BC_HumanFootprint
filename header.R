library(sf)
library(dplyr)
library(readr)
library(raster)
library(bcmaps)
library(bcdata)
library(fasterize)
library(readxl)
library(mapview)
library(WriteXLS)
library(foreign)
library(ggplot2)
library(ggnewscale)
library(viridis)
library(stars)
library(terra)
#library(RCurl)

options(scipen=999)
options(warn = 1)
options(timeout=100000)

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
tileOutDir <- file.path(dataOutDir,'tile')
figsOutDir <- file.path(OutDir,'figures')
spatialOutDir <- file.path(OutDir,'spatial')
SpatialDir <- file.path('data','spatial')
RoadsDir<-file.path(SpatialDir,'roads')
DisturbDir<-file.path(SpatialDir,'disturb')
DataDir <- 'data'
#Change this to a local path for storing footprint data as input to conservation connectivity model
CorrDir<- file.path('/Users/dmorgan/Sync/_dev/Biodiversity/BC_ConservationConnectivity/data/spatial')
#Local directory of GIS files such as HillShade for plotting
#GISLibrary<- file.path('/Users/dmorgan/ProjectLibrary/Library/GISFiles/BC')
GoogleDir<-file.path('/Users/dmorgan/Google Drive/Shared drives/Provincial Connectivity/Data Wrangling/Required Spatial Files')

dir.create(file.path(OutDir), showWarnings = FALSE)
dir.create(file.path(dataOutDir), showWarnings = FALSE)
dir.create(file.path(tileOutDir), showWarnings = FALSE)
dir.create(file.path(RoadsDir), showWarnings = FALSE)
dir.create(file.path(DisturbDir), showWarnings = FALSE)
dir.create(file.path(figsOutDir), showWarnings = FALSE)
dir.create(DataDir, showWarnings = FALSE)
dir.create("tmp", showWarnings = FALSE)
dir.create("tmp/AOI", showWarnings = FALSE)
dir.create(file.path(spatialOutDir), showWarnings = FALSE)





