# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#Clean Disturbance  Layer

#Assign weights to layer - based on age of cutbocks
#this is built off the spreadsheet built off raster's legend in data directory
#More recent cutblocks will have the same resistance level as the current level from the lookup table

Age<-rast(file.path(spatialOutDir,'Age.tif'))
disturbance_sfR<-rast(file.path(spatialOutDir,'disturbance_sfR.tif'))
AreaDisturbance_LUT<-data.frame(read_excel(file.path(DataDir,'AreaDisturbance_LUTH.xlsx'))) %>%
  dplyr::select(disturb,ID=disturb_Code,Resistance,SourceWt, BinaryHF)
#Reclass disturbance to pull out cutblocks only
reclass_m<-matrix(c(
  0,8,0,
  9,10,1,
  11,20,0
),ncol=3,byrow=TRUE)
#Raster of cutblock age
cutblockR <- classify(disturbance_sfR, reclass_m, right = TRUE)
cblock_Age<-cutblockR*Age

#Load the excel file created previously and get the cutblokcs only
#AreaDisturbance_LUT_cutblock<-AreaDisturbance_LUT %>%
#  filter(grepl("Cutblock",disturb)) %>%
#  dplyr::select(ID,Resistance,SourceWt, BinaryHF)

#Getting the lowest and highest age of the cutblocks
minmax_cutblock_age<-c(
  cellStats(raster(cblock_Age),"min"),
  cellStats(raster(cblock_Age),"max")
  )

#Create a lookup table
AreaDisturbance_LUT_cblock<-data.frame(Resistance=seq(from=min(AreaDisturbance_LUT$Resistance),max(AreaDisturbance_LUT$Resistance),
            length.out=10),
           SourceWt=seq(from=max(AreaDisturbance_LUT$SourceWt),min(AreaDisturbance_LUT$SourceWt),length.out=10),
          Age=seq(min(minmax_cutblock_age),max(minmax_cutblock_age),length.out=10),
          BinaryHF=1) %>% mutate_at(c("Resistance",'Age'),round)

training_data<-data.frame(Age=c(1900,1928,1983,2011,2025),
           Resistance=c(1,1,15,45,64))

model_cb<-lm(log(Resistance)~ 1+Age,data=training_data)

AreaDisturbance_LUT_cblock$Resistance=round(exp(predict(model_cb,newdata=AreaDisturbance_LUT_cblock)))

#Create a matrix to reclassify the cutblock raster:
AreaDisturbance_LUT_cblock$Age2<- AreaDisturbance_LUT_cblock$Age %>% lag(default=0)

AreaDisturbance_LUT_cblock_reclass<-AreaDisturbance_LUT_cblock %>% dplyr::select(Age2,Age,Resistance) %>%
  as.matrix()

#Provincial weights
cblock_sf_raster_WP_file <- file.path(spatialOutDir,"cblock_sf_raster_WP.tif")
if (!file.exists(cblock_sf_raster_WP_file)) {
  cblock_sf_raster_WP<-classify(rast(cblock_Age),
#cblock_sf_raster_WP<-classify(rast(file.path(spatialOutDir,'cblock_sf_raster.tif')),
                                                              rcl=AreaDisturbance_LUT_cblock_reclass,
                                  include.lowest=T)

  writeRaster(cblock_sf_raster_WP, filename=file.path(spatialOutDir,'cblock_sf_raster_WP.tif'), overwrite=TRUE)

 }else{
   cblock_sf_raster_WP<-rast(file.path(spatialOutDir,'cblock_sf_raster_WP.tif'))
 }

