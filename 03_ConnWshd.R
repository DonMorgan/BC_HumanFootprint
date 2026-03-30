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

#assign corridor/climate matrix values to make new raster
#Read in files and prep
BCrBuff <-raster(file.path(spatialOutDir,'BCrBuff.tif'))
wshd<-st_read(file.path(spatialOutDir,"wshd.gpkg")) %>%
  dplyr::select(WTRSHD_FID,GNIS_NM_1,WTRSHD_OR,WTRSHD_MAG,AREA_HA,OBJECTID)
#wshdr<- fasterize(wshd, BCrBuff, field="wshd_id")
wshdv<-vect(wshd)
ConCorr_c<-rast(file.path(spatialOutDir,paste0('ConCorr_c.tif')))
ConCC<-rast(file.path(spatialOutDir,paste0('ConCC.tif')))
CC_c<-rast(file.path(spatialOutDir,paste0('CC_c.tif')))
#Multiply EcoFeat by 10 to get into the class used
EcoFeat_c<-rast(file.path(SpatialDir,paste0('1_eco_focal_class_final.tif')))*10
Threat_c<-subst(rast(file.path(SpatialDir,paste0('3_threat_combined_wt_max_class.tif'))),NA,1)
#unique(values(Threat_c))


#Get modal value in watershed
get_mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- stats::na.omit(x)
  }
  # Calculate mode for a vector of values
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
### Assign modal to watershed - climate connectivity classification, conservation connectivity classification,
# conservation-climate connectivity score, Ecological score, Threats score
wshd_connect.1<-wshd
#First modal of climate connectivity
#ClimateConnect_Rank <- terra::extract(CC_c, wshdv, fun = max, na.rm = TRUE)
ClimateConnect_Rank <- terra::extract(CC_c, wshdv, fun = get_mode, na.rm = TRUE)
wshd_connect.1$ClimateConnect_Rank <- ClimateConnect_Rank[,2] # Column 2 is the result, ID maps to row id
CCCls_LUT<-data.frame(ClimateConnect_Rank=c(1,2,3,4,5),ClimateConnect_Class=c('VeryLow','Low','Mod','High','VeryHigh'))
#wshd_cc<-wshd %>%
#  left_join(CCCls_LUT)
#Second modal of straight conservation connectivity
#ConserveConnect_Rank <- terra::extract(ConCorr_c, wshdv, fun = max, na.rm = TRUE)
ConserveConnect_Rank <- terra::extract(ConCorr_c, wshdv, fun = get_mode, na.rm = TRUE)
wshd_connect.1$ConserveConnect_Rank <- ConserveConnect_Rank[,2] # Column 2 is the result, ID maps to row id
ConnCls_LUT<-data.frame(ConserveConnect_Rank=c(1,2,3,4,5),ConserveConnect_Class=c('Limited','Impeded','Diffused','Intensified','Channelized'))
#wshd_con_cc<-wshd_cc %>%
##  left_join(ConnCls_LUT)
#Third modal of combined conservation and climate change
#ConserveClimateConnect_Score <- terra::extract(ConCC, wshdv, fun = max, na.rm = TRUE)
ConserveClimateConnect_Score <- terra::extract(ConCC, wshdv, fun = get_mode, na.rm = TRUE)
wshd_connect.1$ConserveClimateConnect_Score <- ConserveClimateConnect_Score[,2] # Column 2 is the result, ID maps to row id
#Fourth modal of Ecological Features
#Eco_Score <- terra::extract(EcoFeat_c, wshdv, fun = max, na.rm = TRUE)
Eco_Score <- terra::extract(EcoFeat_c, wshdv, fun = get_mode, na.rm = TRUE)
wshd_connect.1$Eco_Score <- Eco_Score[,2] # Column 2 is the result, ID maps to row id
Eco_LUT<-data.frame(Eco_Score=c(10,20,30,40,50),EcoClass=c('VeryLow','Low','Mod','High','VeryHigh'))
#wshd_con_cc_E<-wshd_con_cc %>%
#  left_join(Eco_LUT)
#Fifth modal of Threat
#Threat_Score <- terra::extract(Threat_c, wshdv, fun = max, na.rm = TRUE)
Threat_Score <- terra::extract(Threat_c, wshdv, fun = get_mode, na.rm = TRUE)
wshd_connect.1$Threat_Score <- Threat_Score[,2] # Column 2 is the result, ID maps to row id
Threat_LUT<-data.frame(Threat_Score=c(1,2,3,4,5),ThreatClass=c('VeryLow','Low','Mod','High','VeryHigh'))
#Assign to watersheds
#wshd_con_cc_ET<-wshd_con_cc_E %>%
#  left_join(Threat_LUT)
wshd_connect.2<-wshd_connect.1 %>%
  left_join(CCCls_LUT) %>%
  left_join(ConnCls_LUT) %>%
  left_join(Eco_LUT) %>%
  left_join(Eco_LUT)

### Look up tables to calculate Ecological_Connectivity_Score, Threat_Ecological_Connectivity_Score
#Read in Ecological table and form as a look up table
ConnEco_LUT<-data.frame(read_excel(file.path(DataDir,'ConnEco_LUT_3Mar2026.xlsx')))
colnames(ConnEco_LUT)<-ConnEco_LUT[1,]
ConnEco_LUT<-ConnEco_LUT[2:5,]
ConnEco_Assign_LUT<-pivot_longer(ConnEco_LUT, cols=c('50','40','30','20','10'),names_to='EcoScore') %>%
  unite(EcoConn, c('Score','EcoScore')) %>%
  rename(EcoConn_Score=value)

#Read in Threat Look up table and format as a look up table
EcoThreat_LUT<-data.frame(read_excel(file.path(DataDir,'ConnEcoThreat_LUT_22Feb2026.xlsx')))
colnames(EcoThreat_LUT)<-EcoThreat_LUT[1,]
EcoThreat_LUT<-EcoThreat_LUT[2:4,]
EcoThreat_Assign_LUT<-pivot_longer(EcoThreat_LUT, cols=c('5','4','3','2','1'),names_to='ThreatScore') %>%
  unite(EcoConnThreat, c('Score','ThreatScore')) %>%
  dplyr::rename(EcoConnThreat_Score=value)

#Assign to watershed
wshd_connect<-wshd_connect.2 %>%
  mutate(EcoConn=paste(as.character(ConserveClimateConnect_Score),as.character(Eco_Score),sep='_')) %>%
  left_join(ConnEco_Assign_LUT) %>%
  mutate(EcoConnThreat=paste(as.character(EcoConn_Score),as.character(Threat_Score),sep='_')) %>%
  left_join(EcoThreat_Assign_LUT)

#Write to spatial
write_sf(wshd_connect, file.path(spatialOutDir,'wshd_connect_mode.gpkg'), overwrite=TRUE)

