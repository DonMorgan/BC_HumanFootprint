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

#Rasterize the Province for subsequent masking
# bring in BC boundary

bc <- bcmaps::bc_bound()
Prov_crs<-crs(bc)
#Prov_crs<-"+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

#Provincial Raster to place rasters in the same geo reference
BCr_file <- file.path(spatialOutDir,"BCr.tif")
if (!file.exists(BCr_file)) {
  BC<-bcmaps::bc_bound_hres(class='sf')
  saveRDS(BC,file='tmp/BC')
  ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5,
                   ymn=173787.5, ymx=1748187.5,
                   crs=Prov_crs,
                   res = c(100,100), vals = 1)
  ProvRast_S<-st_as_stars(ProvRast)
  write_stars(ProvRast_S,dsn=file.path(spatialOutDir,'ProvRast_S.tif'))
  BCr <- fasterize(BC,ProvRast)
  #Linear rasterization of roads works better using the stars package
  BCr_S <-st_as_stars(BCr)
  write_stars(BCr_S,dsn=file.path(spatialOutDir,'BCr_S.tif'))
  writeRaster(BCr, filename=BCr_file, format="GTiff", overwrite=TRUE)
  writeRaster(ProvRast, filename=file.path(spatialOutDir,'ProvRast'), format="GTiff", overwrite=TRUE)

  #Generate a buffer around BC
  BC_BB <- st_bbox(BC)
  #Make a bounding box x% larger than width
  BCBuff_l<-round((BC_BB$xmax-BC_BB$xmin)*0.2,0)
  BCBuff <- BC %>%
    st_simplify(dTolerance = 1000) %>%
    st_buffer(dist=BCBuff_l)
  st_write(BCBuff,file.path(spatialOutDir,'BCBuff.gpkg'))

  #Generate a raster 20% larger than ProvRast
  ProvRastBT<-rast(ProvRast)
  extProv<-ext(ProvRastBT)
  new_ext<-extProv * 1.4
  # Extend the raster to the new extent
  ProvRastB <- extend(ProvRastBT, new_ext)
  writeRaster(ProvRastB,file.path(spatialOutDir,'ProvRastB.tif'),overwrite=T)

  #Use ProvRastB to create a new Provincial buffer
  BCBuff<-st_read(file.path(spatialOutDir,'BCBuff.gpkg'))
  BCrBuff <- rasterize(vect(BCBuff),ProvRastB)
  writeRaster(BCrBuff,file.path(spatialOutDir,'BCrBuff.tif'), overwrite=T)
  BCrB_S <-st_as_stars(BCrBuff)

} else {
  BCr <- raster(BCr_file)
  BCrT <- rast(BCr_file)
  ProvRast<-raster(file.path(spatialOutDir,'ProvRast.tif'))
  #BCr_S <- read_stars(file.path(spatialOutDir,'BCr_S.tif'))
  ProvRastB<-rast(file.path(spatialOutDir,'ProvRastB.tif'))
  BC <-readRDS('tmp/BC')
  BCBuff<-st_read(file.path(spatialOutDir,'BCBuff.gpkg'))
  BCrBuff <- rasterize(vect(BCBuff),ProvRastB)
  BCrB_S <-st_as_stars(BCrBuff)
}

message('Breaking')
break

############
EcoR<-bcdc_get_data("WHSE_TERRESTRIAL_ECOLOGY.ERC_ECOREGIONS_SP")
write_sf(EcoR, file.path(spatialOutDir,"EcoR.gpkg"))

EcoP<-bcdc_get_data("WHSE_TERRESTRIAL_ECOLOGY.ERC_ECOPROVINCES_SP")
write_sf(EcoP, file.path(spatialOutDir,"EcoP.gpkg"))


#################
#Download latest CE integrated roads layer - current is 2025
rd_file<-'tmp/roads_sf_in'
if (!file.exists(rd_file)) {
  #Download CE road data -   #https://catalogue.data.gov.bc.ca/dataset/bc-cumulative-effects-framework-integrated-roads-current
  url<-'https://coms.api.gov.bc.ca/api/v1/object/e161d871-ccb2-43ae-bbb8-54dce5b449a5'
  CE_rd<-"BC_CE_Integrated_Roads_2025.zip"
  #use URL to download CE road file
  download.file(url, file.path(RoadsDir, CE_rd), mode = "wb")
  #unzip into roads directory with 'gdb' holder file
  unzip(file.path(RoadsDir,CE_rd), exdir = file.path(RoadsDir,'CE_roads.gdb'),junkpaths=TRUE)
  #Read gdb and select layer for sf_read
  Roads_gdb <- list.files(file.path(RoadsDir), pattern = "gdb", full.names = TRUE)[1]
  st_layers(file.path(Roads_gdb))
  #Read file and save to temp directory
  roads_sf_in <- read_sf(Roads_gdb, layer = "integrated_roads_2025")
  saveRDS(roads_sf_in,file=rd_file)
} else {
  roads_sf_in<-readRDS(file=rd_file)
}

##Download latest Provincial Human Disturbance Layers compiled for CE - current is 2023
#Needs refinement to differentiate rural/urban and old vs young cutblocks, rangeland, etc.
dist_file<-'tmp/disturbance_sf'
if (!file.exists(dist_file)) {
  #Download CE road data -  https://catalogue.data.gov.bc.ca/dataset/bc-cumulative-effects-framework-human-disturbance-current
  url<-'https://coms.api.gov.bc.ca/api/v1/object/ecea4b04-055a-49d1-8910-60d726d2d1bf'
  CE_dist<-"CE_disturb.zip"
  #use URL to download CE road file
  download.file(url, file.path(DisturbDir, CE_dist), mode = "wb")
  #unzip into disturbance directory with 'gdb' holder file
  unzip(file.path(DisturbDir,CE_dist), exdir = file.path(DisturbDir,'CE_disturb.gdb'),junkpaths=TRUE)
  #Read gdb and select layer for sf_read
  Disturb_gdb <- list.files(file.path(DisturbDir), pattern = "gdb", full.names = TRUE)[1]
  st_layers(file.path(Disturb_gdb))
  #Read file and save to temp directory
  disturbance_sf_in <- read_sf(Disturb_gdb, layer = "BC_CEF_Human_Disturb_BTM_2023")
  saveRDS(disturbance_sf_in,file=dist_file)
} else {
  disturbance_sf_in<-readRDS(file='tmp/disturbance_sf')
  disturbance_sf<-disturbance_sf_in
}

#VRI to get cutblock age
#Download and process the VRI file if needed- should only need to do when a new version of VRI is posted
VRI_file <- file.path(GoogleDir,"03 Threats Data/VRI/VRIP.gpkg")
if (!file.exists(VRI_file)) {
  #VRI - go to VRI link and get the zip location
  browseURL("https://catalogue.data.gov.bc.ca/dataset/vri-2024-forest-vegetation-composite-rank-1-layer-r1-")
  #Set location of VRI file
  VRI_DC<-"https://pub.data.gov.bc.ca/datasets/02dba161-fdb7-48ae-a4bb-bd6ef017c36d/current/VEG_COMP_LYR_R1_POLY_2023.gdb.zip"
  # https://catalogue.data.gov.bc.ca/dataset/vri-2023-forest-vegetation-composite-rank-1-layer-r1-
  #### Warning R fails to read in downloaded file.
  ### Read downloaded gdb into QGIS - use the MMQGIS plugin to Modify/Geometry Convert to POLYGON to clean
  #Cant read from GoogleDir location - file may be to lare so read from local
  #VRIP<-st_read(file.path(GoogleDir,"05 Geographical Data/Adminstrative Conservation Lands/WCLCNSRVTN_polygon.gpkg"))
  VRIP<-st_read(file.path(SpatialDir,"VRI/VRIP.gpkg"))

  #pull out age from VRI
  Age<- fasterize(VRIP,ProvRast,field='PROJ_AGE_1')
  writeRaster(Age,file.path(spatialOutDir,'Age.tif'),overwrite=TRUE)

  }else{
    Age<-rast(file.path(spatialOutDir,'writeRaster.tif'))
  }

NightTimeP<-raster(file.path(spatialOutDir,"Nighttime Lights alias/VNL_npp_2024_global_vcmslcfg_v2_c202502261200.average.dat_bc_extended_100m.tif"))

#https://www.hydrosheds.org/products/hydrorivers
Hydro_gdb <- list.files(file.path(SpatialDir,"HydroRIVERS_v10_na"), pattern = "gdb", full.names = TRUE)[1]
st_layers(file.path(Hydro_gdb))
Hydro_in <- read_sf(Hydro_gdb, layer = "HydroRIVERS_v10_na") %>%
  st_transform(Prov_crs)
#Hydro_sf_intersect <- Hydro_in %>%
#  st_intersects(BC)
Hydro_sf <- st_join(Hydro_in, BCBuff, join = st_intersects)
HydroR<- rasterize(vect(Hydro_sf), BCrBuff, field="DIS_AV_CMS") %>%
  crop(BCrBuff, mask=TRUE)
writeRaster(HydroR,file.path(spatialOutDir,'HydroR.tif'),overwrite=TRUE)

Rivers<-bcdc_get_data("WHSE_BASEMAPPING.FWA_RIVERS_POLY")
write_sf(Rivers, file.path(spatialOutDir,"Rivers.gpkg"))

Invasives<-bcdc_get_data("WHSE_FOREST_VEGETATION.IBC_INVASIVE_SPECIES_OBS_SP")
write_sf(Invasives, file.path(spatialOutDir,"Invasives.gpkg"))

BEC_Park_rep<-bcdc_get_data("WHSE_LAND_AND_NATURAL_RESOURCE.PA_PCT_BGC_ZONE_PRTCTD_SP")
write_sf(BEC_Park_rep, file.path(spatialOutDir,"BEC_Park_rep.gpkg"))

BEC_rep<-bcdc_get_data("WHSE_LAND_AND_NATURAL_RESOURCE.PASO_PCT_BGC_CODE_PRTCTD_SVW")
write_sf(BEC_rep, file.path(spatialOutDir,"BEC_rep.gpkg"))

mapview(BEC_rep)

Pither_resistance_surface<-rast(file.path(SpatialDir,'Pither_Movement_Cost_Layer.tif')) %>%
  project(Prov_crs,method='near') #compare with max?
writeRaster(Pither_resistance_surface,file.path(SpatialDir,'Pither_resistance_surface.tif'),overwrite=TRUE)
Pither_resistance_surface<-rast(file.path(SpatialDir,'Pither_resistance_surface.tif'))

Lakes<-bcdc_get_data("WHSE_BASEMAPPING.FWA_LAKES_POLY")
write_sf(Lakes, file.path(spatialOutDir,"Lakes.gpkg"))

Glaciers<-bcdc_get_data("WHSE_BASEMAPPING.GBA_GLACIERS_SP")
write_sf(Glaciers, file.path(spatialOutDir,"Glaciers.gpkg"))

IceMasses<-bcdc_get_data("WHSE_BASEMAPPING.TRIM_EBM_ICEMASSES")
write_sf(IceMasses, file.path(spatialOutDir,"IceMasses.gpkg"))

KBLUP_ConnectLegal<-bcdc_get_data("WHSE_LAND_USE_PLANNING.RMP_PLAN_LEGAL_POLY_SVW")
write_sf(KBLUP_ConnectLegal, file.path(spatialOutDir,"KBLUP_ConnectLegal.gpkg"))

KBLUP_ConnectNonLegal<-bcdc_get_data("WHSE_LAND_USE_PLANNING.RMP_PLAN_NON_LEGAL_POLY_SVW")
write_sf(KBLUP_ConnectNonLegal, file.path(spatialOutDir,"KBLUP_ConnectNonLegal.gpkg"))

wshd<-bcdc_get_data("WHSE_LAND_USE_PLANNING.RMP_PLAN_NON_LEGAL_POLY_SVW")
write_sf(wshd, file.path(spatialOutDir,"wshd.gpkg"))





message('Breaking')
break

############


##########################
#Layers for doing AOI for testing and printing

EcoS<-bcmaps::ecosections()

#EcoRegions
EcoRegions<-bcmaps::ecoregions()

#Watersheds
ws <- get_layer("wsc_drainages", class = "sf") %>%
  dplyr::select(SUB_DRAINAGE_AREA_NAME, SUB_SUB_DRAINAGE_AREA_NAME) %>%
  dplyr::filter(SUB_DRAINAGE_AREA_NAME %in% c("Nechako", "Skeena - Coast"))
st_crs(ws)<-3005
saveRDS(ws, file = "tmp/ws")
write_sf(ws, file.path(spatialOutDir,"ws.gpkg"))

