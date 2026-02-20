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

#Load DEM by EcoProvince

#Test for 1 EcoProvince
#Moderate test area

EcoP<-st_read(file.path(spatialOutDir,"EcoP.gpkg"))

DEMList<-lapply(9:(nrow(EcoP)-1), function (i) {
  AOI<-slice(EcoP,i)
  DEM<-bcmaps::cded_stars(aoi = AOI)
  write_stars(DEM,dsn=file.path(spatialOutDir,paste0('DEM_',AOI$ECOPROVINCE_CODE,'.tif')))
  DEM.t<-terra::rast(file.path(spatialOutDir,paste0('DEM_',AOI$ECOPROVINCE_CODE,'.tif')))
  crs(DEM.t, proj=TRUE)
  DEM.tp<-terra::project(DEM.t,crs(bcmaps::bc_bound()))
  writeRaster(DEM.tp, filename=file.path(spatialOutDir,paste0('DEMtp_',AOI$ECOPROVINCE_CODE,'.tif')), overwrite=TRUE)
})

DEMList.1<-lapply(1:(nrow(EcoP)), function (i) {
  AOI<-slice(EcoP,i)
  DEM.t<-terra::rast(file.path(spatialOutDir,paste0('DEMtp_',AOI$ECOPROVINCE_CODE,'.tif')))
})

DEMP<-do.call(terra::merge, DEMList.1)
writeRaster(DEMP, filename=file.path(spatialOutDir,paste0('DEMP.tif')), overwrite=TRUE)

DEMP<-terra::rast(file.path(spatialOutDir,'DEMP.tif'))

#Elevation Threshold
DEM2300rP<-terra::resample(DEMP,rast(BCr),method="cubic") %>%
  crop(rast(BCr), mask=TRUE)
writeRaster(DEM2300rP, filename=file.path(spatialOutDir,paste0('DEM2300rP.tif')), overwrite=TRUE)

#Slope
Slope <- terrain(DEMP, v = "slope", unit = "degrees")
SloperP<-terra::resample(Slope,rast(BCr),method="cubic") %>%
  crop(BCrT, mask=TRUE)
writeRaster(SloperP, filename=file.path(spatialOutDir,paste0('SloperP.tif')), overwrite=TRUE)


#######
DEM_file <- file.path(spatialOutDir,paste0('DEMtp_',AOI$ECOPROVINCE_CODE,'.tif'))
if (!file.exists(DEM_file)) {
  DEM<-bcmaps::cded_stars(aoi = AOI)
  write_stars(DEM,dsn=file.path(spatialOutDir,paste0('DEM_',AOI$ECOPROVINCE_CODE,'.tif')))
  DEM.t<-terra::rast(file.path(spatialOutDir,paste0('DEM_',AOI$ECOPROVINCE_CODE,'.tif')))
  crs(DEM.t, proj=TRUE)
  DEM.tp<-terra::project(DEM.t,crs(bcmaps::bc_bound()))
  writeRaster(DEM.tp, filename=file.path(spatialOutDir,paste0('DEMtp_',AOI$ECOPROVINCE_CODE,'.tif')), overwrite=TRUE)

} else
  DEM.tp<-rast(file.path(spatialOutDir,paste0('DEMtp_',AOI$ECOPROVINCE_CODE,'.tif')))



#Potential AOIs for testing

EcoR<-bcdc_get_data("WHSE_TERRESTRIAL_ECOLOGY.ERC_ECOREGIONS_SP")
write_sf(EcoR, file.path(spatialOutDir,"EcoR.gpkg"))

EcoP<-bcdc_get_data("WHSE_TERRESTRIAL_ECOLOGY.ERC_ECOPROVINCES_SP")
write_sf(EcoP, file.path(spatialOutDir,"EcoP.gpkg"))


#Ecosections
EcoS_file <- file.path("tmp/EcoS")
ESin <- read_sf(file.path(ProvData,'Boundaries/Ecosections/Ecosections.shp')) %>%
  #st_transform(3005)
  st_set_crs(3005)
EcoS <- st_cast(ESin, "MULTIPOLYGON")
write_sf(EcoS, file.path(spatialOutDir,"EcoS.gpkg"))

#Watersheds
ws <- get_layer("wsc_drainages") %>%
  dplyr::select(SUB_DRAINAGE_AREA_NAME, SUB_SUB_DRAINAGE_AREA_NAME) %>%
  dplyr::filter(SUB_DRAINAGE_AREA_NAME %in% c("Nechako", "Skeena - Coast"))
st_crs(ws)<-3005
saveRDS(ws, file = "tmp/ws")
write_sf(ws, file.path(spatialOutDir,"ws.gpkg"))

#DEM
DEM_file<-file.path(spatialOutDir,"DEM.tif")
if (!file.exists(DEM_file)) {
  DEM<-bcmaps::cded_stars(aoi = AOIs) #crs=4269
  write_stars(DEM,dsn=file.path(spatialOutDir,paste0('DEM_BuMo.tif')))
  DEM.t<-terra::rast(file.path(spatialOutDir,paste0('DEM_BuMo.tif')))
  crs(DEM.t, proj=TRUE)
  DEM.tp<-terra::project(DEM.t,crs(bcmaps::bc_bound()))
  writeRaster(DEM.tp, filename=file.path(spatialOutDir,paste0('DEMtp_BuMo.tif')), overwrite=TRUE)
} else {
  DEM.tp<-raster(file.path(spatialOutDir,'DEMtp_BuMo.tif'))
}

