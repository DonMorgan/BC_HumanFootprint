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

#Roads - clean and split into low, medium, high use
#If roads raster is already made then skip this section
roads_file <- file.path(spatialOutDir,"roadsSR.tif")
if (!file.exists(roads_file)) {
  #Check the types
  unique(roads_sf_in$DRA_ROAD_CLASS)
  unique(roads_sf_in$DRA_ROAD_SURFACE)
  unique(roads_sf_in$OG_DEV_PRE06_PETRLM_DEVELOPMENT_ROAD_TYPE)


### Check Petro roads
#Appears petro roads are typed with SURFACE and CLASSS
  table(roads_sf_in$DRA_ROAD_SURFACE,roads_sf_in$OG_DEV_PRE06_PETRLM_DEVELOPMENT_ROAD_TYPE)
  table(roads_sf_in$DRA_ROAD_CLASS,roads_sf_in$OG_DEV_PRE06_PETRLM_DEVELOPMENT_ROAD_TYPE)

#Additional petro road checks
  #Check if all petro roads have a OG_DEV_PRE06_PETRLM_DEVELOPMENT_ROAD_TYPE
 tt<-roads_sf_in %>%
  st_drop_geometry() %>%
  dplyr::filter(is.na(DRA_ROAD_CLASS))

  Petro_Tbl <- st_set_geometry(roads_sf_in, NULL) %>%
    count(OG_DEV_PRE06_PETRLM_DEVELOPMENT_ROAD_TYPE, LENGTH_METRES)

  roads_sf_petro <- roads_sf_in %>%
    mutate(DRA_ROAD_SURFACE=if_else(is.na(OG_DEV_PRE06_OG_PETRLM_DEV_RD_PRE06_PUB_ID),DRA_ROAD_SURFACE,'OGC')) %>%
    mutate(DRA_ROAD_CLASS=if_else(is.na(OG_DEV_PRE06_OG_PETRLM_DEV_RD_PRE06_PUB_ID),DRA_ROAD_CLASS,OG_DEV_PRE06_PETRLM_DEVELOPMENT_ROAD_TYPE))

  Petro_Tbl <- st_set_geometry(roads_sf_petro, NULL) %>%
    dplyr::count(DRA_ROAD_SURFACE, DRA_ROAD_CLASS)
#### End Petro road check

#Eliminate non-roads
  notRoadsCls <- c("ferry", "water", "Road Proposed")
  notRoadsSurf<-c("boat")

  roads_sf_1<-roads_sf_in %>%
    filter(!DRA_ROAD_CLASS %in% notRoadsCls,
           !DRA_ROAD_SURFACE %in% notRoadsSurf)
#Move 'minor' to mod use? Add a 4th class as minor high use?
  HighWayCls<-c("Road arterial major","Road highway major",
                "Road collector major","Road freeway")
#Mod use are typically not high use, not low use, have a surface and class type runway
  TwoLaneMajorCls<-c("Road arterial minor","Road highway minor","Road collector minor",
                    "Road local","Road alleyway",
                    "Road service","Road strata","Road runway",
                    "Road ramp","Road yield lane")
  TwoLaneMinorCls<-c("Road local","Road recreation","Road alleyway","Road restricted",
                       "Road service","Road driveway","Road strata",
                       "Road recreation demographic","Road runway non-demographic",
                       "Road driveway non-demographic","Road resource demographic",
                       "Private driveway demographic","Road controlled")
  #Low use are poor surface, un-named, unknown class or surface, likely dont impede movement
  SingleMinorCls<-c("Trail recreation","Road resource","Road resource non status",
               "Road lane","Road skid","Road trail","Road pedestrian","Road passenger",
               "Trail", "Trail demographic","Trail skid", "Road pedestrian mall",
               "Road unclassified")

  PavedSurf<-c("paved")
  NonPavedSurf<-c("loose","rough","rehabilitated")
  MinorSurf<-c("overgrown","decommissioned","seasonal","unknown")

  #Add new attribute that holds the use classification Road unclassified
  roads_sf <- roads_sf_1 %>%
    mutate(RoadUse = case_when((DRA_ROAD_CLASS %in% HighWayCls & DRA_ROAD_SURFACE %in% PavedSurf) ~ 4,
                               ((DRA_ROAD_CLASS %in% TwoLaneMajorCls & DRA_ROAD_SURFACE %in% PavedSurf) |
                                #  ((DRA_ROAD_CLASS %in% TwoLaneMajorCls) |
                                (DRA_ROAD_CLASS %in% c("Road arterial minor","Road highway minor","Road collector minor","Road runway")) |
                               (DRA_ROAD_CLASS %in% HighWayCls & DRA_ROAD_SURFACE %in% NonPavedSurf)) ~ 3,
                               ((DRA_ROAD_CLASS %in% TwoLaneMinorCls & DRA_ROAD_SURFACE %in% NonPavedSurf) |
                               (DRA_ROAD_CLASS %in% TwoLaneMinorCls | DRA_ROAD_SURFACE == "loose") & !is.na(DRA_ROAD_NAME_FULL) |
                               (DRA_ROAD_CLASS %in% TwoLaneMinorCls & DRA_ROAD_SURFACE %in% PavedSurf)) ~ 2,
                               #(DRA_ROAD_CLASS %in% TwoLaneMajorCls & DRA_ROAD_SURFACE %in% NonPavedSurf)) ~ 2,
                               #((DRA_ROAD_CLASS %in% TwoLaneMinorCls | DRA_ROAD_SURFACE == "loose") & is.na(DRA_ROAD_NAME_FULL)|
                                ((DRA_ROAD_CLASS %in% SingleMinorCls | DRA_ROAD_SURFACE %in% MinorSurf) |
                                (DRA_ROAD_SURFACE %in% MinorSurf & is.na(DRA_ROAD_NAME_FULL)) |
                                (is.na(DRA_ROAD_CLASS) & is.na(DRA_ROAD_SURFACE))) ~ 1,
                               TRUE ~ 5)) # all the rest are medium use

  #Check the assignment
  Rd_Tbl <- st_set_geometry(roads_sf, NULL) %>%
    dplyr::count(DRA_ROAD_SURFACE, DRA_ROAD_CLASS, is.na(DRA_ROAD_NAME_FULL), RoadUse)

  #Data check
  nrow(roads_sf)-nrow(roads_sf_1)
  table(roads_sf$RoadUse)

  # Save as RDS for quicker access later.
  saveRDS(roads_sf, file = "tmp/DRA_roads_sf_clean.rds")
  # Also save as geopackage format for use in GIS and for buffer anlaysis below
  write_sf(roads_sf, file.path(spatialOutDir,"roads_clean.gpkg"))

  roads_sf<-readRDS(file = "tmp/DRA_roads_sf_clean.rds")

  #
  Highway<-roads_sf %>%
    dplyr::filter(RoadUse==4) %>%
    st_buffer(dist=300) %>%
    dplyr::select(RoadUse)
  HighwayR<-rasterize(vect(Highway), rast(BCr_file), field="RoadUse") %>%
    crop(rast(BCr), mask=TRUE)
  writeRaster(HighwayR, file.path(spatialOutDir,'HighwayR.tif'), overwrite=TRUE)

  TwoLaneMajor<-roads_sf %>%
    dplyr::filter(RoadUse==3) %>%
    st_buffer(dist=150) %>%
    dplyr::select(RoadUse)
  TwoLaneMajorR<-rasterize(vect(TwoLaneMajor), rast(BCr_file), field="RoadUse") %>%
    crop(rast(BCr), mask=TRUE)
  writeRaster(TwoLaneMajorR, file.path(spatialOutDir,'TwoLaneMajorR.tif'), overwrite=TRUE)

  TwoLaneMinor<-roads_sf %>%
    dplyr::filter(RoadUse==2) %>%
    st_buffer(dist=150) %>%
    dplyr::select(RoadUse)
  TwoLaneMinorR<-rasterize(vect(TwoLaneMinor), rast(BCr_file), field="RoadUse") %>%
    crop(rast(BCr), mask=TRUE)
  writeRaster(TwoLaneMinorR, file.path(spatialOutDir,'TwoLaneMinorR.tif'), overwrite=TRUE)

  SingleLaneMinor<-roads_sf %>%
    dplyr::filter(RoadUse==1) %>%
    st_buffer(dist=150) %>%
    dplyr::select(RoadUse)
  write_sf(SingleLaneMinor, file.path(spatialOutDir,"SingleLaneMinor.gpkg"))

 # SingleLaneMinorR<-rasterize(vect(SingleLaneMinor), rast(BCr_file), field="RoadUse") %>%
 #   crop(rast(BCr), mask=TRUE)
 # writeRaster(SingleLaneMinorR, file.path(spatialOutDir,'SingleLaneMinorR.tif'), overwrite=TRUE)
 #Use Stars to rasterize according to RoadUse and save as a tif
  #terra::rast struggles with the complexity of the single lane road layer
  #first st_rasterize needs a template to 'burn' the lines onto
  template = BCr_S
  template[[1]][] = NA
  SingleLaneMinorR<-stars::st_rasterize(SingleLaneMinor[,"RoadUse"], template)
  write_stars(SingleLaneMinorR,dsn=file.path(spatialOutDir,'SingleLaneMinorR.tif'))
  #Read back is a terra raster
  SingleLaneMinorR<-rast(file.path(spatialOutDir,'SingleLaneMinorR.tif'))
  #values(SingleLaneMinorR)[values(SingleLaneMinorR) ==0]=NA
  SingleLaneMinorR <- classify(SingleLaneMinorR, cbind(-Inf, 0, NA), right=TRUE)
  #Make a new rast that has the minimum value - ie the highest weighted road
  RoadsR<-max(c(HighwayR,TwoLaneMajorR,TwoLaneMinorR,SingleLaneMinorR),na.rm=TRUE)
  writeRaster(RoadsR, file.path(spatialOutDir,'RoadsR.tif'), overwrite=TRUE)

} else {
  #Read in raster roads with values 0-none, 1-high use, 2&3-moderate use, 4-low use)
  roadsR<-raster(file.path(spatialOutDir,'roadsSR.tif'))
}

