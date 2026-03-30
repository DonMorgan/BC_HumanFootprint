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

SouthernCorr.1<-st_read(file.path(OutDir,'CorrAreas/Southern2.gpkg')) %>%
  mutate(CorrName='Southern')
Y2Y2.1<-st_read(file.path(OutDir,'CorrAreas/Y2Y2.gpkg')) %>%
  mutate(CorrName='Y2Y')
CoastMountain.1<-st_read(file.path(OutDir,'CorrAreas/CoastMountain2.gpkg')) %>%
  mutate(CorrName='CoastMountains')
ButeInlet.1<-st_read(file.path(OutDir,'CorrAreas/ButeInlet2.gpkg')) %>%
  mutate(CorrName='ButeInlet')
Corridors.1<-rbind(SouthernCorr.1,Y2Y2.1,CoastMountain.1,ButeInlet.1)


Corridors<-Corridors.1 %>%
  group_by(CorrName) %>%
  dplyr::summarise() %>%
  ungroup()
plot(Corridors)

st_write(Corridors,file.path(spatialOutDir,'Corridors.gpkg'))
