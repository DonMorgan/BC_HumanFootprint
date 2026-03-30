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

source('header.R')

#only run load if necessary - clean
source("01_load.R")
source("01_load_ExtraProv.R")
#clean will clip to AOI
source("02_clean_Area.R")
source("02_clean_Roads.R")
source("02_clean_AlpineGlacier.R")
source("02_clean_Cutblocks_and_Water.R")
source("02_clean_DEM.R")
source("02_clean_NatHF.R")
source("02_clean_PipelinesTransmission.R")
source("02_clean_Water.R")

source("03_analysis.R")
source("03_ConnCC.R")
source("03_ConnWshd.R")
source("03_CorridorWshd.R")

source("04_output_report.R")
source("04_output.R")

