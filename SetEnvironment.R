#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
# Sets the core directories to use for the script and daa repositories

# Version: February 2021 - v1.0
# Authors: Edgar Scrase

# Copyright © 2021 UNHCR Global Data Service, Statistics and Demographics Section
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------
# This is the root directory for all data
rootDirectory <- "C:/Dropbox/UNHCR Statistics/"

#-------------------------------------------------------------------------------------------------------
# This is the root directory for all scripts
# Note that the scripts are stored here in the UNHCR One Drive
uName <- Sys.getenv("USERNAME")
rootOneDriveDirectory <- paste0("C:/Users/",uName,"/UNHCR/Global Data Service - Statistics and Demographics Section - ASR_MYSR/")

rootScriptDirectory <- paste0(rootOneDriveDirectory, "Code/")

#-------------------------------------------------------------------------------------------------------
# And the data directory is currently based still on the Dropbox repository.
rootDataDirectory <- rootDirectory


#-------------------------------------------------------------------------------------------------------------------------
# Declare all the relevant data folders and file names
genericDataDirectoryName <- paste0(rootDataDirectory, "Data/")

# The master data directory - within this there will be files containing the historic data on refugees, asylum seekers and IDPs
masterDataDirectoryName <- paste0(rootDataDirectory, "Data/Master_Data/")

# The current ASR data - this will be updated depending on whether or not this is a MYSR or ASR event
workingDirectoryName <- paste0(rootDataDirectory, "Data_ASR/")

# Demographic data
demographicDataDirectoryName <- paste0(rootDataDirectory, "Data/Demographics/")


# Footnotes folder
footnotesFolder <- paste0(rootDataDirectory, "PopStats/Footnotes/")



#-------------------------------------------------------------------------------------------------------------------------
# The key population types to keep!!
popTypes <- c("REF", "ROC", "ASY", "IDP", "IOC", "STA", "OOC", "VDA", "HST", "RET", "RDP")
#popTypes <- c("REF", "ROC", "ASY", "IDP", "IOC", "STA", "OOC", "VDA", "HST", "RET", "RDP", "NAT" "RST")


#-------------------------------------------------------------------------------------------------------------------------
# Periodically update the packages installed
# update.packages()



#-------------------------------------------------------------------------------------------------------------------------
###### Load scripts #####
# Load the required libraries using the worker function
source(paste0(rootScriptDirectory, "StatisticsCore/DNCore.R"))

# Load the required libraries
using("readxl", "dplyr", "tidyr", "stringr", # These are the core libraries we use for everything
      "forcats", # for formatting numbers
      "leaflet", # for interactive maps
      "ggplot2", # for charts
      "extrafont", # for fonts
      "scales", # for formatting numbers
      "png", # for exporting charts as PNG
      "gridExtra", # for reading and writing CSV using utf-8
      "curl", # for downloading JSON
      "jsonlite", # for explorting / importing JSON
#      "rjson",
      "stringi", # For reading and manipulating strings - see http://www.gagolewski.com/software/stringi/
      "readr",  # for reading CSV files (does it better than in write.table...)
      "writexl", # for writing Excel
      "validate" # for the SQAF
      )

# Open sans rules!
fontsForCharts <- c( "Open Sans" )


#-------------------------------------------------------------------------------------------------------------------------
# Load the scripts with the worker functions

#-----a----- Statistics loader worker functions
source(paste0(rootScriptDirectory,"StatisticsLoader/PopStatsDataRedactionHelper.R"))
source(paste0(rootScriptDirectory,"StatisticsLoader/GlobalTrendsDataMasher.R"))
source(paste0(rootScriptDirectory,"StatisticsLoader/PopStatsDataRedaction.R"))
source(paste0(rootScriptDirectory,"StatisticsLoader/DemographicsWithLocation.R"))

#-----b----- Quality assurance worker functions
source(paste0(rootScriptDirectory,"StatisticalQualityAssurance/StatisticalQualityAssuranceFrameworkHelper.R"))

#-----c----- And the country special cases
source(paste0(rootScriptDirectory,"StatisticsLoader/CountrySpecialCases.R"))

#-----d----- DNFunctions - for charting
fileNameDNFunctions <- paste0(rootScriptDirectory,"StatisticsCore/DNFunctions.R")
if( file.exists(fileNameDNFunctions)) {
   print("Also loading DN functions (for charting)")
   source(fileNameDNFunctions)
}

