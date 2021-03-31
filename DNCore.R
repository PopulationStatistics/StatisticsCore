#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# Core utility functions for using R to achieve Data Nirvana
# 
# Version:  September 2020 - v1.0
# Authors: Edgar Scrase
# 
# Copyright ? 2021 UNHCR Global Data Service, Statistics and Demographics Section
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
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
# This is the root directory for all scripts (duplicated here just to make sure it is preloaded)
if(! exists("rootScriptDirectory")) {
  rootScriptDirectory <- "C:/Dropbox/UNHCR Statistics/Code/RRR/"  
  warning("DNCore.R: The rootScriptDirectory was not set - setting it to the default now")
}
print(paste("rootScriptDirectory is set to: ", rootScriptDirectory))



#-------------------------------------------------------------------------------------------------------
# installs a set of packages of they did not already exist, otherwise loads them into memory
using <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs,require,character.only = TRUE))
  need <- libs[req == FALSE]
  if (length(need) > 0) { 
    install.packages(need)
    lapply(need,require,character.only = TRUE)
  }
}
# usage:
#using('tidyverse','gganimate','gghighlight','ggpubr', 'dplyr', 'tidyr', 'gapminder', 'ggplot2',  'ggalt', 'forcats', 'R.utils', 'png', 'grid', 'ggpubr', 'scales',  'markdown', 'pander', 'ISOcodes', 'wbstats', 'sf', 'rnaturalearth', 'rnaturalearthdata', 'ggspatial',  'unhcrdatapackage')


#-------------------------------------------------------------------------------------------------------
# turn-off scientific notation like 1e+48
options(scipen = 999) 


#-------------------------------------------------------------------------------------------------------
# Simple logger functionality
logList <- c()

LogClear <-
  function() {
    logList <<- c()
  }
  
LogMessage <-
  function(message, severity=1) {
    
    if ( severity == 1) { # Normal message
      #print(message)
    } else if ( severity == 2) { # Warning
      # This is a better approach as the warning messages are transitory and dissappear
      #print(message)
      message <- paste("WARNING -", message)
      warning(message)
    }
    
    print(message)
    
    logList <<- append(logList, message)
    
  }


#-------------------------------------------------------------------------------------------------------
IsNull <- function(field = NULL) {
  returnValue <- is.null(field)
}

#-------------------------------------------------------------------------------------------------------
# Checks if a value is NULL, NA
IsNN <- function(field = NULL) {
  
  hasNoVal <- is.null(field)
  if(hasNoVal == FALSE) {
    hasNoVal <- is.na(field)
  }
  
  returnValue <- hasNoVal
}
#-------------------------------------------------------------------------------------------------------
# Checks if a value is NULL, NA or NaN
IsNNN <- function(field = NULL) {
  
  hasNoVal <- is.null(field)
  if(hasNoVal == FALSE) {
    hasNoVal <- is.na(field) | is.nan(field)
  }
  
  returnValue <- hasNoVal
}


#-------------------------------------------------------------------------------------------------------
PrettyNum <- function(num) {
  if ( ! IsNNN(num)) {
    num <- format(round(num, 1), nsmall=0, big.mark=",")
  }
  returnValue <- num
}


#-------------------------------------------------------------------------------------------------------
DataFrameColumnExists <- function(data, colName) {
  
  returnValue <- colName %in% colnames(data)
  #returnValue <- length(grep(colName, names(data), value=TRUE)) > 0
}


#-------------------------------------------------------------------------------------------------------
# Takes the variable name to test for as a string
VariableExists <- function(varName) {
  returnValue <- exists(varName)
}



#-------------------------------------------------------------------------------------------------------
# Identifies any differences in the number of columns and returns three sets of column names (in first DF, intersects, in second DF)
CompareDataFrameColumns <- function(df1, df2) {
  
  n1 <- names(df1)
  n2 <- names(df2)
  
  returnValue <- CompareLists(n1, n2)
  
}


#-------------------------------------------------------------------------------------------------------
# Identifies any differences in the number of columns and returns three sets of column names (in first DF, intersects, in second DF)
CompareLists <- function(n1, n2) {

  set1 <- setdiff(n1, n2)
  set2 <- intersect(n1, n2)
  set3 <- setdiff(n2, n1)
  
  returnValue <- list(set1=set1, set2=set2, set3=set3)
  
}


