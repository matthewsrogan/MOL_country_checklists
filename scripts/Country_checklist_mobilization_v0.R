#-----------------------------------------------------------------------

##NAME: Country_checklist_mobilization
#
##DESCRIPTION: Prep odonate country checklists from Sandall et al. in prep for upload to MOL
#
#CREATED BY: Matt Rogan @ MOL September 2021
#CONTACT: matthew.rogan@yale.edu

#-----------------------------------------------------------------------

##### STEP 1: SET ENVIRONMENT VARIABLES #####

### Specify taxa
# taxa = "dragonflies"
# taxa = "butterflies"

### Specify source folders
#Various scripts
codeDir = "C:/Users/mr2577/Dropbox/BGC/MoL/Code/Country_checklists"
setwd(codeDir)

#Github MOL_country_checklists repository
pckgDir = paste0(codeDir, "/MOL_country_checklists")

#migration directory - parent directory for processing regional checklists
migrationDir <- "G:/Shared drives/MOL/Data/datasets/country_checklists"


### load packages and functions
library(tidyverse)
source(paste0(pckgDir, "/scripts/ccl_source_funs.r"))

### specify data files
ccl_name <- "MOLOdonateChecklistData_0821.csv" #For dragonflies
ccl_name <- "long_format (072021, literature only)_CCL_allsources(072021,QC,harmonized).csv" #For butterflies 1
ccl_name <- "unique_combinations_(072021, literature and gbif).csv" #For butterflies 2

#dragonfly synonym list
synList <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1_WSXtJa3kD32VxHeCKhSe6ovLtWgO7AuEsFpFoj61Fo/edit#gid=306640760")


##### STEP 2: IMPORT DATA #####

#path for checklist
rawPath <- file.path(migrationDir, taxa, ccl_name)

#read in country checklist (ccl) using read_checklist
ccl <- read_checklist(rawPath = rawPath,
                      geo_col = "GID_0", #column name with ISO3 codes
                      species_col = "ValidBinomial", #column name for species scientific names
                      source_col = "Full Reference") #column name for sources

sum(is.na(ccl$iso3))

#read in MOL Geom ID codes
geomIDs <- read_csv(paste0(pckgDir, "/data/gadm_geomid.csv"))

##### STEP 3: HARMONIZE TAXONOMY

##### STEP 4: CALCULATE RICHNESS
#calculate richness
richness <- ccl_richness(ccl)
