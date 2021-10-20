#-----------------------------------------------------------------------

##NAME: Country_checklist_mobilization
#
##DESCRIPTION: Prep odonate country checklists from Sandall et al. in prep for upload to MOL
#
#CREATED BY: Matt Rogan @ MOL September 2021
#CONTACT: matthew.rogan@yale.edu

#-----------------------------------------------------------------------

##### STEP 1: SET ENVIRONMENT VARIABLES

### Specify taxa
taxa = "butterflies"

### Specify source folders
#scrips
codeDir = "C:/Users/mr2577/Dropbox/BGC/MoL/Code/Country_checklists"
setwd(codeDir)

#migration directory - parent directory for processing regional checklists
migrationDir <- "G:/Shared drives/MOL/Data/datasets/country_checklists"

#checklist file name
checklist <- "long_format (072021, literature only)_CCL_allsources(072021,QC,harmonized).csv"

### load packages and functions
library(tidyverse)
source("R_Scripts/ccl_source_funs.r")

##### STEP 2: IMPORT DATA

#path for checklist
rawPath <- file.path(migrationDir, taxa, checklist)

#read in country checklist (ccl) using read_checklist
ccl <- read_checklist(rawPath = rawPath,
                      geo_col = "country_QC", #column name with ISO3 codes
                      species_col = "ValidBinomial", #column name for species scientific names
                      source_col = "source") #column name for sources

sum(is.na(ccl$iso3))

#calculate richness
richness <- ccl_richness(ccl)
