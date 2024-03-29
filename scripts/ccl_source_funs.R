##### READ_CCL #####
#A function to read in raw country checklist data and standardize column names.
#It also checks for NAs.
#It returns a ccl tibble and prints summaries of the data
#Wrapper aroun read_ccl.csv() and read_ccl.rds()

read_ccl <- function(rawPath,
                     geo_col,
                     species_col,
                     country_col = NULL,
                     source_col = NULL,
                     cols2keep = NULL){
  require(readr)
  require(dplyr)
  
  if(!grepl(".csv$|.rds$", tolower(rawPath))){
    stop("The raw country checklist must be in a CSV or RDS format.")
  }
  
  if(grepl("\\.csv$", tolower(rawPath))){
    out <- read_ccl.csv(rawPath = rawPath,
                        geo_col = geo_col,
                        species_col = species_col,
                        country_col = country_col,
                        source_col = source_col,
                        cols2keep = cols2keep)
  }
  
  if(grepl("\\.rds", tolower(rawPath))){
    out <- read_ccl.rds(rawPath = rawPath,
                        geo_col = geo_col,
                        species_col = species_col,
                        country_col = country_col,
                        source_col = source_col,
                        cols2keep = cols2keep)
  }
  
  return(out)
}
#### READ_CCL.csv #####
read_ccl.csv <- function(rawPath,
                         geo_col,
                         species_col,
                         country_col = NULL,
                         source_col = NULL,
                         cols2keep = NULL){
  require(readr)
  require(dplyr)
  
  dat <- read_csv(rawPath,
                  locale = locale(encoding = "UTF-8"),
                  show_col_types = F) %>%
    rename(scientificname = all_of(species_col),
           iso3 = all_of(geo_col))
  
  vars <- c("scientificname", "iso3")
  if(! is.null(country_col)){
    dat <- dat %>%
      rename(country = {{country_col}})
    vars <- c(vars, "country")
  }
  if(! is.null(source_col)){
    dat <- dat %>%
      rename(source = {{source_col}})
    vars <- c(vars, "source")
  }

  dat <- dat %>% dplyr::select(all_of(vars), all_of(cols2keep))
  
  geo_na <- sum(is.na(dat$iso3))
  if(geo_na > 0) warning(paste(geo_na, "observations are missing geographic IDs."))
  
  spp_na <- sum(is.na(dat$scientificname))
  if(spp_na > 0) warning(paste(spp_na, "observations are missing scientific names."))
  
  dat <- dat %>%
    filter(!is.na(iso3))
  
  print(paste("This country checklist includes",
              nrow(dat),
              "observations of",
              n_distinct(dat$scientificname),
              "species across",
              n_distinct(dat$iso3),
              "countries."))
  return(dat)
}

#### READ_CCL.rds #####
read_ccl.rds <- function(rawPath,
                         geo_col,
                         species_col,
                         country_col = NULL,
                         source_col = NULL,
                         cols2keep = NULL){
  require(readr)
  require(dplyr)
  
  dat <- read_rds(rawPath) %>%
    rename(scientificname = all_of(species_col),
           iso3 = all_of(geo_col))
  
  vars <- c("scientificname", "iso3")
  if(! is.null(country_col)){
    dat <- dat %>%
      rename(country = {{country_col}})
    vars <- c(vars, "country")
  }
  if(! is.null(source_col)){
    dat <- dat %>%
      rename(source = {{source_col}})
    vars <- c(vars, "source")
  }
  
  dat <- dat %>% dplyr::select(all_of(vars), all_of(cols2keep))
  
  geo_na <- sum(is.na(dat$iso3))
  if(geo_na > 0) warning(paste(geo_na, "observations are missing geographic IDs."))
  
  spp_na <- sum(is.na(dat$scientificname))
  if(spp_na > 0) warning(paste(spp_na, "observations are missing scientific names."))
  
  dat <- dat %>%
    filter(!is.na(iso3))
  
  print(paste("This country checklist includes",
              nrow(dat),
              "observations of",
              n_distinct(dat$scientificname),
              "species across",
              n_distinct(dat$iso3),
              "countries."))
  return(dat)
}

##### CCL_RICHNESS #####
#Calculates country richness from ccl tibble
#Assumes data have already been matched to MOL GeomIDs via ccl_geoms()
ccl_richness <- function(ccl,
                         species_col = "scientificname",
                         country_col = "iso3",
                         taxa){
  
  richness <- ccl %>%
    select(all_of(c({{species_col}}, {{country_col}})), geomid) %>%
    group_by(across(all_of(c({{country_col}}, "geomid")))) %>%
    summarise(n = n_distinct(.data[[species_col]]),
              .groups = "drop") %>%
    mutate(taxa = {{taxa}}) %>%
    select(geomid, all_of({{country_col}}), n, taxa)
  
  print(paste(taxa,
              "country richness ranged between",
              min(richness$n),
              "and",
              max(richness$n),
              "species per country."))
  return(richness)
}

##### CCL_TAXO #####
# This function matches canonicals in a country checklist (ccl) against
# a list of species in a master taxonomy and reports unmatched canonicals.
# If harmonize = TRUE, it will merge accepted canonicals to ccl canonicals
ccl_taxo <- function(ccl         = ccl, #country checklist tibble
                     species_col = "scientificname", #name of column with canonicals in ccl
                     synlist     = synlist, #master taxonomy
                     canonical   = "canonical", #name of column with canonicals in synlist
                     harmonize   = FALSE, #if true, also harmonizes
                     accepted_col = NULL){ #column of accepted canonicals to merge 
  spp <- unique(ccl[[species_col]])
  
  unmatched <- spp[which(!spp %in% synlist[[canonical]])]
  
  if(length(unmatched) == 0) print("No unmatched canonicals.")
  
  if(length(unmatched) > 0){
    warning(paste(length(unmatched), "canonicals are not listed in the master taxonomy."))
    out <- ccl %>% 
      filter(!.data[[species_col]] %in% synlist[[canonical]])
  }
  
  if(harmonize){
    if(is.null(accepted_col)) stop("Must specify name of accepted column when harmonize = TRUE.")
    
    synlist <- synlist %>%
      dplyr::select(all_of(c(canonical, accepted_col))) %>%
      distinct() %>%
      rename(verbatimscientificname = {{canonical}},
             scientificname = {{accepted_col}})
    
    out <- ccl %>% 
      rename(verbatimscientificname = {{species_col}}) %>%
      inner_join(synlist, by = "verbatimscientificname")
    
    if(nrow(out) < nrow(ccl)){
      warning(paste(nrow(ccl) - nrow(out), "records could not be harmonized and have been discarded."))
    }           
  }
  return(out)
}

##### CCL_GEOMS #####
#matches ISO3 codes to the GeomIDs of GADM boundaries in MOL
ccl_geoms <- function(ccl,
                      pckgDir = pckgDir,
                      unmatch_fail = TRUE){
  
  # read in MOL geom ids
  geomIDs <- read_csv(paste0(pckgDir, "/data/gadm_geomid.csv"))
  
  #check for unatched ISO 3 codes and either fail or warn depending on the unmatch_fail argument
  unmatched <- anti_join(ccl, geomIDs, by = "iso3")
  if(nrow(unmatched) > 0 & unmatch_fail){
    warning(paste(nrow(unmatched), "records in the country checklist do not have valid ISO3 codes. Returning tibble of unmatched records."))
    return(unmatched)
  }
  
  if(nrow(unmatched) > 0 & !unmatch_fail){
    warning(paste(nrow(unmatched), "records in the country checklist do not have valid ISO3 codes. Returning a list of length two containing a tibble of matched records and a tibble of unmatched records."))
  }
  
  ccl_geo <- ccl %>%
    inner_join(geomIDs, by = "iso3")
  
  if(nrow(unmatched) == 0){
    return(ccl_geo)
  } else{
    out <- list(matched = ccl_geo, unmatched = unmatched)
    return(out)
  }
}

ccl_write <- function(ccl,
                      directory,
                      geo_col = "geomid",
                      species_col = "scientificname",
                      country_col = "iso3",
                      cols2keep = NULL){
  ccl %>% 
    rename(geomid = {{geo_col}},
           scientificname = {{species_col}},
           iso3 = {{country_col}}) %>%
    select(geomid, scientificname, iso3, all_of(cols2keep)) %>%
    arrange(geomid, scientificname) %>%
    write_csv(file.path(directory, paste0(taxa, "_country_checklist.csv")))
}




#Check character in canonical (limited accepted characters)-------
# This function is copied from taxotools
check_odd_chr_canonical <- function(file_name,field){
  odd_chr_canonical = file_name %>% 
    #scientific name field allows less character options
    dplyr::mutate(bad_sciname = grepl("[^-a-zA-Z ]",{{field}}))  %>% 
    filter(bad_sciname == TRUE) 
  
  if (nrow(odd_chr_canonical) > 0){
    print("Check characters in canonical")
  } else {
    print("No odd characters in canonical")
  }
  return(odd_chr_canonical)
}


#Borrowed from checks_taxotools script in the taxonomy repository:
#https://github.com/yanisica/taxonomy

convert_taxotool <- function(file_name){
  colnames(file_name) <- tolower(colnames(file_name))
  
  accepted <- file_name %>% dplyr::filter(accid == 0)
  synonym <- file_name %>% dplyr::filter(accid > 0)
  accepted_dup <- accepted
  accepted_dup$accid <- accepted_dup$id
  syns_all <- rbind(synonym,accepted_dup)
  
  file_name <- left_join(accepted,syns_all,by=c("id" = "accid")) 
  
  file_name <- file_name %>%
    dplyr::select(canonical.x,canonical.y) %>%
    rename("Accepted" = canonical.x, "Synonym" = canonical.y) %>%
    dplyr::distinct(Accepted,Synonym) %>% 
    dplyr::group_by(Synonym) %>%
    dplyr::mutate("U.or.A" = if_else(n() == 1, "U","A")) %>%
    ungroup()
}



