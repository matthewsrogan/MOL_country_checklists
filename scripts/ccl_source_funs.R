read_ccl <- function(rawPath,
                     geo_col,
                     species_col,
                     country_col = NULL,
                     source_col = NULL){
  require(readr)
  require(dplyr)
  
  dat <- read_csv(rawPath,
                  locale = locale(encoding = "UTF-8"),
                  show_col_types = F) %>%
    rename(verbatimscientificname = {{species_col}},
           iso3 = {{geo_col}})
  
  vars <- c("verbatimscientificname", "iso3")
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

  dat <- dat %>% dplyr::select(all_of(vars))
  
  geo_na <- sum(is.na(dat$iso3))
  if(geo_na > 0) warning(paste(geo_na, "observations are missing geographic IDs."))
  
  spp_na <- sum(is.na(dat$verbatimscientificname))
  if(spp_na > 0) warning(paste(spp_na, "observations are missing scientific names."))
  
  dat <- dat %>%
    filter(!is.na(iso3))
  
  print(paste("This country checklist includes",
              nrow(dat),
              "observations of",
              n_distinct(dat$verbatimscientificname),
              "species across",
              n_distinct(dat$iso3),
              "countries."))
  return(dat)
}

ccl_richness <- function(ccl,
                         species_col = "verbatimscientificname",
                         taxa){
  richness <- ccl %>%
    select(all_of(species_col), geomid) %>%
    group_by(geomid) %>%
    summarise(n = n_distinct(.data[[species_col]])) %>%
    ungroup() %>%
    mutate(taxa = {{taxa}}) %>%
    select(geomid, n, taxa)
  
  print(paste(taxa,
              "country richness ranged between",
              min(richness$n),
              "and",
              max(richness$n),
              "species per country."))
  return(richness)
}

### This function matches canonicals in a country checklist (ccl) against
# a list of species in a master taxonomy and reports unmatched canonicals.
# If harmonize = TRUE, it will merge accepted canonicals to ccl canonicals
check_taxonomy <- function(ccl         = ccl, #country checklist tibble
                           species_col = "verbatimscientificname", #name of column with canonicals in ccl
                           synlist     = synlist, #master taxonomy
                           canonical   = "canonical", #name of column with canonicals in synlist
                           harmonize   = FALSE, #if true, also harmonizes
                           method      = c("merge", "matchID"),
                           accid_col   = NULL){
  spp <- unique(ccl[[species_col]])
  
  unmatched <- spp[which(!spp %in% synlist[[canonical]])]
}

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
                      species_col = "verbatimscientificname",
                      source_col = NULL){
  ccl %>% 
    mutate(accepted = .data[[species_col]]) %>%
    rename(geomid = {{geo_col}}) %>%
    select(accepted, geomid, verbatimscientificname, iso3, all_of(source_col)) %>%
    arrange(geomid, accepted) %>%
    write_csv(file.path(outDir, paste0(taxa, "_MOL_country_checklist.csv")))
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






