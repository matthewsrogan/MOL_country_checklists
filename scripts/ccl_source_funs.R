read_checklist <- function(rawPath,
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

  dat <- dat %>% select(all_of(vars))
  
  geo_na <- sum(is.na(dat$iso3))
  if(geo_na > 0) warning(paste(geo_na, "observations are missing geographic IDs."))
  
  spp_na <- sum(is.na(dat$verbatimscientificname))
  if(spp_na > 0) warning(paste(spp_na, "observations are missing scientific names."))
  
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
                         geo_col = "iso3",
                         species_col = "verbatimscientificname"){
  richness <- ccl %>%
    select(all_of(c(species_col, geo_col))) %>%
    group_by(.data[[geo_col]]) %>%
    summarise(n = n_distinct(.data[[species_col]])) %>%
    ungroup()
  
  print(paste(taxa,
              "country richness ranged between",
              min(richness$n),
              "and",
              max(richness$n),
              "species per country."))
  return(richness)
}











