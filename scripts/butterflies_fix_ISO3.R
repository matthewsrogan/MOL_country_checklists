### Ad hoc script to replace invalid ISO3 codes from the butterflies long-format country checklist ###

# This script assumes that the checklist has be read into the R environment using read_checklist()
# This step should be run prior to running ccl_geoms

ccl_hrmnzd <- ccl_hrmnzd %>%
  mutate(iso3 = str_replace_all(iso3, c("FRA-GUINEA" = "GUF",
                                        "FRA-REUNION" = "REU",
                                        "FRA_GUIANA" = "GUF")))
