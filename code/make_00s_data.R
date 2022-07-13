library(tidyverse)

files00 <- c("nhgis0026_ds151_2000_tract.csv", "nhgis0026_ds146_2000_tract.csv")
d00 <- s3::s3_get_files(glue::glue("s3://geomarker/harmonized_historical_census_data/nhgis2000/{files00}"))
d00 <- mappp::mappp(d00$file_path, read_csv)

d00_all <- d00[[1]]

for (i in 2:length(d00)) {
  d00_all <- left_join(d00_all, d00[[i]])
}

d00 <- d00_all %>%
  mutate(census_tract_fips = glue::glue('{STATEA}{COUNTYA}{TRACTA}'),

         population_under_18 = FMZ001 + FMZ002 + FMZ003 + FMZ004 +
           FMZ024 + FMZ025 + FMZ026 + FMZ027,

         adj_2010_1999 = (320.4/244.6) ,

         median_income_2010_adj = GMY001 * adj_2010_1999,

         pct_assisted_income = GNB001/ (GNB001 + GNB002) * 100,

         race_denom = FMR001 + FMR002 + FMR003 + FMR004 + FMR005 +
           FMR006 + FMR007,

         pct_white = FMR001 / race_denom * 100,

         pct_black = FMR002 / race_denom * 100,

         pct_asian = FMR004 / race_denom * 100,

         pct_other = (race_denom - (FMR001 + FMR002 + FMR004)) / race_denom * 100,

         pct_hispanic = FMC001 / (FMC001 + FMC002)  * 100,

         nh_denom = FMS001 + FMS002 + FMS003 + FMS004 + FMS005 +
           FMS006 + FMS007 + FMS008 + FMS009 + FMS010 + FMS011 +
           FMS012 + FMS013 + FMS014,

         pct_white_nh = FMS001 / nh_denom * 100,

         pct_black_nh = FMS002 / nh_denom * 100,

         pct_asian_nh = FMS004 / nh_denom * 100,

         pct_other_nh = ((FMS001 + FMS002 + FMS003 + FMS004 + FMS005 +
                            FMS006 + FMS007) -
           (FMS001 + FMS002 + FMS004))/ nh_denom * 100,

         pct_poverty = GN6001/ (GN6001 + GN6002) * 100,

         pct_poverty_under_18 = (GN7001 + GN7002 + GN7003 + GN7004) /
           (GN7001 + GN7002 + GN7003 + GN7004 + GN7008 + GN7009 + GN7010 + GN7011) * 100,

         edu_denom = GKT001 + GKT002 + GKT003 + GKT004 + GKT005 + GKT006 + GKT007 +
           GKT008 + GKT009 + GKT010 + GKT011 + GKT012 + GKT013 + GKT014 + GKT015 +
           GKT016 + GKT017 + GKT018 + GKT019 + GKT020 + GKT021 + GKT022 + GKT023 +
           GKT024 + GKT025 + GKT026 + GKT027 + GKT028 + GKT029 + GKT030 + GKT031 + GKT032,

         pct_no_hs = (GKT001 + GKT002 + GKT003 + GKT004 + GKT005 + GKT006 + GKT007 +
           GKT008 + GKT017 + GKT018 + GKT019 + GKT020 + GKT021 + GKT022 + GKT023 +
           GKT024)/ edu_denom * 100,

         pct_hs = (GKT009 + GKT010 + GKT011 + GKT012 + GKT025 + GKT026 + GKT027 + GKT028) /
           edu_denom * 100,

         pct_bach = GKT013 + GKT029 / edu_denom * 100,

         pct_higher_than_bach = GKT014 + GKT015 + GKT016 +
           GKT030 + GKT031 + GKT032 / edu_denom * 100,

         pct_vacant_housing = FKL002 / (FKL001 + FKL002) * 100,

         pct_owner_occupied = FKN001 / (FKN001 + FKN002) * 100,

         pct_hs_dropout = NA,

         pct_female_hh = (GIO005 + GIO006) / (GIO001 + GIO002 + GIO003 +
                                                GIO004 + GIO005 + GIO006) * 100,

         pct_single_parent = (GIO003 + GIO005) / (GIO001 + GIO002 + GIO003 +
                                                    GIO004 + GIO005 + GIO006) * 100,

         pct_unemployed = (GLR002 + GLR004) / (GLR001 + GLR002 + GLR003 + GLR004) * 100,

         median_rent_2010_adj = GBO001 * adj_2010_1999,

         median_home_value_2010_adj = GB7001 * adj_2010_1999
         ) %>%
  dplyr::select(
    census_tract_fips,
    population_total = FL5001,
    population_under_18,
    pct_white,
    pct_black,
    pct_asian,
    pct_other,
    pct_hispanic,
    pct_white_nh,
    pct_black_nh,
    pct_asian_nh,
    pct_other_nh,
    median_income = GMY001,
    median_income_2010_adj,
    pct_poverty,
    pct_poverty_under_18,
    pct_assisted_income,
    pct_no_hs,
    pct_hs,
    pct_bach,
    pct_higher_than_bach,
    pct_hs_dropout,
    pct_female_hh,
    pct_single_parent,
    pct_unemployed,
    pct_vacant_housing,
    pct_owner_occupied,
    median_rent = GBO001,
    median_rent_2010_adj,
    median_home_value = GB7001,
    median_home_value_2010_adj
  ) %>%
  mutate_if(is.numeric, round)

library(sf)

tract_area <-
  s3::s3_get('s3://geomarker/geometries/census_tracts_1970_to_2020_valid.rds') %>%
  readRDS()

tract_area00 <- tract_area[[4]]

tract_area00$area_m2 <- st_area(tract_area00)

tract_area00 <-
  tract_area00 %>%
  st_drop_geometry() %>%
  mutate(area_m2 = round(as.numeric(area_m2))) %>%
  select(census_tract_fips = census_tract_id, area_m2)

d00 <- left_join(d00, tract_area00, by = 'census_tract_fips')

write_csv(d00, 'nhgis_data/2000_census_data.csv')
fs::dir_delete("s3_downloads")
