library(tidyverse)

files80 <- c("nhgis0024_ds116_1980_tract.csv", "nhgis0024_ds107_1980_tract.csv",
             "nhgis0024_ds104_1980_tract.csv", "nhgis0030_ds107_1980_tract.csv")
d80 <- s3::s3_get_files(glue::glue("s3://geomarker/harmonized_historical_census_data/nhgis1980/{files80}"))

d80_1 <- read_csv(d80$file_path[1]) %>%
  select(GISJOIN, STATEA, COUNTYA, TRACTA, C6X001:C6X005)

d80_2 <- read_csv(d80$file_path[2]) %>%
  select(GISJOIN, STATEA, COUNTYA, TRACTA, DGR001:DI8002)

d80_3 <- read_csv(d80$file_path[3])

d80_4 <- read_csv(d80$file_path[4], col_names = TRUE) %>%
  select(GISJOIN, STATEA, COUNTYA, TRACTA, DEC001:DHI004, DHS001:DHS005, DFU001:DFY001)


d80_all <- d80_1
d80_all <- left_join(d80_all, d80_2)
d80_all <- left_join(d80_all, d80_3)
d80_all <- left_join(d80_all, d80_4)

d80 <- d80_all %>%
  mutate(
    census_tract_fips = glue::glue('{STATEA}{COUNTYA}{TRACTA}'),

         population_under_18 = C67001 + C67002 + C67003 + C67004 + C67005 +
           C67006 + C67007 + C67008 + C67009 + C67010 + C67011,

         adj_2010_1979 = (320.4/114.3),

         median_income_2010_adj = DIE001 * adj_2010_1979,

         pct_assisted_income = DII006 / (DII001 + DII002 + DII003 + DII004 +
                                           DII005 + DII006 + DII007) * 100,

         race_denom = C6X001 + C6X002 + C6X003 + C6X004 + C6X005,

         pct_white = C6X001 / race_denom * 100,

         pct_black = C6X002 / race_denom * 100,

         pct_asian = C6X004 / race_denom * 100,

         pct_other = (race_denom - (C6X001 + C6X002 + C6X004)) / race_denom * 100,

         pct_hispanic = (C9E002 + C9E003 + C9E004 + C9E005) /
           (C9E001 + C9E002 + C9E003 + C9E004 + C9E005)  * 100,

         pct_white_nh = NA,
         pct_black_nh = NA,
         pct_asian_nh = NA,
         pct_other_nh = NA,

         pct_poverty = DI8002/ (DI8001 + DI8002) * 100,

         pct_poverty_under_18 = DI1005 / (DI1005 + DI1001) * 100,

         edu_denom = DHS001 + DHS002 + DHS003 + DHS004 + DHS005,

         pct_no_hs = (DHS001 + DHS002) / edu_denom * 100,

         pct_hs = (DHS002+DHS003) / edu_denom * 100,

         pct_bach = DHS004 / edu_denom * 100,

         pct_higher_than_bach = DHS005 / edu_denom * 100,

         pct_vacant_housing = C9B002 / (C9B001 + C9B002) * 100,

         pct_owner_occupied = C7W001 / (C7W001 + C7W002) * 100,

         pct_hs_dropout = (DHL006 + DHL007 + DHL008) / (DHL001 +  DHL002 + DHL003 +
                                                          DHL004 + DHL005 + DHL006 +
                                                          DHL007 + DHL008) * 100,

         pct_female_hh = (DGR005 + DGR006) / (DGR001 + DGR002 + DGR003 + DGR004 +
                                                DGR005 + DGR006 + DGR007) * 100,

         pct_single_parent = (DGR003 + DGR005) / (DGR001 + DGR002 + DGR003 + DGR004 +
                                                    DGR005 + DGR006 + DGR007) * 100,

         pct_unemployed = (DHX003 + DHX007) / (DHX001 + DHX002 +  DHX003 +  DHX004 +
                                                 DHX005 + DHX006 + DHX007 + DHX008) * 100,
         median_rent_2010_adj = C8O001 * adj_2010_1979,
         median_home_value_2010_adj = C8J001 * adj_2010_1979
         ) %>%
  dplyr::select(
    census_tract_fips,
    # STATEA,
    # COUNTYA,
    # TRACTA,
    population_total = C7L001,
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
    median_income = DIE001,
    median_income_2010_adj = DIE001,
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
    median_rent = C8O001,
    median_rent_2010_adj,
    median_home_value = C8J001,
    median_home_value_2010_adj
  ) %>%
  mutate_if(is.numeric, round)

library(sf)

tract_area <-
  s3::s3_get('s3://geomarker/geometries/tracts_1980_5072.rds') %>%
  readRDS()

tract_area$area_m2 <- st_area(tract_area)

tract_area <-
  tract_area %>%
  st_drop_geometry() %>%
  mutate(area_m2 = round(as.numeric(area_m2)),
         state_fips = stringr::str_sub(census_tract_id_1980, 1, 2),
         county_fips = stringr::str_sub(census_tract_id_1980, 3, 5),
         tract_fips = stringr::str_sub(census_tract_id_1980, 6),
         census_tract_fips = glue::glue('{state_fips}{county_fips}{tract_fips}')) %>%
  select(census_tract_fips, area_m2)

d80 <- left_join(d80, tract_area, by = 'census_tract_fips')

nrow(d80) == length(unique(d80$census_tract_fips))

write_csv(d80, 'nhgis_data/1980_census_data.csv')

fs::dir_delete("s3_downloads")
