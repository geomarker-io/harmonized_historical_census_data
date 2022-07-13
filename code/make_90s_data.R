library(tidyverse)

files90 <- c("nhgis0025_ds123_1990_tract.csv", "nhgis0025_ds120_1990_tract.csv")
d90 <- s3::s3_get_files(glue::glue("s3://geomarker/harmonized_historical_census_data/nhgis1990/{files90}"))
d90 <- mappp::mappp(d90$file_path, read_csv)

d90_all <- d90[[1]]

for (i in 2:length(d90)) {
  d90_all <- left_join(d90_all, d90[[i]])
}

d90 <- d90_all %>%
  mutate(census_tract_fips = glue::glue('{STATEA}{COUNTYA}{TRACTA}'),

         population_under_18 = ET3001 + ET3002 + ET3003 + ET3004 + ET3005 +
           ET3006 + ET3007 + ET3008 + ET3009 + ET3010 + ET3011 + ET3012,

         adj_2010_1989 = (320.4/188.6),

         median_income_2010_adj = E4U001 * adj_2010_1989,

         pct_assisted_income = E5A001/ (E5A001 + E5A002) * 100,

         race_denom = EUY001 + EUY002 + EUY003 + EUY004 + EUY005,

         pct_white = EUY001 / race_denom * 100,

         pct_black = EUY002 / race_denom * 100,

         pct_asian = EUY004 / race_denom * 100,

         pct_other = (race_denom - (EUY001 + EUY002 + EUY004)) / race_denom * 100,

         pct_hispanic = (EU1002 + EU1003 + EU1004 + EU1005) /
           (EU1001 + EU1002 + EU1003 + EU1004 + EU1005)  * 100,

         nh_denom = ET2001 + ET2002 + ET2003 + ET2004 + ET2005 +
           ET2006 + ET2007 + ET2008 + ET2009 + ET2010,

         pct_white_nh = ET2001 / nh_denom * 100,

         pct_black_nh = ET2002 / nh_denom * 100,

         pct_asian_nh = ET2004 / nh_denom * 100,

         pct_other_nh = ((ET2001 + ET2002 + ET2003 + ET2004 + ET2005) -
           (ET2001 + ET2002 + ET2004))/ nh_denom * 100,

         pct_poverty = (E07013 + E07014 + E07015 + E07016 + E07017 + E07018 +
                          E07019 + E07020 + E07021 + E07022 + E07023 + E07024) /
           (E07001 + E07002 + E07003 + E07004 + E07005 + E07006 + E07007 +
              E07008 + E07009 + E07010 + E07011 + E07012 +
              E07013 + E07014 + E07015 + E07016 + E07017 + E07018 +
              E07019 + E07020 + E07021 + E07022 + E07023 + E07024) * 100,

         pct_poverty_under_18 = (E07013 + E07014 + E07015 + E07016) /
           (E07001 + E07002 + E07003 + E07004 + E07013 + E07014 + E07015 + E07016) * 100,

         edu_denom = E33001 + E33002 + E33003 + E33004 + E33005 +
           E33006 + E33007,

         pct_no_hs = (E33001 + E33002) / edu_denom * 100,

         pct_hs = (E33003 + E33004 + E33005) / edu_denom * 100,

         pct_bach = E33006 / edu_denom * 100,

         pct_higher_than_bach = E33007 / edu_denom * 100,

         pct_vacant_housing = ESN002/ (ESN001 + ESN002) * 100,

         pct_owner_occupied = ES1001 / (ES1001 + ES1002) * 100,

         pct_hs_dropout = (E38011 + E38012 + E38013) /
           (E38005 + E38006 + E38007 + E38008 + E38009 +
              E38010 + E38011 + E38012 + E38013) * 100,

         pct_female_hh = (ET8002 + ET8007 + ET8008 + ET8010) /
           (ET8001 + ET8002 + ET8003 + ET8004 + ET8005 + ET8006 +
              ET8007 + ET8008 + ET8009 + ET8010) * 100,

         pct_single_parent = (ET8005 + ET8007) /
           (ET8001 + ET8002 + ET8003 + ET8004 + ET8005 + ET8006 +
              ET8007 + ET8008 + ET8009 + ET8010) * 100,

         pct_unemployed = (E4I003 + E4I007) / (E4I001 + E4I002 + E4I003 + E4I004 +
                                                 E4I005 + E4I006 + E4I007 + E4I008) * 100,

         median_rent_2010_adj = EYU001 * adj_2010_1989,

         median_home_value_2010_adj = EST001 * adj_2010_1989

         ) %>%
  dplyr::select(
    census_tract_fips,
    population_total = ET1001,
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
    median_income = E4U001,
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
    median_rent = EYU001,
    median_rent_2010_adj,
    median_home_value = EST001,
    median_home_value_2010_adj
  ) %>%
  mutate_if(is.numeric, round)

length(unique(d90$census_tract_fips)) == nrow(d90)

library(sf)

tract_area <-
  s3::s3_get('s3://geomarker/geometries/census_tracts_1970_to_2020_valid.rds') %>%
  readRDS()

tract_area90 <- tract_area[[3]]

tract_area90$area_m2 <- st_area(tract_area90)

tract_area90 <-
  tract_area90 %>%
  st_drop_geometry() %>%
  mutate(area_m2 = as.numeric(area_m2)) %>%
  select(census_tract_fips = census_tract_id, area_m2)

d90 <- left_join(d90, tract_area90, by = 'census_tract_fips')

write_csv(d90, 'nhgis_data/1990_census_data.csv')

fs::dir_delete("s3_downloads")

