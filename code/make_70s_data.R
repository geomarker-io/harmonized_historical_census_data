library(tidyverse)

files70 <- c("nhgis0028_ds99_1970_tract.csv", "nhgis0023_ds99_1970_tract.csv",
             "nhgis0023_ds98_1970_tract.csv", "nhgis0023_ds97_1970_tract.csv",
             "nhgis0023_ds96_1970_tract.csv", "nhgis0023_ds95_1970_tract.csv",
             "nhgis0029_ts_nominal_tract.csv")
d70 <- s3::s3_get_files(glue::glue("s3://geomarker/harmonized_historical_census_data/nhgis1970/{files70}"))
d70 <- mappp::mappp(d70$file_path, read_csv)

d70[[7]] <- d70[[7]] %>%
  filter(!is.na(GJOIN1970)) %>%
  select(GISJOIN = GJOIN1970, A68AA1970)

d70_all <- d70[[1]]

for (i in 2:6) {
  d70_all <- left_join(d70_all, d70[[i]], by = c("GISJOIN", "YEAR", "STATE", "STATEA", "COUNTY", "COUNTYA", "CTY_SUBA", "PLACEA", "TRACTA",
                                            "SCSAA", "SMSAA", "URB_AREAA", "BLOCKA", "CDA", "AREANAME"))
}

d70_all <- left_join(d70_all, d70[[7]], by = c("GISJOIN"))

d70 <- d70_all %>%
  mutate(
    census_tract_fips = glue::glue('{STATEA}{COUNTYA}{TRACTA}'),

    population_under_18 = CE6001 + CE6002 + CE6003 + CE6004 + CE6005 + CE6006 + CE6007 +
      CE6008 + CE6009 + CE6010 + CE6011 + CE6012 + CE6013 + CE6014 + CE6015 +
      CE6016 + CE6017 + CE6018 +
      CE6102 + CE6103 + CE6104 + CE6105 + CE6106 + CE6107 + CE6108 + CE6109 + CE6110 +
      CE6111 + CE6112 + CE6113 + CE6114 + CE6115 + CE6116 + CE6117 + CE6118 + CE6119,

    total_families = A68AA1970,
    total_families = ifelse(total_families == 0, NA, total_families),

    adj_1969_2010 = (320.4 / 60.9),

    C1K001 = ifelse(C1K001 < 0, NA, C1K001),
    median_income = C1K001 / total_families,

    median_income_2010_adj = median_income * adj_1969_2010,

    pct_assisted_income = (C34005 + C34011) / (C34001 + C34002 + C34003 + C34004 +
                                                 C34005 + C34006 + C34007 +
                                                 C34008 + C34009 + C34010 +
                                                 C34011 + C34012) * 100,

    race_denom = CEB001 + CEB002 + CEB003 + CEB004 + CEB005 + CEB006 + CEB007 +
      CEB008 + CEB009 + CEB010 + CEB011 + CEB012 + CEB013 + CEB014 + CEB015 +
      CEB016 + CEB017 + CEB018,

    pct_white = (CEB001 + CEB010) / race_denom * 100,

    pct_black = (CEB002 + CEB011) / race_denom * 100,

    pct_asian = (CEB003 + CEB004 + CEB005 + CEB006 + CEB008 +
                   CEB012 + CEB013 + CEB014 + CEB015 + CEB017) / race_denom * 100,

    pct_other = (race_denom - (CEB001 + CEB010 + CEB002 + CEB011 +
                                 CEB003 + CEB004 + CEB005 + CEB006 + CEB008 +
                                 CEB012 + CEB013 + CEB014 + CEB015 + CEB017)) / race_denom * 100,

    pct_hispanic = (C11001 + C11002) / (C11001 + C11002 + C11003 + C11004) * 100,

    pct_white_nh = NA,
    pct_black_nh = NA,
    pct_asian_nh = NA,
    pct_other_nh = NA,

    pct_poverty = (C37007 + C37008 + C37009 + C37010 + C37011 + C37012) /
      (C37001 + C37002 + C37003 + C37004 + C37005 + C37006 +
         C37007 + C37008 + C37009 + C37010 + C37011 + C37012) * 100,

    pct_poverty_under_18 = (C37008 + C37011) / (C37002 + C37005 + C37008 + C37011) * 100,

    edu_denom = C06001 + C06002 + C06003 + C06004 + C06005 + C06006 +
      C06007 + C06008 + C06009 + C06010,

    pct_no_hs = (C06001 + C06002 + C06003 + C06004 + C06005 + C06006) / edu_denom * 100,

    pct_hs = (C06007 + C06008) / edu_denom * 100,

    pct_bach = C06009 / edu_denom * 100,

    pct_higher_than_bach = C06010 / edu_denom * 100,

    pct_vacant_housing = (CE9002 + CE9003 + CE9004 + CE9005 + CE9006) /
      (CE9001 + CE9002 + CE9003 + CE9004 + CE9005 + CE9006) * 100,

    pct_owner_occupied = (CFA001 + CFA002) / (CFA001+ CFA002 + CFA003+ CFA004) * 100,

    pct_hs_dropout = (C2L001 + C2L003 + C2L005 + C2L007) /
      (CE6017 + CE6018 + CE6019 + CE6020 + CE6021 + CE6022 +
         CE6118 + CE6119 + CE6120 + CE6121 + CE6122 + CE6123) * 100,

    pct_female_hh = (CL4003 + CL4005) / (CL4001 + CL4002 + CL4003 + CL4004 + CL4005) * 100,

    pct_single_parent = NA,

    pct_unemployed = C07003 / (C07001 + C07002 + C07003 + C07004) * 100,

    median_rent = CEW001 / total_families,

    median_rent_2010_adj = median_rent * adj_1969_2010,

    median_home_value = CET001 / total_families,

    median_home_value_2010_adj = median_home_value * adj_1969_2010
  ) %>%
  select(
    census_tract_fips,
    population_total = CY7001,
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
    median_income,
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
    median_rent,
    median_rent_2010_adj,
    median_home_value,
    median_home_value_2010_adj
  ) %>%
  mutate_if(is.numeric, round)

# check for unique census_tract_fips
# nrow(d70[duplicated(d70$census_tract_fips),]) == 0

library(sf)
tract_area <-
  s3::s3_get('s3://geomarker/geometries/tracts_1970_5072.rds') %>%
  readRDS()

tract_area$area_m2 <- st_area(tract_area)

tract_area <-
  tract_area %>%
  st_drop_geometry() %>%
  mutate(area_m2 = round(as.numeric(area_m2)),
         state_fips = stringr::str_sub(census_tract_id_1970, 1, 2),
         county_fips = stringr::str_sub(census_tract_id_1970, 3, 5),
         tract_fips = stringr::str_sub(census_tract_id_1970, 6),
         census_tract_fips = glue::glue('{state_fips}{county_fips}{tract_fips}')) %>%
  select(census_tract_fips, area_m2)

d70 <- left_join(d70, tract_area, by = 'census_tract_fips')

write_csv(d70, 'nhgis_data/1970_census_data.csv')

fs::dir_delete("s3_downloads")
