library(tidyverse)

files10 <- c("nhgis0027_ds177_20105_tract.csv", "nhgis0027_ds176_20105_tract.csv",
             "nhgis0027_ds172_2010_tract.csv")
d10 <- s3::s3_get_files(glue::glue("s3://geomarker/harmonized_historical_census_data/nhgis2010/{files10}"))
d10 <- mappp::mappp(d10$file_path, read_csv)

d10[[2]] <- d10[[2]] %>% select(GISJOIN, JLZE001:JTIM001)
d10[[3]] <- d10[[3]] %>% select(GISJOIN, H7V001:IFF004)

d10_all <- d10[[1]]
d10_all <- left_join(d10_all, d10[[2]])
d10_all <- left_join(d10_all, d10[[3]])


d10 <- d10_all %>%
  mutate(census_tract_fips = glue::glue('{STATEA}{COUNTYA}{TRACTA}'),

         population_under_18 = JLZE003 + JLZE004 + JLZE005 + JLZE006 +
           JLZE027 + JLZE028 + JLZE029 + JLZE030,

         pct_assisted_income = JPBE002 / JPBE001 * 100,

         pct_white = H7X002/H7X001 * 100,

         pct_black = H7X003/H7X001 * 100,

         pct_asian = H7X005/H7X001 * 100,

         pct_other = (H7X001 - (H7X002 + H7X003 + H7X005))/H7X001 * 100,

         pct_hispanic = H7Y003 / H7Y001  * 100,

         pct_white_nh = H7Z003/H7Z001 * 100,

         pct_black_nh = H7Z004/H7Z001 * 100,

         pct_asian_nh = H7Z006/H7Z001 * 100,

         pct_other_nh = (H7Z002 - (H7X003 + H7X004 + H7X006))/H7X001 * 100,

         pct_poverty = J2VE002 / J2VE001 * 100,

         pct_poverty_under_18 = (J2VE004 + J2VE005 + J2VE006 + J2VE007 +
                                   J2VE008 + J2VE009 + J2VE018 + J2VE019 + J2VE020 +
                                   J2VE021 + J2VE022 + J2VE023) / J2VE001 * 100,

         pct_no_hs = (JN9E003 + JN9E004 + JN9E005 + JN9E006 + JN9E007 +
                        JN9E008 + JN9E009 +  JN9E010 + JN9E020 + JN9E021 +
                        JN9E022 + JN9E023 + JN9E024 + JN9E025 + JN9E026 + JN9E027) / JN9E001 * 100,

         pct_hs = (JN9E011 + JN9E028) / JN9E001* 100,

         pct_bach = (JN9E015 + JN9E032) / JN9E001 * 100,

         pct_higher_than_bach = (JN9E016 + JN9E017 + JN9E018 +
                                   JN9E033 + JN9E034 + JN9E035) / JN9E001 * 100,

         pct_vacant_housing = IFE003 / IFE001 * 100,

         pct_owner_occupied = (IFF002 + IFF003) / IFF001 * 100,

         pct_hs_dropout = (JNZE012 + JNZE026) / JNZE001,

         pct_female_hh = H8C006 / H8C001 * 100,

         pct_single_parent = (H8C005 + H8C006) / H8C001 * 100,

         pct_unemployed = (J6QE008 + J6QE015 + J6QE022 + J6QE029 + J6QE036 +
                             J6QE043 + J6QE050 + J6QE057 + J6QE064 + J6QE071 +
                             J6QE076 + J6QE081 + J6QE086 + J6QE094 + J6QE101 +
                             J6QE108 + J6QE115 + J6QE122 + J6QE129 + J6QE136 +
                             J6QE143 + J6QE150 + J6QE157 + J6QE162 + J6QE167 +
                             J6QE172) / J6QE001 * 100) %>%
  dplyr::select(
    census_tract_fips,
    population_total = H7V001,
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
    median_income = JOIE001,
    median_income_2010_adj = JOIE001,
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
    median_rent = JS5E001,
    median_rent_2010_adj = JS5E001,
    median_home_value = JTIE001,
    median_home_value_2010_adj = JTIE001
  ) %>%
  mutate_if(is.numeric, round)

library(sf)

tract_area <-
  s3::s3_get('s3://geomarker/geometries/census_tracts_1970_to_2020_valid.rds') %>%
  readRDS()

tract_area10 <- tract_area[[5]]

tract_area10$area_m2 <- st_area(tract_area10)

tract_area10 <-
  tract_area10 %>%
  st_drop_geometry() %>%
  mutate(area_m2 = round(as.numeric(area_m2))) %>%
  select(census_tract_fips = census_tract_id, area_m2)

d10 <- left_join(d10, tract_area10, by = 'census_tract_fips')

write_csv(d10, 'nhgis_data/2010_census_data.csv')

fs::dir_delete("s3_downloads")
