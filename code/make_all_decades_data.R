library(tidyverse)

f <- list.files(pattern = "*census_data.csv", path = "nhgis_data/")
d <- mappp::mappp(f, ~read_csv(glue::glue("nhgis_data/{.x}")))

years <- c("1970", "1980", "1990", "2000", "2010")

d <- map2(d, years, ~.x %>%
            mutate(census_tract_vintage = .y) %>%
            relocate(census_tract_vintage) %>%
            rename(census_tract_id = census_tract_fips))

d <- bind_rows(d)
d

saveRDS(d, 'harmonized_historical_census_data.rds')
write_csv(d, 'harmonized_historical_census_data.csv')


### check for unique ids
d70 <- d %>%
  filter(census_tract_vintage == 1970)

nrow(d70) == length(unique(d70$census_tract_id))

d80 <- d %>%
  filter(census_tract_vintage == 1980)

nrow(d80) == length(unique(d80$census_tract_id))

d90 <- d %>%
  filter(census_tract_vintage == 1990)

nrow(d90) == length(unique(d90$census_tract_id))

d00 <- d %>%
  filter(census_tract_vintage == 2000)

nrow(d00) == length(unique(d00$census_tract_id))

d10 <- d %>%
  filter(census_tract_vintage == 2010)

nrow(d10) == length(unique(d10$census_tract_id))

