# Decennial Deprivation Index
## Objective
To create a deprivation index for each census tract in the United States from 1980 to 2010 based on a principal components analysis of six different decennial census measures.

## Details on Creating the Index
The following census tract level variables from the harmonized_historical_census_data were used in the analysis:

- `pct_poverty`: percent popultution below poverty level
- `median_income_2010_adj`: median household income (inflation adjusted dollars)
- `pct_no_hs`: percent population without a high school diploma
- `pct_unemployed`: percnet of population >= 16 years of age that are unemployed
- `pct_assisted_income`: percentage of households receiving assisted income
- `pct_vacant_housing`: percentage of houses that are vacant

The decennial deprivation index was calculated using all decennial census data from 1980 to 2010. The plot below compares the loading weights on the first principal component calculated using all available decennial data from 1980 to 2010 and using decennial data from each decade. It shows that the loading weights are consistent regardless of data source.
  

