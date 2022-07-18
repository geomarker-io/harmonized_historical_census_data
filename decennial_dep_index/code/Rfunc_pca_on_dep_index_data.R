

library(tidyverse)
library(ggpubr)


pca_dep_index <- function(d){
  
  year = d$census_tract_vintage[1]
  
  d_pca <- d %>%
    na.omit() %>%
    ungroup() %>%
    select(-census_tract_vintage,
           -census_tract_id) %>%
    prcomp(center=TRUE, scale=TRUE)
  
  # table variance explained by component
  summary(d_pca)$importance %>%
    as_tibble() %>%
    mutate(measure = row.names(summary(d_pca)$importance)) %>%
    slice(-1) %>%
    select(measure, everything()) %>% 
    knitr::kable(digits=2) %>%
    cat(file=paste0('figs/variance_of_explained_by_dep_index_', year,'.md'), sep='\n')
  
  # For plotting variance explained by component
  d_pca_imp <- summary(d_pca)$importance %>%
    as_tibble() %>%
    mutate(measure = row.names(summary(d_pca)$importance)) %>%
    gather(component, value, -measure) %>% 
    mutate(census_tract_vintage = year) 
  
  if(year < 2010){
    # inverse sign all weights so higher PC1 means more deprivation
    dep_weights <- d_pca$rotation %>%
      as_tibble() %>%
      mutate(measure = row.names(d_pca$rotation)) %>%
      gather(component, weight, -measure) %>%
      mutate(weight = -1 * weight, 
             measure = factor(measure, levels = unique(measure)),
             census_tract_vintage = year)
    
    # take the first pc and norm to [0,1]
    # reverse magnitude so higher value means higher deprivation
    dep_index <- d_pca$x %>%
      as_tibble() %>%
      select(dep_index = PC1) %>%
      mutate(dep_index = -1 * dep_index) %>%
      mutate(dep_index = (dep_index - min(dep_index)) / diff(range(dep_index))) %>%
      mutate(census_tract_id = d %>% na.omit() %>% pull(census_tract_id),
             census_tract_vintage = year,
             dep_index_rank = ntile(dep_index, 100)) 
  }
  
  if(year == 2010){
    # not inverse sign 
    dep_weights <- d_pca$rotation %>%
      as_tibble() %>%
      mutate(measure = row.names(d_pca$rotation)) %>%
      gather(component, weight, -measure) %>%
      mutate(measure = factor(measure, levels = unique(measure)),
             census_tract_vintage = year)
    
    # take the first pc and norm to [0,1]
    # reverse magnitude so higher value means higher deprivation
    dep_index <- d_pca$x %>%
      as_tibble() %>%
      select(dep_index = PC1) %>%
      mutate(dep_index = (dep_index - min(dep_index)) / diff(range(dep_index))) %>%
      mutate(census_tract_id = d %>% na.omit() %>% pull(census_tract_id),
             census_tract_vintage = year,
             dep_index_rank = ntile(dep_index, 100)) 
  }
  
  output <- list('importance'=d_pca_imp, 
                 'dep_weights'=dep_weights,
                  'dep_index'=dep_index)
  
  return(output)
  
} # end of pca_dep_index


#--------------------------------------------
# compare with 2015 acs dep index

plot_dep_index_comp_with_acs <- function(year){
  
  dep_index <- dep_index_perdec_alldec %>% 
    filter(census_tract_vintage == year)
  
  d_acs <- readRDS("data/ACS_deprivation_index_by_census_tracts.rds")%>% 
    as_tibble() %>% 
    ungroup() %>% 
    transmute(dep_index_acs = dep_index,
              census_tract_id = census_tract_fips) %>% 
    left_join(dep_index, by="census_tract_id") 
  
  d_acs %>% 
    ggplot(aes(x=dep_index, y=dep_index_acs)) + 
    geom_point(alpha = 0.3) +
    geom_abline(intercept = 0, slope = 1, color='blue') +
    xlab(paste0('deprivation index - ', year, ' 10-year census data')) +
    ylab('deprivation index - 2015 acs data') +
    xlim(c(0,1)) +
    ylim(c(0,1)) +
    theme_bw() + 
    stat_cor(method="pearson")
}

plot2d_dep_index_comp_with_acs <- function(year){
  
  dep_index <- dep_index_perdec_alldec %>% 
    filter(census_tract_vintage == year)
  
  d_acs <- readRDS("data/ACS_deprivation_index_by_census_tracts.rds")%>% 
    as_tibble() %>% 
    ungroup() %>% 
    transmute(dep_index_acs = dep_index,
              census_tract_id = census_tract_fips) %>% 
    left_join(dep_index, by="census_tract_id") 
  
  d_acs %>% 
    ggplot(aes(x=dep_index, y=dep_index_acs)) + 
    geom_bin2d(bins = 70) +
    scale_fill_continuous(type = "viridis") +
    geom_abline(intercept = 0, slope = 1, color='blue') +
    xlab(paste0('deprivation index - ', year, ' 10-year census data')) +
    ylab('deprivation index - 2015 acs data') +
    xlim(c(0,1)) +
    ylim(c(0,1)) +
    theme_bw() + 
    stat_cor(method="pearson")
}

