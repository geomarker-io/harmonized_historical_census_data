
library(tidyverse)
library(forcats)
library(GGally)
library(reshape2)


census_data <- readRDS("data/harmonized_historical_census_data.rds") %>% 
  as_tibble()

dir.create('figs')


# Perform PCA on data from all decades
# remove duplicates
# keep the first row if values are different for the same census tract id

d <- census_data %>% 
  select(census_tract_vintage, census_tract_id,
         median_income_2010_adj, pct_vacant_housing, 
         pct_poverty, pct_unemployed, 
         pct_no_hs, pct_assisted_income) %>% 
  filter(census_tract_vintage != '1970')    # not include 1970 data in dep index calculation

############## principal components analysis across 1980 to 2010 -----------------------

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
  cat(file = 'figs/variance_of_acs_explained_by_dep_index_1980-2010.md', sep='\n')

# For plotting variance explained by component
d_pca_imp_alldec <- summary(d_pca)$importance %>%
  as_tibble() %>%
  mutate(measure = row.names(summary(d_pca)$importance)) %>%
  gather(component, value, -measure) %>% 
  mutate(census_tract_vintage = '1980-2010') 

# inverse sign all weights so higher PC1 means more deprivation
dep_weights_alldec <- d_pca$rotation %>%
  as_tibble() %>%
  mutate(measure = row.names(d_pca$rotation)) %>%
  gather(component, weight, -measure) %>%
  mutate(weight = -1 * weight, 
         measure = factor(measure, levels = unique(measure)),
         census_tract_vintage = '1980-2010')

# take the first pc and norm to [0,1]
# reverse magnitude so higher value means higher deprivation
dep_index_alldec <- d_pca$x %>%
  as_tibble() %>%
  select(dep_index = PC1) %>%
  mutate(dep_index = -1 * dep_index) %>%
  mutate(dep_index = (dep_index - min(dep_index)) / diff(range(dep_index))) %>%
  mutate(census_tract_id = d %>% na.omit() %>% pull(census_tract_id),
         census_tract_vintage = '1980-2010',
         year = d %>% na.omit() %>% pull(census_tract_vintage),
         dep_index_rank = ntile(dep_index, 100))  

saveRDS(d_pca_imp_alldec, "data/d_pca_importance_1980-2010.rds")
saveRDS(dep_weights_alldec, "data/dep_weights_1980-2010.rds")
saveRDS(dep_index_alldec, "data/dep_index_1980-2010.rds")

#--------------------------------------------------------------------
# merge deprivation index calculated using all decennial data into original data and save 

census_data_v2 <- dep_index_alldec %>% 
  transmute(census_tract_id = census_tract_id,
            census_tract_vintage = year,
            dep_index = dep_index) %>% 
  left_join(x=census_data,y=., by = c('census_tract_vintage', 'census_tract_id'))  

saveRDS(census_data_v2, 'data/harmonized_historical_census_data_w_dep-index.rds')
rio::export(census_data_v2, 'data/harmonized_historical_census_data_w_dep-index.csv')




############ principal components analysis by census tract vintage from 1980 to 2010 ----------------------------------------

source('Rfunc_pca_on_dep_index_data.R')

# pca

output <- d %>% 
  split(.$census_tract_vintage) %>% 
  map(pca_dep_index) %>% 
  transpose()

d_pca_imp_perdec <- output[[1]] %>% bind_rows()
dep_weights_perdec <- output[[2]] %>% bind_rows()
dep_index_perdec <- output[[3]] %>% bind_rows()

#
#saveRDS(d_pca_imp_perdec, "data/d_pca_importance_1980-2010_perdec.rds")
#saveRDS(dep_weights_perdec, "data/dep_weights_1980-2010_perdec.rds")
#saveRDS(dep_index_perdec, "data/dep_index_1980-2010_perdec.rds")

#---------------------------------------------------------------------
# merge deprivation index calculated using all decennial data with that calculated using each one-decade data 

dep_index_perdec_alldec <- dep_index_alldec %>% 
  transmute(census_tract_id = census_tract_id,
            census_tract_vintage = year,
            dep_index_alldec = dep_index,
            dep_index_rank_alldec = dep_index_rank) %>% 
  left_join(x=dep_index_perdec,y=., by = c('census_tract_vintage', 'census_tract_id')) 




######### Plots -------------------------------------------------------------------

#--------------------------------------------
# plot variable distribution
pdf('figs/00a_variable_distribution_by_decade_1980-2010.pdf', width=10, height=5)

for(var in c("median_income_2010_adj", "pct_vacant_housing",
             "pct_poverty", "pct_unemployed", "pct_no_hs", 
             "pct_assisted_income")){
  
  p <- d %>%
    ggplot(aes( !!sym(var) )) +
    geom_density(fill='lightgrey') +
    facet_wrap(~ factor(census_tract_vintage, levels=c(unique(d$census_tract_vintage)))) + 
    labs(title = paste0('Distribution for All US Census Tracts', ' (', var, ')')) +
    xlab(var)
  print(p)
  
}

dev.off()


#--------------------------------------------
# correlation between variables

pdf('figs/00b_variable_correlation_by_decade_1980-2010.pdf', width=10, height=8)

## 1980-2010 all decades
cormat<-round(cor(d[3:8], method="pearson", use = "complete.obs"),2)
melted_cormat <- melt(cormat)

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()) +
  labs(title = paste0('Pairwise Correlation among Variables (1980-2010)'))

print(ggheatmap)

## 1980-2010 per decade
for (year in c('1980','1990','2000','2010')){
  cormat<-round(cor(subset(d, census_tract_vintage==year)[3:8], method="pearson", use = "complete.obs"),2)
  melted_cormat <- melt(cormat)
  
  # Create a ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 10, hjust = 1))+
    coord_fixed() + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank()) +
    labs(title = paste0('Pairwise Correlation among Variables', ' (', year, ')'))
  
  print(ggheatmap)
}

dev.off()


#--------------------------------------------
# plot variance explained by component
pdf('figs/01_variance_explained_by_dep_index_1980-2010.pdf', width=10, height=4)
  
  # all years on one page
  plot <- d_pca_imp_perdec %>% 
    rbind(d_pca_imp_alldec) %>% 
    filter(measure != 'Standard deviation') %>% 
    ggplot(aes(component, value, alpha=measure)) +
    geom_bar(stat='identity', position=position_dodge(0)) +
    facet_wrap(~ factor(census_tract_vintage, levels=c('1980-2010', unique(d_pca_imp_perdec$census_tract_vintage)))) +
    labs(title = 'Variance of Measures Expained by Deprivation Indices (1980-2010)') +
    theme(legend.title=element_blank()) +
    xlab('index') + ylab('variance')
  print(plot)

  # one decade per page
  plots <- d_pca_imp_perdec %>%
    rbind(d_pca_imp_alldec) %>% 
    mutate(census_tract_vintage = factor(census_tract_vintage, levels = c('1980-2010',seq(1980, 2010, by=10)))) %>% 
    filter(measure != 'Standard deviation') %>% 
    split(.$census_tract_vintage) %>% 
    map(~ggplot(., aes(component, value, alpha=measure)) +
          geom_bar(stat='identity', position=position_dodge(0)) +
          labs(title = paste0('Variance of Measures Expained by Deprivation Indices', ' (', unique(.$census_tract_vintage), ')')) +
          theme(legend.title=element_blank()) +
          xlab('index') + ylab('variance'))
  invisible(lapply(plots, print))

dev.off()


#--------------------------------------------
# plot loading weights

pdf('figs/02_measure_weights_on_dep_index_1980-2010.pdf', width=10, height=6)

  # by pc
  plots <- dep_weights_perdec %>%
    rbind(dep_weights_alldec) %>%  
    split(.$component) %>% 
    map(~ggplot(., aes(measure, weight)) +
          geom_bar(stat='identity') +
          scale_x_discrete(limits=rev(levels(unique(.$measure)))) +
          coord_flip() +
          facet_wrap(~ factor(census_tract_vintage, levels=c('1980-2010', unique(dep_weights_perdec$census_tract_vintage)))) +
          labs(title = paste0('Weights of Measure on Deprivation Indices', ' (', unique(.$component), ')')) +
          xlab(' '))
  invisible(lapply(plots, print))

  # by year
  plots <- dep_weights_perdec %>%
    rbind(dep_weights_alldec) %>%  
    mutate(census_tract_vintage = factor(census_tract_vintage, levels = c('1980-2010',seq(1980, 2010, by=10)))) %>% 
    split(.$census_tract_vintage) %>% 
    map(~ggplot(., aes(measure, weight)) +
          geom_bar(stat='identity') +
          scale_x_discrete(limits=rev(levels(unique(.$measure)))) +
          coord_flip() +
          facet_wrap(~ component) +
          labs(title = paste0('Weights of Measure on Deprivation Indices', ' (', unique(.$census_tract_vintage), ')')) +
          xlab(' '))
  invisible(lapply(plots, print))
  
dev.off()


#--------------------------------------------
# plot univariate distribution of dep_index

pdf('figs/03_dep_index_density_1980-2010.pdf', width=10, height=5)
  
  # all years on one page
  p <- dep_index_perdec %>%
    rbind(dep_index_alldec %>% select(-year)) %>%
    ggplot(aes(dep_index)) +
    geom_density(fill='lightgrey') +
    facet_wrap(~ factor(census_tract_vintage, levels=c('1980-2010', unique(dep_index_perdec$census_tract_vintage)))) +
    labs(title = 'Distribution of Deprivation Index for All US Census Tracts') +
    xlab('deprivation index')
  print(p)

  # one decade per page
  plots <- dep_index_perdec %>%
    rbind(dep_index_alldec %>% select(-year)) %>% 
    mutate(census_tract_vintage = factor(census_tract_vintage, levels = c('1980-2010',seq(1980, 2010, by=10)))) %>% 
    split(.$census_tract_vintage) %>% 
    map(~ggplot(., aes(dep_index)) +
          geom_density(fill='lightgrey') +
          labs(title = paste0('Distribution of Deprivation Index for All US Census Tracts', ' (', unique(.$census_tract_vintage), ')')) +
          xlab('deprivation index'))
  invisible(lapply(plots, print))
  
dev.off()


#--------------------------------------------
# plot comparisons of dep_index estimated using one decennial data and all decennial data

pdf('figs/04_dep_index_comparison_by_decade_1980-2010.pdf', width=10, height=8)

plots <- dep_index_perdec_alldec %>% 
  split(.$census_tract_vintage) %>% 
  map(~ggplot(., aes(x=dep_index, y=dep_index_alldec)) +
        geom_bin2d(bins = 70) +
        scale_fill_continuous(type = "viridis") +
        geom_abline(intercept = 0, slope = 1, color='blue') +
        labs(title = paste0('Comparison of Deprivation Index for All US Census Tracts', ' (', unique(.$census_tract_vintage), ')')) +
        xlab(paste0('deprivation index - ', unique(.$census_tract_vintage), ' 10-year census data')) +
        ylab('deprivation index - 1980-2010 10-year census data') +
        xlim(c(0,1)) +
        ylim(c(0,1)) +
        theme_bw())
invisible(lapply(plots, print))

dev.off()

#--------------------------------------------
# plot comparisons of dep_index 10-year vs 2015 acs 


pdf('figs/05_dep_index_comparison_perdec_vs_acs2015.pdf', width=10, height=8)

year <- list(1980, 1990, 2000, 2010)

plots <- year %>% 
  map(plot2d_dep_index_comp_with_acs)
invisible(lapply(plots, print))

dev.off()


#--------------------------------------------
# rank comparison - estimated using one decade data

dep_index_perdec_wide <-  dep_index_perdec %>% 
  pivot_wider(names_from = census_tract_vintage, values_from=c(dep_index, dep_index_rank))

pdf('figs/06_dep_index_rank_comparison_by_decade_perdec_data.pdf', width=10, height=8)

ggpairs(dep_index_perdec_wide, columns = 6:9)

for(i in 6:8){
  
  p <- ggplot(dep_index_perdec_wide, aes_string(x=names(dep_index_perdec_wide)[i], y=names(dep_index_perdec_wide)[i+1])) + 
    geom_bin2d(bins = 70) +
    scale_fill_continuous(type = "viridis") +
    geom_abline(intercept = 0, slope = 1, color='blue') +
    labs(title = paste0('Comparison of Deprivation Index Rank for All US Census Tracts')) +
    xlab(names(dep_index_perdec_wide)[i]) +
    ylab(names(dep_index_perdec_wide)[i+1]) +
    xlim(c(0,100)) +
    ylim(c(0,100)) +
    theme_bw()
  
  print(p)
}

dev.off()


#--------------------------------------------
# rank comparison - estimated using all decades data

dep_index_alldec_wide <-  dep_index_alldec %>% 
  pivot_wider(names_from = year, values_from=c(dep_index, dep_index_rank))

pdf('figs/06b_dep_index_rank_comparison_by_decade_alldec_data.pdf', width=10, height=8)

ggpairs(dep_index_alldec_wide, columns = 7:10)

for(i in 7:9){
  
  p <- ggplot(dep_index_alldec_wide, aes_string(x=names(dep_index_alldec_wide)[i], y=names(dep_index_alldec_wide)[i+1])) + 
    geom_bin2d(bins = 70) +
    scale_fill_continuous(type = "viridis") +
    geom_abline(intercept = 0, slope = 1, color='blue') +
    labs(title = paste0('Comparison of Deprivation Index Rank for All US Census Tracts')) +
    xlab(names(dep_index_alldec_wide)[i]) +
    ylab(names(dep_index_alldec_wide)[i+1]) +
    xlim(c(0,100)) +
    ylim(c(0,100)) +
    theme_bw()
  
  print(p)
}

dev.off()

#--------------------------------------------
# rank comparison - estimates from one decade data vs all decade data

pdf('figs/06c_dep_index_rank_comparison_by_decade_perdec_vs_alldec.pdf', width=10, height=8)

plots <- dep_index_perdec_alldec %>% 
  split(.$census_tract_vintage) %>% 
  map(~ggplot(., aes(x=dep_index_rank, y=dep_index_rank_alldec)) +
        geom_bin2d(bins = 70) +
        scale_fill_continuous(type = "viridis") +
        geom_abline(intercept = 0, slope = 1, color='blue') +
        labs(title = paste0('Comparison of Deprivation Index for All US Census Tracts', ' (', unique(.$census_tract_vintage), ')')) +
        xlab(paste0('deprivation index rank - ', unique(.$census_tract_vintage), ' 10-year census data')) +
        ylab('deprivation index rank - 1980-2010 10-year census data') +
        xlim(c(0,100)) +
        ylim(c(0,100)) +
        theme_bw())
invisible(lapply(plots, print))

dev.off()


#--------------------------------------------
# another way to look at rank change over time

pdf('figs/07_dep_index_rank_comparison_over_time_alldec.pdf', width=10, height=8)


  for (i in seq(0,95,by=5)) {
    j=i+5
    
    id <- dep_index_alldec_wide %>% 
      filter(dep_index_rank_2010 >= i & dep_index_rank_2010 <= j) %>% 
      select(census_tract_id)
    
    p <- ggplot(subset(dep_index_alldec, census_tract_id %in% id$census_tract_id), 
                aes(x=year, y=dep_index_rank, group = census_tract_id)) + 
      geom_line(color = "steelblue") + 
      geom_point(alpha=0.3, color = "steelblue") +
      labs(title = paste0('Deprivation Index Change Over Time (dep_index_rank_2010 between ', 
                          i, " and ", j, ")")) +
      theme_bw()
    print(p)
  }
  
dev.off()
  


