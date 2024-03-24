# map and plot explicit cutting restrictions for mangroves and their ecosystem services

library(tidyverse)
library(sf)
library(tmap)
sf_use_s2(FALSE)
crsrobin <- "+proj=robin +lon_0=145 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # robinson crs centered on oceania

# project spatial database of jurisdiction boundaries
World <- st_read('data/EEZ_Boundaries/EEZ_Land_v3_202030_erase_eez_v2.gpkg') %>% 
  st_simplify(preserveTopology = T, dTolerance = 0.01) %>% 
  st_break_antimeridian(lon_0 = 145) %>%
  st_transform(crs = crsrobin) %>% 
  st_make_valid() %>% 
  st_crop(World,  xmin = -17817528, ymin = -8000000, xmax = 16923579, ymax = 8619631)

# read in country codes and european territories that have not been individually assessed
codes <- read.csv('data/country-codes.csv') # country codes
euro_terr <- read.csv('data/european-territories.csv') %>% 
  filter(!Jurisdiction %in% codes$Jurisdiction) # filter only for territories that have not been individually assessed

# read in national socio-ecological covariate data 
nat_covar <- read.csv('data/national-covariate-master.csv')

# read in national desktop assessment of mangrove laws and policies - wrangle and join to other data
dat <- read.csv('data/law-policy-database.csv') %>% 
  mutate_at(vars(Mangrove_policy, Clearing_restrictions, Community_management, Environmental_impact_assessment), ~ifelse(. == 'N', 0, 1)) %>% 
  mutate(Clearing_restrictions = case_when(Clearing_restrictions_level == '2' ~ 2, Clearing_restrictions_level %in% c('3A', '3B') ~ 3, .default = Clearing_restrictions),
         Coastal_zone_planning = case_when(is.na(Coastal_zone_planning) ~ 0, Coastal_zone_planning == 'N' ~ 0, Coastal_zone_planning == 'Y' ~ 2)) %>% 
  mutate(Coordination_mechanism = case_when(Coordination_mechanism == 0 ~ 'None',
                                            Coordination_mechanism == 1 ~ 'General',
                                            Coordination_mechanism == 2 ~ 'Mangrove-specific')) %>% 
  mutate(Coordination_mechanism = factor(Coordination_mechanism, levels = c('None', 'General', 'Mangrove-specific'))) %>% 
  mutate(Clearing_restrictions = case_when(Clearing_restrictions == 0 ~ 'None',
                                           Clearing_restrictions == 1 ~ 'Low',
                                           Clearing_restrictions == 2 ~ 'Medium',
                                           Clearing_restrictions == 3 ~ 'High')) %>% 
  mutate(Clearing_restrictions = factor(Clearing_restrictions, levels = c('None', 'Low', 'Medium', 'High'))) %>% 
  mutate(Mangrove_policy = ifelse(Mangrove_policy == 1, 'Policy', 'No Policy')) %>% 
  mutate(Mangrove_policy = factor(Mangrove_policy, levels = c('No Policy', 'Policy'))) %>% 
  mutate(Government_type = ifelse(Federal_status == 'Y', 'Federal', NA)) %>% # combine federal status and territory into one variable
  mutate(Government_type = ifelse(Federal_status == 'N', 'Unitary independent', Government_type)) %>% 
  mutate(Government_type = ifelse(Territory_status == 3, 'Not fully independent', Government_type)) %>% 
  left_join(nat_covar, 'Country')

dat2 <- select(euro_terr, SOVEREIGN1, TERRITORY1, Juris_ISO) %>% # make data for territories of European sovereign nations
  left_join(select(dat, -Juris_ISO), by = c('SOVEREIGN1' = 'TERRITORY1'))

datmap <- dat %>% 
  filter(!Country %in% c('United Kingdom', 'Netherlands', 'France')) %>% # remove euro sovereign nations
  bind_rows(dat2) # bind data for their territories

world.p <- World %>% 
  inner_join(datmap, by = 'Juris_ISO')

#####
# Clearing restrictions maps and plots
#####

m1 <- tm_shape(World) +
  tm_polygons('gray93', lwd = 0.1) +
  tm_shape(world.p) +
  tm_polygons('Clearing_restrictions', palette = c('darkgrey', 'darkslategray3', 'darkslategray4', 'darkslategrey'), 
              lwd = 0.1, title = '', legend.is.portrait = F) +
  #tm_shape(mang_cover) +
  #tm_dots(col = 'forestgreen', alpha = 0.3, size = 0.01) +
  tm_layout(frame = F, legend.position = c(0.63,0.2))
m1  
tmap_save(m1, 'outputs/maps-time-series/maps/clearing_restrictions_map.png', dpi = 1000)

# summarise and plot of % of global mangrove area and ecosystem services under protection by national laws preventing cutting of mangroves
dat_p <- dat %>% 
  mutate(num = 1) %>% 
  group_by(Clearing_restrictions) %>% 
  summarise(`Number of Jurisdictions` = (sum(num)/nrow(dat))*100,
            `Global mangrove extent` = (sum(gmw_area_2020_ha)/sum(dat$gmw_area_2020_ha))*100,
            `Mangroves in protected areas` = sum(gmw_area_2020_wdpa_ha)/sum(dat$gmw_area_2020_wdpa_ha)*100,
            `Carbon stored (MtC02e)` = (sum(total_carbon_storage_MtC02e)/sum(dat$total_carbon_storage_MtC02e))*100,
            `Fisheries (Fisher days per year)` = (sum(fisher_days_yr)/sum(dat$fisher_days_yr))*100,
            `Tourism attractions` = (sum(mangrove_attractions)/sum(dat$mangrove_attractions))*100,
            `People protected (million)` = (sum(protection_num_people)/sum(dat$protection_num_people))*100,
            `Property protected ($US billion)` = (sum(protection_property_USD_billion)/sum(dat$protection_property_USD_billion))*100) %>% 
  pivot_longer(cols = `Number of Jurisdictions`:`Property protected ($US billion)`, names_to = 'indicator', values_to = 'percent_coverage') %>% 
  mutate(indicator = factor(indicator, levels = c( "Property protected ($US billion)", "People protected (million)", "Tourism attractions", "Fisheries (Fisher days per year)", "Carbon stored (MtC02e)", "Mangroves in protected areas", "Global mangrove extent","Number of Jurisdictions")))
write.csv(dat_p, 'outputs/maps-time-series/other/percent-coverage_clearing-restrictions.csv', row.names = F)

# plot
dat_p %>% 
  filter(Clearing_restrictions != 'None') %>% 
  ggplot() +
  geom_bar(aes(x = percent_coverage, y = indicator, fill = forcats::fct_rev(factor(Clearing_restrictions))), position="stack", stat="identity") +
  scale_fill_manual(values = c('darkslategray3', 'darkslategray4','darkslategrey')) +
  ylab('') +
  xlab('Percentage') +
  xlim(c(0,100)) +
  theme_classic() +
  theme(legend.title = element_blank())
ggsave('outputs/maps-time-series/other/clearing-restrictions_coverage_Level123.png', width = 5, height = 2)

# save total values for each bar in .csv for annotating plot later
datsum <- dat %>% 
  mutate(num = 1) %>% 
  group_by(Clearing_restrictions) %>% 
  summarise(`Number of Jurisdictions` = sum(num),
            `Global mangrove extent (km2)` = sum(gmw_area_2020_ha)/100, # convert to km2
            `Mangroves in protected areas` = sum(gmw_area_2020_wdpa_ha)/sum(dat$gmw_area_2020_wdpa_ha)*100,
            `Carbon (MtC02e)` = sum(total_carbon_storage_MtC02e),
            `Fisheries (Fisher days per year)` = sum(fisher_days_yr),
            `Tourism attractions (number)` = sum(mangrove_attractions),
            `People protected (million)` = sum(protection_num_people)/1000000, # convert to millions
            `Property protected ($US billion)` = sum(protection_property_USD_billion)) %>% 
  pivot_longer(cols = `Number of Jurisdictions`:`Property protected ($US billion)`)
write.csv(datsum, 'outputs/maps-time-series/other/clearing-restrictions_total-values.csv', row.names = F)

# time series - cumulative number of jurisdictions with clearing restrictions

dat %>% 
  filter(Clearing_restrictions != 'None') %>% 
  droplevels(.$Clearing_restrictions) %>% 
  mutate(n = 1) %>% 
  select(Clearing_restrictions, Clearing_restrictions_year, n) %>% 
  complete(Clearing_restrictions, Clearing_restrictions_year = min(dat$Clearing_restrictions_year, na.rm =T):max(dat$Clearing_restrictions_year, na.rm = T), fill = list(n = 0)) %>%
  group_by(Clearing_restrictions, Clearing_restrictions_year) %>% 
  summarise(n = sum(n)) %>% 
  group_by(Clearing_restrictions) %>%
  arrange(Clearing_restrictions_year) %>% 
  mutate(cumulative_sum = cumsum(n)) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = Clearing_restrictions_year, y = cumulative_sum, fill = forcats::fct_rev(factor(Clearing_restrictions))) +
  geom_area() +
  xlab('Year') +
  ylab('Cumulative number of jurisdictions') +
  scale_fill_manual(values = c('darkslategray3', 'darkslategray4','darkslategrey')) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.2,0.8))

ggsave('outputs/maps-time-series/time-series/clearing-restrictions_time-series.png', width = 4, height = 3)

#####
# Mangrove policy maps and plots
#####

m2 <- tm_shape(World) +
  tm_polygons('gray93', lwd = 0.1) +
  tm_shape(world.p) +
  tm_polygons('Mangrove_policy', palette = c('darkgrey', 'darkseagreen3'), 
              lwd = 0.1, title = '', legend.is.portrait = F) +
  tm_layout(frame = F, legend.position = c(0.63,0.2))
m2  
tmap_save(m2, 'outputs/maps-time-series/maps/mangrove-policy_map.png', dpi = 1000)

# time series - cumulative number of jurisdictions with policies

dat %>% 
  filter(Mangrove_policy != 'No Policy') %>% 
  mutate(n = 1) %>% 
  select(Mangrove_policy, Mangrove_policy_year, n) %>% 
  complete(Mangrove_policy, Mangrove_policy_year = min(dat$Mangrove_policy_year, na.rm =T):max(dat$Mangrove_policy_year, na.rm = T), fill = list(n = 0)) %>%
  group_by(Mangrove_policy_year) %>% 
  summarise(n = sum(n)) %>% 
  arrange(Mangrove_policy_year) %>%
  mutate(cumulative_sum = cumsum(n)) %>% 
  ggplot() +
  aes(x = Mangrove_policy_year, y = cumulative_sum) +
  geom_area(fill = 'darkseagreen3') +
  xlab('Year') +
  ylab('Cumulative number of jurisdictions') +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.2,0.8))

ggsave('outputs/maps-time-series/time-series/mangrove-policy_time-series.png', width = 4, height = 3)

#####
# Coordination mechanism maps and plots
#####

m2 <- tm_shape(World) +
  tm_polygons('gray93', lwd = 0.1) +
  tm_shape(world.p) +
  tm_polygons('Coordination_mechanism', palette = c('darkgrey', 'aquamarine3', 'aquamarine4'), 
              lwd = 0.1, title = '', legend.is.portrait = F) +
  tm_layout(frame = F, legend.position = c(0.63,0.2))
m2  
tmap_save(m2, 'outputs/maps-time-series/maps/coordination-mechanism_map.png', dpi = 1000)

# time series - cumulative number of jurisdictions with coordination mechanisms

dat %>% 
  filter(Coordination_mechanism != 'None') %>% 
  droplevels(.$Coordination_mechanism) %>% 
  mutate(n = 1) %>% 
  select(Coordination_mechanism, Coordination_mechanism_year, n) %>% 
  complete(Coordination_mechanism, Coordination_mechanism_year = min(dat$Coordination_mechanism_year, na.rm =T):max(dat$Coordination_mechanism_year, na.rm = T), fill = list(n = 0)) %>%
  group_by(Coordination_mechanism, Coordination_mechanism_year) %>% 
  summarise(n = sum(n)) %>% 
  group_by(Coordination_mechanism) %>%
  arrange(Coordination_mechanism_year) %>% 
  mutate(cumulative_sum = cumsum(n)) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = Coordination_mechanism_year, y = cumulative_sum, fill = forcats::fct_rev(factor(Coordination_mechanism))) +
  geom_area() +
  xlab('Year') +
  ylab('Cumulative number of jurisdictions') +
  scale_fill_manual(values = c('aquamarine3', 'aquamarine4')) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.2,0.8))

ggsave('outputs/maps-time-series/time-series/coordination-mechanism_time-series.png', width = 4, height = 3)

