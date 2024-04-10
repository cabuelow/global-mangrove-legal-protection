# make a database of all national covariates for mangrove analysis

library(tidyverse)
library(sf)
sf_use_s2(FALSE) # turn off s2 so can calculate geodesic areas in WGS84

# read in country codes and European territories that haven't been individually assessed
codes <- read.csv('data/country-codes.csv') # country codes
euro_terr <- read.csv('data/european-territories.csv') %>% 
  filter(!Jurisdiction %in% codes$Jurisdiction) # filter only for territories that have not been individually assessed

# make a unique jurisdiction code for all land-eez polygons that overlap with GMW 2020 extent with the following rules:
# if the ISO_TER1 code is 'NA', make it ISO_SOV1 (sovereign nation code)
# if there is an overlapping claim, include ISO_SOV2 in the ID

World <- st_read('data/EEZ_Boundaries/EEZ_Land_v3_202030_erase_eez.shp') %>%
  mutate(TERRITORY1 = ifelse(TERRITORY1 == 'Curaçao', 'Curacao', TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  mutate(Juris_ISO = paste0(.$Jurisdiction, '_', .$ISO_code))
st_write(World, 'data/EEZ_Boundaries/EEZ_Land_v3_202030_erase_eez_v2.gpkg', append = F, overwrite = T)

#####
### Jurisdiction area
#####

mang_terr <- read.csv('data/covariate-data/gmw_v3_mangrove_area_landeez.csv') %>% 
  mutate(TERRITORY1 = ifelse(TERRITORY1 == 'Curaçao', 'Curacao', TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  mutate(Juris_ISO = paste0(.$Jurisdiction, '_', .$ISO_code))
juris_area <- World %>% 
  filter(Juris_ISO %in% mang_terr$Juris_ISO) %>% 
  mutate(area_ha = st_area(.)/10000) %>% # s2 off so calculating geodesic areas in WGS84
  group_by(Juris_ISO, Jurisdiction, ISO_code) %>% 
  summarise(jurisdiction_area_ha = as.numeric(sum(area_ha))) %>% 
  st_drop_geometry()

# for France, Netherlands and UK, find all of their territories with mangroves that aren't individually assessed and summarise

juris_fra <- juris_area %>% 
  filter(Juris_ISO %in% filter(euro_terr, SOVEREIGN1 == 'France')$Juris_ISO) %>% 
  ungroup() %>% 
  summarise(jurisdiction_area_ha = sum(jurisdiction_area_ha))
juris_nld <- juris_area %>% 
  filter(Juris_ISO %in% filter(euro_terr, SOVEREIGN1 == 'Netherlands')$Juris_ISO) %>% 
  ungroup() %>% 
  summarise(jurisdiction_area_ha = sum(jurisdiction_area_ha))
juris_gbr <- juris_area %>% 
  filter(Juris_ISO %in% filter(euro_terr, SOVEREIGN1 == 'United Kingdom')$Juris_ISO) %>% 
  ungroup() %>% 
  summarise(jurisdiction_area_ha = sum(jurisdiction_area_ha))
juris_area <- juris_area %>% rbind(data.frame(Juris_ISO = c('France_FRA', 'Netherlands_NLD', 'United Kingdom_GBR'), 
                                Jurisdiction = c('France', 'Netherlands', 'United Kingdom'),
                                ISO_code = c('FRA', 'NLD', 'GBR'), jurisdiction_area_ha = unname(unlist(c(juris_fra, juris_nld, juris_gbr)))))

#####
### Mangrove area
#####

gmw <- read.csv('data/covariate-data/gmw_v3_mangrove_area_landeez.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  mutate(Juris_ISO = paste0(.$Jurisdiction, '_', .$ISO_code)) %>% 
  group_by(Juris_ISO, Jurisdiction, ISO_code) %>% 
  summarise(gmw_area_2020_ha = sum(gmw_area_2020_ha)) 

gmw_wdpa <- read.csv('data/covariate-data/gmw_v3_mangrove_area_wdpa_landeez.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  mutate(Juris_ISO = paste0(.$Jurisdiction, '_', .$ISO_code)) %>% 
  group_by(Juris_ISO, Jurisdiction, ISO_code) %>% 
  summarise(gmw_area_2020_wdpa_ha = sum(gmw_area_2020_wdpa_ha))

# for France, Netherlands and UK, find all of their territories with mangroves that aren't individually assessed and summarise

gmw_fra <- read.csv('data/covariate-data/gmw_v3_mangrove_area_landeez.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'FRA' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(gmw_area_2020_ha = sum(gmw_area_2020_ha)) 
gmw_nld <- read.csv('data/covariate-data/gmw_v3_mangrove_area_landeez.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'NLD' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(gmw_area_2020_ha = sum(gmw_area_2020_ha)) 
gmw_gbr <- read.csv('data/covariate-data/gmw_v3_mangrove_area_landeez.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'GBR' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(gmw_area_2020_ha = sum(gmw_area_2020_ha)) 
gmw <- gmw %>% rbind(data.frame(Juris_ISO = c('France_FRA', 'Netherlands_NLD', 'United Kingdom_GBR'), 
                                Jurisdiction = c('France', 'Netherlands', 'United Kingdom'),
                                ISO_code = c('FRA', 'NLD', 'GBR'), gmw_area_2020_ha = unname(unlist(c(gmw_fra, gmw_nld, gmw_gbr)))))


gmw_fra <- read.csv('data/covariate-data/gmw_v3_mangrove_area_wdpa_landeez.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'FRA' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(gmw_area_2020_wdpa_ha = sum(gmw_area_2020_wdpa_ha)) 
gmw_nld <- read.csv('data/covariate-data/gmw_v3_mangrove_area_wdpa_landeez.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'NLD' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(gmw_area_2020_wdpa_ha = sum(gmw_area_2020_wdpa_ha)) 
gmw_gbr <- read.csv('data/covariate-data/gmw_v3_mangrove_area_wdpa_landeez.csv')%>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'GBR' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(gmw_area_2020_wdpa_ha = sum(gmw_area_2020_wdpa_ha)) 
gmw_wdpa <- gmw_wdpa %>% rbind(data.frame(Juris_ISO = c('France_FRA', 'Netherlands_NLD', 'United Kingdom_GBR'), 
                                Jurisdiction = c('France', 'Netherlands', 'United Kingdom'),
                                ISO_code = c('FRA', 'NLD', 'GBR'), gmw_area_2020_wdpa_ha = unname(unlist(c(gmw_fra, gmw_nld, gmw_gbr)))))

#####
### Shoreline length (** Note this is high resolution (30m) shoreline length with very small islands)
#####

shore <- read.csv('data/covariate-data/shoreline-length-eez-gmwv3_crop.csv') %>% # this is shoreline cropped to max and min latitude of mapped mangrove extent
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  mutate(Juris_ISO = paste0(.$Jurisdiction, '_', .$ISO_code)) %>% 
  filter(Juris_ISO %in% gmw$Juris_ISO) %>% 
  group_by(TERRITORY1) %>% 
  group_by(Juris_ISO, Jurisdiction, ISO_code) %>% 
  summarise(Shoreline_length_m = sum(Shoreline_length_m)) %>% 
  filter(!ISO_code %in% c('FRA', 'GBR', 'NLD'))

shore_mang <- read.csv('data/covariate-data/gmw_v3_2020_vec_landeez_shoreline_v3.csv') %>% # this is shoreline length that has mangroves
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  mutate(Juris_ISO = paste0(.$Jurisdiction, '_', .$ISO_code)) %>% 
  group_by(Juris_ISO, Jurisdiction, ISO_code) %>% 
  summarise(Mangrove_shoreline_length_m = sum(Mangrove_shoreline_length_m))

# for France, Netherlands and UK, find all of their territories with mangroves and summarise

fra <- read.csv('data/covariate-data/shoreline-length-eez-gmwv3_crop.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'FRA' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(Shoreline_length_m = sum(Shoreline_length_m)) 
nld <- read.csv('data/covariate-data/shoreline-length-eez-gmwv3_crop.csv')  %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'NLD' & !Jurisdiction %in% codes$Jurisdiction) %>%  
  summarise(Shoreline_length_m = sum(Shoreline_length_m)) 
gbr <- read.csv('data/covariate-data/shoreline-length-eez-gmwv3_crop.csv')  %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'GBR' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(Shoreline_length_m = sum(Shoreline_length_m)) 
shore <- shore %>% rbind(data.frame(Juris_ISO = c('France_FRA', 'Netherlands_NLD', 'United Kingdom_GBR'), 
                                Jurisdiction = c('France', 'Netherlands', 'United Kingdom'),
                                ISO_code = c('FRA', 'NLD', 'GBR'), Shoreline_length_m = unname(unlist(c(fra, nld, gbr)))))

fra <- read.csv('data/covariate-data/gmw_v3_2020_vec_landeez_shoreline_v3.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'FRA' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(Mangrove_shoreline_length_m = sum(Mangrove_shoreline_length_m)) 
nld <- read.csv('data/covariate-data/gmw_v3_2020_vec_landeez_shoreline_v3.csv')  %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'NLD' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(Mangrove_shoreline_length_m = sum(Mangrove_shoreline_length_m))
gbr <- read.csv('data/covariate-data/gmw_v3_2020_vec_landeez_shoreline_v3.csv')  %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'GBR' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(Mangrove_shoreline_length_m = sum(Mangrove_shoreline_length_m))
shore_mang <- shore_mang %>% rbind(data.frame(Juris_ISO = c('France_FRA', 'Netherlands_NLD', 'United Kingdom_GBR'), 
                                    Jurisdiction = c('France', 'Netherlands', 'United Kingdom'),
                                    ISO_code = c('FRA', 'NLD', 'GBR'), Mangrove_shoreline_length_m = unname(unlist(c(fra, nld, gbr)))))

#####
### Mangrove above ground biomass (Mg_ha) converted to above ground carbon storage (MgC_ha)
#####
# assume a stoichiometric conversion factor (agb to carbon) of 0.45176 (IPCC. 2013 Supplement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories: Wetlands. (2014))
# convert to MtCO2e using a conversion factor of 3.67 (Howard et al. 2014) - GMW website

agb_carbon <- read.csv('data/covariate-data/national-mangrove-above-ground-biomass_mean_simard.csv') %>% 
  mutate(TERRITORY1 = ifelse(ISO_TER1 == "CUW", 'Curacao', TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  mutate(Juris_ISO = paste0(.$Jurisdiction, '_', .$ISO_code)) %>% 
  group_by(Juris_ISO, Jurisdiction, ISO_code) %>% 
  summarise(mean_agb_Mg_ha = mean(mean_agb_Mg_ha))
meanAGB <- mean(agb_carbon$mean_agb_Mg_ha, na.rm = T)

fra <- read.csv('data/covariate-data/national-mangrove-above-ground-biomass_mean_simard.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'FRA' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(mean_agb_Mg_ha = mean(mean_agb_Mg_ha)) 
nld <- read.csv('data/covariate-data/national-mangrove-above-ground-biomass_mean_simard.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'NLD' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(mean_agb_Mg_ha = mean(mean_agb_Mg_ha)) 
gbr <- read.csv('data/covariate-data/national-mangrove-above-ground-biomass_mean_simard.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'GBR' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(mean_agb_Mg_ha = mean(mean_agb_Mg_ha))
agb_carbon<- agb_carbon %>% rbind(data.frame(Juris_ISO = c('France_FRA', 'Netherlands_NLD', 'United Kingdom_GBR'), 
                                              Jurisdiction = c('France', 'Netherlands', 'United Kingdom'),
                                              ISO_code = c('FRA', 'NLD', 'GBR'), 
                                              mean_agb_Mg_ha = unname(unlist(c(fra$mean_agb_Mg_ha, nld$mean_agb_Mg_ha, gbr$mean_agb_Mg_ha)))))

#####
# missing jurisdictions
#####

miss <- filter(gmw, !Juris_ISO %in% agb_carbon$Juris_ISO)
agb_carbon <- agb_carbon %>% rbind(data.frame(select(miss, Juris_ISO:ISO_code), mean_agb_Mg_ha = NA)) %>% 
  mutate(mean_agb_Mg_ha  = ifelse(is.na(mean_agb_Mg_ha ), meanAGB, mean_agb_Mg_ha)) %>% 
  mutate(mean_abovegroundcarbon_MtCO2e_ha = (((mean_agb_Mg_ha*0.45176)*1.102)*3.67)/1000000) # convert to Megagrams to tonnes (*1.102) and tonnes to megatonnes (/1000000)
# document gap-filling from partially and fully assessed countries
gap <- data.frame(filter(codes, Juris_ISO %in% miss$Juris_ISO), gap = 'agb_carbon')

#####
### Mangrove soil carbon up to 1m depth (MgC_ha)
#####
# convert to MtCO2e using a conversion factor of 3.67 (Howard et al. 2014) GMW website

soil_carbon <- read.csv('data/covariate-data/national-mangrove-soil-organic-carbon-1m.csv') %>% 
  mutate(TERRITORY1 = ifelse(ISO_TER1 == "CUW", 'Curacao', TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  mutate(Juris_ISO = paste0(.$Jurisdiction, '_', .$ISO_code)) %>% 
  group_by(Juris_ISO, Jurisdiction, ISO_code) %>% 
  summarise(mean_SOC_MgC_ha = mean(mean_SOC_MgC_ha))
meanSOC <- mean(filter(soil_carbon, mean_SOC_MgC_ha != 0)$mean_SOC_MgC_ha, na.rm = T)
# document gap-filling from partially and fully assessed countries
gap <- gap %>% rbind(data.frame(filter(codes, Juris_ISO %in% filter(soil_carbon, mean_SOC_MgC_ha==0|is.na(mean_SOC_MgC_ha))$Juris_ISO), gap = 'soil_carbon'))
soil_carbon <- soil_carbon %>% 
  mutate(mean_SOC_MgC_ha = ifelse(mean_SOC_MgC_ha==0|is.na(mean_SOC_MgC_ha), meanSOC, mean_SOC_MgC_ha))

fra <- read.csv('data/covariate-data/national-mangrove-soil-organic-carbon-1m.csv')  %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'FRA' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(mean_SOC_MgC_ha = mean(mean_SOC_MgC_ha))
nld <- read.csv('data/covariate-data/national-mangrove-soil-organic-carbon-1m.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'NLD' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(mean_SOC_MgC_ha = mean(mean_SOC_MgC_ha))
gbr <- read.csv('data/covariate-data/national-mangrove-soil-organic-carbon-1m.csv')  %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'GBR' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(mean_SOC_MgC_ha = mean(mean_SOC_MgC_ha))
  
soil_carbon <- soil_carbon %>% rbind(data.frame(Juris_ISO = c('France_FRA', 'Netherlands_NLD', 'United Kingdom_GBR'), 
                                             Jurisdiction = c('France', 'Netherlands', 'United Kingdom'),
                                             ISO_code = c('FRA', 'NLD', 'GBR'), 
                                             mean_SOC_MgC_ha = unname(unlist(c(fra$mean_SOC_MgC_ha, nld$mean_SOC_MgC_ha, gbr$mean_SOC_MgC_ha)))))
# missing jurisdictions  
miss <- filter(gmw, !Juris_ISO %in% soil_carbon$Juris_ISO)
soil_carbon <- soil_carbon %>% rbind(data.frame(select(miss, Juris_ISO:ISO_code), mean_SOC_MgC_ha = NA)) %>% 
  mutate(mean_SOC_MgC_ha = ifelse(is.na(mean_SOC_MgC_ha), meanSOC, mean_SOC_MgC_ha)) %>% 
  mutate(mean_soilcarbon_MtCO2e_ha = ((mean_SOC_MgC_ha*1.102)*3.67)/1000000) # convert to Megagrams to tonnes (*1.102) and tonnes to megatonnes (/1000000)
# document gap-filling from partially and fully assessed countries
if(nrow(filter(codes, Juris_ISO %in% miss$Juris_ISO))>0){gap <- gap %>% rbind(data.frame(filter(codes, Juris_ISO %in% miss$Juris_ISO), gap = 'soil_carbon'))}

#####
### Flood protection benefits
#####
# AEB = Annual expected benefits of mangroves
# A_km2 = Area flooded, P = Population flooded (number of people), STOCK = residential and industrial property flooded (US$ value) (Industrial + Residential)
# RC = regular climate, TC = Tropical cyclone - we will consider both combined as a measure of maximum possible AEB

coast_prot <- read.csv('data/covariate-data/national-flood-protection-benefits.csv') %>% 
  filter(variable %in% c('Ranking_AEB_P_paises(RC+TC)', 'Ranking_AEB_TOTAL_STOCK_paises(RC+TC)')) %>% 
  mutate(variable = recode(variable, 'Ranking_AEB_P_paises(RC+TC)' = 'protection_num_people',
                           'Ranking_AEB_TOTAL_STOCK_paises(RC+TC)' = 'protection_property_USD_billion')) %>% 
  pivot_wider(names_from = 'variable', values_from = 'value') %>% 
  mutate(protection_property_USD_billion = protection_property_USD_billion/1000) %>% # convert to billions
  right_join(codes, by = c('country' = 'FLD_PROTECTION')) %>% 
  mutate(TERRITORY1 = ifelse(ISO_TER1 == "CUW", 'Curacao', TERRITORY1)) %>% 
  mutate(ISO_code = ISO_TER1) %>% 
  mutate(Jurisdiction = TERRITORY1) %>% 
  mutate(Juris_ISO = paste0(.$Jurisdiction, '_', .$ISO_code)) %>% 
  group_by(Juris_ISO, Jurisdiction, ISO_code) %>% 
  select(Juris_ISO, Jurisdiction, ISO_code, protection_num_people, protection_property_USD_billion) %>% 
  filter(!ISO_code %in% c('FRA', 'GBR', 'NLD'))
medianCP_ppl <- median(coast_prot$protection_num_people, na.rm = T)
medianCP_prop <- median(coast_prot$protection_property_USD_billion, na.rm = T)
# document gap-filling from partially and fully assessed countries
gap <- gap %>% rbind(data.frame(filter(codes, Juris_ISO %in% filter(coast_prot, is.na(protection_num_people))$Juris_ISO), gap = 'coast_protection'))
coast_prot <- coast_prot %>% 
  mutate(protection_num_people = ifelse(is.na(protection_num_people), medianCP_ppl, protection_num_people),
         protection_property_USD_billion = ifelse(is.na(protection_property_USD_billion), medianCP_prop, protection_property_USD_billion))

fra <- read.csv('data/covariate-data/gmw_v3_mangrove_area_landeez.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'FRA' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  inner_join(coast_prot, by = 'Jurisdiction') %>% 
  summarise(protection_num_people = sum(protection_num_people, na.rm = T),
            protection_property_USD_billion = sum(protection_property_USD_billion, na.rm = T))
nld <- read.csv('data/covariate-data/gmw_v3_mangrove_area_landeez.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'NLD' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  inner_join(coast_prot, by = 'Jurisdiction') %>% 
  summarise(protection_num_people = sum(protection_num_people, na.rm = T),
            protection_property_USD_billion = sum(protection_property_USD_billion, na.rm = T))
gbr <- read.csv('data/covariate-data/gmw_v3_mangrove_area_landeez.csv') %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'GBR' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  inner_join(coast_prot, by = 'Jurisdiction') %>% 
  summarise(protection_num_people = sum(protection_num_people, na.rm = T),
            protection_property_USD_billion = sum(protection_property_USD_billion, na.rm = T))
coast_prot <- coast_prot %>% rbind(data.frame(Juris_ISO = c('France_FRA', 'Netherlands_NLD', 'United Kingdom_GBR'), 
                                                Jurisdiction = c('France', 'Netherlands', 'United Kingdom'),
                                                ISO_code = c('FRA', 'NLD', 'GBR'), 
                                                protection_num_people  = unname(unlist(c(fra$protection_num_people, nld$protection_num_people, gbr$protection_num_people))),
                                                protection_property_USD_billion  = unname(unlist(c(fra$protection_property_USD_billion, nld$protection_property_USD_billion, gbr$protection_property_USD_billion)))))
# missing jurisdictions  
miss <- filter(gmw, !Juris_ISO %in% coast_prot$Juris_ISO)
coast_prot <- coast_prot %>% rbind(data.frame(select(miss, Juris_ISO:ISO_code), protection_num_people = NA, protection_property_USD_billion = NA)) %>% 
  mutate(protection_num_people = ifelse(is.na(protection_num_people), medianCP_ppl, protection_num_people),
         protection_property_USD_billion = ifelse(is.na(protection_property_USD_billion), medianCP_prop, protection_property_USD_billion))

### Ramsar

rams <- read.csv('data/covariate-data/ramsar-eez-intertidal-forested-wetlands.csv') %>% 
  mutate(TERRITORY1 = ifelse(ISO_TER1 == "CUW", 'Curacao', TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  mutate(Juris_ISO = paste0(.$Jurisdiction, '_', .$ISO_code)) %>% 
  group_by(Juris_ISO, Jurisdiction, ISO_code) %>% 
  summarise(ramsar_num_sites = n()) %>% 
  filter(!ISO_code %in% c('FRA', 'GBR', 'NLD'))

fra <- read.csv('data/covariate-data/ramsar-eez-intertidal-forested-wetlands.csv')%>% 
  filter(!ISO_TER1 %in% c('FRA', 'GBR', 'NLD')) %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'FRA' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(ramsar_num_sites = n())
nld <- read.csv('data/covariate-data/ramsar-eez-intertidal-forested-wetlands.csv')%>% 
  filter(!ISO_TER1 %in% c('FRA', 'GBR', 'NLD')) %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'NLD' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(ramsar_num_sites = n())
gbr <- read.csv('data/covariate-data/ramsar-eez-intertidal-forested-wetlands.csv')%>% 
  filter(!ISO_TER1 %in% c('FRA', 'GBR', 'NLD')) %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(!is.na(ISO_SOV2), paste0(.$ISO_code, '_', .$ISO_SOV2), ISO_code)) %>% 
  mutate(Jurisdiction = ifelse(!is.na(SOVEREIGN2), paste0(.$Jurisdiction, '_', .$SOVEREIGN2), Jurisdiction)) %>% 
  filter(ISO_SOV1 == 'GBR' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(ramsar_num_sites = n())
rams <- rams %>% rbind(data.frame(Juris_ISO = c('France_FRA', 'Netherlands_NLD', 'United Kingdom_GBR'), 
                                              Jurisdiction = c('France', 'Netherlands', 'United Kingdom'),
                                              ISO_code = c('FRA', 'NLD', 'GBR'), ramsar_num_sites = unname(unlist(c(fra, nld, gbr)))))

#####
### Miscellaneous National indicators
#####

nat_dat <- read.csv('data/covariate-data/national-indicators.csv') %>% 
  mutate(TERRITORY1 = ifelse(ISO_TER1 == "CUW", 'Curacao', TERRITORY1)) %>% 
  mutate(ISO_code = ifelse(is.na(ISO_TER1), ISO_SOV1, ISO_TER1)) %>% 
  mutate(Jurisdiction = ifelse(is.na(TERRITORY1), SOVEREIGN1, TERRITORY1)) %>% 
  mutate(Juris_ISO = paste0(.$Jurisdiction, '_', .$ISO_code))
# document gap-filling from partially and fully assessed countries
gap <- gap %>% rbind(data.frame(filter(codes, Juris_ISO %in% filter(nat_dat, is.na(fishing_pressure_median))$Juris_ISO), gap = 'fisheries'))
# document gap-filling from partially and fully assessed countries
gap <- gap %>% rbind(data.frame(filter(codes, Juris_ISO %in% filter(nat_dat, is.na(mangrove_attractions))$Juris_ISO), gap = 'mang_attractions'))
medianFish <- median(nat_dat$fishing_pressure_median, na.rm=T)
minFish <- min(nat_dat$fishing_pressure_median, na.rm = T)
maxFish <- max(nat_dat$fishing_pressure_median, na.rm = T)
medianAttract <- median(nat_dat$mangrove_attractions, na.rm=T)

nat_dat <- nat_dat %>% 
  mutate(fishing_pressure_median_medgap = ifelse(is.na(fishing_pressure_median)&!ISO_TER1%in%c('GBR', 'FRA', 'NLD'), medianFish, fishing_pressure_median),
         fishing_pressure_median_mingap = ifelse(is.na(fishing_pressure_median)&!ISO_TER1%in%c('GBR', 'FRA', 'NLD'), minFish, fishing_pressure_median),
         fishing_pressure_median_maxgap = ifelse(is.na(fishing_pressure_median)&!ISO_TER1%in%c('GBR', 'FRA', 'NLD'), maxFish, fishing_pressure_median),
         mangrove_attractions = ifelse(is.na(mangrove_attractions)&!ISO_TER1%in%c('GBR', 'FRA', 'NLD'), medianAttract, mangrove_attractions))
# here calculate median fishing pressure or sum of mangrove attractions in territories of european sovereign nations
fra <- nat_dat %>% 
  filter(ISO_SOV1 == 'FRA') %>% 
  summarise(fishing_pressure_median_medgap = median(fishing_pressure_median_medgap, na.rm = T),
            fishing_pressure_median_mingap = median(fishing_pressure_median_mingap, na.rm = T),
            fishing_pressure_median_maxgap = median(fishing_pressure_median_maxgap, na.rm = T))
fra2 <- nat_dat %>% 
  filter(ISO_SOV1 == 'FRA' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(mangrove_attractions = sum(mangrove_attractions, na.rm = T))
nld <- nat_dat %>% 
  filter(ISO_SOV1 == 'NLD') %>% 
  summarise(fishing_pressure_median_medgap = median(fishing_pressure_median_medgap, na.rm = T),
            fishing_pressure_median_mingap = median(fishing_pressure_median_mingap, na.rm = T),
            fishing_pressure_median_maxgap = median(fishing_pressure_median_maxgap, na.rm = T))
nld2 <- nat_dat %>% 
  filter(ISO_SOV1 == 'NLD' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(mangrove_attractions = sum(mangrove_attractions, na.rm = T))
gbr <- nat_dat %>% 
  filter(ISO_SOV1 == 'GBR') %>% 
  summarise(fishing_pressure_median_medgap = median(fishing_pressure_median_medgap, na.rm = T),
            fishing_pressure_median_mingap = median(fishing_pressure_median_mingap, na.rm = T),
            fishing_pressure_median_maxgap = median(fishing_pressure_median_maxgap, na.rm = T))
gbr2 <- nat_dat %>% 
  filter(ISO_SOV1 == 'GBR' & !Jurisdiction %in% codes$Jurisdiction) %>% 
  summarise(mangrove_attractions = sum(mangrove_attractions, na.rm = T))
nat_dat <- nat_dat %>% 
  mutate(fishing_pressure_median_medgap = case_when(ISO_TER1 == 'FRA' ~ fra$fishing_pressure_median_medgap,
                                             ISO_TER1 == 'NLD' ~ nld$fishing_pressure_median_medgap,
                                             ISO_TER1 == 'GBR' ~ gbr$fishing_pressure_median_medgap,
                                             .default = fishing_pressure_median_medgap),
         fishing_pressure_median_mingap = case_when(ISO_TER1 == 'FRA' ~ fra$fishing_pressure_median_mingap,
                                             ISO_TER1 == 'NLD' ~ nld$fishing_pressure_median_mingap,
                                             ISO_TER1 == 'GBR' ~ gbr$fishing_pressure_median_mingap,
                                             .default = fishing_pressure_median_mingap),
         fishing_pressure_median_maxgap = case_when(ISO_TER1 == 'FRA' ~ fra$fishing_pressure_median_maxgap,
                                             ISO_TER1 == 'NLD' ~ nld$fishing_pressure_median_maxgap,
                                             ISO_TER1 == 'GBR' ~ gbr$fishing_pressure_median_maxgap,
                                             .default = fishing_pressure_median_maxgap),
         mangrove_attractions = case_when(ISO_TER1 == 'FRA' ~ fra2$mangrove_attractions,
                                             ISO_TER1 == 'NLD' ~ nld2$mangrove_attractions,
                                             ISO_TER1 == 'GBR' ~ gbr2$mangrove_attractions,
                                             .default = mangrove_attractions))

##### 
### make master database
#####

master <- gmw %>% 
  left_join(gmw_wdpa) %>% 
  left_join(juris_area) %>% 
  left_join(shore) %>% 
  left_join(shore_mang) %>% 
  left_join(agb_carbon) %>% 
  left_join(soil_carbon) %>% 
  left_join(coast_prot) %>% 
  left_join(rams) %>% 
  mutate(gmw_area_2020_wdpa_ha = ifelse(is.na(gmw_area_2020_wdpa_ha), 0, gmw_area_2020_wdpa_ha)) %>% 
  mutate(total_abovegroundcarbon_storage_MtCO2e = (mean_abovegroundcarbon_MtCO2e_ha*gmw_area_2020_ha), 
         total_belowgroundcarbon_storage_MtCO2e = (mean_soilcarbon_MtCO2e_ha*gmw_area_2020_ha)) %>% 
  mutate(total_carbon_storage_MtC02e = total_abovegroundcarbon_storage_MtCO2e + total_belowgroundcarbon_storage_MtCO2e) %>% 
  mutate(ramsar_num_sites = ifelse(is.na(ramsar_num_sites), 0, ramsar_num_sites)) %>% 
  left_join(nat_dat) %>% 
  mutate(fisher_days_yr = fishing_pressure_median_medgap * (gmw_area_2020_ha/100)) # convert median number of fisher days/km2 per year to total per year
  
write.csv(master, 'data/national-covariate-master.csv', row.names = F)
write.csv(gap, 'data/covariate-data/national-master_missing-values.csv', row.names = F)

gapsum <- gap %>% 
  mutate(num = 1) %>% 
  group_by(gap) %>% 
  summarise(number_jurisdictions = sum(num),
            percent_total = sum(num)/nrow(codes)*100) %>% 
  mutate(Assessment = 'All')

gapsum2 <- gap %>% 
  filter(Assessment == 'Full') %>% 
  mutate(num = 1) %>% 
  group_by(gap) %>% 
  summarise(number_jurisdictions = sum(num),
            percent_total = sum(num)/nrow(filter(codes, Assessment == 'Full'))*100) %>% 
  mutate(Assessment = 'Full')

finalgap <- rbind(gapsum, gapsum2)
write.csv(finalgap, 'data/covariate-data/national-master_missing-values_summary.csv', row.names = F)
