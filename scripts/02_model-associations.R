# model associations between socio-ecological factors and policy/law adoption
# note to pay attention to ## code depending on whether you wish to include individual ecosystem service factors, or a cumulative ecosystem service index

library(tidyverse)
library(sf)
library(tmap)
library(see)
library(Hmsc)
library(bayestestR)
library(factoextra)
library(FactoMineR)
library(GGally)
library(patchwork)
library(scales)
source('scripts/functions/calculate-residuals.R')
set.seed(123)
sf_use_s2(FALSE)
crsrobin <- "+proj=robin +lon_0=145 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # robinson crs centered on oceania

# read in national codes, covariates
codes <- read.csv('data/country-codes.csv') # country codes
nat_covar <- read.csv('data/national-covariate-master.csv') # national socio-ecological covariate data

# read in national spatial data and project
World <- st_read('data/EEZ_Boundaries/EEZ_Land_v3_202030_erase_eez_v2.gpkg') %>% 
  st_simplify(preserveTopology = T, dTolerance = 0.01) %>% 
  st_break_antimeridian(lon_0 = 145) %>%
  st_transform(crs = crsrobin) %>% 
  st_make_valid() %>% 
  st_crop(World,  xmin = -17817528, ymin = -8000000, xmax = 16923579, ymax = 8619631)
World <- World[-263,] # get rid of duplicate NZ from offsetting projection

# read in policy data and wrangle for analysis
dat <- read.csv('data/law-policy-database.csv') %>% 
  left_join(codes, 'Country') %>% 
  left_join(select(nat_covar, Country, WB_REGION, gmw_area_2020_ha:total_carbon_storage_MtC02e, GDP_year:fisher_days_yr), by = 'Country') %>%
  filter(Type_assessment == 'Full') %>% # choose only jurisdictions that were fully assessed for laws/policies
  mutate(extent = gmw_area_2020_ha*(Shoreline_length_m/jurisdiction_area_ha),# standardise mangrove area
         people_protected = protection_num_people/gmw_area_2020_ha, # people protected per hectare of mangroves
         property_protected = protection_property_USD_billion/gmw_area_2020_ha, # property protected per hectare of mangroves
         Carbon = mean_abovegroundcarbon_MtCO2e_ha + mean_soilcarbon_MtCO2e_ha, # total above and below ground carbon
         Fishing_med = fishing_pressure_median_medgap, # this is per fisher days/km2
         Fishing_min = fishing_pressure_median_mingap, # this is per fisher days/km2
         Fishing_max = fishing_pressure_median_maxgap, # this is per fisher days/km2
         Government_type = case_when(Federal_status == 'Y' ~ 'Federal',
                                     Federal_status == 'N' ~ 'Unitary independent',
                                     Territory_status == 3 ~ 'Not fully independent',
                                     .default = NA)) %>% # combine federal status and territory into one variable
  mutate(across(c(people_protected, property_protected, Carbon, Fishing_med, Fishing_min, Fishing_max, extent, GDP_pc), ~ logtrans(.))) %>% # log transform skewed variables
  mutate(across(c(people_protected, property_protected, Carbon, Fishing_med, Fishing_min, Fishing_max, extent, GDP_pc), scale, .names = '{.col}_norm')) %>%  # mean center and scale continuous variables
  mutate_at(vars(Mangrove_policy, Clearing_restrictions, Community_management, Environmental_impact_assessment), ~ifelse(. == 'N', 0, 1)) %>% 
  mutate(Clearing_restrictions = case_when(Clearing_restrictions_level == '2' ~ 2,
                                           Clearing_restrictions_level %in% c('3A', '3B') ~ 3,
                                           .default = Clearing_restrictions),
         Coastal_zone_planning = case_when(is.na(Coastal_zone_planning) ~ 0, # coastal zone planning only present if refers to mangroves explicitly
                                           Coastal_zone_planning == 'N' ~ 0,
                                           Coastal_zone_planning == 'Y' ~ 1)) %>%
  mutate(Coordination_mechanism = ifelse(Coordination_mechanism == 2, 1, 0), # only present if its a 2
         Clearing_restrictions = ifelse(Clearing_restrictions == 3, 1, 0)) %>% # only present if its a 3
  mutate(WB_REGION = ifelse(WB_REGION == 'Europe & Central Asia', 'North America', WB_REGION)) %>%  # grouping Europe (really France) and North America together
  mutate(WB_REGION = ifelse(WB_REGION == 'North America', 'North America & Europe', WB_REGION))


#####
# reduce coastal protection and world governance indicators into single covariates
#####

# coastal protection
coastprot <- dat %>% select(people_protected_norm, property_protected_norm)
ggcorr(coastprot, c('pairwise', 'pearson'))
coastprot_pca <- PCA(coastprot, graph = FALSE) # pca
fviz_screeplot(coastprot_pca , addlabels = TRUE) # visualize eigenvalues/variance

# governance
governance <- dat %>% select(WGI_control_corruption, WGI_rule_of_law, WGI_governance_effectiveness)
ggcorr(governance, c('pairwise', 'pearson'))
govpca <- PCA(governance, graph = FALSE) # pca
fviz_screeplot(govpca, addlabels = TRUE) # visualize eigenvalues/variances

# add new covariates to dataframe

dat$Protection_norm = get_pca_ind(coastprot_pca)$coord[,1]
dat$Governance_norm = get_pca_ind(govpca)$coord[,1]

####
# make cumulative ecosystem service index
####

#dat <- dat %>% 
#mutate(across(c(Carbon_norm, Fishing_med_norm, Fishing_min_norm, Fishing_max_norm, Protection_norm), ~rescale(., c(0,1)), .names = '{.col}_scale')) %>% 
 # mutate(ecoserv_min_norm = scale(Carbon_norm_scale + Fishing_min_norm_scale + Protection_norm_scale),
  #       ecoserv_med_norm = scale(Carbon_norm_scale + Fishing_med_norm_scale + Protection_norm_scale),
   #      ecoserv_max_norm = scale(Carbon_norm_scale + Fishing_max_norm_scale + Protection_norm_scale)) %>% 
  #select(-c(Carbon_norm_scale, Fishing_min_norm_scale, Fishing_med_norm_scale, Fishing_max_norm_scale, Protection_norm_scale))
  
# check collinearity of all covariates

#ggcorr(select(dat, extent_norm, GDP_pc_norm, ecoserv_med_norm, Governance_norm), c('pairwise', 'pearson'), label = T)
ggcorr(select(dat, extent_norm, GDP_pc_norm, Carbon_norm, Fishing_med_norm, Fishing_min_norm, Fishing_max_norm, Protection_norm, Governance_norm), c('pairwise', 'pearson'), label = T)

write.csv(dat, 'data/model-data.csv', row.names = F)
dat <- read.csv('data/model-data.csv')

#####
# multivariate regression to relate predictors to presence/absence of law or policy
######
# loop through different fishing gap-fills (median, max or min)

gap <- c('Fishing_med_norm', 'Fishing_min_norm', 'Fishing_max_norm')
#gap <- c('ecoserv_med_norm', 'ecoserv_min_norm', 'ecoserv_max_norm')
for(h in seq_along(gap)){
  
  # choose fishing gapfill
  dat2 <- dat %>% 
    pivot_longer(cols = Fishing_med_norm:Fishing_max_norm) %>% 
    #pivot_longer(cols = ecoserv_min_norm:ecoserv_max_norm) %>% 
    filter(name == gap[h]) %>% 
    mutate(name = 'Fishing') %>% 
    #mutate(name = 'Ecoservice') %>% 
    pivot_wider(names_from = 'name', values_from = 'value')
  
  # set up multivariate response and covariates
  y <- as.matrix(dat2 %>% select(Mangrove_policy:Environmental_impact_assessment))
  x <- dat2 %>% select(extent_norm, GDP_pc_norm, Fishing, Protection_norm, Carbon_norm, Government_type, WB_REGION) %>% 
  #x <- dat2 %>% select(extent_norm, GDP_pc_norm, Ecoservice, Government_type, WB_REGION) %>% 
    mutate(Government_type = as.factor(Government_type), WB_REGION = as.factor(WB_REGION)) %>% 
    mutate(Government_type = relevel(Government_type, ref = 'Unitary independent'))  # make unitary independent the reference level
  
  # set up random effect for residual covariation
  studyDesign <- data.frame(sample = as.factor(1:nrow(dat2)))
  rL <- HmscRandomLevel(units = studyDesign$sample) # random effect at country level to estimate residual covariation
  
  m <- Hmsc(Y = y, XData = x, 
            XFormula = as.formula(paste('~', paste(colnames(x), collapse = '+'))),
            studyDesign = studyDesign, 
            ranLevels = list('sample' = rL),
            distr = 'probit')
  
  # run model
  
  m <- sampleMcmc(m, thin = 10, samples = 10000, transient = 10000, nChains = 4) # bump up to 10000 for final run
  saveRDS(m, paste0('outputs/model-associations/models/mod-multivar-regression_', gap[h], '.rds')) # save model
  #m<- readRDS(paste0('outputs/models/mod-multivar-regression_', gap[h], '.rds'))
  
  # check mcmc convergence
  mpost <- convertToCodaObject(m)
  gel <- data.frame(param = row.names(gelman.diag(mpost$Beta, multivariate = F)$psrf), gelman.diag(mpost$Beta, multivariate = F)$psrf)  # gelman diagnostics
  write.csv(gel, paste0('outputs/model-associations/models/gelman-diagnostics_', gap[h], '.csv'), row.names = F)
  # get trace plots
  pdf(paste0('outputs/model-associations/models/beta-trace-plots_', gap[h], '.pdf'))
  plot(mpost$Beta)
  dev.off()
  # check structural model assumptions
  preds <- computePredictedValues(m)
  dim(preds) # large array storing data matrices of predictions from each posterior sample
  preds.mean <- apply(preds, FUN = mean, MARGIN = c(1,2)) # get mean predicted value across all matrices for each response, then calculate residual values
  png(paste0('outputs/model-associations/models/residuals_', gap[h], '.png'), width = 500, height = 300)
  resid(m$Y, preds.mean, plotds = T, family = 'binomial')
  dev.off()
  evaluateModelFit(hM = m, predY = preds)
  
  # plot the model results
  
  m <- readRDS(paste0('outputs/model-associations/models/mod-multivar-regression_', gap[h], '.rds'))
  mpost <- convertToCodaObject(m)
  
  # partition explained variance among fixed and random effects
  
  laws <- colnames(m$Y)
  VP <- computeVariancePartitioning(m)
  plotVariancePartitioning(m, VP = VP)
  vardf <- data.frame(VP$vals)
  vardf$cat <- c(colnames(m$XData), 'Residual correlations')
  vardf.long <- vardf %>%
    pivot_longer(Mangrove_policy:Environmental_impact_assessment, names_to = 'Law', values_to = 'Variance_prop') %>% 
    mutate(cat = recode(cat, extent_norm = 'Relative extent', WB_REGION = 'Geographic region', Government_type = 'Government type', GDP_pc_norm = 'GDP per capita', Protection_norm = 'Coastal protection', Carbon_norm = 'Carbon stocks', Fishing = 'Fisheries')) %>% 
    #mutate(cat = recode(cat, extent_norm = 'Relative extent', WB_REGION = 'Geographic region', Government_type = 'Government type', GDP_pc_norm = 'GDP per capita')) %>% 
    mutate(cat = factor(cat, levels = c('Residual correlations', 'Relative extent', 'Coastal protection', 'Fisheries', 'Carbon stocks', 'GDP per capita', 'Government type', 'Geographic region')),
   # mutate(cat = factor(cat, levels = c('Residual correlations', 'Relative extent', 'Ecoservice', 'GDP per capita', 'Government type', 'Geographic region')),
           Law = recode(Law, Mangrove_policy = 'Mangrove instrument', Environmental_impact_assessment = 'EIA',
                        Coastal_zone_planning = 'Coastal planning', Community_management = 'Community management',
                        Clearing_restrictions = 'Cutting restrictions', Coordination_mechanism = 'Coordination mechanisms')) %>% 
    mutate(Law = factor(Law, levels = c('Mangrove instrument','Community management', 'Coastal planning','EIA','Coordination mechanisms', 'Cutting restrictions')))
  
  aa <- ggplot(vardf.long) +
    aes(y = Law, x = Variance_prop, fill = cat) +
    geom_bar(stat = 'identity') +
    theme_classic() +
    ylab('') +
    xlab('') +
    scale_fill_okabeito('black_original', reverse = T) +
    ggtitle('A) Proportion variance explained') +
    theme_classic() +
    theme(legend.text = element_text(size = 8),
          legend.title = element_blank(),
          #legend.position = 'none',
          plot.margin = margin(0, 0, 0, 0, "cm"),
          plot.title = element_text(size = 10),
          legend.key.size = unit(0.4, 'cm'))
  aa
  #ggsave(paste0('outputs/model-associations/law-variance-explained_', gap[h], '.png'), width = 5, height = 1.8)
  
  # summarise beta coefficents from posterior distribution, extract mean coefs and credible intervals from each mcmc chain
  
  beta.coefs <- lapply(mpost$Beta, function(x){
    df <- data.frame(cbind(p_direction = sapply(apply(x, 2, p_direction, 'direct'), '[[', 2),
                           mean = apply(x, 2, mean),
                           CI_25 = apply(x, 2, quantile, 0.25),
                           CI_75 = apply(x, 2, quantile, 0.75),
                           predictor = rep(as.character(colnames(m$X)), length(colnames(m$Y))),
                           response = rep(as.character(colnames(m$Y)), each = length(colnames(m$X))))) %>% 
      filter(predictor != '(Intercept)') %>%
      mutate_at(vars(p_direction, mean, CI_25, CI_75), ~as.numeric(.))
    return(df)
  })
  
  beta <- do.call(rbind, beta.coefs) %>%
    group_by(predictor, response) %>%
    summarise(across(c(p_direction, mean, CI_25, CI_75), ~mean(.))) %>%
    mutate(direction = ifelse(mean>0, 'Positive', 'Negative')) %>% 
    mutate(evidence = case_when(direction == 'Positive' & CI_25 < 0 ~ 'No',
                                direction == 'Positive' & CI_75 > 0 ~ 'Yes',
                                direction == 'Negative' & CI_75 > 0 ~ 'No',
                                direction == 'Negative' & CI_25 < 0 ~ 'Yes', .default = NA)) %>% 
    mutate(p_direction_plot = ifelse(mean>0, p_direction, 1-p_direction)) %>% 
    mutate(label = ifelse(evidence == 'Yes', p_direction, NA)) %>% 
    mutate(label = round(label, 2)*100) %>%
    mutate(predictor = recode(predictor,'WB_REGIONLatin America & Caribbean' = 'Latin America & Cari- bbean',
                              'WB_REGIONMiddle East & North Africa' = 'Middle East & North Africa',
                              'WB_REGIONNorth America & Europe' = 'North America & Europe',
                              'WB_REGIONSouth Asia' = 'South Asia',
                              'WB_REGIONSub-Saharan Africa' = 'Sub- Saharan Africa',
                              "Government_typeNot fully independent" = 'Not fully indep- endent',
                              "Government_typeFederal" = 'Federal',
                              #extent_norm = 'Relative extent', WB_REGION = 'Geographic region', Government_type = 'Government type', Ecoservice = 'Eco- service', GDP_pc_norm = 'GDP per capita')) %>% 
                              extent_norm = 'Relative extent', WB_REGION = 'Geographic region', Government_type = 'Government type', GDP_pc_norm = 'GDP per capita', Protection_norm = 'Coastal Prot- ection', Carbon_norm = 'Carbon stocks', Fishing = 'Fisheries')) %>% 
    mutate(response = recode(response, Mangrove_policy = 'Mangrove instrument', Environmental_impact_assessment = 'EIA',
                             Coastal_zone_planning = 'Coastal planning', Community_management = 'Community management',
                             Clearing_restrictions = 'Cutting restrictions', Coordination_mechanism = 'Coordination mechanisms')) %>% 
    mutate(response = factor(response, levels = c('Mangrove instrument','Community management', 'Coastal planning','EIA','Coordination mechanisms', 'Cutting restrictions')))
  
  # save 
  write.csv(data.frame(beta, model = gap[h]), paste0('outputs/model-associations/p_direction_', gap[h], '.csv'), row.names = F)
  
  # plot probability of +ve or -ve effect

  a <- beta %>% 
    #filter(predictor %in% c('Relative extent', 'GDP per capita', 'Eco- service')) %>% 
    #mutate(predictor = factor(predictor, levels = c('Relative extent', 'Eco- service', 'GDP per capita'))) %>% 
    filter(predictor %in% c('Relative extent', 'GDP per capita', 'Fisheries', 'Carbon stocks', 'Coastal Prot- ection')) %>% 
    mutate(predictor = factor(predictor, levels = c('Relative extent', 'Coastal Prot- ection', 'Fisheries', 'Carbon stocks', 'GDP per capita'))) %>% 
    ggplot() +
    geom_tile(aes(x = predictor, y = response, fill = p_direction_plot)) +
    geom_text(aes(x = predictor, y = response, label=label), size = 3) +
    scale_fill_distiller(palette = 'RdBu', 
                         name = 'Probability', 
                         direction = 1,
                         breaks = c(0, 0.25, 0.5, .75, 1),
                         limit = c(0, 1),
                         labels = c("-100", "-75", "50", '75', '100')) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    xlab('') +
    ylab('') +
    ggtitle('B) Probability of association') +
    theme_classic() +
    theme(legend.text = element_text(size = 8),
          legend.title = element_text(size = 9),
          legend.position = 'none',
          plot.margin = margin(0, 0, 0, 0, "cm"),
          plot.title = element_text(size = 10),
          legend.key.size = unit(0.4, 'cm'))
  a
  #ggsave(paste0('outputs/models/law-predictor-associations_', h, '.png'), width = 6.2, height = 2)
  
  b <- beta %>% 
    filter(predictor %in% c('Not fully indep- endent', 'Federal')) %>%
    ggplot() +
    geom_tile(aes(x = predictor, y = response, fill = p_direction_plot)) +
    geom_text(aes(x = predictor, y = response, label=label), size = 3) +
    scale_fill_distiller(palette = 'RdBu', 
                         name = 'Probability', 
                         direction = 1,
                         breaks = c(0, 0.25, 0.5, .75, 1),
                         limit = c(0, 1),
                         labels = c("-100", "-75", "50", '75', '100')) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    xlab('') +
    ylab('') +
    #ggtitle('B) Government type') +
    theme_classic() +
    theme(legend.text = element_text(size = 8),
          legend.title = element_text(size = 9),
          axis.text.y = element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          plot.title = element_text(size = 10),
          legend.key.size = unit(0.4, 'cm'))
  b
  
  c <- dat2 %>% 
    group_by(WB_REGION) %>% 
    summarise_at(vars(Mangrove_policy:Environmental_impact_assessment), ~sum(.)/n()) %>% 
    pivot_longer(cols = -WB_REGION, names_to = 'law', values_to = 'proportion') %>% 
    mutate(WB_REGION = recode(WB_REGION, 'Latin America & Caribbean' = 'Latin America & Cari- bbean','Sub-Saharan Africa' = 'Sub- Saharan Africa')) %>% 
    mutate(law = recode(law, Mangrove_policy = 'Mangrove instrument', Environmental_impact_assessment = 'EIA',
                        Coastal_zone_planning = 'Coastal planning', Community_management = 'Community management',
                        Clearing_restrictions = 'Cutting restrictions', Coordination_mechanism = 'Coordination mechanisms')) %>% 
    mutate(law = factor(law, levels = c('Mangrove instrument','Community management', 'Coastal planning','EIA','Coordination mechanisms', 'Cutting restrictions'))) %>% 
    ggplot() +
    geom_tile(aes(x = WB_REGION, y =law, fill = proportion)) +
    scale_fill_distiller(palette = 'BuGn', 
                         direction = 1,
                         name = 'Proportion') +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    xlab('') +
    ylab('') +
    ggtitle('C) Prevalance in geographic region') +
    theme_classic() +
    theme(legend.text = element_text(size = 8),
          legend.title = element_text(size = 9),
          # axis.text.y = element_blank(),
          #legend.position = 'none',
          plot.margin = margin(0, 0, 0, 0, "cm"),
          plot.title = element_text(size = 10),
          legend.key.size = unit(0.4, 'cm'))
  c
  
  #ggsave(paste0('outputs/models/law-predictor-associations_WB_region_', h, '.png'), width = 6.2, height = 2)
  
#  layout <- '
#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA###########
#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB#
#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC#
#'

   layout <- '
  AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA##
  BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB#
  CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC#
  '
  
  d <- (a + b) + plot_layout(widths = c(1,0.4))
  #d <- (a + b) + plot_layout(widths = c(1,0.65))
  aa/d/c+plot_layout(design = layout)
  ggsave(paste0('outputs/model-associations/law-predictor-associations_probability_', gap[h], '.png'), height = 6, width = 6.5)
  
  # use model residuals to identify countries with missing/unexpected laws/policies
  # extract mean and distribution of prediction residuals using all mcmc samples
  # calculate the probability that the residual is an outlier (less than or greater than XX SDs)
  thresh <- 2 # SD threshold for identifying outliers
  prob_thresh <- 0.5 # threshold for defining probability of being an outlier
  
  # get predictions, without using random effect for resid correlations
  preds <- predict(m, predictEtaMean=TRUE, expected=TRUE)
  preds <- array(as.numeric(unlist(preds)), dim=c(nrow(dat2), 6, 40000))
  resids <- array(NA, dim(preds)) # empty array for storing residuals across all mcmc samples
  resids[] <- apply(preds, 3, `-`, m$Y) # subtract observations from predicted values across all mcmc samples
  
  # within each each mcmc sample determine whether country is an outlier or not
  outliers_missing <- array(NA, dim(resids)) # arrays for storing results
  outliers_unexpected <- array(NA, dim(resids))
  for(i in 1:dim(resids)[3]){
    sample <- resids[,,i] # residuals in a posterior sample
    sd <- apply(sample, sd, MARGIN = 2) # calculate standard deviation across countries
    for(j in 1:length(sd)){
      outliers_missing[,j,i] <- sample[,j] > sd*thresh
      outliers_unexpected[,j,i] <- sample[,j] < -sd*thresh
    }
  }
  # calculate probability of being a missing or unexpected outlier
  prob_missing <- data.frame(apply(outliers_missing, sum, MARGIN = c(1,2))/dim(resids)[3])
  colnames(prob_missing) <- colnames(m$Y)
  prob_unexpected <- data.frame(apply(outliers_unexpected, sum, MARGIN = c(1,2))/dim(resids)[3])
  colnames(prob_unexpected) <- colnames(m$Y)
  
  # identify countries with greater than 50% probability of being an outlier
  prob_missing <- data.frame(select(dat2, Country), prob_missing) %>% 
    mutate_at(vars(Mangrove_policy:Environmental_impact_assessment), funs(miss = factor(ifelse(. >= prob_thresh, 'Missing', NA))))
  prob_unexpected <- data.frame(select(dat2, Country), prob_unexpected) %>% 
    mutate_at(vars(Mangrove_policy:Environmental_impact_assessment), funs(unex = factor(ifelse(. >= prob_thresh, 'Unexpected', NA))))
  
  map.df <- select(prob_missing, Country, Mangrove_policy_miss:Environmental_impact_assessment_miss) %>% 
    left_join(select(prob_unexpected, Country, Mangrove_policy_unex:Environmental_impact_assessment_unex)) %>% 
    mutate(Mangrove_policy = ifelse(is.na(Mangrove_policy_miss), as.character(Mangrove_policy_unex), as.character(Mangrove_policy_miss)),
           Clearing_restrictions = ifelse(is.na(Clearing_restrictions_miss), as.character(Clearing_restrictions_unex), as.character(Clearing_restrictions_miss)),
           Coordination_mechanism = ifelse(is.na(Coordination_mechanism_miss), as.character(Coordination_mechanism_unex), as.character(Coordination_mechanism_miss)),
           Community_management = ifelse(is.na(Community_management_miss), as.character(Community_management_unex), as.character(Community_management_miss)),
           Coastal_zone_planning = ifelse(is.na(Coastal_zone_planning_miss), as.character(Coastal_zone_planning_unex), as.character(Coastal_zone_planning_miss)),
           Environmental_impact_assessment = ifelse(is.na(Environmental_impact_assessment_miss), as.character(Environmental_impact_assessment_unex), as.character(Environmental_impact_assessment_miss))) %>% 
    left_join(codes) %>% 
    pivot_longer(cols= Mangrove_policy:Environmental_impact_assessment, names_to = 'policy', values_to = 'outcome') %>% 
    mutate(label = ifelse(!is.na(outcome), Jurisdiction, NA)) %>% 
    mutate(policy = recode(policy, Mangrove_policy = 'Mangrove instrument', Environmental_impact_assessment = 'EIA',
                           Coastal_zone_planning = 'Coastal planning', Community_management = 'Community management',
                           Clearing_restrictions = 'Cutting restrictions', Coordination_mechanism = 'Coordination mechanisms')) %>% 
    mutate(policy = factor(policy, levels = c('Mangrove instrument','Community management', 'Coastal planning','EIA','Coordination mechanisms', 'Cutting restrictions')))
  
  # map countries with high probability of being an outlier (either missing or unexpected)
  map <- World %>% inner_join(map.df, by = 'Jurisdiction')
  
  m2 <- tm_shape(st_crop(World, st_bbox(map))) +
    tm_polygons('lightgrey', lwd = 0.1) + 
    tm_shape(map) +
    tm_dots('outcome', colorNA = NULL, showNA = F, title = '', size = 0.3, palette = c(Missing = 'lightgoldenrod3', Unexpected = 'darkslategray3'), legend.show = F) +
    tm_text('label', size = 0.2) +
    tm_facets('policy', free.coords = F) +
    tm_add_legend('symbol', col =  c('lightgoldenrod3', 'darkslategray3'), 
                  labels =  c('Missing', 'Unexpected'), border.alpha = 0, size = 0.4) +
    tm_layout(legend.position = c(0,0), panel.label.size = 0.4, legend.text.size = 0.3)
  m2
  tmap_save(m2, paste0('outputs/model-associations/maps/missing-unexpected_all_', gap[h], '.png'), width = 4, height = 2, dpi = 1000)
  
}

# make a table of probabilities of association and save
probs <- do.call(rbind, lapply(list.files('outputs/model-associations/', pattern = 'p_direction', full.names = T), read.csv)) %>% 
  mutate(p_direction = round(p_direction, 2)*100) %>% 
  filter(model %in% gap) %>% 
  pivot_wider(id_cols = c(predictor, response), names_from = 'model', values_from = c('p_direction', 'evidence')) %>% 
  #mutate(Deviation_max_gapfill = p_direction_ecoserv_max_norm - p_direction_ecoserv_med_norm,
   #      Deviation_min_gapfill = p_direction_ecoserv_min_norm - p_direction_ecoserv_med_norm) %>% 
  #filter(evidence_ecoserv_med_norm == 'Yes') %>% 
  #select(-c(p_direction_ecoserv_min_norm, p_direction_ecoserv_max_norm, evidence_ecoserv_med_norm,
   #         evidence_ecoserv_min_norm, evidence_ecoserv_max_norm)) %>% 
  #rename('Probability_assocation' = p_direction_ecoserv_med_norm, 'Predictor_variable' = predictor, 'Response_variable' = response) %>% 
  #filter(Predictor_variable %in% c('GDP per capita', 'Eco- service', 'Relative extent')) %>% 
  #mutate(Predictor_variable = factor(Predictor_variable, levels = c('Relative extent','Eco- service', 'GDP per capita')))
  mutate(Deviation_max_gapfill = p_direction_Fishing_max_norm - p_direction_Fishing_med_norm,
         Deviation_min_gapfill = p_direction_Fishing_min_norm - p_direction_Fishing_med_norm) %>% 
  filter(evidence_Fishing_med_norm == 'Yes') %>% 
  select(-c(p_direction_Fishing_min_norm, p_direction_Fishing_max_norm, evidence_Fishing_med_norm,
            evidence_Fishing_min_norm, evidence_Fishing_max_norm)) %>% 
  rename('Probability_assocation' = p_direction_Fishing_med_norm, 'Predictor_variable' = predictor, 'Response_variable' = response) %>% 
  filter(Predictor_variable %in% c('Carbon stocks', 'GDP per capita', 'Fisheries', 'Coastal Prot- ection', 'Relative extent')) %>% 
  mutate(Predictor_variable = factor(Predictor_variable, levels = c('Relative extent', 'Coastal Prot- ection', 'Fisheries', 'Carbon stocks', 'GDP per capita')))

write.csv(probs, 'outputs/model-associations/probability-association-table_fishing.csv', row.names = F)
#write.csv(probs, 'outputs/model-associations/probability-association-table_ecoservice.csv', row.names = F)



