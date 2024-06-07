# BVI per herd

# Set up ----

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(tibble)


# National average farm ----

# average for crops
## estimates the average agricultural practices
tmp_ref_farm_crops <- BVI_to_RICA_crops %>%
  # add coefficient to weight mean and production methods
  left_join(.,RICA_2020 %>% select(IDENT,EXTR2,AGBIO)) %>%
  # filter crops and farms
  filter(
    # remove partially organic farms
    AGBIO != 5) %>%
  # estimate national averages for yield, BVI / ha and BVI / kg
  group_by(CODE3,AGBIO) %>%
  summarise(
    # !!! check les n : certains sont <3
    n = n(),
    # !!! na.rm = T pour test code mais ptetre à enlever par la suite
    yield = weighted.mean(yield,EXTR2,na.rm = T),
    A.3.1_max = weighted.mean(A.3.1_max,EXTR2,na.rm = T),
    A.4.5_max = weighted.mean(A.4.5_max,EXTR2,na.rm = T),
    A.4.5_min_max = weighted.mean(A.4.5_min_max,EXTR2,na.rm = T),
    A.4.5_org_max = weighted.mean(A.4.5_org_max,EXTR2,na.rm = T),
    A.5.1_max = weighted.mean(A.5.1,EXTR2,na.rm = T),
    BVI_ha = weighted.mean(BVI_ha,EXTR2,na.rm = T),
    BVI_kg = weighted.mean(BVI_kg,EXTR2,na.rm = T)
  ) %>% ungroup()

# quality check : compare calculated average with official national average

# Pseudo-farm purchased feed ----

tmp_pseudofarm_purchased_fr <- feed_purchased %>%
  # add production methods
  left_join(.,RICA_2020 %>% select(IDENT,AGBIO)) %>%
  # filter
  filter(
    # farms kept in BVI
    IDENT %in% BVI_to_RICA_crops$IDENT
    # filter concentrates
    #& feed_type == "feed_concent"
    # remove partially organic farms
    & AGBIO != 5) %>%
  # estimate yield, BVI / ha and BVI / kg
  rowwise() %>%
  mutate(
    yield = mean(tmp_ref_farm_crops$yield[
      # by production methods (i.e. organic or conventional) & by crop
      tmp_ref_farm_crops$AGBIO == AGBIO & tmp_ref_farm_crops$CODE3 %in% unlist(strsplit(CODE3,";"))]),
    SAU_c_ha = DM_kg_p_CODE3 / yield,
    # BVI parameters
    A.3.1_max = mean(tmp_ref_farm_crops$A.3.1_max[
      # by production methods (i.e. organic or conventional) & by crop
      tmp_ref_farm_crops$AGBIO == AGBIO & tmp_ref_farm_crops$CODE3 %in% unlist(strsplit(CODE3,";"))]),
    A.4.5_max = mean(tmp_ref_farm_crops$A.4.5_max[
      # by production methods (i.e. organic or conventional) & by crop
      tmp_ref_farm_crops$AGBIO == AGBIO & tmp_ref_farm_crops$CODE3 %in% unlist(strsplit(CODE3,";"))]),
    A.4.5_min_max = mean(tmp_ref_farm_crops$A.4.5_min_max[
      # by production methods (i.e. organic or conventional) & by crop
      tmp_ref_farm_crops$AGBIO == AGBIO & tmp_ref_farm_crops$CODE3 %in% unlist(strsplit(CODE3,";"))]),
    A.4.5_org_max = mean(tmp_ref_farm_crops$A.4.5_org_max[
      # by production methods (i.e. organic or conventional) & by crop
      tmp_ref_farm_crops$AGBIO == AGBIO & tmp_ref_farm_crops$CODE3 %in% unlist(strsplit(CODE3,";"))]),
    A.5.1_max = mean(tmp_ref_farm_crops$A.5.1_max[
      # by production methods (i.e. organic or conventional) & by crop
      tmp_ref_farm_crops$AGBIO == AGBIO & tmp_ref_farm_crops$CODE3 %in% unlist(strsplit(CODE3,";"))]),
    # BVI
    BVI_ha = mean(tmp_ref_farm_crops$BVI_ha[
      # by production methods (i.e. organic or conventional) & by crop
      tmp_ref_farm_crops$AGBIO == AGBIO & tmp_ref_farm_crops$CODE3 %in% unlist(strsplit(CODE3,";"))]),
    BVI_kg = mean(tmp_ref_farm_crops$BVI_kg[
      # by production methods (i.e. organic or conventional) & by crop
      tmp_ref_farm_crops$AGBIO == AGBIO & tmp_ref_farm_crops$CODE3 %in% unlist(strsplit(CODE3,";"))])
  ) %>% ungroup()

# Soybean meal from Brasil ----

# soybean meal used to feed livestock in France mostly comes from Brasil (Sailley, 2021 ???; Overmars  ???)
# to estimate the biodiversity impact of soybean meal produced in Brasil, we compared the agricultal practices estimated by Lindner (Lindner 2022) for soy and wheat production from Agribalyse data to the agricultural practices we estimated for wheat production from FADN data

tmp_soybean <- tibble(
  "param" = c("A.3.1","A.4.5","A.4.5_min","A.4.5_org","A.5.1"),
  "wheat_Lindner" = c(1.488558,180.7,162.7,18,51580.36245417),
  "soy_Lindner" = c(1.2926064,0,0,0,36888.53241),
  "wheat_RICA" = c(mean(BV_A.3.1$A.3.1[BV_A.3.1$CODE3 == 111]),
                   mean(BV_A.4.5$A.4.5[BV_A.4.5$CODE3 == 111]),
                   mean(BV_A.4.5$A.4.5_min[BV_A.4.5$CODE3 == 111]),
                   mean(BV_A.4.5$A.4.5_org[BV_A.4.5$CODE3 == 111]),
                   mean(BV_A.5.1$A.5.1[BV_A.5.1$CODE3 == 111]))
  ) %>%
  mutate(
  soy_RICA = soy_Lindner * wheat_RICA / wheat_Lindner
  )
# then we applied the BVI model to these estimated parameters
## import constant from Lindner 2019 SM
# import constant from Lindner 2019 SM 
tmp_BV_constant <-  read_excel("data_in/supp_data.xlsx",sheet = "Lindner_2019_BV_LU_function_con")

tmp_BVI_soy <- tmp_soybean %>%
  select(param,soy_RICA) %>%
  rename(x = soy_RICA) %>%
  rowwise() %>%
  mutate(
    # estimate max values for each parameter
    max = case_when(
      param == "A.3.1" ~ quantile(unique(BV_A.3.1$A.3.1),0.95,na.rm = T),
      param == "A.4.5" ~ quantile(unique(BV_A.4.5$A.4.5),0.95,na.rm = T),
      param == "A.4.5_min" ~ quantile(unique(BV_A.4.5$A.4.5_min),0.95,na.rm = T),
      param == "A.4.5_org" ~ quantile(unique(BV_A.4.5$A.4.5_org),0.95,na.rm = T),
      param == "A.5.1" ~ quantile(unique(BV_A.5.1$A.5.1),0.95,na.rm = T)
      ),
    # normalize data
    x_norm = case_when(
      x > max ~ 1,
      .default =  x / max ),
    ## calculate BV
    y = case_when(
      x_norm == 0 ~ 1,
      x_norm == 1 ~ 0,
      .default = tmp_BV_constant$gamma[tmp_BV_constant$metric_number == str_sub(param,start = 1,end = 5)] +
        tmp_BV_constant$epsilon[tmp_BV_constant$metric_number == str_sub(param,start = 1,end = 5)] *
        exp(-(abs(((x_norm^tmp_BV_constant$delta[tmp_BV_constant$metric_number == str_sub(param,start = 1,end = 5)]) -
                     tmp_BV_constant$beta[tmp_BV_constant$metric_number == str_sub(param,start = 1,end = 5)]) ^
                    tmp_BV_constant$alpha[tmp_BV_constant$metric_number == str_sub(param,start = 1,end = 5)]) /
                (2*tmp_BV_constant$sigma[tmp_BV_constant$metric_number == str_sub(param,start = 1,end = 5)] ^
                   tmp_BV_constant$alpha[tmp_BV_constant$metric_number == str_sub(param,start = 1,end = 5)]))) 
      ),
    ## constrain BV
    y = case_when(
      y <0 ~ 0,
      y >1 ~ 1,
      .default = y
    ) ) %>%
  pivot_wider(names_from = param,values_from = !param) %>%
  # ??? how to change name paste order in pivot wider ?
  rename(
    # x
    A.3.1 = x_A.3.1,
    A.4.5 = x_A.4.5,
    A.4.5_min = x_A.4.5_min,
    A.4.5_org = x_A.4.5_org,
    A.5.1 = x_A.5.1,
    # max
    A.3.1_max = max_A.3.1,
    A.4.5_max = max_A.4.5,
    A.4.5_min_max = max_A.4.5_min,
    A.4.5_org_max = max_A.4.5_org,
    A.5.1_max = max_A.5.1,
    # norm
    A.3.1_norm = x_norm_A.3.1,
    A.4.5_norm = x_norm_A.4.5,
    A.4.5_min_norm = x_norm_A.4.5_min,
    A.4.5_org_norm = x_norm_A.4.5_org,
    A.5.1_norm = x_norm_A.5.1,
    # y
    A.3.1_y = y_A.3.1,
    A.4.5_y = y_A.4.5,
    A.4.5_min_y = y_A.4.5_min,
    A.4.5_org_y = y_A.4.5_org,
    A.5.1_y = y_A.5.1) %>%
  mutate(
    ## Land use specific biodiversity value 
    BV_LU = (A.3.1_y + A.4.5_y + A.5.1_y)/3, # average
    # set BV_norm min and max according to Lindner et al. (2019)
    # Normalize BV_LU
    BV_norm = (1/6)+BV_LU*((2/3)-(1/6)),
    ## Local biodiversity value
    BV_loc = case_when(
      ## !!! quality check : why a case_when ??? is there a lot of BV_loc <0 ???
      1.017626088*(1-exp(-4.055847776*BV_norm))<0 ~ 0,
      .default = 1.017626088*(1-exp(-4.055847776*BV_norm))),
    ## !!! quality check : why a case_when ??? is there a lot of deltaQ_loc NAs ???
    BVI_ha = case_when(
      is.finite(1 - BV_loc) == F ~ 0.5,
      .default = 1 - BV_loc),
    # yield estimated from Overmars et al., 2015 as 2.45 t ha-1 for soy cultivated in Latin America
    yield = 2.45*10^3,
    # BVI / kg
    BVI_kg = BVI_ha / yield,
    # add crop code
    CODE3 = 223,
    feed_type = "feed_concent",
    feed_origin = "feed_purchased"
  ) %>%
  select(feed_origin,feed_type,CODE3,
         #A.3.1,A.4.5,A.4.5_min,A.4.5_org,A.5.1,
         A.3.1_max,A.4.5_max,A.4.5_min_max,A.4.5_org_max,A.5.1_max,
         BVI_ha,yield,BVI_kg) %>%
  # remove attributes
  mutate(across(everything(),as.vector))

# replace soybean values in pseudofarm
tmp_pseudofarm_purchased <- tmp_pseudofarm_purchased_fr %>%
  mutate(
    yield = case_when(
      CODE3 == "223" & feed_type == "feed_concent" ~ tmp_BVI_soy$yield[1],
      .default = yield
    ),
    SAU_c_ha = case_when(
      CODE3 == "223" & feed_type == "feed_concent" ~ DM_kg_p_CODE3 / tmp_BVI_soy$yield[1],
      .default = SAU_c_ha
    ),
    # BVI parameters
    A.3.1_max = case_when(
      CODE3 == "223" & feed_type == "feed_concent" ~ tmp_BVI_soy$A.3.1_max[1],
      .default = A.3.1_max
    ),
    A.4.5_max = case_when(
      CODE3 == "223" & feed_type == "feed_concent" ~ tmp_BVI_soy$A.4.5_max[1],
      .default = A.4.5_max
    ),
    A.4.5_min_max = case_when(
      CODE3 == "223" & feed_type == "feed_concent" ~ tmp_BVI_soy$A.4.5_min_max[1],
      .default = A.4.5_min_max
    ),
    A.4.5_org_max = case_when(
      CODE3 == "223" & feed_type == "feed_concent" ~ tmp_BVI_soy$A.4.5_org_max[1],
      .default = A.4.5_org_max
    ),
    A.5.1_max = case_when(
      CODE3 == "223" & feed_type == "feed_concent" ~ tmp_BVI_soy$A.5.1_max[1],
      .default = A.5.1_max
    ),
    # BVI
    BVI_ha = case_when(
      CODE3 == "223" & feed_type == "feed_concent" ~ tmp_BVI_soy$BVI_ha[1],
      .default = BVI_ha
    ),
    BVI_kg = case_when(
      CODE3 == "223" & feed_type == "feed_concent" ~ tmp_BVI_soy$BVI_kg[1],
      .default = BVI_kg
    )
  )

# Pseudo-farm produced feed  ----

tmp_pseudofarm_produced <- feed_produced %>%
  # add coefficient to weight mean and production methods
  left_join(.,RICA_2020 %>% select(IDENT,AGBIO)) %>%
  # remove partially organic farms
  filter(AGBIO != 5) %>%
  # add yields and BVI
  inner_join(BVI_to_RICA_crops %>% select(IDENT,CODE3,yield,A.3.1_max,A.4.5_max,A.4.5_min_max,A.4.5_org_max,A.5.1_max,BVI_ha,BVI_kg)) %>%
  # recalculate SAU_c_ha for feed
  mutate(SAU_c_ha = DM_kg_p_CODE3 / yield)

# qualitéy check : j'ai des culture pour certaines fermes dans les feed mais pas dans les bvi : ce sont des fermes qui ont du être virées en cours
## cela représente : 3233 - 3090 = 143 fermes


# Pseudo-farm grassland  ----

tmp_pseudofarm_grassland <- feed_grassland %>%
  # add coefficient to weight mean and production methods
  left_join(.,RICA_2020 %>% select(IDENT,AGBIO)) %>%
  # remove partially organic farms
  filter(AGBIO != 5) %>%
  # add yields and BVI
  inner_join(BVI_to_RICA_crops %>% select(IDENT,CODE3,yield,A.3.1_max,A.4.5_max,A.4.5_min_max,A.4.5_org_max,A.5.1_max,BVI_ha,BVI_kg)) %>%
  # recalculate SAU_c_ha for feed
  mutate(SAU_c_ha = DM_kg_p_CODE3 / yield)



# BVI by livestock category ----

# join 'kg DM animal-1 y-1' and 'BVI/kg' for the farm and the pseudo farm

feed_by_pseudofarm = Reduce(rbind,list(
  tmp_pseudofarm_produced %>%
    mutate(feed_origin = "feed_produced"),
  
  tmp_pseudofarm_purchased %>%
    mutate(feed_origin = "feed_purchased"),
  
  tmp_pseudofarm_grassland %>% 
    mutate(feed_origin = "feed_grassland"))) %>% # x farms
  # keep only farms and crops for which a BVI was successfully estimated ???
  filter(!is.na(BVI_ha)) # 4985 farms

# calculate BVI / herd

## transfert table
tmp_TT_livestock <- read_xlsx("data_in/supp_data.xlsx",
                              sheet ="TT_livestock")

tmp_pseudofarm_livestock_n_feed <- feed_by_livestock %>%
  inner_join(.,feed_by_pseudofarm %>% select(IDENT,CODE3,feed_origin,AGBIO,yield,A.3.1_max,A.4.5_max,A.4.5_min_max,A.4.5_org_max,A.5.1_max,BVI_ha,BVI_kg)) %>%
  inner_join(.,tmp_TT_livestock %>% select(CODE6,species)) %>%
  mutate(
    SAU_c_ha = (DM_kg_p_CODE36 / yield)
  )
# 3458 farms

# BVI herd ----

tmp_BVI_herd <- tmp_pseudofarm_livestock_n_feed %>%
  # summarise BVI / herd
  group_by(IDENT,species) %>%
  summarise(
    # number of animals per species
    nb_anim = sum(EFFEC),
    # total feed per animal
    feed_kg_p_anim = mean(DM_kg_p_CODE3anim),
    # estimate area per animal
    feed_ha_p_anim = mean(DM_kg_p_CODE3anim / yield),
    # total feed for species
    feed_kg_p_species = sum(DM_kg_p_CODE36),
    # estimate area for feed
    feed_ha_p_species = sum(DM_kg_p_CODE36 / yield),
    # BVI_herd: total BVI of the feed for the herd
    BVI_herd = sum(DM_kg_p_CODE36*BVI_kg),
    # BVI_ha_feed: weighted average BVI_ha of the feed for the herd
    BVI_ha_feed = sum( (DM_kg_p_CODE36 / yield) * BVI_ha ) / sum(DM_kg_p_CODE36 / yield)
  )

# 4017 farms

# quality check :
## je dois retrouver les bon totaux de population et de kg de feed
# tmp_pop
 tmp_kg_feed = feed_by_pseudofarm %>% group_by(IDENT) %>% summarise(
   # total feed for species
   feed_kg_p_species = sum(DM_kg_p_CODE3),
   # estimate area for feed
   feed_ha_p_species = sum(SAU_c_ha),
   # BVI_herd: total BVI of the feed for the herd
   BVI_herd = sum(DM_kg_p_CODE3*BVI_kg),
   # BVI_ha_feed: weighted average BVI_ha of the feed for the herd ???
   BVI_ha_feed1 = BVI_herd / feed_ha_p_species,
   BVI_ha_feed2 = sum( (DM_kg_p_CODE3 / yield) * BVI_ha ) / sum(DM_kg_p_CODE3 / yield)
   
 )
 
##### Output ----

BVI_to_RICA_herd <- tmp_BVI_herd %>% ungroup()


rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])











