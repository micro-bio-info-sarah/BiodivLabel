# BVIAS per herd

# Set up ----

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(tibble)
library(stringr)


# National average farm ----

if (my_DB == "RICA") {
  
  tmp_input <- optim_final$MSE$MSE_optim$BVIAS$y %>%
    mutate(crop = as.character(crop))
  
  # average for crops
  tmp_practices <- tmp_input %>%
    select(farm_id,land_use_type,crop,org_farming,metric_number,x_max) %>%
    rename(practice = metric_number,value = x_max) %>%
    # add BVIAS
    bind_rows(.,
              BVIAS_to_RICA_crops %>%
                mutate(practice = "BVIAS_ha",value = BVIAS_ha) %>%
                select(farm_id,land_use_type,crop,org_farming,practice,value) %>%
                distinct()
    ) %>%
    bind_rows(.,
              BVIAS_to_RICA_crops %>%
                mutate(practice = "BVIAS_kg",value = BVIAS_kg) %>%
                select(farm_id,land_use_type,crop,org_farming,practice,value) %>%
                distinct()
    ) %>%
    # add yields
    bind_rows(.,
              BVIAS_to_RICA_crops %>%
                mutate(practice = "yield",value = yield) %>%
                select(farm_id,land_use_type,crop,org_farming,practice,value) %>%
                distinct()
    ) %>%
    mutate(crop = as.character(crop))
  
  
  ## estimates the average agricultural practices
  tmp_ref_farm_crops <- tmp_practices %>%
    # add coefficient to weight mean and production methods /!\ only for RICA, no extrapolation coefficient in FADN
    left_join(.,RICA_2020 %>%
                rename(farm_id = IDENT) %>%
                select(farm_id,EXTR2),
              by = join_by(farm_id)) %>%
    # estimate national averages for yield, BVIAS / ha and BVIAS / kg
    group_by(land_use_type,crop,org_farming,practice) %>%
    summarise(
      w_mean = weighted.mean(value,EXTR2),
      .groups = "keep"
    ) %>% ungroup()
  
  # transfert table
  tmp_TT_livestock <- readxl::read_xlsx("data_in/supp_data.xlsx", sheet ="TT_livestock") %>%
    rename(code_livestock = RICA_code_number)
  
  
}

if (my_DB == "FADN") {
  
  # average for crops
  ## estimates the average agricultural practices
  tmp_ref_farm_crops <- BVIAS_to_RICA_crops %>%
    # estimate national averages for yield, BVIAS / ha and BVIAS / kg
    group_by(crop,org_farming) %>%
    summarise(
      # !!! check les n : certains sont <3
      n = n(),
      # !!! na.rm = T pour test code mais ptetre à enlever par la suite
      yield = mean(yield,na.rm = T),
      A.3.1 = mean(A.3.1,na.rm = T),
      A.4.5 = mean(A.4.5,na.rm = T),
      A.4.3 = mean(A.4.3,na.rm = T),
      A.4.5_org_max = mean(A.4.5_org_max,na.rm = T),
      A.5.1 = mean(A.5.1,na.rm = T),
      BVIAS_ha = mean(BVIAS_ha,na.rm = T),
      BVIAS_kg = mean(BVIAS_kg,na.rm = T)
    ) %>% ungroup()
  
  # transfert table
  tmp_FADN_code <- read_xlsx("data_in/supp_data.xlsx",sheet = "FADN_livestock_code")
  tmp_TT_livestock0 <- read_xlsx("data_in/supp_data.xlsx", sheet ="TT_livestock")
  tmp_TT_livestock <- left_join(
    tmp_FADN_code,
    tmp_TT_livestock0) %>%
    rename(code_livestock = FADN_code_letter) %>%
    select(code_livestock,species) %>%
    distinct()
  
}

# quality check : compare calculated average with official national average

# Purchased feed in Pseudo-farm ----

tmp_pseudofarm_purchased_fr <- feed_purchased %>%
  # add production mode and filter farms with inner join
  inner_join(.,tmp_practices %>% select(farm_id,org_farming) %>% distinct(),by = join_by(farm_id)) %>%
  # add average practices
  left_join(.,
            bind_rows(
              tmp_ref_farm_crops,
              expand.grid(setdiff(unique(feed_purchased$crop),unique(tmp_ref_farm_crops$crop)),
                          c(T,F),
                          unique(tmp_ref_farm_crops$practice)) %>%
                rename(crop = Var1,org_farming = Var2,practice = Var3) %>%
                mutate(crop = as.character(crop)) %>%
                rowwise() %>%
                mutate(
                  w_mean = mean(tmp_ref_farm_crops$w_mean[
                    # by production methods (i.e. organic or conventional) & by crop
                    tmp_ref_farm_crops$org_farming == org_farming &
                      tmp_ref_farm_crops$crop %in% unlist(strsplit(crop,";")) &
                      tmp_ref_farm_crops$practice == practice
                  ]))) %>%
              pivot_wider(id_cols = c(land_use_type,crop,org_farming),
                          names_from = practice,
                          values_from = w_mean),
            by = join_by(crop,org_farming)
  ) %>%
  mutate(SAU_c_ha = DM_kg_crop / yield)

# Soybean meal from Brasil ----

# soybean meal used to feed livestock in France mostly comes from Brasil (Sailley, 2021 ???; Overmars  ???)
# to estimate the biodiversity impact of soybean meal produced in Brasil, we compared the agricultal practices estimated by Lindner (Lindner 2022) for soy and wheat production from Agribalyse data to the agricultural practices we estimated for wheat production from FADN data

tmp_soybean <- tibble(
  metric_number = c("A.3.1","A.4.5","A.4.3","A.5.1"),
  wheat_Lindner = c(1.488558,180.7,162.7/180.7,51580.36245417),
  soy_Lindner = c(1.2926064,0,0,36888.53241),
  wheat_RICA = c(mean(tmp_practices$value[tmp_practices$crop == "111" & tmp_practices$practice == "A.3.1"]),
                 mean(tmp_practices$value[tmp_practices$crop == "111" & tmp_practices$practice == "A.4.5"]),
                 mean(tmp_practices$value[tmp_practices$crop == "111" & tmp_practices$practice == "A.4.3"]),
                 mean(tmp_practices$value[tmp_practices$crop == "111" & tmp_practices$practice == "A.5.1"]))
) %>%
  mutate(
    soy_RICA = soy_Lindner * wheat_RICA / wheat_Lindner
  ) %>%
  # for other variables, we considered that soybean cultivation in Brasil is an intensive crop cultivation, we thus apply the variable value of the most intensive quartile of french wheat cultivation
  ## for A.2.2 & A.3.2, most intensive quartile is q75
  bind_rows(.,tmp_input %>%
              filter(crop == 111 & metric_number %in% c("A.2.2","A.3.2")) %>%
              group_by(metric_number) %>%
              summarise(wheat_RICA = quantile(value,0.75), soy_RICA = quantile(value,0.75),
                        .groups = "keep")) %>%
  ## for A.2.1 & A.3.3, most intensive quartile is q25
  bind_rows(.,tmp_input %>%
              filter(crop == 111 & metric_number %in% c("A.2.1","A.3.3")) %>%
              group_by(metric_number) %>%
              summarise(wheat_RICA = quantile(value,0.25), soy_RICA = quantile(value,0.25),
                        .groups = "keep"))

# then we applied the BVIAS model to these estimated parameters
## import constant from Lindner 2019 SM
# import constant from Lindner 2019 SM
#tmp_BV_constant <-  read_excel("data_in/supp_data.xlsx",sheet = "Lindner_2019_BV_LU_function_con")
tmp_BV_constant <- optim_final$constants$BV_constant_optim
tmp_BV_weight <- optim_final$weights$weight_optim

tmp_x_norm_soybean <- tmp_soybean %>%
  select(metric_number,soy_RICA) %>%
  rename(value = soy_RICA) %>%
  # add maximum of french wheat
  left_join(.,
            tmp_practices %>%
              filter(crop == 111 & practice %in% tmp_soybean$metric_number) %>%
              group_by(practice) %>%
              summarise(max = max(value),
                        .groups = "keep") %>%
              rename(metric_number = practice),
            by = join_by(metric_number)) %>%
  mutate(
    id_col = "soybean",
    land_use_type = "arable",
    crop = 223,
    ## set max
    x_max =
      case_when(
        value > max ~ max,
        .default =  value ),
    ## Normalize data
    x_norm =
      case_when(
        value == 0 ~ 0,
        value > max ~ 1,
        .default =  value / max ))


tmp_BVIAS_soybean <- BVIAS(tmp_x_norm_soybean,c("id_col","crop","land_use_type"),optim_final$constants$BV_constant_optim,optim_final$weights$weight_optim)

tmp_BVIAS_soybean[["BVIAS_full"]] <- tmp_BVIAS_soybean$BVIAS %>%
  mutate(
    # yield estimated from Overmars et al., 2015 as 2.45 t ha-1 for soy cultivated in Latin America
    yield = 2.45*10^3,
    # BVIAS / kg
    BVIAS_kg = BVIAS_ha / yield,
    # add feed type and origin
    feed_type = "feed_concent",
    feed_origin = "feed_purchased"
  )

# replace soybean values in pseudofarm
tmp_pseudofarm_purchased <- tmp_pseudofarm_purchased_fr %>%
  mutate(
    yield = case_when(
      crop == "223" & feed_type == "feed_concent" ~ tmp_BVIAS_soybean$BVIAS_full$yield[1],
      .default = yield
    ),
    SAU_c_ha = case_when(
      crop == "223" & feed_type == "feed_concent" ~ DM_kg_crop / tmp_BVIAS_soybean$BVIAS_full$yield[1],
      .default = SAU_c_ha
    ),
    # BVIAS parameters
    A.3.1 = case_when(
      crop == "223" & feed_type == "feed_concent" ~ tmp_BVIAS_soybean$y$value[tmp_BVIAS_soybean$y$metric_number == "A.3.1"],
      .default = A.3.1
    ),
    A.4.5 = case_when(
      crop == "223" & feed_type == "feed_concent" ~ tmp_BVIAS_soybean$y$value[tmp_BVIAS_soybean$y$metric_number == "A.4.5"],
      .default = A.4.5
    ),
    A.4.3 = case_when(
      crop == "223" & feed_type == "feed_concent" ~ tmp_BVIAS_soybean$y$value[tmp_BVIAS_soybean$y$metric_number == "A.4.3"],
      .default = A.4.3
    ),
    A.5.1 = case_when(
      crop == "223" & feed_type == "feed_concent" ~ tmp_BVIAS_soybean$y$value[tmp_BVIAS_soybean$y$metric_number == "A.5.1"],
      .default = A.5.1
    ),
    # BVIAS
    BVIAS_ha = case_when(
      crop == "223" & feed_type == "feed_concent" ~ tmp_BVIAS_soybean$BVIAS_full$BVIAS_ha,
      .default = BVIAS_ha
    ),
    BVIAS_kg = case_when(
      crop == "223" & feed_type == "feed_concent" ~ tmp_BVIAS_soybean$BVIAS_full$BVIAS_kg,
      .default = BVIAS_kg
    )
  ) %>% ungroup()

# Produced feed in Pseudo-farm ----

tmp_pseudofarm_produced <- feed_produced %>%
  mutate(crop = as.character(crop)) %>%
  # add production mode, methods and filter farms with inner join
  inner_join(.,tmp_practices %>%
               pivot_wider(id_cols = c(farm_id,land_use_type,crop,org_farming),
                           names_from = practice,
                           values_from = value),
             by = join_by(farm_id,crop)) %>%
  # recalculate SAU_c_ha for feed
  mutate(SAU_c_ha = DM_kg_crop / yield)

# Grassland in Pseudo-farm ----

tmp_pseudofarm_grassland <- feed_grassland %>%
  mutate(crop = as.character(crop)) %>%
  # add production mode, methods and filter farms with inner join
  inner_join(.,tmp_practices %>%
               pivot_wider(id_cols = c(farm_id,land_use_type,crop,org_farming),
                           names_from = practice,
                           values_from = value),
             by = join_by(farm_id,crop)) %>%
  # recalculate SAU_c_ha for feed
  mutate(SAU_c_ha = DM_kg_crop / yield)

# BVIAS by livestock category ----

# join 'kg DM animal-1 y-1' and 'BVIAS/kg' for the farm and the pseudo farm

feed_by_pseudofarm = Reduce(bind_rows,list(
  tmp_pseudofarm_produced %>%
    mutate(feed_origin = "feed_produced"),
  
  tmp_pseudofarm_purchased %>%
    mutate(feed_origin = "feed_purchased"),
  
  tmp_pseudofarm_grassland %>%
    mutate(feed_origin = "feed_grassland")))

# calculate BVIAS / herd

tmp_pseudofarm_livestock_n_feed <- feed_by_livestock %>%
  # add feed production practice
  inner_join(.,feed_by_pseudofarm %>% 
               select(farm_id,org_farming,crop,feed_origin,org_farming,
                      yield,BVIAS_ha,BVIAS_kg),
             by = join_by(farm_id, crop, feed_origin)) %>%
  # add livestock species
  inner_join(.,tmp_TT_livestock %>% 
               select(code_livestock,species) %>%
               distinct(),
             by = join_by(code_livestock)) %>%
  mutate(
    SAU_c_ha = (DM_kg_crop_livestock / yield)
  )

# BVIAS herd ----

tmp_BVIAS_herd <- tmp_pseudofarm_livestock_n_feed %>%
  # summarise BVIAS / herd
  group_by(farm_id,org_farming,species) %>%
  summarise(
    # number of livestock unit per species
    nb_anim = sum(livestock_unit),
    # total feed per animal
    feed_kg_p_anim = mean(DM_kg_crop_LU),
    # estimate area per animal
    feed_ha_p_anim = mean(DM_kg_crop_LU / yield),
    # total feed for species
    feed_kg_p_species = sum(DM_kg_crop_livestock),
    # estimate area for feed
    feed_ha_p_species = sum(DM_kg_crop_livestock / yield),
    # BVIAS_herd: total BVIAS of the feed for the herd
    BVIAS_herd = sum(DM_kg_crop_livestock*BVIAS_kg),
    # BVIAS_ha_feed: weighted average BVIAS_ha of the feed for the herd
    BVIAS_ha_feed = sum( (DM_kg_crop_livestock / yield) * BVIAS_ha ) / sum(DM_kg_crop_livestock / yield),
    .groups = "keep"
  )

# 3796 farms

# check ----
## je dois retrouver les bon totaux de kg de feed
tmp_check <- feed_by_pseudofarm %>%
  group_by(farm_id,feed_origin,crop) %>%
  summarise(
    # total feed for species
    feed_kg_p_species1 = sum(DM_kg_crop),
    .groups = "keep") %>%
  left_join(.,
            feed_by_livestock %>%
              group_by(farm_id,feed_origin,crop) %>%
              summarise(
                # total feed for species
                feed_kg_p_species2 = sum(DM_kg_crop_livestock),
                .groups = "keep")
  ) %>%
  mutate(check = round(feed_kg_p_species1) == round(feed_kg_p_species2))
print(table(tmp_check$check))

# Output ----

BVIAS_to_RICA_herd <- tmp_BVIAS_herd %>% ungroup()

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])











