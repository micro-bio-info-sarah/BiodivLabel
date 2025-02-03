# A.3.1 Intensity of soil movement ----

library(tibble)
library(readxl)

# here we choose a proxy of "Soil movement" metric as the total GNR consumption (in liter) minus the mean consumption without tillage per hectare divided by the UAA in crop production
# we considering that only arable land use receive tillage
# I calculate the mean off-road diesel used without tillage (i.e., direct seedling) per hectare for wheat, maize and the wheat-maize rotation (Chenu, 2013)
tmp_mean_GNR = mean(c(60,48,54))

# Input data ----

if (my_DB == "RICA") {
  
  # transfert table
  tmp_TT_crops <- readxl::read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops") %>%
    rename(crop = RICA_code_number)
  
  tmp_input <- RICA_2020_veg %>%
    # summaries areas by crops
    group_by(IDENT,CODE3)%>%
    summarise(
      area_ha = sum(SUPER3*10^-2,na.rm = T),
      .groups = "keep") %>%
    rename(farm_id = IDENT,crop = CODE3) %>%
    # add land use type
    left_join(.,tmp_TT_crops %>% select(crop,land_use_type), by = join_by(crop)) %>%
    # add off-road diesel consumption
    inner_join(., RICA_2020 %>% select(IDENT,CHRCAQG) %>%
                rename(farm_id = IDENT,diesel_L = CHRCAQG),
              by = join_by(farm_id)) %>%
    # select variables and obs
    select(farm_id,land_use_type,crop,area_ha,diesel_L) %>%
    filter(
      # keep only crops with areas
      area_ha > 0,
      # remove farms without any consumption of off-road diesel as it seems improbable
      diesel_L >0 # 6560 farms
    )
  
  tmp_areas <- tmp_input %>%
    # summaries areas in hectares
    group_by(farm_id,land_use_type)%>%
    summarise(
      area_ha_LU = sum(area_ha),
      .groups = "keep"
    ) %>% ungroup() %>%
    pivot_wider(id_cols = farm_id,names_from = land_use_type,values_from = area_ha_LU) %>%
    rowwise() %>%
    mutate(area_ha_tot = sum(c(arable,grassland),na.rm = T)) %>% ungroup()
  
}

if (my_DB == "FADN") {

  # transfert table
  tmp_FADN_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "FADN_crop_code")
  tmp_TT_crops0 <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops")
  tmp_TT_crops <- left_join(
    tmp_FADN_crops %>% rename(crop = code_letter),
    tmp_TT_crops0 %>% rename(crop = FADN_code_letter) %>% select(crop)
  ) %>% distinct()


  tmp_input <- FADN_18 %>%
    # area in hectares for each crop
    # off-road diesel consumption
    ## IFULS_V	H_LM_1040_V	Motor fuels and lubricants Value	in EUR
    select(ID,IFULS_V, all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_TA"),colnames(FADN_18)))) %>%
    pivot_longer(cols = !c(ID,IFULS_V),names_to = "crop",values_to = "area_ha") %>%
    mutate(crop = gsub("_TA","",crop)) %>%
    rename(farm_id = ID,diesel_L = IFULS_V) %>%
    # select variables and obs
    select(farm_id,crop,area_ha,diesel_L) %>%
    filter(
      # keep only crops with areas
      area_ha > 0, # 6869 farms
      # remove farms without any consumption of off-road diesel as it seems improbable
      diesel_L >0 # 6264 farms
    )

}

# Estimate parameter ----

# filters
tmp_tillage <- tmp_input %>%
  left_join(.,tmp_areas,by = join_by(farm_id)) %>%
  # estimate parameter
  mutate(
    # x_i
    x_i = (diesel_L* area_ha/area_ha_tot) - tmp_mean_GNR*area_ha
  ) %>%
  mutate(A.3.1 = case_when(
    # we considered that only arable land use receive tillage
    land_use_type != "arable" ~ 0,
    # for farms that consume less off-road diesel than the no-till average, we considered that they practice no-till
    ## 277 farms with l_per_ha < tmp_mean_GNR
    x_i < 0 ~ 0,
    # for farms that do not have arable land area or less than 1 hectare, we consider that they do not till
    ## 505 farms with SAU_i (arable) < 1
    is.na(arable) ~ 0,
    arable < 1 ~ 0,
    # for other farms, we apply the following formula
    .default = x_i / area_ha
  ))

# Check ----

# in Chenu et al, 2015, average total gazole consumption for tillage = 41 L/ha
mean(tmp_tillage$A.3.1[tmp_tillage$A.3.1 <= quantile(unique(tmp_tillage$A.3.1),0.95)])

# compare sum to original value
## in Chenu et al., 2015, 43% of off-road disesel consumption due to mechanization is attributed to tillage
sum(tmp_tillage$A.3.1 * tmp_tillage$area_ha) / sum(RICA_2020$CHRCAQG) *100


# Output ----

BV_A.3.1 = tmp_tillage %>%
  mutate(land_use_type = "arable") %>%
  select(farm_id,land_use_type,crop,A.3.1) %>%
  ungroup()

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])
