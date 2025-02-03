# A.3.1 Intensity of soil movement ----

library(tibble)
library(readxl)

# here we choose a proxy of "Soil movement" metric as the total GNR consumption (in liter) minus the mean consumption without tillage per hectare divided by the UAA in crop production
# we considering that only arable land use receive tillage
# I calculate the mean off-road diesel used without tillage (i.e., direct seedling) per hectare for wheat, maize and the wheat-maize rotation (Pellerin, 2013)
tmp_mean_GNR = mean(c(60,48,54))

# Input data ----

if (my_DB == "RICA") {

  # transfert table
  tmp_TT_crops <- readxl::read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops") %>%
    rename(crop = RICA_code_number)

tmp_input <- RICA_2020_veg %>%
  rename(farm_id = IDENT,crop = CODE3) %>%
  # add land use type
  left_join(.,tmp_TT_crops %>% select(crop,land_use_type), by = join_by(crop)) %>%
  # convert areas in hectares
  mutate(
    area_ha = SUPER3*10^-2
    ) %>%
  # add off-road diesel consumption
  left_join(., RICA_2020 %>% select(IDENT,CHRCAQG) %>%
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

# Estimate parameter ----

# V1
tmp_tillage1 <- tmp_areas %>%
  left_join(.,tmp_input %>% select(farm_id,diesel_L) %>% distinct(),by=join_by(farm_id)) %>%
  # estimate parameter
  mutate(
    # x_i
    x_i = (diesel_L - (tmp_mean_GNR*area_ha_tot)) / arable) %>%
  mutate(A.3.1 = case_when(
    # for farms that consume less off-road diesel than the no-till average, we considered that they practice no-till
    ## 277 farms with l_per_ha < tmp_mean_GNR
    x_i < 0 ~ 0,
    # for farms that do not have arable land area or less than 1 hectare, we consider that they do not till
    ## 505 farms with SAU_i (arable) < 1
    is.na(arable) ~ 0,
    arable < 1 ~ 0,
    # for other farms, we apply the following formula
    .default = x_i
  ))

# V2

tmp_tillage2 <- tmp_input %>%
  left_join(.,tmp_areas,by = join_by(farm_id)) %>%
  # estimate parameter
  mutate(
    # x_i
    x_i = (diesel_L* area_ha/area_ha_tot) - tmp_mean_GNR*area_ha
    ) %>%
  mutate(A.3.1 = case_when(
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

length(unique(tmp_tillage1$farm_id)) == nrow(tmp_tillage1)

# in Pellerin et al, 2015, average total gazole consumption for wheat with tillage = 90 L/ha
mean(tmp_tillage1$A.3.1[tmp_tillage1$A.3.1 <= quantile(unique(tmp_tillage1$A.3.1),0.95)])

mean(tmp_tillage2$A.3.1[tmp_tillage2$A.3.1 <= quantile(unique(tmp_tillage2$A.3.1),0.95)])
mean(tmp_tillage2$A.3.1[tmp_tillage2$A.3.1 <= quantile(unique(tmp_tillage2$A.3.1),0.95) & tmp_tillage2$crop == 111])


# compare sum to original value
sum(RICA_2020$CHRCAQG[RICA_2020$IDENT %in% tmp_tillage$farm_id[tmp_tillage$A.3.1 >0]]) == sum(tmp_tillage$A.3.1[tmp_tillage$A.3.1 >0]*tmp_tillage$arable[tmp_tillage$A.3.1 >0] +(tmp_mean_GNR*tmp_tillage$area_ha_tot[tmp_tillage$A.3.1 >0]),na.rm = T)

# Output ----

BV_A.3.1 = tmp_tillage %>%
  mutate(land_use_type = "arable") %>%
  select(farm_id,land_use_type,A.3.1) %>%
  ungroup()

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])
