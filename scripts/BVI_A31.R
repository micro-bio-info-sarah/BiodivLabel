# A.3.1 Intensity of soil movement ----

library(tibble)
library(readxl)

# here we choose a proxy of "Soil movement" metric as the total GNR consumption (in liter) minus the mean consumption without tillage per hectare divided by the UAA in crop production
# I distribute the metric value as: total to arable, zero to pasture
# I calculate the mean GNR used without tillage per hectare as the mean value of GNR used for direct seeding in wheat, maize and the wheat-maize rotation (Pellerin, 2013)
tmp_mean_GNR = mean(c(60,48,54))

# Input data ----

if (my_DB == "RICA") {
  
# transfert table
tmp_TT_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops") %>%
  rename(crop = RICA_code_number)


tmp_input <- RICA_2020_veg %>%
  # summaries areas
  group_by(IDENT,CODE3)%>%
  summarise(
    area_ha = sum(SUPER3*10^-2,na.rm = T)
  ) %>%
  # add off-road diesel consumption
  left_join(., RICA_2020 %>% select(IDENT,CHRCAQG)) # 7266 farms
}

if (my_DB == "FADN") {
  
  # transfert table
  tmp_FADN_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "FADN_crop_code")
  tmp_TT_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops") %>%
    rename(crop = FADN_code_letter)
  
  tmp_input <- FADN_18 %>%
    # area in hectares for each crop
    # off-road diesel consumption
    ## IFULS_V	H_LM_1040_V	Motor fuels and lubricants Value	in EUR
    select(ID,IFULS_V, all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_TA"),colnames(FADN_18)))) %>%
    pivot_longer(cols = !c(ID,IFULS_V),names_to = "crop",values_to = "area_ha") %>%
    mutate(crop = gsub("_TA","",crop)) %>%
    rename(farm_id = ID,fuels_value = IFULS_V) %>%
    # select variables
    select(farm_id,crop,area_ha,fuels_value)
  
}

# Estimate parameter ----

# filters
tmp_tillage <- tmp_input %>%
  filter(
    # keep only arable land use type
    crop %in% tmp_TT_crops$crop[tmp_TT_crops$land_use_type == "arable"], # 7108 farms
    # keep only crops with areas
    area_ha > 0, # 6869 farms
    # remove farms without any consumption of off-road diesel as it seems improbable
    fuels_value >0 # 6264 farms
    ) %>%
  # calculate areas by farm
  left_join(.,
            tmp_input %>% group_by(farm_id) %>% summarise(SAU_i_ha = sum(area_ha))) %>%
  # estimate parameter
  mutate(
    # x_i
    x_i = fuels_value / SAU_i_ha,
    # calculate A.3.1
    A.3.1 = case_when(
      # for farms that consume less off-road diesel than the no-till average, we considered that they practice no-till
      ## 277 farms with l_per_ha < tmp_mean_GNR
      x_i < tmp_mean_GNR ~ 0,
      # for farms that have arable land area lower than 1 hectare, we consider that they do not till
      ## 505 farms with SAU_i (arable) < 1
      #SAU_c_ha < 1 ~ 0,
      # for other farms, we apply the following formula
      .default = x_i - tmp_mean_GNR
    )) 

# 6264 farms left

# Check ----

quantile(tmp_tillage$A.3.1,c(0.1,0.25,0.5,0.75,0.9))

# Output ----

BV_A.3.1 = tmp_tillage %>%
  select(farm_id,crop,A.3.1)

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])