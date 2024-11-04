
# A.3.3	Crop Diversity

library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringi)
library(stringr)

# here we estimated two diversity indices: Shannon index and Reciprocal Simpson index

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
    # keep only crops & grasslands with areas
    filter(area_ha > 0) %>%
    # add land use type
    rename(farm_id = IDENT, crop = CODE3) %>%
    left_join(.,tmp_TT_crops %>% select(crop,land_use_type), by = join_by(crop)) %>%
    # select variables and obs
    select(farm_id,land_use_type,crop,area_ha)
  # 7069 farms

  }

if (my_DB == "FADN") {

  # transfert table
  tmp_FADN_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "FADN_crop_code")

  tmp_input <- FADN_18 %>%
    # area in hectares for each crop
    select(ID,ORGANIC, all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_TA"),colnames(FADN_18)))) %>%
    pivot_longer(cols = all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_TA"),colnames(FADN_18))),
                 names_to = "crop",values_to = "area") %>%
    mutate(
      crop = gsub("_TA","",crop),
      org_farming = case_when(ORGANIC %in% c(2,4) ~ T,.default = F),
      area_ha = area
      ) %>%
    rename(farm_id = ID) %>%
    # select variables and obs
    select(farm_id,org_farming,crop,area_ha) %>%
    filter(
      # keep only crops & grasslands with areas
      area_ha > 0
      )

}

# Estimate Total Farm Area and crop number ----

tmp_area_tot <- tmp_input %>%
  group_by(farm_id,land_use_type) %>%
  summarise(
    area_LU_ha = sum(area_ha),
    crop_nb_LU = length(unique(crop)),
    .groups = "keep") %>%
  ungroup()

#hist(tmp_area_tot$area_LU_ha)
#summary(tmp_area_tot$area_LU_ha)
#hist(tmp_area_tot$crop_nb_LU)
#summary(tmp_area_tot$crop_nb_LU)

# Shannon ----

tmp_Shannon <- tmp_input %>%
  left_join(.,tmp_area_tot,by = join_by(farm_id,land_use_type)) %>%
  group_by(farm_id,land_use_type) %>%
  summarise(
    Shannon = - sum( (area_ha / area_LU_ha) * log(area_ha/area_LU_ha)),
    .groups = "keep") %>%
  ungroup()

#hist(tmp_Shannon$Shannon)
#summary(tmp_Shannon$Shannon)

# Reciprocal Simpson ----

tmp_Simpson <- tmp_input %>%
  left_join(.,tmp_area_tot,by = join_by(farm_id,land_use_type)) %>%
  mutate(
    n = (area_ha*(area_ha-1))^2,
    N = (area_LU_ha*(area_LU_ha-1))^2,
    R = n / N
  ) %>%
  group_by(farm_id,land_use_type) %>%
  mutate(
    sum_R = sum(R),
    recip_simps = 1/sum_R
  ) %>%
  summarise(
    Simpson = mean(recip_simps),
    .groups = "keep") %>%
  ungroup()

#hist(tmp_Simpson$Simpson)
#summary(tmp_Simpson$Simpson[is.finite(tmp_Simpson$Simpson)],na.rm = T)


# Output ----

BV_A.3.3 = tmp_Shannon %>%
  left_join(.,tmp_Simpson,by=join_by(farm_id,land_use_type)) %>%
  left_join(.,tmp_area_tot,by=join_by(farm_id,land_use_type)) %>%
  mutate(A.3.3 = Shannon) %>%
  filter(is.finite(A.3.3))

# 6918 farms

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])
