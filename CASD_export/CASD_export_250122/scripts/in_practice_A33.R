
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

# Shannon ----

tmp_Shannon <- tmp_input %>%
  group_by(farm_id,land_use_type) %>%
  summarise(
    #farm_nb = length(unique(farm_id)),
    Shannon = - sum((area_ha / sum(area_ha))*log((area_ha / sum(area_ha)))),
    #crop_nb_LU = length(unique(crop)),
    .groups = "keep") %>%
  ungroup()

#hist(tmp_Shannon$Shannon)
#summary(tmp_Shannon$Shannon)

# Reciprocal Simpson ----

# as some areas are <1ha, I cannot use n_i(n_i-1) version of the formula so I choose the n_i^2 version of the formula
tmp_Simpson <- tmp_input %>%
  group_by(farm_id,land_use_type) %>%
  summarise(
    #farm_nb = length(unique(farm_id)),
    R_Simpson = 1/sum(area_ha^2 / sum(area_ha)^2),
    crop_nb_LU = length(unique(crop)),
    #area_ha = sum(area_ha),
    .groups = "keep") %>%
  ungroup()

#hist(tmp_Simpson$Simpson)
#summary(tmp_Simpson$Simpson[is.finite(tmp_Simpson$Simpson)],na.rm = T)


# Output ----

BV_A.3.3 = tmp_Shannon %>%
  left_join(.,tmp_Simpson,by=join_by(farm_id,land_use_type)) %>%
  mutate(A.3.3 = R_Simpson) %>%
  filter(is.finite(A.3.3))

# 6918 farms

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])
