
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
  tmp_TT_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops") %>%
    rename(crop = RICA_code_number)

  tmp_input <- RICA_2020_veg %>%
    # summaries areas by crops
    group_by(IDENT,CODE3)%>%
    summarise(
      area_ha = sum(SUPER3*10^-2,na.rm = T)
    ) %>%
    # add mineral nitrogen consumption (kg N)
    left_join(.,RICA_2020 %>% select(IDENT,CONSON,AGBIO)) %>%
    mutate(crop = CODE3,
           org_farming = case_when(
             AGBIO %in% c(2,4) ~ T,
             .default = F
           )) %>%
    filter(
      # keep only crops & grasslands with areas
      area_ha > 0, # 7264 farms
      # remove organic farms with CONSON>0, as mineral fertilization is supposed to be null in organic farms
      !(org_farming == 1 & CONSON >0)
    )
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


# TEST
tmp_input <- tmp_input %>%
  filter(farm_id %in% FADN_18$ID[FADN_18$COUNTRY =="FRA"])

# Estimate Total Farm Area ----

tmp_area_tot <- tmp_input %>%
  group_by(farm_id) %>%
  summarise(area_tot_ha = sum(area_ha),.groups = "keep") %>%
  ungroup()

hist(tmp_area_tot$area_tot_ha)
summary(tmp_area_tot$area_tot_ha)

# Shannon ----

tmp_Shannon <- tmp_input %>%
  left_join(.,tmp_area_tot,by = join_by(farm_id)) %>%
  group_by(farm_id) %>%
  summarise(
    Shannon = - sum( (area_ha / area_tot_ha) * log(area_ha/area_tot_ha)),
    .groups = "keep") %>%
  ungroup()

hist(tmp_Shannon$Shannon)
summary(tmp_Shannon$Shannon)

# Reciprocal Simpson ----

tmp_Simpson <- tmp_input %>%
  left_join(.,tmp_area_tot,by = join_by(farm_id)) %>%
  mutate(
    n = (area_ha*(area_ha-1))^2,
    N = (area_tot_ha*(area_tot_ha-1))^2,
    R = n / N
  ) %>%
  group_by(farm_id) %>%
  mutate(
    sum_R = sum(R),
    recip_simps = 1/sum_R
  ) %>%
  summarise(
    Simpson = mean(recip_simps),
    .groups = "keep") %>%
  ungroup()

hist(tmp_Simpson$Simpson)
summary(tmp_Simpson$Simpson[is.finite(tmp_Simpson$Simpson)],na.rm = T)


# Output ----

BV_A.3.3 = tmp_Shannon %>%
  left_join(.,tmp_Simpson,by=join_by(farm_id)) %>%
  mutate(A.3.3 = Simpson) %>%
  filter(is.finite(A.3.3))

# 6918 farms

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])
