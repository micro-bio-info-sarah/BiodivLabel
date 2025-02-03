
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
    ungroup() %>%
    # keep only crops & grasslands with areas
    filter(area_ha > 0) %>%
    # add land use type
    rename(farm_id = IDENT, crop = CODE3) %>%
    left_join(.,tmp_TT_crops %>%
                select(crop,land_use_type,species),
              by = join_by(crop)) %>%
    # select variables and obs
    select(farm_id,land_use_type,species,crop,area_ha)
  # 7069 farms

}

# crop nb ----

# all crops
tmp_crop_nb <- tmp_input %>%
  filter(crop %in% tmp_TT_crops$crop[tmp_TT_crops$land_use_type == "arable"]) %>%
  # add org farming
  left_join(., RICA_2020 %>%
              mutate(org_farming = as.character(AGBIO %in% c(2,4))) %>%
              select(IDENT, org_farming) %>%
              rename(farm_id = IDENT),
            by = join_by(farm_id)) %>%
  # crop nb
  group_by(farm_id,org_farming) %>%
  summarise(crop_nb = length(unique(crop)),.groups = "keep") %>%
  # mean crop nb
  group_by(org_farming) %>%
  summarise(crop_nb = mean(crop_nb))




# by species
tmp_crop_nb <- tmp_input %>%
  # add org farming
  left_join(., RICA_2020 %>%
              mutate(org_farming = as.character(AGBIO %in% c(2,4))) %>%
              select(IDENT, org_farming) %>%
              rename(farm_id = IDENT),
            by = join_by(farm_id)) %>%
  # crop nb
  group_by(farm_id,org_farming,species) %>%
  summarise(crop_nb = length(unique(crop)),.groups = "keep") %>%
  # mean crop nb
  group_by(org_farming,species) %>%
  summarise(crop_nb = mean(crop_nb))
  

# OFB method ----

# add departement
tmp_div <- tmp_input %>%
  left_join(., RICA_2020 %>%
              mutate(org_farming = as.character(AGBIO %in% c(2,4))) %>%
              select(IDENT, EXTR2, CDEPT,NREG, AGBIO,org_farming) %>%
              rename(farm_id = IDENT,
                     spatial_scale = NREG),
            by = join_by(farm_id))

## Shannon ----

tmp_Shannon <- tmp_div %>%
  group_by(spatial_scale,org_farming,crop) %>%
  summarise(area_ha = sum(area_ha,na.rm = T),.groups = "keep") %>%
  ungroup() %>%
  group_by(org_farming,spatial_scale) %>%
  summarise(
    #farm_nb = length(unique(farm_id)),
    Shannon = - sum((area_ha / sum(area_ha))*log((area_ha / sum(area_ha)))),
    #crop_nb = length(unique(crop)),
    .groups = "keep") %>%
  ungroup()


#hist(tmp_Shannon$Shannon)
#summary(tmp_Shannon$Shannon)

## Reciprocal Simpson ----
# as some areas are <1ha, I cannot use n_i(n_i-1) version of the formula so I choose the n_i^2 version of the formula
tmp_Simpson <- tmp_div %>%
  group_by(spatial_scale,org_farming,crop) %>%
  summarise(area_ha = sum(area_ha,na.rm = T),.groups = "keep") %>%
  ungroup() %>%
  group_by(org_farming,spatial_scale)%>%
  summarise(
    #farm_nb = length(unique(farm_id)),
    R_Simpson = 1/sum(area_ha^2 / sum(area_ha)^2),
    #crop_nb = length(unique(crop)),
    #area_ha = sum(area_ha),
    .groups = "keep") %>%
  ungroup()

#hist(tmp_Simpson$Simpson)
#summary(tmp_Simpson$Simpson[is.finite(tmp_Simpson$Simpson)],na.rm = T)

## weighted mean

tmp_div_mean <- tmp_div %>%
  left_join(.,tmp_Shannon, by = join_by(spatial_scale, org_farming)) %>%
  left_join(.,tmp_Simpson, by = join_by(spatial_scale, org_farming)) %>%
  group_by(crop,org_farming) %>%
  summarise(w.mean_Shannon = weighted.mean(Shannon,area_ha),
            w.mean_simpson = weighted.mean(R_Simpson,area_ha),
            .groups = "keep") %>%
  # add crop libelle
  left_join(.,
            tmp_TT_crops %>% select(crop,LIBELLE),
            by = join_by(crop))

write.csv(tmp_div_mean,"tmp.csv")
esquisse::esquisser(tmp_div_mean)

library(ggplot2)
tmp_div_mean %>%
  ggplot(.) +
  aes(x = LIBELLE, y = w.mean_simpson, fill = org_farming) +
  geom_col(position = "dodge") +
  scale_fill_hue(direction = 1) +
  coord_flip() +
  theme_minimal()

# area proportion ----

tmp_p100 <- tmp_div %>%
  group_by(spatial_scale,org_farming,crop) %>%
  summarise(area_ha = sum(area_ha,na.rm = T),.groups = "keep") %>%
  ungroup() %>%
  group_by(spatial_scale,org_farming) %>%
  mutate(area_p100 = area_ha / sum(area_ha, na.rm = T) *100)


# OFB method but with total area ----

# add departement
tmp_div <- tmp_input %>%
  left_join(., RICA_2020 %>%
              mutate(org_farming = as.character(AGBIO %in% c(2,4))) %>%
              select(IDENT, EXTR2,CDEPT,NREG, AGBIO,org_farming) %>%
              rename(farm_id = IDENT,
                     spatial_scale = CDEPT),
            by = join_by(farm_id))

## Estimate Total Farm Area and crop number ----

tmp_area_tot <- tmp_div %>%
  group_by(spatial_scale) %>%
  summarise(
    sum_area_ha = sum(area_ha),
    .groups = "keep") %>%
  ungroup()

## Shannon ----

tmp_Shannon <- tmp_div %>%
  left_join(.,tmp_area_tot) %>%
  group_by(org_farming,spatial_scale) %>%
  summarise(
    #farm_nb = length(unique(farm_id)),
    Shannon = - sum((area_ha / sum_area_ha)*log((area_ha / sum_area_ha))),
    #crop_nb = length(unique(crop)),
    .groups = "keep") %>%
  ungroup()


#hist(tmp_Shannon$Shannon)
#summary(tmp_Shannon$Shannon)

## Reciprocal Simpson ----
# as some areas are <1ha, I cannot use n_i(n_i-1) version of the formula so I choose the n_i^2 version of the formula
tmp_Simpson <- tmp_div %>%
  left_join(.,tmp_area_tot) %>%
  group_by(org_farming,spatial_scale)%>%
  summarise(
    #farm_nb = length(unique(farm_id)),
    R_Simpson = 1/sum((area_ha / sum_area_ha)^2),
    #crop_nb = length(unique(crop)),
    #area_ha = sum(area_ha),
    .groups = "keep") %>%
  ungroup()

#hist(tmp_Simpson$Simpson)
#summary(tmp_Simpson$Simpson[is.finite(tmp_Simpson$Simpson)],na.rm = T)

## weighted mean

tmp_div_mean <- tmp_div %>%
  left_join(.,tmp_Shannon, by = join_by(spatial_scale, org_farming)) %>%
  left_join(.,tmp_Simpson, by = join_by(spatial_scale, org_farming)) %>%
  group_by(crop,org_farming) %>%
  summarise(w.mean_Shannon = weighted.mean(Shannon,area_ha),
            w.mean_simpson = weighted.mean(R_Simpson,area_ha),
            .groups = "keep") %>%
  # add crop libelle
  left_join(.,
            tmp_TT_crops %>% select(crop,LIBELLE),
            by = join_by(crop))

write.csv(tmp_div_mean,"tmp.csv")
esquisse::esquisser(tmp)



# By farm ----

## Estimate Total Farm Area and crop number ----

tmp_area_tot <- tmp_input %>%
  group_by(farm_id,land_use_type) %>%
  summarise(
    area_LU_ha = sum(area_ha),
    crop_nb = length(unique(crop)),
    .groups = "keep") %>%
  ungroup()

#hist(tmp_area_tot$area_LU_ha)
#summary(tmp_area_tot$area_LU_ha)
#hist(tmp_area_tot$crop_nb)
#summary(tmp_area_tot$crop_nb)


## Shannon ----

tmp_Shannon <- tmp_input %>%
  left_join(.,tmp_area_tot,by = join_by(farm_id,land_use_type)) %>%
  group_by(farm_id,land_use_type) %>%
  summarise(
    Shannon = - sum( (area_ha / area_LU_ha) * log(area_ha/area_LU_ha)),
    .groups = "keep") %>%
  ungroup()

#hist(tmp_Shannon$Shannon)
#summary(tmp_Shannon$Shannon)

## Reciprocal Simpson ----

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

# area tot by departement ----

# add departement
tmp_div <- tmp_input %>%
  left_join(., RICA_2020 %>%
              mutate(org_farming = as.character(AGBIO %in% c(2,4))) %>%
              select(IDENT,CDEPT,NREG, AGBIO,org_farming) %>%
              rename(farm_id = IDENT,
                     spatial_scale = NREG),
            by = join_by(farm_id))

## Estimate Total Farm Area and crop number ----

## group by region and crop species

tmp_area_tot <- tmp_div %>%
  group_by(spatial_scale,species) %>%
  summarise(
    sum_farm_nb = length(unique(farm_id)),
    sum_area_ha = sum(area_ha),
    sum_crop_nb = length(unique(crop)),
    .groups = "keep") %>%
  ungroup()

## Shannon ----

tmp_Shannon <- tmp_div %>%
  left_join(.,tmp_area_tot,by = join_by(spatial_scale,species)) %>%
  group_by(org_farming,spatial_scale,species) %>%
  summarise(
    farm_nb = length(unique(farm_id)),
    Shannon = - sum((area_ha / sum_area_ha)*log((area_ha / sum_area_ha))),
    crop_nb = length(unique(crop)),
    .groups = "keep") %>%
  ungroup()


#hist(tmp_Shannon$Shannon)
#summary(tmp_Shannon$Shannon)

## Reciprocal Simpson ----
# as some areas are <1ha, I cannot use n_i(n_i-1) version of the formula so I choose the n_i^2 version of the formula

tmp_Simpson <- tmp_div %>%
  left_join(.,tmp_area_tot,
            by = join_by(spatial_scale,species)) %>%
  group_by(org_farming,spatial_scale,species)%>%
  summarise(
    farm_nb = length(unique(farm_id)),
    R_Simpson = 1/sum((area_ha / sum_area_ha)^2),
    crop_nb = length(unique(crop)),
    area_ha = sum(area_ha),
    .groups = "keep") %>%
  ungroup()

#hist(tmp_Simpson$Simpson)
#summary(tmp_Simpson$Simpson[is.finite(tmp_Simpson$Simpson)],na.rm = T)

## Comparison conv / bio ----

tmp <- left_join(tmp_Shannon,
                 tmp_Simpson,
                 by = join_by(org_farming,spatial_scale,species,"farm_nb","crop_nb")
) %>%
  filter(complete.cases(.)) %>%
  group_by(species,org_farming) %>%
  summarise(
    nb_crop = mean(crop_nb),
    area_ha = mean(area_ha),
    
    Shannon = mean(Shannon),
    R_Simpson = mean(R_Simpson)
    )
write.csv(tmp,"tmp.csv")
esquisse::esquisser(tmp)

# area tot by production mode ----

# add departement
tmp_div <- tmp_input %>%
  left_join(., RICA_2020 %>%
              mutate(org_farming = as.character(AGBIO %in% c(2,4))) %>%
              select(IDENT,CDEPT,NREG, AGBIO,org_farming) %>%
              rename(farm_id = IDENT,
                     spatial_scale = NREG),
            by = join_by(farm_id))

## Shannon ----

tmp_Shannon <- tmp_div %>%
  group_by(org_farming,spatial_scale,species) %>%
  summarise(
    farm_nb = length(unique(farm_id)),
    Shannon = - sum((area_ha / sum(area_ha))*log((area_ha / sum(area_ha)))),
    crop_nb = length(unique(crop)),
    .groups = "keep") %>%
  ungroup()


#hist(tmp_Shannon$Shannon)
#summary(tmp_Shannon$Shannon)

## Reciprocal Simpson ----
# as some areas are <1ha, I cannot use n_i(n_i-1) version of the formula so I choose the n_i^2 version of the formula
tmp_Simpson <- tmp_div %>%
  group_by(org_farming,spatial_scale,species)%>%
  summarise(
    farm_nb = length(unique(farm_id)),
    R_Simpson = 1/sum((area_ha / sum(area_ha))^2),
    crop_nb = length(unique(crop)),
    area_ha = sum(area_ha),
    .groups = "keep") %>%
  ungroup()

#hist(tmp_Simpson$Simpson)
#summary(tmp_Simpson$Simpson[is.finite(tmp_Simpson$Simpson)],na.rm = T)

## Comparison conv / bio ----

tmp <- left_join(tmp_Shannon,
                 tmp_Simpson,
                 by = join_by(org_farming,spatial_scale,species,"farm_nb","crop_nb")
) %>%
  filter(complete.cases(.)) %>%
  group_by(species,org_farming) %>%
  summarise(
    mean_nb_crop = mean(crop_nb),
    mean_area_ha = mean(area_ha),
    
    Shannon = mean(Shannon),
    R_Simpson = mean(R_Simpson)
  )
write.csv(tmp,"tmp.csv")
esquisse::esquisser(tmp)

# Crop family ----

# add crop family
tmp_div <- tmp_input %>%
  left_join(., RICA_2020 %>%
              mutate(org_farming = as.character(AGBIO %in% c(2,4))) %>%
              select(IDENT,OTEFDA,CDEPT,NREG, AGBIO,org_farming) %>%
              rename(farm_id = IDENT),
            by = join_by(farm_id)) %>%
  left_join(.,tmp_TT_crops %>%
              select(crop,crop_family),
            by = join_by(crop)
  )

## Shannon ----

tmp_Shannon <- tmp_div %>%
  group_by(farm_id,OTEFDA,org_farming,crop_family) %>%
  summarise(area_ha = sum(area_ha,na.rm = T),.groups = "keep") %>%
  group_by(farm_id,OTEFDA,org_farming) %>%
  summarise(
    Shannon = - sum((area_ha / sum(area_ha))*log((area_ha / sum(area_ha)))),
    .groups = "keep") %>%
  ungroup()


#hist(tmp_Shannon$Shannon)
#summary(tmp_Shannon$Shannon)

## Reciprocal Simpson ----
# as some areas are <1ha, I cannot use n_i(n_i-1) version of the formula so I choose the n_i^2 version of the formula
tmp_Simpson <- tmp_div %>%
  group_by(farm_id,OTEFDA,org_farming,crop_family) %>%
  summarise(area_ha = sum(area_ha,na.rm = T),.groups = "keep") %>%
  group_by(farm_id,OTEFDA,org_farming) %>%
  summarise(
    R_Simpson = 1/sum((area_ha / sum(area_ha))^2),
    .groups = "keep") %>%
  ungroup()

#hist(tmp_Simpson$Simpson)
#summary(tmp_Simpson$Simpson[is.finite(tmp_Simpson$Simpson)],na.rm = T)

## Comparison conv / bio ----

tmp <- left_join(tmp_Shannon,
                 tmp_Simpson,
                 by = join_by(farm_id,OTEFDA, org_farming)) %>%
  filter(complete.cases(.)) %>%
  group_by(org_farming,OTEFDA) %>%
  summarise(
    Shannon = mean(Shannon),
    R_Simpson = mean(R_Simpson),
    .groups = "keep"
  )

# Output ----

BV_A.3.3 = tmp_Shannon %>%
  left_join(.,tmp_Simpson,by=join_by(farm_id,land_use_type)) %>%
  left_join(.,tmp_area_tot,by=join_by(farm_id,land_use_type)) %>%
  mutate(A.3.3 = Shannon) %>%
  filter(is.finite(A.3.3))

# 6918 farms

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])
