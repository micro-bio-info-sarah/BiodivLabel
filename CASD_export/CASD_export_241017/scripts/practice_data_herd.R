# Husbandry practices: livestock management


# transfert table
tmp_TT_livestock <- readxl::read_xlsx("data_in/supp_data.xlsx", sheet ="TT_livestock") %>%
  rename(code_livestock = RICA_code_number)


# areas to feed dairy cows ----

tmp_areas <- feed_by_livestock %>%
  # filter dairy cow
  filter(code_livestock == 929) %>%
  # add yields for produced and purchased feed
  inner_join(.,feed_by_pseudofarm %>% 
               select(farm_id,crop,yield,feed_origin),
             by = join_by(farm_id, crop, feed_origin)) %>%
  # summarise areas
  group_by(farm_id,feed_origin) %>%
  summarise(area_ha = sum(DM_kg_crop_livestock / yield),
            .groups = "keep") %>%
  pivot_wider(id_cols = farm_id,
              values_from = area_ha,
              names_from = feed_origin,
              names_prefix = "area_",
              values_fill = 0)

# temporary grassland ----

tmp_temp_pastures <- feed_by_livestock %>%
  # filter dairy cow
  filter(code_livestock == 929) %>%
  # filter temp pastures
  filter(crop == 331) %>%
  # add yields
  inner_join(.,feed_by_pseudofarm %>%
               select(farm_id,crop,yield,feed_origin) %>%
               filter(crop == 331 & feed_origin == "feed_produced"),
             by = join_by(farm_id, crop, feed_origin)) %>%
  # summarise areas
  mutate(area_ha_temp_pasture = DM_kg_crop_livestock / yield) %>%
  select(farm_id,area_ha_temp_pasture) %>%
  replace_na(list(area_ha_temp_pasture = 0))

# dairy cow population ----
tmp_cow_pop <- RICA_2020_ani %>%
  rename(farm_id = IDENT,code_livestock = CODE6) %>%
  # add species and units
  left_join(.,tmp_TT_livestock %>% select(code_livestock,EFFEC6_unit,species),
            by = join_by(code_livestock)) %>%
  # filter dairy cows
  filter(code_livestock == "929" & EFFEC6 >0) %>%
  # summaries populations
  group_by(farm_id) %>%
  summarise(cow_pop = sum(EFFEC6*EFFEC6_unit,na.rm = T),
            .groups = "keep") %>%
  ungroup() %>%
  replace_na(list(cow_pop = 0))

# main forage area ----

# Main Forage Area
# surface fourragère see instruction de collecte de 311 - 371

tmp_MFA <- feed_by_livestock %>%
  # filter forage
  filter(crop %in% as.character(seq(311,371,1)) & feed_type == "feed_rough" & feed_origin != "feed_purchased") %>%  
  # filter dairy cow
  filter(code_livestock == 929) %>%
  # add yields
  inner_join(.,feed_by_pseudofarm %>% select(farm_id,crop,yield,feed_origin),
             by = join_by(farm_id, crop, feed_origin)) %>%
  # estimate main forage area for dairy cows
  group_by(farm_id) %>%
  summarise(MFA_dairy_cow = sum(DM_kg_crop_livestock / yield,na.rm = T),
            .groups = "keep") %>%
  ungroup() %>%
  replace_na(list(MFA_dairy_cow = 0))

# share of protein crops ----

tmp_protein_crops <- feed_by_livestock %>%
  # filter protein crops
  filter(crop %in% as.character(c(seq(214,220,1),"324")) & feed_origin != "feed_purchased") %>%  
  # filter dairy cow
  filter(code_livestock == 929) %>%
  # add yields
  inner_join(.,feed_by_pseudofarm %>% select(farm_id,crop,yield,feed_origin),
             by = join_by(farm_id, crop, feed_origin)) %>%
  # estimate main forage area for dairy cows
  group_by(farm_id) %>%
  summarise(protein_crops_ha = sum(DM_kg_crop_livestock / yield,na.rm = T),
            .groups = "keep") %>%
  replace_na(list(protein_crops_ha = 0))


# kg of DM to feed dairy cows ----

tmp_feed_kg <- feed_by_livestock %>%
  # filter dairy cow
  filter(code_livestock == 929) %>%
  # summarise kg of feed
  group_by(farm_id,feed_origin) %>%
  summarise(feed_kg = sum(DM_kg_crop_livestock),
            .groups = "keep") %>%
  pivot_wider(id_cols = farm_id,values_from = feed_kg,names_from = feed_origin,names_prefix = "kg_DM_",values_fill = 0)

# kg of maize per cow ----

tmp_kg_maize <- feed_by_livestock %>%
  # filter dairy cow
  filter(code_livestock == 929) %>%
  # filter maize produced
  filter(crop == "321" & feed_origin == "feed_produced") %>%
  # summarise kg of maize per cow
  mutate(kg_DM_panim_maize_produced = DM_kg_crop_LU) %>%
  select(farm_id,kg_DM_panim_maize_produced) %>%
  replace_na(list(kg_DM_panim_maize_produced = 0))


# kg of soy meal per cow ----

tmp_kg_soy <- feed_by_livestock %>%
  # filter dairy cow
  filter(code_livestock == 929) %>%
  # filter maize produced
  filter(crop == "223" & feed_origin == "feed_purchased") %>%
  # summarise kg of maize per cow
  mutate(kg_DM_panim_soy_meal = sum(DM_kg_crop_LU)) %>%
  select(farm_id,kg_DM_panim_soy_meal) %>%
  replace_na(list(kg_DM_panim_soy_meal = 0))

# share of imported soybean meal kg ----

tmp_share_soy <- feed_by_livestock %>%
  # filter dairy cow
  filter(code_livestock == 929) %>%
  # identify soybean
  mutate(
    soybean_share = case_when(
      feed_origin == "feed_purchased" & crop == 223 ~ "kg_DM_soybean",
      .default = "kg_DM_no_soybean")) %>%
  # summarise kg of feed
  group_by(farm_id,soybean_share) %>%
  summarise(feed_kg = sum(DM_kg_crop_livestock),
            .groups = "keep") %>%
  pivot_wider(id_cols = farm_id,values_from = feed_kg,names_from = soybean_share,values_fill = 0)

# share of purchased concentrates kg ----

tmp_share_concent_purchased <- feed_by_livestock %>%
  # filter dairy cow
  filter(code_livestock == 929) %>%
  # filter purchased feed
  filter(feed_origin == "feed_purchased") %>%
  # summarise kg of feed
  group_by(farm_id,feed_type) %>%
  summarise(feed_kg = sum(DM_kg_crop_livestock),
            .groups = "keep") %>%
  pivot_wider(id_cols = farm_id,values_from = feed_kg,names_from = feed_type,names_prefix = "kg_DM_purchased_",values_fill = 0)

# share of all concentrates kg ----

tmp_share_concent <- feed_by_livestock %>%
  # filter dairy cow
  filter(code_livestock == 929) %>%
  # summarise kg of feed
  group_by(farm_id,feed_type) %>%
  summarise(feed_kg = sum(DM_kg_crop_livestock),
            .groups = "keep") %>%
  pivot_wider(id_cols = farm_id,values_from = feed_kg,names_from = feed_type,names_prefix = "kg_DM_",values_fill = 0)

# Concatenate data ----
tmp_practice_data <- BVIAS_to_RICA_RA_SIQO %>%
  # keep only milk
  filter(production_type == "milk") %>%
  # add practice data
  left_join(.,tmp_areas,by = join_by(farm_id)) %>%
  left_join(.,tmp_temp_pastures,by = join_by(farm_id)) %>%
  left_join(.,tmp_cow_pop,by = join_by(farm_id)) %>%
  left_join(.,tmp_MFA,by = join_by(farm_id)) %>%
  left_join(.,tmp_protein_crops,by = join_by(farm_id)) %>%
  left_join(.,tmp_feed_kg,by = join_by(farm_id)) %>%
  left_join(.,tmp_kg_maize,by = join_by(farm_id)) %>%
  left_join(.,tmp_kg_soy,by = join_by(farm_id)) %>%
  left_join(.,tmp_share_soy,by = join_by(farm_id)) %>%
  left_join(.,tmp_share_concent_purchased,by = join_by(farm_id)) %>%
  left_join(.,tmp_share_concent,by = join_by(farm_id)) %>%
  # add production data
  left_join(.,BVIAS_to_RICA_RA_SIQO_milk %>%
              select(farm_id,prod_kg),
            by = join_by(farm_id))

# Gather data ----


# estimate practice ----

tmp_practice_data <- tmp_practice_data %>%
  # yield L milk / ha pseudofarm
  mutate(yield_l_pha = prod_kg / (area_feed_grassland+area_feed_produced+area_feed_purchased)) %>%
  # yield L milk / dairy cow
  mutate(yield_l_panim = prod_kg / cow_pop) %>% 
  # nb cow / ha pseudofarm
  mutate(nb_cow_pha = cow_pop / (area_feed_grassland+area_feed_produced+area_feed_purchased)) %>%
  # feed autonomy
  mutate(feed_autonomy = (kg_DM_feed_grassland+kg_DM_feed_produced) /
           (kg_DM_feed_grassland+kg_DM_feed_produced+kg_DM_feed_purchased)) %>%
  # main forage area
  mutate(MFA_pcow = MFA_dairy_cow / cow_pop) %>%
  mutate(cow_pMFA = cow_pop / MFA_dairy_cow) %>%
  # kg of maize produced per cow
  mutate(kg_DM_panim_maize_produced = kg_DM_panim_maize_produced) %>%
  # kg of soy meal purchased per cow
  mutate(kg_DM_panim_soy_meal = kg_DM_panim_soy_meal) %>%
  # share of imported soybean meal
  mutate(share_soybean = kg_DM_soybean / (kg_DM_soybean+kg_DM_no_soybean)) %>%
  # share of purchased concentrates
  mutate(share_concent_purchased = kg_DM_purchased_feed_concent / (kg_DM_purchased_feed_concent+kg_DM_purchased_feed_rough)) %>%
  # share of concentrates
  mutate(share_concent = kg_DM_feed_concent / (kg_DM_feed_concent+kg_DM_feed_rough)) %>%
  # share of protein crop area
  mutate(protein_crop_ha_pha_pseudofarm = protein_crops_ha / (area_feed_grassland+area_feed_produced+area_feed_purchased)) %>%
  # share of main forage area
  mutate(MFA_pha_pseudofarm = MFA_dairy_cow / (area_feed_grassland+area_feed_produced+area_feed_purchased)) %>%
  # share of temporary grassland
  mutate(ha_temp_pasture_pha_pseudofarm = area_ha_temp_pasture / (area_feed_grassland+area_feed_produced+area_feed_purchased)) %>%
  # share of grassland 
  mutate(
    grassland_share_pseudofarm = area_feed_grassland / (area_feed_grassland+area_feed_produced+area_feed_purchased),
    grassland_share_farm = area_feed_grassland / (area_feed_grassland+area_feed_produced)) %>%
  # add farm characteristic data
  left_join(., RICA_2020 %>%
              rename(farm_id = IDENT) %>%
              select(farm_id,OTEFDD,EXTR2),
            by = join_by(farm_id))

tmp_table <- tmp_practice_data %>%
  select(FQS,tidyselect::all_of(practice_names$herd$practice)) %>%
  # summarise var
  group_by(FQS) %>%
  summarise_all(.funs = mean,na.rm=T)

#ggsave(filename = "~/BiodivLabel/figure/fig2.svg",plot = tmp_plot, width = 297,height = 210, units = "mm")
#write.csv(tmp_table,paste0(tmp_output_dir,"tmp.csv",quote = F,row.names = F))

tmp_practice_data_herd <- tmp_practice_data %>%
  select(farm_id,crop,FQS,tidyselect::all_of(practice_names$herd$practice)) %>%
  mutate(product_name = "Lait",production_type = "milk")

# output ----

if (exists("practice_data")) {
  practice_data[["herd"]] <- tmp_practice_data_herd %>%
    inner_join(.,BVIAS_to_RICA_RA_SIQO %>%
                 select(farm_id,production_type,product_name,FQS,product_FQS),
               by = join_by(farm_id, FQS, product_name, production_type))
} else {
  practice_data <- list()
  practice_data[["herd"]] <- tmp_practice_data_herd %>%
    inner_join(.,BVIAS_to_RICA_RA_SIQO %>%
                 select(farm_id,production_type,product_name,FQS,product_FQS),
               by = join_by(farm_id, FQS, product_name, production_type))
}

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])


