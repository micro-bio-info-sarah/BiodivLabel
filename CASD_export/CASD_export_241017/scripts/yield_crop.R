# Crop yields


# Input data ----

if (my_DB == "RICA") {
  
  # transfert table
  tmp_TT_crops <- readxl::read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops") %>%
    rename(crop = RICA_code_number)
  
  tmp_input <- RICA_2020_veg %>%
    # summaries areas, productions
    # area in hectares for each crop
    # production quantity in kg for each crop
    group_by(IDENT,CODE3) %>% 
    summarise(area_ha = sum(SUPER3*10^-2,na.rm = T),
              prod_kg = sum(QPROD3*10^2,na.rm = T)
    ) %>%
    # farm region
    left_join(.,RICA_2020 %>% select(IDENT,CDEPT,AGBIO),
              by = join_by(IDENT)) %>%
    # remove crops without area
    filter(area_ha > 0) %>%
    # select variables and obs
    rename(farm_id = IDENT,farm_NUTS3 = CDEPT,crop = CODE3) %>%
    mutate(org_farming = case_when(
      AGBIO %in% c(2,4) ~ T,
      .default = F
    ),
    crop = as.character(crop)) %>%
    select(farm_id,farm_NUTS3,crop,org_farming,area_ha,prod_kg)
  
}

if (my_DB == "FADN") {
  # transfert table
  tmp_FADN_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "FADN_crop_code")
  tmp_TT_crops0 <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops")
  tmp_TT_crops <- left_join(
    tmp_FADN_crops %>% rename(crop = code_letter),
    tmp_TT_crops0 %>% rename(crop = FADN_code_letter) %>% select(crop,SAA_Agreste_2020)
  ) %>% distinct()
  
  tmp_input <- FADN_18 %>%
    # area in hectares for each crop
    ## IPROT_V	H_SC_3040_V	Crop protection products Value	in EUR
    select(ID, NUTS3,all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_TA"),colnames(FADN_18)))) %>%
    pivot_longer(cols = all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_TA"),colnames(FADN_18))),
                 names_to = "crop",values_to = "area_ha") %>%
    mutate(crop = gsub("_TA","",crop)) %>%
    ## production quantity in kg for each crop
    left_join(.,FADN_18 %>%
                select(ID, NUTS3, all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_PRQ"),colnames(FADN_18)))) %>%
                pivot_longer(cols = !c(ID,NUTS3),names_to = "crop",values_to = "prod_t") %>%
                mutate(crop = gsub("_PRQ","",crop),
                       # production quantity in kg for each crop
                       prod_kg = prod_t*10^3)) %>%
    # remove crops without area
    filter(area_ha > 0) %>%
    # select variables and obs
    rename(farm_id = ID,farm_NUTS3 = NUTS3) %>%
    select(farm_id,farm_NUTS3,crop,area_ha,prod_kg)
  
}

# Estimate crop production ----


# in RICA, areas and production of crops used to feed livestock can be registered in different variables
# for some crops, on-farm consumption to feed livestock is registered as intra-consumption
## QINTH3 Intraconsommations pour herbiv. (volume)
## QINTL3 Intraconsommations pour volailles (vol.)
## QINTP3 Intraconsommations pour porcins (volume)
## QINTA3 Intraconsommations pour autres animaux (vol.)
## tmp_crops_used_to_feed_livestock = unique(tmp_input$crop[tmp_input$QINTH3 >0 | tmp_input$QINTL3 >0 | tmp_input$QINTP3 >0 | tmp_input$QINTA3 >0])
## WIP but we don't use these variables because ... WIP instead, we estimate the amount of crops used to feed livestock as the difference between the production and the sales
## We will consider only crops used to feed livestock

# estimate yields for crops with registered production
tmp_yield_crops <- tmp_input %>%
  filter(
    # select crop used to feed livestock without registered production
    crop %in% unique(na.omit(tmp_TT_crops$crop[is.na(tmp_TT_crops$SAA_Agreste_2020)]))
  ) %>%
  # summaries areas, productions and sales
  group_by(farm_id,farm_NUTS3,crop,org_farming) %>%
  summarise(SAU_c_ha = sum(area_ha,na.rm = T),
            prod_kg = sum(prod_kg,na.rm = T),
            .groups = "keep"
  ) %>%
  # estimate yield for crops
  mutate(
    yield = prod_kg / SAU_c_ha # kg ha-1
  ) %>% ungroup()

# forage yields ----

tmp_TT_forage <- tmp_TT_crops %>%
  filter(!is.na(SAA_Agreste_2020)) %>%
  select(crop,SAA_Agreste_2020) %>%
  # filter crops in data
  filter(crop %in% tmp_input$crop)

### only production for alfalfa (crop = 323) is well registered with an unweighted average of 0.8 t/ha, similar to Agreste data
### no production for crop %in% c(311,321,324,331,371)
### no production for grassland, i.e. crop %in% c(341,342)
### no area nor production for crop %in% c(352,353,354)
# thus, we differentiate between crops with or without a registered production


# estimate yields for crops without registered production
# for forage crops and for grassland, no production is registered, as specified in the "Instruction de collecte" documentation
# so we used the french official yield data for 2020 (Statistiques agricoles annuelles 2020, Chiffres définitifs, Agreste, 11/2021)

tmp_SAA_yield <- read_xlsx("data_in/supp_data.xlsx",sheet = "yield_SAA_Agreste_2020") %>%
  mutate(
    Departement = case_when(
      Departement %in% c("971 - Guadeloupe") ~ "9A",
      Departement %in% c("972 - Martinique") ~ "9B",
      Departement %in% c("973 - Guyane") ~ "9C",
      Departement %in% c("974 - La Réunion") ~ "9D",
      .default = substr(Departement,1,2)
    ),
    yield = Production_seche_volume*10^2 / Superficie_correspondante_hectare) %>%
  # replace NaN in yield as zeros
  replace_na(list(yield=0)) %>%
  # aggregate crops
  rename(SAA_Agreste_2020 = Cultures_developpees_5) %>%
  group_by(SAA_Agreste_2020) %>%
  summarise(yield = mean(yield)) %>%
  left_join(.,tmp_TT_forage,
            by = join_by(SAA_Agreste_2020)) %>%
  filter(!is.na(crop)) %>%
  mutate(crop = as.character(crop))


# estimate forage yield
tmp_yield_forage <- tmp_input %>%
  filter(
    # remove forage without area
    area_ha > 0
    # select crop used to feed livestock without registered production
    & crop %in% tmp_TT_forage$crop) %>%
  # summaries areas, productions and sales
  group_by(farm_id,farm_NUTS3,crop,org_farming) %>%
  summarise(SAU_c_ha = sum(area_ha,na.rm = T),
            prod_kg = sum(prod_kg,na.rm = T),
            .groups = "keep") %>% ungroup() %>%
  # add yield as estimated using SAA Agreste data
  ## add average yields by crop
  left_join(.,tmp_SAA_yield,
            by = join_by(crop)) %>%
  # estimate production
  mutate(
    prod_kg = SAU_c_ha * yield
  )

tmp_yield <- bind_rows(tmp_yield_crops,tmp_yield_forage)


# WIP quality check : average yield by crops to compare with Agreste Infos rapides - Grandes Cultures - Nov. 2021 - n°149
## WIP

# average yields ----

# for some crops with area but zero production registered while it should have, we use the averages from farm in the same area (departement)
## estimate average by departement
tmp_avrg_yields <- tmp_yield %>%
  # remove crops without area or production
  filter(SAU_c_ha > 0 & prod_kg > 0) %>%
  # summarise yields by crop and departement
  group_by(crop,farm_NUTS3) %>%
  summarise(SAU_c_ha = sum(SAU_c_ha,na.rm = T),
            prod_kg = sum(prod_kg,na.rm = T),
            .groups = "keep"
  ) %>% ungroup() %>%
  # estimate yield for crops
  mutate(
    yield_avrg = prod_kg / SAU_c_ha # kg ha-1
  ) %>%
  select(crop,farm_NUTS3,yield_avrg)

## insert average
crop_yield <- tmp_yield %>%
  # add average yield
  left_join(.,tmp_avrg_yields, 
            by = join_by(crop,farm_NUTS3)) %>%
  # add average
  mutate(
    yield = case_when(
      yield > 0 ~ yield,
      SAU_c_ha >0 & prod_kg == 0 ~ yield_avrg
    )
  ) %>%
  # for crops and NUTS without average => national average by crop
  group_by(crop) %>%
  mutate(
    yield = case_when(
      yield > 0 ~ yield,
      is.na(yield) ~ mean(yield,na.rm = T)
    )
  ) %>% ungroup() %>%
  select(farm_id,crop,SAU_c_ha,prod_kg,yield)

# still NAs for tree nurseries, vineyards, fallows, etc.
#view(crop_yield %>% filter(is.na(yield)) %>% group_by(crop) %>% count())

# Output ----

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])


