# Crop yields

# transfert table
tmp_TT_crops <- read_xlsx("supp_data.xlsx",sheet = "TT_crops")



# crop yields ----

# estimate yields for crops with registered production 
tmp_yield_crops <- tmp_input %>%
  filter(
    # remove crops without area or production
    area_ha > 0   
    # select crop used to feed livestock without registered production
    & crop %in% c(tmp_TT_crops$RICA_code_number[is.na(tmp_TT_crops$SAA_Agreste_2020)],
                  tmp_TT_crops$FADN_code_letter[is.na(tmp_TT_crops$SAA_Agreste_2020)])
  ) %>%
  # summaries areas, productions and sales
  group_by(farm_id,farm_region,crop) %>% 
  summarise(SAU_c_ha = sum(area_ha,na.rm = T),
            prod_kg = sum(prod_kg,na.rm = T)
  ) %>%
  # estimate yield for crops
  mutate(
    yield = prod_kg / SAU_c_ha # kg ha-1
  ) %>% ungroup()

# average yields ----

# for some crops with area but zero production registered while it should have, we use the averages from farm in the same area (departement) 
## estimate average by departement
tmp_avrg_yields <- tmp_input %>%
  # remove crops without area or production
  filter(area_ha > 0 & prod_kg > 0) %>%
  # summarise yields by crop and departement
  group_by(crop,farm_region) %>%
  summarise(SAU_c_ha = sum(area_ha,na.rm = T),
            prod_kg = sum(prod_kg,na.rm = T)
  ) %>%
  # estimate yield for crops
  mutate(
    yield_avrg = prod_kg / SAU_c_ha # kg ha-1
  ) %>% 
  select(crop,farm_region,yield_avrg) %>%
  ungroup()

## insert average
tmp_yield_crops <- tmp_yield_crops %>%
  # add average
  rowwise() %>%
  mutate(
    yield = case_when(
      SAU_c_ha >0 & prod_kg == 0 ~ tmp_avrg_yields$yield_avrg[tmp_avrg_yields$crop == crop 
                                                              & tmp_avrg_yields$farm_region == farm_region],
      .default = yield
    )
  )

# forage yields ----

### only production for alfalfa (CODE3 = 323) is well registered with an unweighted average of 0.8 t/ha, similar to Agreste data
### no production for CODE3 %in% c(311,321,324,331,371)
### no production for grassland, i.e. CODE3 %in% c(341,342)
### no area nor production for CODE3 %in% c(352,353,354)
# thus, we differentiate between crops with or without a registered production


# estimate yields for crops without registered production
# for forage crops and for grassland, no production is registered, as specified in the "Instruction de collecte" documentation
# so we used the french official yield data for 2020 (Statistiques agricoles annuelles 2020, Chiffres définitifs, Agreste, 11/2021)

# create a table that match NUTS3 code and french departement number
# from https://gisco-services.ec.europa.eu/tercet/flat-files
tmp_NUTS_code <- read_xlsx("supp_data.xlsx",sheet = "pc2024_FR_NUTS-2024_v1.0") %>%
  mutate(Departement = str_extract(CODE,"[0-9]{2}")) %>% # WIP /!\ ne fonctionne pas pour les domtom => a revoir
  rename(farm_region = NUTS3) %>%
  select(farm_region,Departement) %>%
  distinct()

tmp_SAA_yield <- read_xlsx("supp_data.xlsx",sheet = "yield_SAA_Agreste_2020") %>%
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
  # add NUTS3 codes
  left_join(.,tmp_NUTS_code,relationship = "many-to-many")

# create a table with both RICA and FADN crop codes
tmp_TT_forage <- tmp_TT_crops %>%
  filter(!is.na(SAA_Agreste_2020)) %>%
  select(RICA_code_number,SAA_Agreste_2020) %>%
  rename(crop = RICA_code_number) %>%
  rbind(.,tmp_TT_crops %>%
          filter(!is.na(SAA_Agreste_2020)) %>%
          select(FADN_code_letter,SAA_Agreste_2020) %>%
          rename(crop = FADN_code_letter))

# estimate forage yield
tmp_yield_forage <- tmp_input %>%
  filter(
    # remove forage without area
    area_ha > 0
    # select crop used to feed livestock without registered production
    & crop %in% tmp_TT_forage$crop) %>%
  # summaries areas, productions and sales
  group_by(farm_id,farm_region,crop) %>% 
  summarise(SAU_c_ha = sum(area_ha,na.rm = T),
            prod_kg = sum(prod_kg,na.rm = T)) %>% ungroup() %>%
  # add yield as estimated using SAA Agreste data
  ## add matching names between tables
  inner_join(.,tmp_TT_forage) %>%
  ## add average yields by crop and by departement
  rowwise() %>%
  mutate(yield = tmp_SAA_yield$yield[tmp_SAA_yield$Cultures_developpees_5 == SAA_Agreste_2020
                                     & tmp_SAA_yield$farm_region == farm_region])
  
  left_join(.,tmp_SAA_yield %>%
              select(Cultures_developpees_5,farm_region,yield) %>%
              rename(SAA_Agreste_2020=Cultures_developpees_5),
            relationship = "many-to-many") %>%
  # estimate production
  mutate(
    prod_kg = SAU_c_ha * yield
  )

tmp_yield <- Reduce(rbind,list(tmp_yield_crops %>% select(IDENT,CODE3,SAU_c_ha,prod_kg,yield),
                               tmp_yield_forage %>% select(IDENT,CODE3,SAU_c_ha,prod_kg,yield)))

# 7264 farms

# quality check : average yield by crops to compare with Agreste Infos rapides - Grandes Cultures - Nov. 2021 - n°149
## pretty good !