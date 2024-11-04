# Produced feed quantity and quality

#####
# packages

library(readxl)

library(tibble)
library(dplyr)
library(tidyr)
library(tidyverse)
library(purrr)

# Input data ----

if (my_DB == "FADN") {

  # transfert table
  tmp_FADN_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "FADN_crop_code")
  tmp_TT_crops0 <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops")
  tmp_TT_crops <- left_join(
    tmp_FADN_crops %>% rename(crop = code_letter),
    tmp_TT_crops0 %>%
      rename(crop = FADN_code_letter) %>%
      group_by(crop) %>%
      reframe(
        feed_type = unique(na.omit(feed_type)),
        feed_tables = paste0(unique(na.omit(feed_tables)),collapse = ";"),
        land_use_type = unique(na.omit(land_use_type))
      )
  ) %>% distinct()


  tmp_input <- FADN_18 %>%
    ## area in hectares for each crop
    select(ID, NUTS3, all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_TA"),colnames(FADN_18)))) %>%
    pivot_longer(cols = !c(ID,NUTS3),names_to = "crop",values_to = "area_ha") %>%
    mutate(crop = gsub("_TA","",crop)) %>%
    ## production quantity in kg for each crop
    left_join(.,FADN_18 %>%
                select(ID, NUTS3, all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_PRQ"),colnames(FADN_18)))) %>%
                pivot_longer(cols = !c(ID,NUTS3),names_to = "crop",values_to = "prod_t") %>%
                mutate(crop = gsub("_PRQ","",crop))) %>%
    ## sales quantity in kg for each crop
    left_join(.,FADN_18 %>%
                select(ID, NUTS3, all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_SQ"),colnames(FADN_18)))) %>%
                pivot_longer(cols = !c(ID,NUTS3),names_to = "crop",values_to = "sales_t") %>%
                mutate(crop = gsub("_SQ","",crop)))

  tmp_input <- tmp_input %>%
    mutate(
      # farm Unique Identifier
      farm_id = ID,
      # farm region (smallest area)
      farm_region = NUTS3,
      # crop code
      crop = crop,
      # area in hectares for each crop
      area_ha = area_ha,
      # production quantity in kg for each crop
      prod_kg = prod_t*10^3,
      # sales quantity in kg for each crop
      sales_kg = sales_t*10^3) %>%
    # select variables
    select(farm_id,farm_region,crop,area_ha,prod_kg,sales_kg)
  
  # create a table with crop codes
  tmp_TT_crops_as_feed <- tmp_TT_crops %>%
    filter(!is.na(feed_type)) %>%
    rename(crop = FADN_code_letter)
  

}

if (my_DB == "RICA") {
  
  # transfert table
  tmp_TT_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops")
  tmp_TT_crops = tmp_TT_crops %>%
    filter(!is.na(feed_type)) %>%
    mutate(crop = as.character(RICA_code_number))
    
  
  
  tmp_input <- RICA_2020_veg %>%
    # summaries areas, productions and sales
    group_by(IDENT,CODE3) %>% 
    summarise(
      # area in hectares for each crop
      area_ha = sum(SUPER3*10^-2,na.rm = T),
      # production quantity in kg for each crop
      prod_kg = sum(QPROD3*10^2,na.rm = T),
      # sales quantity in kg for each crop
      sales_kg = sum(QVENT3*10^2,na.rm = T)
    ) %>%
    # farm region
    left_join(.,RICA_2020 %>% select(IDENT,CDEPT)) %>%
    # select variables
    rename(farm_id = IDENT, crop = CODE3,farm_region = CDEPT) %>%
    select(farm_id,farm_region,crop,area_ha,prod_kg,sales_kg) %>%
  mutate(crop = as.character(crop))
  
  # create a table with crop codes
  tmp_TT_crops_as_feed <- tmp_TT_crops %>%
    filter(!is.na(feed_type)) %>%
    mutate(crop = as.character(RICA_code_number))
  
  
  }


# Estimate Feed quantity ----
## To estimate the amount of feed produced at the farm, we retrieve the amount of crop sold to the amount of crop produced at the farm.

tmp_feed_produced_qty <- crop_yield %>%
  left_join(.,tmp_input %>% select(farm_id,crop,sales_kg)) %>%
  # filter crops used as feed
  filter(crop %in% tmp_TT_crops_as_feed$crop) %>%
  # estimate produced feed quantity
  mutate(
    feed_kg_crop = prod_kg - sales_kg) %>% ungroup()


# Estimate Feed quality ----
# For each feed, we assign an average dry matter and crude protein content based on the INRAe feed tables (ref ???).

# feed tables
## we considered that crop productions are recorded as dry matter
tmp_feed_table <- read_excel("data_in/supp_data.xlsx",sheet = "feed_table_all_as_DM")

# average GE, DM & CP
tmp_avrg_feed_qlty <- tmp_TT_crops_as_feed %>%
  filter(crop %in% tmp_input$crop) %>%
  # add GE, DM & CP
  rowwise() %>%
  mutate(
    GE_MJ_kg = mean(tmp_feed_table$`GE MJ/kg`[
      tmp_feed_table$Feed %in% unlist(strsplit(feed_tables,";"))
    ],na.rm = T),
    CP_pc = mean(tmp_feed_table$`CP %`[
      tmp_feed_table$Feed %in% unlist(strsplit(feed_tables,";"))
    ],na.rm = T),
    Ash_pc = mean(tmp_feed_table$`Ash %`[
      tmp_feed_table$Feed %in% unlist(strsplit(feed_tables,";"))
    ],na.rm = T)
    ) %>% ungroup()

# Total GE, DM & CP
tmp_feed_produced_qlty <- tmp_feed_produced_qty %>%
  # add feed_type
  left_join(.,tmp_TT_crops_as_feed %>% select(feed_type,crop,land_use_type)) %>%
  select(farm_id,crop,land_use_type,feed_type,feed_kg_crop) %>%
  # add average feed DM and CP content
  left_join(.,tmp_avrg_feed_qlty %>% select(crop,GE_MJ_kg,CP_pc,Ash_pc)) %>%
  mutate(
    GE_MJ_crop = feed_kg_crop * GE_MJ_kg,
    DM_kg_crop = feed_kg_crop,
    CP_kg_crop = feed_kg_crop * (CP_pc / 100),
    Ash_kg_crop = feed_kg_crop * (Ash_pc / 100)
  ) %>% ungroup()

# CHECK ----
# WIP
print("Everything's good if TRUE")
print(nrow(tmp_input[tmp_input$crop %in% tmp_TT_crops_as_feed$crop & tmp_input$area_ha >0,]) == 
        nrow(tmp_feed_produced_qlty[tmp_feed_produced_qlty$crop %in% tmp_TT_crops_as_feed$crop,]))


#####
# Output

feed_produced <- tmp_feed_produced_qlty %>%
  filter(DM_kg_crop >0
         & land_use_type == "arable") %>%
  select(farm_id,feed_type,crop,GE_MJ_crop,DM_kg_crop,CP_kg_crop,Ash_kg_crop)

feed_grassland <- tmp_feed_produced_qlty %>%
  filter(DM_kg_crop >0
         & land_use_type == "grassland") %>%
  select(farm_id,feed_type,crop,GE_MJ_crop,DM_kg_crop,CP_kg_crop,Ash_kg_crop)



rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])









