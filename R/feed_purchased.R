# Purchased feed quantity and quality"

#####
# packages

library(readr)
library(readxl)

library(tibble)
library(dplyr)
library(tidyr)
library(tidyverse)
library(purrr)

# here, we estimate the total amount of feed by farm and their dry matter (DM) and crude protein (CP) content

# Input data ----


if (my_DB == "FADN") {

  # FADN data
  tmp_input <- FADN_18 %>%
    mutate(
      farm_id = ID,
      # cattle
      feed_cattle_total = SE310,
      feed_cattle_produced = SE315,
      feed_cattle_purch_concent = IGRFEDCNCTRPUR_V,
      feed_cattle_purch_rough = IGRFEDCRSPUR_V,
      # pigs and poultry => maybe one day I'll take the time to estimate for those animals as well
      feed_concent = IGRFEDCNCTRPUR_V,
      feed_rough = IGRFEDCRSPUR_V
    )

  tmp_IPAMPA = 99.4
  
}

if (my_DB == "RICA") {
  
  tmp_input <- RICA_2020 %>%
    select(IDENT,CHRAC,CHRAG) %>%
    # rename variables
    rename(farm_id = IDENT,
           feed_concent = CHRAC,
           feed_rough = CHRAG)
  
  tmp_IPAMPA = 112.7
  
  
}

# Estimate Feed quantity from feed cost ----

## To convert the concentrate and rough feed actual cost in euros to kilos of feed, we used a price index for animal feed established by the INSEE (IPAMPA - Aliments pour animaux (INSEE id 010539256) pour 2019 = 99.4 €/t).

# Convert euros to kilos
## We used the actual cost of concentrate and rough feed to estimate their mass
## kg of feed= € of feed / IPAMPA*1000
## we considered IPAMPA in tonne of dry matter
## IPAMPA - Aliments pour animaux (INSEE id 010539256) 2019 = 99,4 €/t

# Feed variables
## SE310	Feed for grazing livestock in EUR
## SE320	Feed for pigs & poultry in EUR
## IGRFEDCNCTRPUR_V	Purchased concentrated feedstuffs for grazing stock (equines, ruminants) Value
## IPIGFEDPUR_V	Purchased feedstuffs for pigs Value
## IPLTRFEDPUR_V Purchased feedstuffs for poultry and other small animals Value

tmp_feed_purchased <- tmp_input  %>%
  # keep farms that purchased feed
  filter(feed_concent >0 | feed_rough >0) %>% # 34922 farms in FADN18 # 4040 farms in RICA20
  mutate(
    # concentrate feed
    feed_concent = case_when(
      feed_concent >0 ~ feed_concent/tmp_IPAMPA*1000,
      .default = 0),
    # rough feed
    feed_rough = case_when(
      feed_rough >0 ~ feed_rough/tmp_IPAMPA*1000,
      .default = 0)
  ) %>%
  select(farm_id,feed_concent,feed_rough) %>%
  pivot_longer(cols = !farm_id,names_to = "feed_type",values_to = "feed_kg_farm")



# Estimate concentrate and rough feed distribution ----
## From Sailley et al. 2021, INRAe Prod. Anim.
tmp_feed_distribution <- read_excel("data_in/supp_data.xlsx",sheet = "TT_feed_purchased")
tmp_feed_distribution <- tmp_feed_distribution %>% rename(crop = RICA_code_number)

## concentrates: select main feed type
tmp_feed_distribution_concent <- tmp_feed_distribution %>%
  # keep main feed type
  filter(percent >= 0.1) %>%
  # select variables
  filter(feed_type == "feed_concent") %>%
  select(feed_type,crop,feed_tables,relative_weight) %>%
  # recalculate percent
  mutate(relative_total = sum(relative_weight),
         relative_percent = relative_weight / relative_total)

## coarse: select main feed type
tmp_feed_distribution_rough <- tmp_feed_distribution %>%
  # keep main feed type
  filter(percent >= 0.1) %>%
  # select variables
  filter(feed_type == "feed_rough") %>%
  select(feed_type,crop,feed_tables,relative_weight) %>%
  # recalculate percent
  mutate(relative_total = sum(relative_weight),
         relative_percent = relative_weight / relative_total)

## distribute feed
tmp_feed_purchased_qty <- tmp_feed_purchased %>%
  full_join(.,Reduce(rbind,list(tmp_feed_distribution_concent,tmp_feed_distribution_rough)),
            relationship = "many-to-many") %>%
  mutate(
    feed_kg_crop = feed_kg_farm * relative_percent
  )

# Estimate Feed quality ----
# For each feed, we assign an average dry matter and crude protein content based on the INRA-CIRAD-AFZ feed tables

# feed tables
tmp_feed_table <- read_excel("data_in/supp_data.xlsx",sheet = "feed_table_all_as_DM")

## average GE, DM & CP
tmp_avrg_feed_qlty <- tmp_feed_distribution %>%
  # keep main feed type
  filter(percent >= 0.1) %>%
  # select variables
  select(feed_type,crop,feed_tables) %>%
  # add GE, DM & CP
  rowwise() %>%
  mutate(
    GE_MJpkg = mean(tmp_feed_table$`GE MJ/kg`[
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
tmp_feed_purchased_qlty <- tmp_feed_purchased_qty %>%
  select(farm_id,crop,feed_type,feed_kg_crop) %>%
  # add average feed DM and CP content
  left_join(.,tmp_avrg_feed_qlty %>% mutate(crop = as.character(crop)) %>% select(crop,GE_MJpkg,CP_pc,Ash_pc)) %>%
  mutate(
    GE_MJ_crop = feed_kg_crop * GE_MJpkg,
    DM_kg_crop = feed_kg_crop,
    CP_kg_crop = feed_kg_crop * (CP_pc / 100),
    Ash_kg_crop = feed_kg_crop * (Ash_pc / 100)
  ) %>% ungroup()

# change crop names for FADN
if (my_DB == "FADN") {

  # transfert table
  tmp_TT_crops <- read_xlsx("data_in/supp_data.xlsx", sheet ="TT_crops")


  tmp_feed_purchased_qlty <- tmp_feed_purchased_qlty %>%
    rowwise() %>%
    mutate(
      crop = paste0(unique(na.omit(
        tmp_TT_crops$FADN_code_letter[tmp_TT_crops$RICA_code_number %in%
                                        unlist(strsplit(crop,";"))])),collapse = ";")
    )

}

# check ----

tmp_check_farms = sample(unique(tmp_feed_purchased_qlty$farm_id),size = 50)

tmp_check_input <- tmp_input %>%
  # filter test farms
  filter(farm_id %in% tmp_check_farms) %>%
  # keep farms that purchased feed
  filter(feed_concent >0 | feed_rough >0) %>%
  mutate(
    # concentrate feed
    feed_concent = case_when(
      feed_concent >0 ~ feed_concent/tmp_IPAMPA*1000,
      .default = 0),
    # rough feed
    feed_rough = case_when(
      feed_rough >0 ~ feed_rough/tmp_IPAMPA*1000,
      .default = 0)
  ) %>%
  select(farm_id,feed_concent,feed_rough)

tmp_check_output <- tmp_feed_purchased_qlty %>%
  # filter test farms
  filter(farm_id %in% tmp_check_farms) %>%
  # summarize by farm and feed type
  group_by(farm_id,feed_type) %>%
  summarise(feed_kg_farm = sum(DM_kg_crop)) %>%
  pivot_wider(id_cols = farm_id,names_from = feed_type,values_from = feed_kg_farm)

tmp_check <- left_join(tmp_check_input,tmp_check_output,by="farm_id") %>%
  mutate(check1 = round(feed_concent.x,2) == round(feed_concent.y,2), # have to round to find the same
         check2 = round(feed_rough.x,2) == round(feed_rough.y,2))

print("Everything's good if FALSE = 0")
print(table(tmp_check$check1))
print(table(tmp_check$check2))


# Output ----

feed_purchased <- tmp_feed_purchased_qlty %>%
  filter(feed_kg_crop >0) %>%
  select(farm_id,feed_type,crop,GE_MJ_crop,DM_kg_crop,CP_kg_crop,Ash_kg_crop) %>%
  ungroup()


rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])









