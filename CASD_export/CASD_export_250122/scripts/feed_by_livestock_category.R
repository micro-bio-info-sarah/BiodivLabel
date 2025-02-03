# Purchased feed quantity and quality

##### packages ----

library(readr)
library(readxl)

library(tibble)
library(dplyr)
library(tidyr)
library(tidyverse)
library(purrr)

# Animal data ----

if (my_DB == "RICA") {

# transfert table
tmp_TT_livestock <- read_xlsx("data_in/supp_data.xlsx", sheet ="TT_livestock") %>%
  rename(code_livestock = RICA_code_number)

# RICA data
tmp_input <- RICA_2020_ani %>%
  filter(
    # remove livestock without population
    EFFEC6 >0
    # remove eggs & chicks
    & CODE6 != 984
    & CODE6 != 974
    # remove bees
    & CODE6 != 993
    ) %>%
  # add species and units
  rename(farm_id =IDENT,code_livestock = CODE6) %>%
  left_join(.,tmp_TT_livestock %>% select(code_livestock,EFFEC6_unit,UGB)) %>%
  # summaries populations
  group_by(farm_id,code_livestock) %>%
  # /!\ warning: animal population units differ between years
    # AROPAJ data are for one livestock unit, so need to convert using LSU coefficient (see eurostat)
  summarise(livestock_unit = sum(EFFEC6*EFFEC6_unit*UGB)) %>% ungroup()

}

if (my_DB == "FADN") {

  # FADN data
  tmp_FADN_code <- read_xlsx("data_in/supp_data.xlsx",sheet = "FADN_livestock_code")

  tmp_input <- FADN_18 %>%
    ## area in hectares for each crop
    select(ID, all_of(intersect(paste0(tmp_FADN_code$FADN_code_letter,"_ALU"),colnames(FADN_18)))) %>%
    pivot_longer(cols = !c(ID),names_to = "code_livestock",values_to = "livestock_unit") %>%
    rename(farm_id = ID) %>%
    mutate(code_livestock = gsub("_ALU","",code_livestock)) %>%
    # remove livestock without population
    filter(livestock_unit >0 )

  # transfert table
  tmp_TT_livestock <- read_xlsx("data_in/supp_data.xlsx", sheet ="TT_livestock")
  tmp_TT_livestock <- tmp_TT_livestock %>%
    # add row for diary buffalo
    rbind(tmp_TT_livestock %>%
            filter(FADN_code_letter == "LCOWDAIR") %>%
            select(!all_of(colnames(tmp_FADN_code))) %>%
            cbind(tmp_FADN_code %>%
                    filter(FADN_code_letter == "LBUFDAIRPRS"))) %>%
    rename(code_livestock = FADN_code_letter) %>%
    # concatenate when multiple rows for one FADN code
    filter(!is.na(code_livestock)) %>%
    group_by(code_livestock) %>%
    summarise(
      AROPAJ = paste0(unique(na.omit(AROPAJ)),collapse = ";")
    )



}

# Average ration ----

## AROPAJ
tmp_AROPAJ <- read_excel("data_in/supp_data.xlsx", sheet = "AROPAJ_France")

## assign average ratio by livestock code
tmp_ref_ration <- tmp_TT_livestock %>%
  # select livestock code
  filter(code_livestock %in% unique(tmp_input$code_livestock)) %>%
  # add average ration
  rowwise() %>%
  mutate(
    ref_DM_kg_LU = mean(tmp_AROPAJ$total_intake_tDM_LU_y[
      tmp_AROPAJ$AROPAJ %in% unlist(strsplit(AROPAJ,";"))
    ],na.rm = T) *10^3
    # AROPAJ data are in tonnes for one livestock unit
  )

# we considered that rabbit have the same average ration than poultry

# Theoretical ration by farm ----

tmp_ration_th_code_livestock <- tmp_input %>%
  # add average ration
  left_join(., tmp_ref_ration %>% select(code_livestock,ref_DM_kg_LU)) %>% ungroup() %>%
  # calculate theoretical average ration per code_livestock
  mutate(th_DM_kg_code_livestock = ref_DM_kg_LU * livestock_unit)

tmp_ration_th_farm <- tmp_ration_th_code_livestock %>%
  group_by(farm_id) %>%
  summarise(th_DM_kg_farm = sum(th_DM_kg_code_livestock))

tmp_ration_th <- left_join(tmp_ration_th_code_livestock,tmp_ration_th_farm)

# Distribute ----

# total amount of feed by farm (kg year-1)
tmp_feed_by_farm = Reduce(rbind,list(feed_purchased %>% mutate(feed_origin = "feed_purchased"),
                                     feed_produced %>% mutate(feed_origin = "feed_produced"),
                                     feed_grassland %>% mutate(feed_origin = "feed_grassland")))

# TEST
#filter(farm_id %in% c("2201871", "2501849", "5320166","4302448", "4322472", "4302545","2201469","8209862", "2501904"))

tmp_feed_by_livestock <- tmp_feed_by_farm %>%
  # add livestock
  inner_join(.,tmp_input,relationship = "many-to-many") %>%
  # add theoretical ration from AROPAJ reference
  left_join(., tmp_ration_th) %>%
  # distribute DM
  mutate(
    # DM of each crop by livestock type: kg DM y-1
    DM_kg_crop_livestock = DM_kg_crop * th_DM_kg_code_livestock / th_DM_kg_farm,
    GE_MJ_crop_livestock = GE_MJ_crop * th_DM_kg_code_livestock / th_DM_kg_farm,
    CP_kg_crop_livestock = CP_kg_crop * DM_kg_crop_livestock / DM_kg_crop,
    Ash_kg_crop_livestock = Ash_kg_crop * DM_kg_crop_livestock / DM_kg_crop,
    # DM of each crop by animal: kg DM animal-1 y-1
    DM_kg_crop_LU = DM_kg_crop_livestock / livestock_unit,
    GE_MJ_crop_LU = GE_MJ_crop_livestock / livestock_unit,
    CP_kg_crop_LU = CP_kg_crop_livestock / livestock_unit,
    Ash_kg_crop_LU = Ash_kg_crop_livestock / livestock_unit)


# quality check ----
# je dois retrouver des totaux similaires aux théroques par LU
tmp_check_farm_id = sample(tmp_feed_by_livestock$farm_id,size = 25)
tmp = tmp_feed_by_livestock %>%
  filter(farm_id %in% tmp_check_farm_id) %>%
  group_by(farm_id,crop,feed_origin) %>%
  summarise(
    DM_kg_crop = sum(unique(DM_kg_crop)),
    DM_kg_crop_livestock = sum(DM_kg_crop_livestock),
    DM_kg_crop_LU = sum(DM_kg_crop_LU*livestock_unit),
    GE_MJ_crop = sum(unique(GE_MJ_crop)),
    GE_MJ_crop_livestock = sum(GE_MJ_crop_livestock),
    GE_MJ_crop_LU = sum(GE_MJ_crop_LU*livestock_unit)) %>%
  mutate(
    check1 = round(DM_kg_crop) == round(DM_kg_crop_livestock),
    check2 = round(DM_kg_crop) == round(DM_kg_crop_LU),
    check3 = round(DM_kg_crop_livestock) == round(DM_kg_crop_LU),
    check4 = round(GE_MJ_crop) == round(GE_MJ_crop_livestock),
    check5 = round(GE_MJ_crop) == round(GE_MJ_crop_LU),
    check6 = round(GE_MJ_crop_livestock) == round(GE_MJ_crop_LU)
  )
print("Everything's good if all TRUE")
print(table(tmp$check1))
print(table(tmp$check2))
print(table(tmp$check3))
print(table(tmp$check4))
print(table(tmp$check5))
print(table(tmp$check6))

## je retrouve bien les mêmes totaux OK


# check for organic farms
#tmp = tmp_feed_by_livestock %>% filter(farm_id %in% RICA_2020$farm_id[RICA_2020$AGBIO %in% c(2) & RICA_2020$OTEFDA %in% c(4500,4700,6184)])
# min 60% of grass or hay in feed mix
#tmp1= tmp %>% group_by(farm_id,feed_type) %>% summarise(DM_kg_feed_type = sum(DM_kg_crop_livestock)) %>% left_join(.,tmp %>% group_by(farm_id) %>% summarise(DM_kg_pfarm = sum(DM_kg_crop_livestock))) %>% mutate(ratio = DM_kg_feed_type / DM_kg_pfarm)
#length(unique(tmp1$farm_id[tmp1$feed_type == "feed_concent" & tmp1$ratio >= 0.6]))
#[1] 43 /246 farms have more than 60% concentrate in their feed mix while they should not among organic farms
#[1] 2 /77 farms have more than 60% concentrate in their feed mix while they should not among organic farms in OTEX c(4500,4700)
#[1] 9 /105 farms have more than 60% concentrate in their feed mix while they should not among organic farms in OTEX c(4500,4700,6184)
# min 60% of feed mix produced on farm
#tmp2= tmp %>% group_by(farm_id,feed_origin) %>% summarise(DM_kg_feed_origin = sum(DM_kg_crop_livestock)) %>% left_join(.,tmp %>% group_by(farm_id) %>% summarise(DM_kg_pfarm = sum(DM_kg_crop_livestock))) %>% mutate(ratio = DM_kg_feed_origin / DM_kg_pfarm)
#length(unique(tmp2$farm_id[tmp2$feed_origin == "feed_purchased" & tmp2$ratio >= 0.6]))
#[1] 57 /246 farms have more than 60% of their feed mix not produced on farm while they should
#[1] 4 / 77 farms have more than 60% of their feed mix not produced on farm while they should OTEX c(4500,4700)
#[1] 12 / 105 farms have more than 60% of their feed mix not produced on farm while they should OTEX c(4500,4700,6184)

# check : compare with theoretical ration
#tmp_ratio = feed_by_livestock %>% group_by(farm_id,code_livestock,livestock_unit,ref_DM_kg_LU) %>% summarise(obs_DM_kg_pLU = sum(DM_kg_crop_LU)) %>% mutate(ratio_obs_on_th = obs_DM_kg_pLU / ref_DM_kg_LU)
#quantile(tmp_ratio$ratio_obs_on_th,0.95)
# je pense qu'on est bien vu les quantiles du ratio obs/th : mediane = 1.2

##### Output ----

feed_by_livestock <- tmp_feed_by_livestock %>% ungroup()
# 4110 farms


rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])









