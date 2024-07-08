# CH4 emission from enteric fermentation

##### packages ----

library(readxl)

library(tibble)
library(dplyr)
library(tidyr)
library(tidyverse)
library(purrr)

##### Estimate livestock population and categories ----

if (my_DB == "RICA") {

  # transfert table
  tmp_TT_livestock <- read_xlsx("data_in/supp_data.xlsx", sheet ="TT_livestock") %>%
    rename(code_livestock = CODE6)

  # RICA data
  tmp_RICA_2020_anim <- RICA_2020_ani %>%
    # add units
    left_join(.,tmp_TT_livestock %>% select(CODE6,EFFEC6_unit,species)) %>%
    # summaries populations
    group_by(IDENT,CODE6) %>%
    # /!\ warning: animal population units differ between years
    summarise(EFFEC = sum(EFFEC6*EFFEC6_unit)) %>% ungroup() %>%
    filter(
      # remove livestock without population
      EFFEC >0
      # remove eggs & chicks
      & CODE6 != 984
      & CODE6 != 974
      # remove bees
      & CODE6 != 993
    ) %>%
    # 4148 farms
    rename(
      farm_id =IDENT,
      code_livestock = CODE6
      ) %>%
    # add species and UGB
    left_join(., tmp_TT_livestock) %>%
      # data are for one animal, so need to convert in livestock unit using LSU coefficient (see eurostat)
    mutate(
      livestock_unit = EFFEC * UGB # add UGB from tmp_TT_livestock
    )

}

if (my_DB == "FADN") {

  # transfert table
  tmp_FADN_code <- read_xlsx("data_in/supp_data.xlsx",sheet = "FADN_livestock_code")
  tmp_TT_livestock0 <- read_xlsx("data_in/supp_data.xlsx", sheet ="TT_livestock")
  tmp_TT_livestock <- tmp_TT_livestock0 %>%
    # add row for diary buffalo
    rbind(tmp_TT_livestock0 %>%
            filter(FADN_code_letter == "LCOWDAIR") %>%
            select(!all_of(colnames(tmp_FADN_code))) %>%
            cbind(tmp_FADN_code %>%
                    filter(FADN_code_letter == "LBUFDAIRPRS"))) %>%
    rename(code_livestock = FADN_code_letter) %>%
    # concatenate when multiple rows for one FADN code
    filter(!is.na(code_livestock)) %>%
    group_by(code_livestock) %>%
    summarise(
      species = unique(na.omit(species)),
      # for livestock categories, see Table 10.1 IPCC Guidelines 2019 Refinement
      IPCC_mix_cat = paste0(unique(na.omit(unlist(strsplit(IPCC_mix_cat,";")))),collapse = ";"),
      UGB = unique(na.omit(UGB))
      )

  # FADN data
  tmp_input <- FADN_18 %>%
    ## livestock unit
    select(ID, all_of(intersect(paste0(tmp_FADN_code$FADN_code_letter,"_ALU"),colnames(FADN_18)))) %>%
    pivot_longer(cols = !c(ID),names_to = "code_livestock",values_to = "livestock_unit") %>%
    rename(farm_id = ID) %>%
    mutate(code_livestock = gsub("_ALU","",code_livestock)) %>%
    # add species
    left_join(., tmp_TT_livestock)  %>%
    # add livestock unit coefficient to convert to animal
    left_join(.,tmp_TT_livestock0 %>%
                filter(!is.na(FADN_code_letter)) %>%
                select(FADN_code_letter,UGB) %>%
                distinct() %>%
                rename(code_livestock = FADN_code_letter)) %>%
    mutate(livestock_head = livestock_unit*UGB) %>%
    # remove livestock without population
    filter(livestock_unit >0 ) %>%
    ## milk production
    left_join(
      .,
      FADN_18 %>%
        # extract milk production
        mutate(code_livestock = "LCOWDAIR",
               prod_milk_kg = PMLKCOW_PRQ*10^3) %>%
        rename(farm_id = ID) %>%
        select(farm_id,code_livestock,prod_milk_kg))


}

##### Feed intake ----

tmp_feed_intake <- feed_by_livestock %>%
  # add livestock unit coefficient to convert to animal
  left_join(.,tmp_TT_livestock %>% select(code_livestock,UGB)) %>%
  # estimate feed intake per animal using livestock unit coefficient to convert to animal
  group_by(farm_id,code_livestock) %>%
  summarise(
    DM_kg_animal = sum(DM_kg_crop_LU*UGB),
    GE_MJ_animal = sum(GE_MJ_crop_LU*UGB),
    # add crude protein content
    CP_p100 = (sum(CP_kg_crop_LU*UGB) / DM_kg_animal)*100)


# CATTLE ----

### Tier 2
#### Step 1: Obtaining methane conversion factor (two methods: Ym and MY, we used the average estimation from both methods)
#### Step 2: Emission factor developpement
#### Step 3: Estimate total emissions

tmp_cattle_CH4_EF <- tmp_input %>%
  # filter cattle
  filter(species == "cattle") %>% # 3053 farms
  # add feed intake data
  inner_join(.,tmp_feed_intake) %>%
  # estimate CH4 emission from enteric fermentation
  rowwise() %>%
  mutate(

    ## Digestibility

    ## DE: digestibility of feed expressed as a fraction of gross energy (digestible energy/gross energy*100, i.e. DE%)
    ## Tables 10A.1 & 10A.2
    DE = case_when(
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) ~ 71,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("bulls_breed")) ~ 60,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("other_mature_cattle")) ~ 60, # as mature males
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("growing_cattle_postweaning")) ~ 65,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("growing_cattle")) ~ 65,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("calves_preweaning")) & code_livestock !=  932 ~ 95, # calves on milk
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("calves_preweaning")) & code_livestock == 932 ~ 73 # calves on forage (broutards only ???)
    ),
    # WIP !!! we could estimate DE = qté aliment - prod lait

    #  Ym = methane conversion factor, per cent of gross energy in feed converted to methane (Table 10.12)
    ## In Table 10.12, the MY of dairy cows is linked to annual milk production levels and to feed quantity and quality
    ## diary cow productivity in kg milk /head/yr-1
    Ym = case_when(
      # diary cattle
      ## WIP /!\ should differentiate between high producing cows depending on Neutral Detergent Fibre (NDF, % DMI)
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) & prod_milk_kg/livestock_head > 8500 ~ (5.7+6)/2,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) & prod_milk_kg/livestock_head <= 8500 & prod_milk_kg/livestock_head >= 5000 ~ 6.3,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) & prod_milk_kg/livestock_head < 5000 ~ 6.5,

      # other cattle
      species == "cattle"
      & !any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod"))
      & DE <= 62 ~ 7.0,
      species == "cattle"
      & !any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod"))
      & DE > 62 & DE <= 71 ~ 6.3,
      species == "cattle"
      & !any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod"))
      & DE >= 72 & DE < 75 ~ 4.0,
      species == "cattle"
      & !any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod"))
      & DE >= 75 ~ 3.0
    ),

    # EQUATION 10.21 METHANE EMISSION FACTORS FOR ENTERIC FERMENTATION FROM A LIVESTOCK CATEGORY
    ## EF = (GE *(Ym / 100)*365)/ 55.65
    ### EF = emission factor, kg CH4 head-1 yr-1
    ### GE = gross energy intake, MJ head-1 day-1
    ### Ym = methane conversion factor, per cent of gross energy in feed converted to methane
    ### The factor 55.65 (MJ/kg CH4) is the energy content of methane
    ### here GE_MJ_animal = gross energy intake, MJ head-1 yr-1
    EF_Ym = (GE_MJ_animal * (Ym/100)) / 55.65,

    #  MY = Methane yield, g CH4 kg DMI-1 (Table 10.12)
    ## In Table 10.12, the MY of dairy cows is linked to annual milk production levels and to feed quantity and quality
    ## diary cow productivity in kg milk /head/yr-1
    MY = case_when(
      # diary cattle
      ## WIP /!\ should differentiate between high producing cows depending on Neutral Detergent Fibre (NDF, % DMI)
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) & prod_milk_kg/livestock_head > 8500 ~ (19+20)/2,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) & prod_milk_kg/livestock_head <= 8500 & prod_milk_kg/livestock_head >= 5000 ~ 21,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) & prod_milk_kg/livestock_head < 5000 ~ 21.4,

      # other cattle
      species == "cattle"
      & !any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod"))
      & DE <= 62 ~ 23.3,
      species == "cattle"
      & !any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod"))
      & DE > 62 & DE <= 71 ~ 21,
      species == "cattle"
      & !any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod"))
      & DE >= 72 & DE < 75 ~ 13.6,
      species == "cattle"
      & !any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod"))
      & DE >= 75 ~ 10
      ),

    # EQUATION 10.21A (NEW) METHANE EMISSION FACTORS FOR ENTERIC FERMENTATION FROM A LIVESTOCK CATEGORY
    ## EF = DMI * (MY/1000) * 365
    ### EF = emission factor, kg CH4 head-1 yr-1
    ### DMI = kg DMI day-1
    ### MY = Methane yield, g CH4 kg DMI-1 (Table 10.12)
    ### 365 = days per year
    ### 1000 = conversion from g CH4 to kg CH4
    ### here DM_kg_animal = kg DMI head-1 yr-1
    EF_MY = DM_kg_animal * (MY / 1000),

    # "To estimate total emissions, the selected emission factors are multiplied by the associated animal population [...]. As described above under Tier 1, the emissions estimates should be reported in gigagrams (Gg)." (IPCC Guidelines 2019 Refinements)
    ## CH4_EF = kg CH4 yr-1
    CH4_EF_Ym = livestock_head * EF_Ym,
    CH4_EF_MY = livestock_head * EF_MY,
    CH4_EF = (CH4_EF_Ym+CH4_EF_MY)/2
    ) %>%
  ungroup()

## check ----

# compare with Tier 1 method
tmp_check_tier1 <- tmp_input %>%
  filter(species == "cattle") %>%
  mutate(
    EF = case_when(
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) ~ 126,
      .default = 52
    ),
    CH4_tier1 = livestock_head * EF
  )

quantile(tmp_check_tier1$CH4_tier1)
quantile(tmp_cattle_CH4_EF$CH4_EF)
## WIP bof les quantiles

# SHEEP ----

### Tier 2
#### Step 1: Obtaining methane conversion factor (one methods: Ym)
#### Step 2: Emission factor developpement
#### Step 3: Estimate total emissions

tmp_sheep_CH4_EF <- tmp_input %>%
  # filter sheep
  filter(species == "sheep") %>%
  # add feed intake data
  inner_join(.,tmp_feed_intake) %>%
  # estimate CH4 emission from enteric fermentation
  rowwise() %>%
  mutate(

    #  Ym = methane conversion factor, per cent of gross energy in feed converted to methane (Table 10.13)
    Ym = 6.7,

    # EQUATION 10.21 METHANE EMISSION FACTORS FOR ENTERIC FERMENTATION FROM A LIVESTOCK CATEGORY
    ## EF = (GE *(Ym / 100)*365)/ 55.65
    ### EF = emission factor, kg CH4 head-1 yr-1
    ### GE = gross energy intake, MJ head-1 day-1
    ### Ym = methane conversion factor, per cent of gross energy in feed converted to methane
    ### The factor 55.65 (MJ/kg CH4) is the energy content of methane
    ### here GE_MJ_animal = gross energy intake, MJ head-1 yr-1
    EF_Ym = (GE_MJ_animal * (Ym/100)) / 55.65,


    # "To estimate total emissions, the selected emission factors are multiplied by the associated animal population [...]. As described above under Tier 1, the emissions estimates should be reported in gigagrams (Gg)." (IPCC Guidelines 2019 Refinements)
    ## CH4_EF = kg CH4 yr-1
    CH4_EF = livestock_head * EF_Ym,
  ) %>%
  ungroup()

# WIP quality check
quantile(tmp_sheep_CH4_EF$CH4_EF)

# GOATS ----

### Tier 2
#### Step 1: Obtaining methane conversion factor (one methods: Ym)
#### Step 2: Emission factor developpement
#### Step 3: Estimate total emissions

tmp_goats_CH4_EF <- tmp_input %>%
  # filter sheep
  filter(species == "goats") %>%
  # add feed intake data
  inner_join(.,tmp_feed_intake) %>%
  # estimate CH4 emission from enteric fermentation
  rowwise() %>%
  mutate(

    #  Ym = methane conversion factor, per cent of gross energy in feed converted to methane (Table 10.13)
    Ym = 5.5,

    # EQUATION 10.21 METHANE EMISSION FACTORS FOR ENTERIC FERMENTATION FROM A LIVESTOCK CATEGORY
    ## EF = (GE *(Ym / 100)*365)/ 55.65
    ### EF = emission factor, kg CH4 head-1 yr-1
    ### GE = gross energy intake, MJ head-1 day-1
    ### Ym = methane conversion factor, per cent of gross energy in feed converted to methane
    ### The factor 55.65 (MJ/kg CH4) is the energy content of methane
    ### here GE_MJ_animal = gross energy intake, MJ head-1 yr-1
    EF_Ym = (GE_MJ_animal * (Ym/100)) / 55.65,


    # "To estimate total emissions, the selected emission factors are multiplied by the associated animal population [...]. As described above under Tier 1, the emissions estimates should be reported in gigagrams (Gg)." (IPCC Guidelines 2019 Refinements)
    ## CH4_EF = kg CH4 yr-1
    CH4_EF = livestock_head * EF_Ym,
  ) %>%
  ungroup()

# WIP quality check
quantile(tmp_goats_CH4_EF$CH4_EF)

##### Output ----

CH4_EF <- Reduce(rbind,list(
  tmp_cattle_CH4_EF %>% select(farm_id,code_livestock,livestock_unit,CH4_EF),
  tmp_sheep_CH4_EF %>% select(farm_id,code_livestock,livestock_unit,CH4_EF),
  tmp_goats_CH4_EF %>% select(farm_id,code_livestock,livestock_unit,CH4_EF))) %>%
  ungroup()

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])

