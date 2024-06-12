# N excretion from livestock

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
    # add crude protein content
    CP_p100 = (sum(CP_kg_crop_LU*UGB) / DM_kg_animal)*100)


# CATTLE ----

# calculate N excretion rates from IPCC N intake estimation equations ----
tmp_cattle_Nin <- tmp_input %>%
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

    #  MY = Methane yield, g CH4 kg DMI-1 (Table 10.12)
    ## In Table 10.12, the MY of dairy cows is linked to annual milk production levels and to feed quantity and quality
    ## diary cow productivity in kg milk /head/yr-1
    MY = case_when(
      # WIP /!\ should differentiate between high producing cows depending on Neutral Detergent Fibre (NDF, % DMI)
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) & prod_milk_kg/livestock_head > 8500 ~ (19+20)/2,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) & prod_milk_kg/livestock_head <= 8500 & prod_milk_kg/livestock_head >= 5000 ~ 21,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) & prod_milk_kg/livestock_head < 5000 ~ 21.4,


      # WIPPP &
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod"))
    )

    # EQUATION 10.21A (NEW) METHANE EMISSION FACTORS FOR ENTERIC FERMENTATION FROM A LIVESTOCK CATEGORY
    ## EF = DMI * (MY/1000) * 365
    ### EF = emission factor, kg CH4 head-1 yr-1
    ### DMI = kg DMI day-1
    ### MY = Methane yield, g CH4 kg DMI-1 (Table 10.12)
    ### 365 = days per year
    ### 1000 = conversion from g CH4 to kg CH4
    EF = DM_kg_animal * (MY / 1000)







    # N_intake from DMI
    ## daily N consumed per animal of category T, kg N animal-1 day-1, per growth stage-1 “i" when applicable
    ## DMI: dry matter intake, kg DM animal-1 day-1
    ## CP_p100: percent crude protein in dry matter, %
    ## Equation 10.32
    Nin_DMI = (DM_kg_animal/365)*((CP_p100/100)/6.25),


    # N_intake estimate
    ## To estimate N intake when no DMI data are available, IPCC Guidelines provided a full set of equations

    ## Net Energy for Maintenance NE_m

    ## Cfi: a coefficient which varies for each animal category, MJ day-1 kg-1
    ## Table 10.4
    Cfi = case_when(
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) ~ 0.386,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("bulls_breed")) ~ 0.370,
      .default = 0.322
    ),

    ## Weight: live-weight of animal, kg
    ## Table 10A.1 & Table 10A.2 for "Regions"="Western Europe" for "calves_preweaning"
    ## Source: FRA_2023_2020_13042023_110851.xlsx from fra-2023-crf-25apr23_AR5
    Weight = case_when( # !!! use average body weight as we don't have live-weight of animals
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) ~ 676.08,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("bulls_breed")) ~ 676.08,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("other_mature_cattle")) ~ 676.08,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("growing_cattle_postweaning")) ~ 439.92,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("growing_cattle")) ~ 439.92,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("calves_preweaning")) ~ 230
    ),

    ## NE_m: net energy required by the animal for maintenance, MJ day-1
    ## Equation 10.3
    NE_m = Cfi*(Weight^0.75),

    ## Net Energy for Activity NE_a

    ## Ca: coefficient corresponding to animal’s feeding situation, dimensionless
    ## Table 10.5
    Ca = case_when( # !!! choice:
      # we considered that cattle in farms with permanent meadow are grazing
      farm_id %in% feed_grassland$farm_id[feed_grassland$crop %in% c("341","CGRSXRG")] ~ 0.17,
      # we considered that cattle in farms with rangelands are grazing large areas
      farm_id %in% feed_grassland$farm_id[feed_grassland$crop %in% c("342","CRG")] ~ 0.36,
      # we considered that cattle in farms without permanent meadow or rangelands are fed in stall
      .default = 0
    ),
    ## NE_a: net energy for animal activity, MJ day-1
    ## Equation 10.4
    NE_a = Ca*NE_m,
    ## /!\ voir moyenne fr +/- cahier charge labels | variable RA

    ## Net Energy for Growth NE_g

    ## BW: the average live body weight of the animals in the population, kg
    ## Tables 10A.1 & 10A.2
    BW = case_when(
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) ~ 600,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("bulls_breed")) ~ 600,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("other_mature_cattle")) ~ 600,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("growing_cattle_postweaning")) ~ 400,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("growing_cattle")) ~ 400,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("calves_preweaning")) ~ 230
    ),
    ## C: a coefficient with a value of 0.8 for females, 1.0 for castrates and 1.2 for bulls
    C =
      case_when(
        any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("bulls_breed")) ~ 1.2,
        any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod","other_mature_cattle","growing_cattle")) ~ 0.8,
        .default = 1
      ),
    ## MW: the mature body weight of an adult animal individually, mature females, mature males and steers) in moderate body condition, kg
    MW = 600, # choice: mature weight = 600 kg as in Tables 10A.1 & 10A.2
    ## WG: the average daily weight gain of the animals in the population, kg day-1
    WG = case_when(
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) ~ 0,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("bulls_breed")) ~ 0,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("other_mature_cattle")) ~ 0,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("growing_cattle_postweaning")) ~ 0.4,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("growing_cattle")) ~ 0.4,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("calves_preweaning")) ~ 0.3
    ),
    ## NE_g: net energy needed for growth, MJ day-1
    ## Equation 10.6
    NE_g = 22.02*((BW/(C*MW))^0.75)*(WG^1.097),

    ## Net Energy for Lactation NE_l

    ## net energy for lactation, MJ day-1
    ## Equation 10.8 & Table 10A.1
    NE_l = case_when(
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) ~ 20.3*(1.47+0.40*4.2),
      .default = 0
    ),
    # /!\ milk production !!!

    ## Net Energy for Work NE_work
    NE_work = 0,# choice: omit work

    ## Net Energy for Pregnancy NE_p
    ## C_preg: pregnancy coefficient
    ## Table 10.7 & /!\ Table 10A.1 mature_dairy_cattle = 90% of indiv. pregnant
    C_preg = case_when(
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) ~ 0.10,
      .default = 0
    ),
    ## NE_p: net energy required for pregnancy, MJ day-1
    ## Equation 10.13
    NE_p = C_preg * NE_m,


    ## Ratio of net energy for maintenance REM

    ## REM: ratio of net energy available in diet for maintenance to digestible energy
    ## Equation 10.14
    REM = 1.123-(4.092*(10^-3)*DE)+(1.126*(10^-5)*(DE^2))-(25.4/DE),

    ## Ratio of net energy for growth REG

    # /!\ choice: even for non-growing cattle? seems yes as in Table 10.3
    ## REG: ratio of net energy available for growth in a diet to digestible energy consumed
    ## Equation 10.15
    REG = 1.164 - (5.16*(10^-3)*DE)+(1.308*(10^-5)*(DE^2))-(37.4/DE),

    ## Gross Energy GE

    ## GE: gross energy, MJ day-1
    ## Equation 10.16
    GE = (((NE_m+NE_a+NE_l+NE_work+NE_p)/REM) + (NE_g/REG))/(DE/100),

    ## CP
    ## CP: percent crude protein in dry matter for growth stage “i”
    ## Tables 10A.1 & 10A.2
    CP = case_when(
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("cows_milk_prod")) ~ 16.1,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("bulls_breed")) ~ 14.7,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("other_mature_cattle")) ~ 14.7, # as mature males
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("growing_cattle_postweaning")) ~ 16.5,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("growing_cattle")) ~ 16.5,
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("calves_preweaning")) & code_livestock != 932  ~ 17.1, # calves on milk
      any(unlist(strsplit(IPCC_mix_cat,";")) %in% c("calves_preweaning")) & code_livestock == 932 ~ 16.5 # calves on forage (broutards only ???)
    ),

    ## N_intake estimate
    ## N_intake: daily N consumed per animal of category T, kg N animal-1 day-1, per growth stage-1 “i" when applicable
    ## Equation 10.32
    Nin_estim = (GE/18.45)*((CP/100)/6.25)
    )


# quality check
## IPCC Guidelines 2019, equation 10.18 et table 10.8 => compare DMI kg day-1
tmp = tmp_cattle_Nin %>%
  mutate(DMI_pday = DM_kg_animal / 365,
         DMI_10.17 = BW^0.75 * ((0.0582*6 - 0.00266*6^2 - 0.1128 )/(0.239 * 6)),
         DMI_10.18 = BW^0.75 * ((0.0582*6 - 0.00266*6^2 - 0.0869 )/(0.239 * 6)),
         DMI_10.18a = 3.83+0.0143*BW*0.96)
tmp %>%
  group_by(species) %>%
  reframe(DMI_pday = quantile(DMI_pday,c(0.1,0.25,0.5,0.75,0.9),na.rm = T),
          BW = quantile(BW,c(0.1,0.25,0.5,0.75,0.9)),
            DMI_10.17 = quantile(DMI_10.17,c(0.1,0.25,0.5,0.75,0.9)),
            DMI_10.18 = quantile(DMI_10.18,c(0.1,0.25,0.5,0.75,0.9)),
            DMI_10.18a = quantile(DMI_10.18a,c(0.1,0.25,0.5,0.75,0.9)))
## les quantiles semblent pas mal => OK

## compare Nin_DMI & Nin_estim
tmp_cattle_Nin %>%
  group_by(species) %>%
  reframe(Nin_DMI = quantile(Nin_DMI,c(0.1,0.25,0.5,0.75,0.9),na.rm = T),
          Nin_estim = quantile(Nin_estim,c(0.1,0.25,0.5,0.75,0.9),na.rm = T))
## quantiles OK

# calculate average N excretion rates from both methods ----

## estimate milk production for diary cows

if (my_DB == "RICA") {

  ### RICA data on animal production
  tmp_RICA_2020_pan <- read_delim(
    "//casd.fr/casdfs/Projets/EVAPDRH/Data/RICA_RICA_2020/RICA2020_tables_CSV/pan20.csv",
    delim = ";", escape_double = FALSE, trim_ws = TRUE,
    col_types = cols(IDENT = col_character()))
  ### select milk data
  tmp_milk <- tmp_RICA_2020_pan %>%
    filter(CODE7 %in% c("021", "022", "031", "032")) %>%
    group_by(IDENT) %>%
    summarise(
      MILK_total = sum(QPROD7*10^2)
    ) %>% # 1427 farms
    # add diary cow population
    inner_join(.,tmp_RICA_2020_anim %>% filter(code_livestock == 929) %>% group_by(IDENT) %>% summarise(EFFEC = sum(EFFEC))) %>%
    # calculate MILK kg animal-1 day-1
    mutate(MILK = (MILK_total / EFFEC)/365)
  # quality check: cf. Table 10A.1 => range for MILK 7 to 23 : ok
  #quantile(tmp_milk$MILK,c(0.1,0.25,0.5,0.75,0.9))

}

if (my_DB == "FADN") {

  # FADN data
  ### select milk data
  tmp_milk <- FADN_18 %>%
    select(ID,PMLKCOW_PRQ) %>%
    filter(PMLKCOW_PRQ >0) %>%
    mutate(code_animprod = "PMLKCOW") %>%
    rename(farm_id = ID,
           MILK_total = PMLKCOW_PRQ) %>%
    # add diary cow population
    inner_join(.,tmp_cattle_Nin %>%
                 filter(code_livestock == "LCOWDAIR") %>%
                 select(farm_id,code_livestock,livestock_unit)) %>%
    # calculate MILK kg animal-1 day-1
    ## for diary cows, livestock unit coefficient =1 thus LU == number of animal
    mutate(MILK = (MILK_total / livestock_unit)/365)




}


### MILK_PR: percent of protein in milk, calculated as 1.9+0.4.%Fat, where %Fat is an input assumed to be 4% (see IPCC Guidelines Equation 10.33)
tmp_MILK_PR <- 1.9+0.4 *0.04

## N excretion
tmp_cattle_Nexc <- tmp_cattle_Nin %>%
  # add milk data
  left_join(.,tmp_milk %>% select(!livestock_unit)) %>%
  mutate(
    # N retention
    ## daily N retained per animal of category T, kg N animal-1 day-1
    ## Equation 10.33
    N_retention = case_when(
      # NB: 53 farms with dairy cows have not registered any milk production
      IPCC_mix_cat == "cows_milk_prod" & MILK >0 ~ ( ( MILK * ( tmp_MILK_PR /100) ) /6.38 ),
      NE_g > 0 ~ ((WG * ( (268-(7.03*NE_g/WG))/1000) )/6.25),
      .default = 0
    ),

    # ANNUAL N EXCRETION RATES, OPTION 2 (TIER 2)
    ## N_ex: annual N excretion rates, kg N animal-1 yr-1
    ## Equation 10.31
    Nex_estim = (Nin_estim - N_retention) * 365,
    Nex_DMI = (Nin_DMI - N_retention) * 365,
    Nex = ((Nex_estim+Nex_DMI)/2),

    # estimate total amount of N excreted, kg N yr-1
    Nex_total = Nex * (livestock_unit/UGB)
  )

# quality check => OK
## Table 10.19: N excretion rate for dairy cows in Western EU = 0.54 kg N / 1000kg animal mass / day
## Table 10A.1: Weight dairy cows in Western EU = 600 kg
## => 118 kg N / dairy cow / an
tmp_cattle_Nexc %>%
  group_by(code_livestock) %>%
  reframe(Nex_DMI = quantile(Nex_DMI,c(0.1,0.25,0.5,0.75,0.9)),
          Nex_estim = quantile(Nex_estim,c(0.1,0.25,0.5,0.75,0.9)),
          Nex = quantile(Nex,c(0.1,0.25,0.5,0.75,0.9))) %>%
  filter(code_livestock == "LCOWDAIR")
## mediane OK

##### SWINE ----

# see IPCC Guidelines 2023

## N intake
tmp_swine_Nin <- tmp_input %>%
  # filter swine
  filter(species == "swine") %>% # 446 farms
  # add feed intake data
  inner_join(.,tmp_feed_intake) %>% # 443 farms
  # estimate N intake
  mutate(
    # N_intake from DMI
    ## daily N consumed per animal of category T, kg N animal-1 day-1, per growth stage-1 “i" when applicable
    ## DMI: dry matter intake, kg DM animal-1 day-1
    ## CP_p100: percent crude protein in dry matter, %
    ## Equation 10.32A
    Nin_DMI = (DM_kg_animal/365)*((CP_p100/100)/6.25))

## N excretion
tmp_swine_Nexc <- tmp_swine_Nin %>%

    mutate(
    # N retention
    ## Default N retention fraction
    ## Table 10.20
      N_retention_frac = 0.30,
    # ANNUAL N EXCRETION RATES, OPTION 1 (TIER 2)
    ## N_ex: annual N excretion rates, kg N animal-1 yr-1
    ## Equation 10.31
    Nex = Nin_DMI * (1- N_retention_frac) * 365,

    # estimate total amount of N excreted, kg N yr-1
    Nex_total = Nex * (livestock_unit/UGB)
  )

# quality check => bof assez bas
## Table 10.19: N excretion rate for swine in Western EU = 0.65 kg N / 1000kg animal mass / day
## Table 10A.5: Weight swine in Western EU = 76 kg
## => 18 kg N / animal / an
## swine livestock unit = 0.3
## => 60 kg N / livestock unit / an (range from 36 to 72) with 0.65/1000*76/0.3*365
quantile(tmp_swine_Nexc$Nex,c(0.1,0.25,0.5,0.75,0.9))

# POULTRY ----

# see IPCC Guidelines 2023

## N intake
tmp_poultry_Nin <- tmp_input %>%
  # filter poultry
  filter(species == "poultry") %>% # 667 farms
  # add feed intake data
  inner_join(.,tmp_feed_intake) %>% # 634 farms
  # estimate N intake
  mutate(

    # N_intake from DMI
    ## daily N consumed per animal of category T, kg N animal-1 day-1, per growth stage-1 “i" when applicable
    ## DMI: dry matter intake, kg DM animal-1 day-1
    ## CP_p100: percent crude protein in dry matter, %
    ## Equation 10.32A
    Nin_DMI = (DM_kg_animal/365)*((CP_p100/100)/6.25))

## N excretion
tmp_poultry_Nexc <- tmp_poultry_Nin %>%

  mutate(
    # N retention
    ## Default N retention fraction
    ## Table 10.20
    N_retention_frac = 0.30,
    # ANNUAL N EXCRETION RATES, OPTION 1 (TIER 2)
    ## N_ex: annual N excretion rates, kg N animal-1 yr-1
    ## Equation 10.31
    Nex = Nin_DMI * (1- N_retention_frac) * 365,

    # estimate total amount of N excreted, kg N yr-1
    Nex_total = Nex * (livestock_unit/UGB)
    )

# quality check => OK !!!
## Table 10.19: N excretion rate for chickens in Western EU = 0.99 kg N / 1000kg animal mass / day (range from 0.58 to 1.14)
## Table 10A.5: Weight chickens in Western EU = 1.4 kg
## => 0.5 kg N / animal / an (range from 0.3 to 0.6)
## poultry livestock unit = 0.007 for broilers and 0.014 for laying hens => mean = 0.0105
## => 48 kg N / livestock unit / an (range from 36 to 72) with 0.99/1000*1.4/0.0105*365
quantile(tmp_poultry_Nexc$Nex,c(0.1,0.25,0.5,0.75,0.9))

# SHEEP ----

# see IPCC Guidelines 2023

## N intake
tmp_sheep_Nin <- tmp_input %>%
  # filter sheep
  filter(species == "sheep") %>% # 625 farms
  # add feed intake data
  left_join(.,tmp_feed_intake) %>%
  # estimate N intake
  mutate(

    # N_intake from DMI
    ## daily N consumed per animal of category T, kg N animal-1 day-1, per growth stage-1 “i" when applicable
    ## DMI: dry matter intake, kg DM animal-1 day-1
    ## CP_p100: percent crude protein in dry matter, %
    ## Equation 10.32A
    Nin_DMI = (DM_kg_animal/365)*((CP_p100/100)/6.25))

## N excretion
tmp_sheep_Nexc <- tmp_sheep_Nin %>%

  mutate(
    # N retention
    ## Default N retention fraction
    ## Table 10.20
    N_retention_frac = 0.10,
    # ANNUAL N EXCRETION RATES, OPTION 1 (TIER 2)
    ## N_ex: annual N excretion rates, kg N animal-1 yr-1
    ## Equation 10.31
    Nex = Nin_DMI * (1- N_retention_frac) * 365,

    # estimate total amount of N excreted, kg N yr-1
    Nex_total = Nex * (livestock_unit/UGB)
    )

# quality check => OK !!!
## Table 10.19: N excretion rate for sheep in Western EU = 0.36 kg N / 1000kg animal mass / day
## Table 10A.5: Weight sheep in Western EU = 40 kg
## => 5 kg N / animal / an
quantile(tmp_sheep_Nexc$Nex,c(0.1,0.25,0.5,0.75,0.9),na.rm = T)

# GOATS ----

# see IPCC Guidelines 2023

## N intake
tmp_goats_Nin <- tmp_input %>%
  # filter goats
  filter(species == "goats") %>% # 185 farms
  # add feed intake data
  left_join(.,tmp_feed_intake) %>%
  # estimate N intake
  mutate(

    # N_intake from DMI
    ## daily N consumed per animal of category T, kg N animal-1 day-1, per growth stage-1 “i" when applicable
    ## DMI: dry matter intake, kg DM animal-1 day-1
    ## CP_p100: percent crude protein in dry matter, %
    ## Equation 10.32A
    Nin_DMI = (DM_kg_animal/365)*((CP_p100/100)/6.25))

## N excretion
tmp_goats_Nexc <- tmp_goats_Nin %>%

  mutate(
    # N retention
    ## Default N retention fraction
    ## Table 10.20
    N_retention_frac = 0.10,
    # ANNUAL N EXCRETION RATES, OPTION 1 (TIER 2)
    ## N_ex: annual N excretion rates, kg N animal-1 yr-1
    ## Equation 10.31
    Nex = Nin_DMI * (1- N_retention_frac) * 365,

    # estimate total amount of N excreted, kg N yr-1
    Nex_total = Nex * (livestock_unit/UGB)
    )

# quality check => OK !!!
## Table 10.19: N excretion rate for goats in Western EU = 0.46 kg N / 1000kg animal mass / day
## Table 10A.5: Weight goats in Western EU = 40 kg
## => 7 kg N / animal / an
quantile(tmp_goats_Nexc$Nex,c(0.1,0.25,0.5,0.75,0.9),na.rm = T)

##### Output ----

N_excr <- Reduce(rbind,list(
  tmp_cattle_Nexc %>% select(farm_id,code_livestock,livestock_unit,Nin_DMI,Nex,Nex_total),
  tmp_swine_Nexc %>% select(farm_id,code_livestock,livestock_unit,Nin_DMI,Nex,Nex_total),
  tmp_poultry_Nexc %>% select(farm_id,code_livestock,livestock_unit,Nin_DMI,Nex,Nex_total),
  tmp_sheep_Nexc %>% select(farm_id,code_livestock,livestock_unit,Nin_DMI,Nex,Nex_total),
  tmp_goats_Nexc %>% select(farm_id,code_livestock,livestock_unit,Nin_DMI,Nex,Nex_total))) %>%
  ungroup()

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])

