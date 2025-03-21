# N2O emissions from manure management

##### packages ----

library(readxl)

library(tibble)
library(dplyr)
library(tidyr)
library(tidyverse)
library(purrr)
library(stringr)

##### Estimate livestock population and categories ----

if (my_DB == "RICA") {

  # transfert table
  tmp_TT_livestock <- read_xlsx("data_in/supp_data.xlsx", sheet ="TT_livestock") %>%
    rename(code_livestock = CODE6)

  # RICA data
  tmp_RICA_2020_anim <- RICA_2020_ani %>%
    # add units
    left_join(.,tmp_TT_livestock %>% dplyr::select(CODE6,EFFEC6_unit,species)) %>%
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
            dplyr::select(!all_of(colnames(tmp_FADN_code))) %>%
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
      UGB = unique(na.omit(UGB)),
      UNFCCC_cat = unique(na.omit(UNFCCC_cat))
      )
  tmp_country_codes <- read_excel("data_in/country_codes.xlsx")

  # FADN data
  tmp_input <- FADN_18 %>%
    ## livestock unit
    dplyr::select(ID,COUNTRY, all_of(intersect(paste0(tmp_FADN_code$FADN_code_letter,"_ALU"),colnames(FADN_18)))) %>%
    pivot_longer(cols = !c(ID,COUNTRY),names_to = "code_livestock",values_to = "livestock_unit") %>%
    rename(farm_id = ID) %>%
    mutate(code_livestock = gsub("_ALU","",code_livestock)) %>%
    # add species, livestock unit coefficient to convert to animal
    left_join(., tmp_TT_livestock)  %>%
    # remove livestock without population
    filter(livestock_unit >0 ) %>%
    # add country code
    left_join(.,tmp_country_codes,by = c("COUNTRY" = "country_FADN"))

}


# Manure Management System by animal category and by country ----

library(readxl)
library(zip)

tmp_data_year = "2018"

# empty tibble
tmp_MMS_AWMS <- tibble()

for (loop_tmp_zip in list.files("data_raw/UNFCCC_ghg_inventories/")) {

  loop_tmp_files = zip_list(paste0("data_raw/UNFCCC_ghg_inventories/",loop_tmp_zip))
  loop_tmp_data_country = str_sub(loop_tmp_zip,1,3)
  print(loop_tmp_data_country)
  loop_tmp_unzip <- tempfile()
  loop_tmp_unzip <- utils::unzip(paste0("data_raw/UNFCCC_ghg_inventories/",loop_tmp_zip),
                                 loop_tmp_files$filename[grep(tmp_data_year,loop_tmp_files$filename)])

  # MMS allocation
  loop_tmp_range <- read_excel(loop_tmp_unzip,sheet = "Table3.B(a)s2", range = "A10:M39",col_names = F)
  loop_tmp_colnames <- c(1:13) %>%
    purrr::set_names(.,c("Option","Animal_cat","Indicator","climate_region",
                         gsub(" ","_",colnames(read_excel(loop_tmp_unzip,sheet = "Table3.B(a)s2", range = "E7:M7",col_names = T)))))
  loop_tmp_MMS <- loop_tmp_range %>%
    mutate(across(c(5:13), function(x) gsub("[a-zA-Z]+",NA,x))) %>%
    mutate(across(c(5:13), as.numeric)) %>%
    rename(.,all_of(loop_tmp_colnames)) %>%
    mutate(
      Indicator = rep(unique(na.omit(Indicator)),
                      each = length(unique(na.omit(climate_region))),
                      times = (length(unique(na.omit(Animal_cat))))),
      Animal_cat = rep(unique(na.omit(Animal_cat)),
                       each = (length(unique(na.omit(climate_region)))) * length(unique(na.omit(Indicator)))),
      species = case_when(
        grepl("(?i)cattle|cow|calve|bull",Animal_cat) ~ "cattle"
      ),
      UNFCCC_cat = case_when(
        species == "cattle" & grepl("(?i)non-dairy|non-lactating",Animal_cat,) ~ "other_mature_cattle",
        species == "cattle" & grepl("(?i)dairy",Animal_cat) ~ "dairy_cattle",
        species == "cattle" & grepl("(?i)growing|calve|suckler|young",Animal_cat) ~ "growing_cattle",
        species == "cattle" & grepl("(?i)mature|bull|other",Animal_cat) ~ "other_mature_cattle")
      ) %>%
    mutate(COUNTRY = case_when(
      loop_tmp_data_country == "dnm" ~ "DAN",
      loop_tmp_data_country == "dnk" ~ "DAN_overseas",
      loop_tmp_data_country == "fra" ~ "FRA_overseas",
      loop_tmp_data_country == "frk" ~ "FRA",
      .default = toupper(loop_tmp_data_country)
    )) %>%
    filter(Indicator == "Allocation (%)") %>%
    pivot_longer(cols = gsub(" ","_",colnames(read_excel(loop_tmp_unzip,sheet = "Table3.B(a)s2", range = "E7:M7",col_names = T))),
                 names_to = "MMS",values_to = "AWMS") %>%
    filter(AWMS >0)

  # extract tibble
  tmp_MMS_AWMS <- tmp_MMS_AWMS %>%
    rbind(.,loop_tmp_MMS)

  unlink(loop_tmp_unzip)
  rm(list = names(.GlobalEnv)[grep("loop",names(.GlobalEnv))])

}

# WIP for Sweden, less than 100% AWMS
# tmp_MMS_AWMS %>% group_by(COUNTRY,UNFCCC_cat) %>% summarise(sum = sum(AWMS)) %>% filter(round(sum) != 100)
# WIP adapt for overseas territories

# Feed intake ----

tmp_feed_intake <- feed_by_livestock %>%
  # add livestock unit coefficient to convert to animal
  left_join(.,tmp_TT_livestock %>% dplyr::select(code_livestock,UGB)) %>%
  # estimate feed intake per animal using livestock unit coefficient to convert to animal
  group_by(farm_id,code_livestock) %>%
  summarise(
    DM_kg_animal = sum(DM_kg_crop_LU*UGB),
    GE_MJ_animal = sum(GE_MJ_crop_LU*UGB),
    # add crude protein content
    CP_pc = (sum(CP_kg_crop_LU*UGB) / DM_kg_animal)*100,
    Ash_pc = (sum(Ash_kg_crop_LU*UGB) / DM_kg_animal)*100)

# CATTLE ----

## TIER 3
### Step 1: Collect population data based on the Livestock Population Characterization (see Section 10.2).
### Step 2: Identify default (Table 10A.5) [or collect country-specific [...] typical animal mass (TAM) values.
#### Calculate volatile solid excretion according to Equation 10.22a or develop country- specific volatile solid excretion rates according to Equation 10.24.
### Step 3: Collect country-specific information on manure management system methods and develop country-specific manure management system fractions or use default manure storage fractions presented in Annex Tables 10A.6 to Tables 10A.9.
### Step 4: Identify either default emission factors Table 10.14 or build country-specific emission factors for each livestock subcategory based on climate zones and manure management system fractions. • Tier 1: Identify default values (Table 10.14) for emission factors for each livestock category in terms of grams of methane per kg VS per year for the appropriate climate zone and productivity class if using advanced Tier 1a. • Tier 2: Select local manure management specific methane conversion factors (MCF’s, Table 10.17) for different climate zones and the animal categories specific maximum methane producing capacity (B0).
### Step 5: Calculate methane emission for each livestock subcategory. According to Equation 10.23, for each livestock category and climate zone calculate the country-specific emission factor based on the country-specific or default quantity of volatile solids (Step 2 ), the manure management system fraction (AWMS) and the MCF and B0 factors ( Step 4); To estimate total emissions, the country specific emission factor is then multiplied by the population number (Step 1).

# Animal waste management system (manure management systems) data have been collected for regions and countries by the FAO and average manure fractions treated by different management systems are presented in Annex 10A.2, Tables 10A.6 to 10A.9.

tmp_cattle_N2O_MM <- tmp_input %>%
  # filter cattle
  filter(species == "cattle") %>%
  # add country
  inner_join(.,FADN_18 %>% dplyr::select(ID,COUNTRY) %>% rename(farm_id = ID)) %>%
  # add feed intake data
  inner_join(.,tmp_feed_intake) %>%
  # add livestock N excretion
  inner_join(.,N_excr %>% select(farm_id,code_livestock,Nex)) %>%
  # add AWMS (  ## AWMS_{(T,S)} = fraction of total annual nitrogen excretion for each livestock species/category T that is managed in manure management system S in the country, dimensionless)
  # estimate sum by category of livestock (T) for EQUATION 10.25 (UPDATED) DIRECT N2O EMISSIONS FROM MANURE MANAGEMENT
  ## N_T = number of head of livestock species/category T in the country
  ## Nex_T = annual average N excretion per head of species/category T in the country, in kg N animal-1 yr-1
  ## AWMS_{(T,S)} = fraction of total annual nitrogen excretion for each livestock species/category T that is managed in manure management system S in the country, dimensionless
  N2O_D_MM_sum_T = ,




    # add country specific values for:
    ## AWMS = fraction of livestock category T's manure handled using animal waste management system S in climate region k, dimensionless
  # add VS, Bo, MCF and AWMS by livestock cat and country WIP and climate region
  left_join(.,tmp_MMS_Bo %>%
              dplyr::filter(species == "cattle") %>%
              dplyr::group_by(UNFCCC_cat,COUNTRY) %>%
              summarise(
                VS = weighted.mean(`VS(2)_daily_excretion_(average)`,alloc_by_climate_region),
                Bo = weighted.mean(`CH4_producing_potential_(Bo)(2)_(average)`,alloc_by_climate_region)
              )
            ) %>%
  # estimate weigthed sum of MCF
  ## MCF = methane conversion factors for each manure management system S by climate region k, percent
  ## AWMS = fraction of livestock category T's manure handled using animal waste management system S in climate region k, dimensionless
  left_join(.,
            tmp_MMS_AWMS %>%
              dplyr::filter(species == "cattle") %>%
              # for empty MCF: average of same animal cat, climate region and MMS ???
              rowwise() %>%
              mutate(
                MCF = case_when(
                  is.na(`MCF(c)`) ~ mean(tmp_MMS_AWMS$`MCF(c)`[tmp_MMS_AWMS$UNFCCC_cat == UNFCCC_cat
                                                              & tmp_MMS_AWMS$climate_region == climate_region
                                                              & tmp_MMS_AWMS$MMS == MMS],
                                         na.rm = T),
                  .default = `MCF(c)`
                )
              ) %>%
              dplyr::group_by(UNFCCC_cat,COUNTRY) %>%
              summarise(
                sum_MCF_AWMS = sum(MCF/100*`Allocation (%)`/100)
              )) %>%
  # for empty variables: mean by UNFCCC cat ??? lots of NAs (more NAs than As)
  rowwise() %>%
  mutate(
    VS = case_when(
      is.na(VS) ~ mean(tmp_MMS_Bo$`VS(2)_daily_excretion_(average)`[tmp_MMS_Bo$UNFCCC_cat == UNFCCC_cat],na.rm = T),
      .default = VS),
    Bo = case_when(
      is.na(Bo) ~ mean(tmp_MMS_Bo$`CH4_producing_potential_(Bo)(2)_(average)`[tmp_MMS_Bo$UNFCCC_cat == UNFCCC_cat],na.rm = T),
      .default = Bo),
    sum_MCF_AWMS = case_when(
      is.na(sum_MCF_AWMS) ~ mean(sum(tmp_MMS_AWMS$`MCF(c)`[tmp_MMS_AWMS$UNFCCC_cat == UNFCCC_cat]/100
                                     *tmp_MMS_AWMS$`Allocation (%)`[tmp_MMS_AWMS$UNFCCC_cat == UNFCCC_cat]/100,na.rm = T),
                                 na.rm = T),
      .default = sum_MCF_AWMS)
  ) %>%





  # estimate
  mutate(

    # sum by category of livestock (T) for EQUATION 10.25 (UPDATED) DIRECT N2O EMISSIONS FROM MANURE MANAGEMENT
    ## N_{(T,P)} = number of head of livestock species/category T in the country, for productivity system P, when applicable
    ## Nex_{(T,P)} = annual average N excretion per head of species/category T in the country, for productivity system P, when applicable in kg N animal-1 yr-1
    ## AWMS_{(T,S,P)} = fraction of total annual nitrogen excretion for each livestock species/category T that is managed in manure management system S in the country, dimensionless ; to consider productivity class P, if using a Tier 1a approach
    N2O_D_MM_sum_T,

    # sum by manure management system (S) for EQUATION 10.25 (UPDATED) DIRECT N2O EMISSIONS FROM MANURE MANAGEMENT
    ## N_{cdg(s)} = annual nitrogen input via co-digestate in the country, kg N yr-1, where the system (s) refers exclusively to anaerobic digestion
    N2O_D_MM_sum_S = sum(N2O_D_MM_sum_T),


    # DIRECT N2O EMISSIONS

    # EQUATION 10.25 (UPDATED) DIRECT N2O EMISSIONS FROM MANURE MANAGEMENT
    ## N_2O_{D(mm)} = direct N2O emissions from Manure Management in the country, kg N2O yr-1
    ## N_{(T,P)} = number of head of livestock species/category T in the country, for productivity system P, when applicable
    ## Nex_{(T,P)} = annual average N excretion per head of species/category T in the country, for productivity system P, when applicable in kg N animal-1 yr-1
    ## N_{cdg(s)} = annual nitrogen input via co-digestate in the country, kg N yr-1, where the system (s) refers exclusively to anaerobic digestion
    ## AWMS_{(T,S,P)} = fraction of total annual nitrogen excretion for each livestock species/category T that is managed in manure management system S in the country, dimensionless ; to consider productivity class P, if using a Tier 1a approach
    ## EF_{3(S)} = emission factor for direct N2O emissions from manure management system S in the country, kg N2O-N/kg N in manure management system S
    ## S = manure management system
    ## T = species/category of livestock
    ## P = productivity class, high or low, to be considered if using the Tier 1a approach
    ## 44/28 = conversion of N2O-N(mm) emissions to N2O(mm) emissions
    N2O_D_MM = (N2O_D_MM_sum_S * EF_3S)*(44/28),

    # INDIRECT N2O EMISSIONS

    # EQUATION 10.26 (UPDATED) N LOSSES DUE TO VOLATILISATION FROM MANURE MANAGEMENT
    ## N_{volatilization-MMS} = amount of manure nitrogen that is lost due to volatilisation of NH3 and NOx, kg N yr-1
    ## Frac_{gasMS(T,S)} = fraction of managed manure nitrogen for livestock category T that volatilises as NH3 and NOx in the manure management system S

    # EQUATION 10.27 (UPDATED) N LOSSES DUE TO LEACHING FROM MANURE MANAGEMENT
    ## N_{leaching-MMS} = amount of manure nitrogen that is lost due to leaching, kg N yr-1
    ## Frac_{LeachMS(T,S)} = fraction of managed manure nitrogen for livestock category T that is leached from the manure management system S (from Table 10.22)


    # EQUATION 10.28 INDIRECT N2O EMISSIONS DUE TO VOLATILISATION OF N FROM MANURE MANAGEMENT
    ## N_2O_{G(mm)} = indirect N2O emissions due to volatilization of N from Manure Management in the country, kg N2O yr-1
    ## EF_4 = = emission factor for N2O emissions from atmospheric deposition of nitrogen on soils and water surfaces, kg N2O-N (kg NH3-N + NOx-N volatilised)-1 ; given in Chapter 11, Table 11.3

    # EQUATION 10.29 INDIRECT N2O EMISSIONS DUE TO LEACHING FROM MANURE MANAGEMENT
    ## N_2O_{L(mm)} = indirect N2O emissions due to leaching and runoff from Manure Management in the country, kg N2O yr-1
    ## EF_5 = emission factor for N2O emissions from nitrogen leaching and runoff, kg N 2O-N/kg N leached and runoff, given in Chapter 11, Table 11.3













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
    # WIP we could estimate DE = qté aliment - prod lait

    # EQUATION 10.24 VOLATILE SOLID EXCRETION RATES
    ## VS = volatile solid excretion per day on a dry-organic matter basis, kg VS day-1
    ## GE = gross energy intake, MJ day-1
    GE = GE_MJ_animal /365,
    ## DE% = digestibility of the feed in percent (e.g. 60%)
    ## (UE •GE) = urinary energy expressed as fraction of GE. Typically 0.04GE can be considered urinary energy excretion by most ruminants (reduce to 0.02 for ruminants fed with 85 percent or more grain in the diet or for swine). Use country-specific values where available.
    UE_GE = 0.04,
    ## ASH = the ash content of feed calculated as a fraction of the dry matter feed intake (e.g., 0.06 for sows: Dämmgen et al. 2011). Use country-specific values where available.
    ASH = Ash_pc/100,
    ## 18.45 = conversion factor for dietary GE per kg of dry matter (MJ kg-1). This value is relatively constant across a wide range of forage and grain-based feeds commonly consumed by livestock.
    VS = (GE*(1-(DE/100))+(UE_GE))*((1-ASH)/18.45),






    # EQUATION 10.23 CH4 EMISSION FACTOR FROM MANURE MANAGEMENT
    ## EF = annual CH4 emission factor for livestock category T, kg CH4 animal-1 yr-1
    ## VS = daily volatile solid excreted for livestock category T, kg dry matter animal-1 day-1
    ## 365 = basis for calculating annual VS production, days yr-1
    ## Bo = maximum methane producing capacity for manure produced by livestock category T, m3 CH4 kg-1 of VS excreted
    ## 0.67 = conversion factor of m3 CH4 to kilograms CH4
    ## MCF = methane conversion factors for each manure management system S by climate region k, percent
    ## AWMS = fraction of livestock category T's manure handled using animal waste management system S in climate region k, dimensionless

    EF = (VS*365)*(Bo* 0.67 * sum_MCF_AWMS),

    # Total CH4 emission from manure management
    ## To estimate total emissions, the country specific emission factor is then multiplied by the population number (Step 1).
    ## CH4 = CH4 emissions from Manure Management in the country, kg CH4 yr-1
    # N = number of head of livestock species/category T in the country, for productivity system P, when applicable
    pop_number = livestock_unit / UGB,
    CH4_MM = EF * pop_number
    )





# quality check => WIP pb avec sum_MCF_AWMS
## VS
## TABLE 10.13A (NEW) DEFAULT VALUES FOR VOLATILE SOLID EXCRETION RATE (KG VS (1000 KG ANIMAL MASS)-1 DAY-1)
### Dairy cattle in Western EU = 8.4 / Eastern EU = 6.7
quantile(tmp_cattle_CH4_MM$VS[tmp_cattle_CH4_MM$COUNTRY == 'DAN' & tmp_cattle_CH4_MM$IPCC_mix_cat == 'cows_milk_prod'])
## Bo
## TABLE 10.16A (UPDATED) DEFAULT VALUES FOR MAXIMUM METHANE PRODUCING CAPACITY (B0) (M3 CH4 KG-1 VS)
### Dairy cattle 0.24
quantile(tmp_cattle_CH4_MM$Bo[tmp_cattle_CH4_MM$COUNTRY == 'DAN' & tmp_cattle_CH4_MM$IPCC_mix_cat == 'cows_milk_prod'])

## UNFCCC sum_MCF_AWMS
## DNM_2023_2018_14042023_135925 <- read_excel("data_raw/DNM_2023_2018_14042023_135925.xlsx", sheet = "Table3.B(a)s1")
## weigthed sum 0.1299
unique(tmp_cattle_CH4_MM$sum_MCF_AWMS[tmp_cattle_CH4_MM$COUNTRY == 'DAN' & tmp_cattle_CH4_MM$IPCC_mix_cat == 'cows_milk_prod'])
## UNFCCC EF
## DNM_2023_2018_14042023_135925 <- read_excel("data_raw/DNM_2023_2018_14042023_135925.xlsx", sheet = "Table3.B(a)s1")
### EF Dairy cattle = 56.66 (kg CH4/head/yr)
quantile(tmp_cattle_CH4_MM$EF[tmp_cattle_CH4_MM$COUNTRY == 'DAN' & tmp_cattle_CH4_MM$IPCC_mix_cat == 'cows_milk_prod'])









##### SWINE ----
 # to be completed

# POULTRY ----
# to be completed


# SHEEP ----
# to be completed

# GOATS ----
# to be completed

##### Output ----

CH4_MM <- Reduce(rbind,list(
  tmp_cattle_CH4_MM %>% dplyr::select(farm_id,COUNTRY,code_livestock,livestock_unit,UNFCCC_cat,IPCC_mix_cat,EF,CH4_MM)#,
  #tmp_swine_Nexc %>% dplyr::select(farm_id,code_livestock,livestock_unit,Nin_DMI,Nex,Nex_total),
  #tmp_poultry_Nexc %>% dplyr::select(farm_id,code_livestock,livestock_unit,Nin_DMI,Nex,Nex_total),
  #tmp_sheep_Nexc %>% dplyr::select(farm_id,code_livestock,livestock_unit,Nin_DMI,Nex,Nex_total),
  #tmp_goats_Nexc %>% dplyr::select(farm_id,code_livestock,livestock_unit,Nin_DMI,Nex,Nex_total)
  )) %>%
  ungroup()

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])

