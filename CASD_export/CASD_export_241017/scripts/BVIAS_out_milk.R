# BVI for crops

##### Set up ----

library(dplyr)
library(readr)


# Economic allocation by production



if (my_DB == "RICA") {
  
  ### transfert tables
  tmp_TT_livestock <- readxl::read_xlsx("data_in/supp_data.xlsx", sheet ="TT_livestock") %>%
    rename(code_livestock = RICA_code_number)
  tmp_TT_livestock_product <- readxl::read_xlsx("data_in/supp_data.xlsx",sheet = "TT_livestock_products") %>%
    rename(code_anim_prod = RICA_code_number) %>%
    select(code_anim_prod,LIBELLE,RICA_QPROD7_unit,species, production_type) %>%
    distinct()
  
  # calculate sales allocation ratio ----
  ## CA : VVENT6 / VVENT7 ??
  ## quality check : CA total = CA ani + CA pan + CA veg => est-ce que l'on retrouve bien le CA total ?  est-ce que l'on retrouve le % du CA lié au lait ?
  tmp_sales <- bind_rows(
    # add livestock sales
    RICA_2020_ani %>%
      rename(farm_id = IDENT,code_livestock = CODE6) %>%
      group_by(farm_id,code_livestock) %>%
      summarise(sales = sum(VVENT6),
                .groups = "keep") %>% # 4232 farms
      filter(sales >0) %>% # 4020 farms
      # add species
      left_join(.,tmp_TT_livestock %>% 
                  select(code_livestock,species),
                by = join_by(code_livestock)),
    
    # add animal products sales
    RICA_2020_pan %>%
      rename(farm_id = IDENT,code_anim_prod = CODE7) %>%
      group_by(farm_id,code_anim_prod) %>%
      summarise(sales = sum(VVENT7),
                .groups = "keep") %>% # 2379 farms
      filter(sales >0) %>% # 2351 farms
      left_join(.,tmp_TT_livestock_product %>% 
                  select(code_anim_prod,species),
                by = join_by(code_anim_prod))) %>%
    group_by(farm_id,species) %>%
    mutate(
      sales_total = sum(sales)
    ) %>% ungroup()
  
  ## select milk
  tmp_sales_milk <- tmp_sales %>%
    filter(
      # filter farms in OTEX diary cattle, mixed cattle, mixed farming
      farm_id %in% unique(RICA_2020$IDENT[RICA_2020$OTEFDD %in% c(4500,4700,6184)]) & # 1800 farms
        # filter by product: keep milk and full cow milk cheese (only 3 farms with mixed milk cheese => discard)
        code_anim_prod %in% c(21,22)) %>% # 1339 farms
    # ??? consider that we can sum milk and cheese sales ?
    group_by(farm_id,sales_total) %>%
    summarise(sales_milk = sum(sales),
              .groups = "keep") %>%
    # calculate allocation ratio
    mutate(p100_sales = sales_milk / sales_total) %>% ungroup()
  
  # production: QPROD7
  tmp_prod <- RICA_2020_pan %>%
    rename(farm_id = IDENT,code_anim_prod = CODE7) %>%
    # add units
    left_join(., tmp_TT_livestock_product %>%
                select(code_anim_prod,RICA_QPROD7_unit),
              by = join_by(code_anim_prod)) %>%
    # summarise production and convert to kg
    group_by(farm_id,code_anim_prod) %>%
    summarise(prod_kg = sum(QPROD7*RICA_QPROD7_unit),
              .groups = "keep") %>% # 2379 farms
    filter(prod_kg >0) %>% # 2070 farms
    ungroup()
  
  ## milk production
  tmp_prod_milk <- tmp_prod %>%
    filter(
      # filter farms in OTEX diary cattle, mixed cattle, mixed farming
      farm_id %in% unique(RICA_2020$IDENT[RICA_2020$OTEFDD %in% c(4500,4700,6184)]) & # 1403 farms
        # filter by product: keep milk and full milk cow cheese (only 3 farms with mixed milk cheese => discard)
        code_anim_prod %in% c(21,22)) %>% 
    # ??? consider that we can sum milk and cheese sales ? => yes, see RICA "Instructions de collecte": crème, beurre et fromage en hectolitres de lait utilisé.
    group_by(farm_id) %>%
    summarise(prod_kg = sum(prod_kg),
              .groups = "keep") %>%
    ungroup()
    # 1340 farms
  
}

if (my_DB == "FADN") {
  
  # transfert table
  tmp_FADN_code_livestock <- read_xlsx("data_in/supp_data.xlsx",sheet = "FADN_livestock_code")
  tmp_TT_livestock0 <- read_xlsx("data_in/supp_data.xlsx", sheet ="TT_livestock")
  tmp_TT_livestock <- left_join(
    tmp_FADN_code_livestock,
    tmp_TT_livestock0) %>%
    rename(code_livestock = FADN_code_letter) %>%
    select(code_livestock,species) %>%
    distinct()
  
  # PMLKCOW_PRQ	K_PR_261_Q	Cows' milk - Production quantity	in tonnes
  tmp_FADN_code_anim_prod <- read_xlsx("data_in/supp_data.xlsx",sheet = "FADN_animal_product_code")
  tmp_TT_livestock_product0 <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_livestock_products")
  tmp_TT_livestock_product <- left_join(
    tmp_FADN_code_anim_prod,
    tmp_TT_livestock_product0) %>%
    rename(code_anim_prod = FADN_code_letter) %>%
    select(code_anim_prod,species,) %>%
    distinct()
  
  # input data
  tmp_sales <- FADN_18 %>%
    ## total sales from livestock
    select(ID,TF, all_of(intersect(paste0(tmp_FADN_code_livestock$FADN_code_letter,"_SV"),colnames(FADN_18)))) %>%
    pivot_longer(cols = !c(ID,TF),names_to = "code_livestock",values_to = "sales_euros") %>%
    mutate(code_livestock = gsub("_SV","",code_livestock)) %>%
    ### add species
    left_join(.,tmp_TT_livestock %>% select(code_livestock,species)) %>%
    rename(code = code_livestock) %>%
    ## total animal product sales
    rbind(
      .,
      FADN_18 %>%
        select(ID,TF, all_of(intersect(paste0(tmp_FADN_code_anim_prod$FADN_code_letter,"_SV"),colnames(FADN_18)))) %>%
        pivot_longer(cols = !c(ID,TF),names_to = "code_anim_prod",values_to = "sales_euros") %>%
        mutate(code_anim_prod = gsub("_SV","",code_anim_prod)) %>%
        ### add species
        left_join(.,tmp_TT_livestock_product %>% select(code_anim_prod,species)) %>%
        rename(code = code_anim_prod)
    ) %>%
    ## total sales from livestock and animal products
    group_by(ID,species) %>%
    mutate(
      sales_total = sum(sales_euros)
    ) %>% ungroup() %>%
    # filter farms in OTEX diary cattle, mixed cattle, mixed farming
    filter(TF %in% c(4500,4700,7310)) %>%
    rename(farm_id = ID)
  
  ## milk sales
  tmp_sales_milk <- tmp_sales %>%
    filter(
      # filter by product: keep milk
      code %in% c("PMLKCOW")
      & sales_euros > 0
    ) %>% # x farms
    # estimate milk sales ratio
    group_by(farm_id,sales_total) %>%
    summarise(sales_euros = sum(sales_euros)) %>%
    mutate(p100_sales = sales_euros / sales_total) %>% ungroup()
  
  # check
  print(length(unique(tmp_sales_milk$farm_id)) == nrow(FADN_18 %>% filter(TF %in% c(4500,4700,7310) & PMLKCOW_SV >0)))
  print("Mean share of milk sales")
  print(mean(tmp_sales_milk$p100_sales))
  
  ## milk production
  tmp_prod_milk <- FADN_18 %>%
    # filter farms in OTEX diary cattle, mixed cattle, mixed farming and have sold milk
    filter(TF %in% c(4500,4700,7310) & PMLKCOW_SV >0) %>%
    # extract milk production
    mutate(code = "PMLKCOW",
           prod_kg = PMLKCOW_PRQ*10^3) %>%
    rename(farm_id = ID) %>%
    select(farm_id,code,prod_kg)
  
}

# allocate BVI ----

tmp_BVIAS_milk <- BVIAS_to_RICA_herd %>%
  # select farm with cattle and a BVI
  filter(
    # filter farms in OTEX diary cattle, mixed cattle, mixed farming and have sold milk
    farm_id %in% tmp_sales_milk$farm_id
    & species == "cattle"
    & is.finite(BVIAS_herd)) %>% # x farms
  # add allocation ratio
  left_join(., tmp_sales_milk, by = join_by(farm_id)) %>%
  # add production data
  left_join(., tmp_prod_milk, by = join_by(farm_id)) %>%
  # calculate BVi / L milk
  mutate(
    BVIAS_kg = (BVIAS_herd * p100_sales) / prod_kg
  )


##### Output ----

BVIAS_to_RICA_milk <- tmp_BVIAS_milk %>% ungroup()


rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])











