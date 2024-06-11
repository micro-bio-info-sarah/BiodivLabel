# BVI for crops

##### Set up ----

library(dplyr)
library(readr)


# Economic allocation by production

### transfert tables
tmp_TT_livestock <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_livestock")
tmp_TT_livestock_product <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_livestock_products")


# calculate sales allocation ratio ----
## CA : VVENT6 / VVENT7 ??
## quality check : CA total = CA ani + CA pan + CA veg => est-ce que l'on retrouve bien le CA total ?  est-ce que l'on retrouve le % du CA lié au lait ?
tmp_sales <- Reduce(rbind,list(
  # add livestock sales
  RICA_2020_ani %>% 
    group_by(IDENT,CODE6) %>%
    summarise(sales = sum(VVENT6)) %>% # 4232 farms
    filter(sales >0) %>% # 4020 farms
  # add species
    left_join(.,tmp_TT_livestock %>% select(CODE6,species)) %>% 
    rename(CODE = CODE6),
  
  # add animal products sales
  RICA_2020_pan %>% 
    group_by(IDENT,CODE7) %>%
    summarise(sales = sum(VVENT7)) %>% # 2379 farms
    filter(sales >0) %>% # 2351 farms
    left_join(.,tmp_TT_livestock_product %>% select(CODE7,species)) %>% 
    rename(CODE = CODE7)
  )) %>%
  group_by(IDENT,species) %>%
  mutate(
    sales_total = sum(sales)
  ) %>% ungroup()

## select milk
tmp_sales_milk <- tmp_sales %>%
  filter(
    # filter farms in OTEX diary cattle, mixed cattle, mixed farming
    IDENT %in% unique(RICA_2020$IDENT[RICA_2020$OTEFDD %in% c(4500,4700,6184)]) & # 1800 farms
      # filter by product: keep milk and full cow milk cheese (only 3 farms with mixed milk cheese => discard)
    CODE %in% c(21,22)) %>% # 1339 farms
  # ??? consider that we can sum milk and cheese sales ?
  group_by(IDENT,sales_total) %>%
  summarise(sales = sum(sales)) %>%
  # calculate allocation ratio
  mutate(p100_sales = sales / sales_total) %>% ungroup()

# production: QPROD7
tmp_prod <- RICA_2020_pan %>%
  # add units 
  left_join(., tmp_TT_livestock_product %>% select(CODE7,QPROD7_unit)) %>%
  # summarise production and convert to liter
  group_by(IDENT,CODE7) %>%
  summarise(prod = sum(QPROD7*QPROD7_unit)) %>% # 2379 farms
  filter(prod >0) %>% # 2070 farms
  ungroup()
## milk production
tmp_prod_milk <- tmp_prod %>%
  filter(
    # filter farms in OTEX diary cattle, mixed cattle, mixed farming
    IDENT %in% unique(RICA_2020$IDENT[RICA_2020$OTEFDD %in% c(4500,4700,6184)]) & # 1403 farms
      # filter by product: keep milk and full milk cow cheese (only 3 farms with mixed milk cheese => discard)
      CODE7 %in% c(21,22)) %>% # 1340 farms
  # ??? consider that we can sum milk and cheese sales ? => yes, see RICA "Instructions de collecte": crème, beurre et fromage en hectolitres de lait utilisé.
  group_by(IDENT) %>%
  summarise(prod = sum(prod)) %>% ungroup()

# allocate BVI ----

tmp_BVI_milk <- BVI_to_RICA_herd %>%
  # select farm with cattle and a BVI
  filter(
    # filter farms in OTEX diary cattle, mixed cattle, mixed farming
    IDENT %in% unique(RICA_2020$IDENT[RICA_2020$OTEFDD %in% c(4500,4700,6184)])
    & species == "cattle" 
    & is.finite(BVI_herd)) %>% # 1554 farms
  # add allocation ratio
  left_join(., tmp_sales_milk) %>%
  # add production data
  left_join(., tmp_prod_milk) %>%
  # calculate BVi / L milk
  mutate(
    BVI_kg = (BVI_herd * p100_sales) / prod
    )
  

##### Output ----

BVI_to_RICA_milk <- tmp_BVI_milk %>% ungroup()


rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])











