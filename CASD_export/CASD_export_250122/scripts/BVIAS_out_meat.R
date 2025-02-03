# BVI for crops

##### Set up ----

library(dplyr)
library(readr)


##### Economic allocation by production ----

### RICA data on animal herd
tmp_RICA_2020_anim <- read_delim(
  "//casd.fr/casdfs/Projets/EVAPDRH/Data/RICA_RICA_2020/RICA2020_tables_CSV/ani20.csv",
  delim = ";", escape_double = FALSE, trim_ws = TRUE,
  col_types = cols(IDENT = col_character(),CODE6 = col_number()))
### RICA data on animal production
tmp_RICA_2020_pan <- read_delim(
  "//casd.fr/casdfs/Projets/EVAPDRH/Data/RICA_RICA_2020/RICA2020_tables_CSV/pan20.csv",
  delim = ";", escape_double = FALSE, trim_ws = TRUE,
  col_types = cols(IDENT = col_character(),CODE7 = col_number()))

### transfert tables
tmp_TT_livestock <- read_xlsx("data_in/transfert_table_livestock.xlsx",sheet = "transfert_table")
tmp_TT_livestock_product <- read_xlsx("data_in/transfert_table_livestock_products.xlsx",sheet = "transfert_table")


# calculate sales allocation ratio 
## CA : VVENT6 / VVENT7 ??
tmp_sales <- Reduce(rbind,list(
  tmp_RICA_2020_anim %>% 
    group_by(IDENT,CODE6) %>%
    summarise(sales = sum(VVENT6)) %>% # 4232 farms
    filter(sales >0) %>% # 4020 farms
  # add species
    left_join(.,tmp_TT_livestock %>% select(CODE6,species)) %>% 
    rename(CODE = CODE6),
  
  tmp_RICA_2020_pan %>% 
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


## select meat
tmp_sales_meat <- tmp_sales %>%
  filter(
    # filter farms in OTEX meat cattle, mixed cattle, mixed farming
    IDENT %in% unique(RICA_2020$IDENT[RICA_2020$OTEFDD %in% c(4600,4700,6184)]) # 1627 farms
    # filter by product:
    & CODE %in% tmp_TT_livestock_product$CODE7[tmp_TT_livestock_product$production_type == "meat"]) %>% #  farms
  # ??? consider that we can sum milk and cheese sales ?
  group_by(IDENT,sales_total) %>%
  summarise(sales = sum(sales)) %>%
  # calculate allocation ratio
  mutate(p100_sales = sales / sales_total) %>% ungroup()

# production: QPROD7
tmp_prod <- tmp_RICA_2020_pan %>%
  # add units 
  left_join(., tmp_TT_livestock_product %>% select(CODE7,QPROD7_unit) ) %>%
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
  # ??? consider that we can sum milk and cheese sales ?
  group_by(IDENT) %>%
  summarise(prod = sum(prod)) %>% ungroup()

# allocate BVI

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


#rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])











