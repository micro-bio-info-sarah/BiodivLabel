# BVIAS for SIQO


# SIQO products by farm

# Crops ----

# crop products
RICA_RA_SIQO_product_crops <- RICA_RA_SIQO_product %>%
  rename(farm_id = IDENT) %>%
  mutate(crop = as.character(RICA_var_code)) %>%
  # filter SIQO for crops
  filter(production_type == "crop") %>%
  filter(!is.na(crop)) %>%
  # aggregate SIQO by crop
  group_by(farm_id,NOM_DOSSIER,crop) %>%
  reframe(LIBELLE_PRODUIT = paste0(sort(unique(na.omit(LIBELLE_PRODUIT))),collapse = ";"),
          SIQO = paste0(sort(unique(na.omit(SIQO))),collapse = ";"),
          SIQO_FILIERE = paste0(sort(unique(na.omit(SIQO_FILIERE))),collapse = ";"),
          #AGBIO = sort(unique(AGBIO),
          n_products = n()) %>% distinct()
# quality check : one observation per crop per farm => OK
#length(sort(unique(paste0(RICA_RA_SIQO_product_crops$farm_id,RICA_RA_SIQO_product_crops$crop))))

# add SIQO products to BVIAS
BVIAS_to_RICA_RA_SIQO_crops <- BVIAS_to_RICA_crops %>%
  # add RICA_RA data
  inner_join(.,RICA_RA %>%
               rename(farm_id = IDENT),
             by = join_by(farm_id)) %>% 
  # add SIQO
  left_join(.,RICA_RA_SIQO_product_crops,
            by = join_by(farm_id, crop, NOM_DOSSIER)) %>%
  # define a variable with the most restrictive specifications as: AB > SIQO
  mutate(
    FQS = case_when(
      org_farming == T ~ "AB",
      org_farming == F & !is.na(SIQO) ~ SIQO,
      .default = "Conventionnel"
    )) %>%
  # remove farms with NAs
  filter(is.finite(BVI_ha+BVI_kg))
# 5463 farms


# Milk ----


## join products by farm
RICA_RA_SIQO_product_milk <- RICA_RA_SIQO_product %>%
  rename(farm_id = IDENT) %>%
  # filter SIQO linked to milk production
  filter(grepl(c("FROMAGE|BEURRE|CREME"),SIQO_FILIERE)) %>%
  # aggregate SIQO by farms
  group_by(farm_id,NOM_DOSSIER) %>%
  reframe(LIBELLE_PRODUIT = paste0(sort(unique(na.omit(LIBELLE_PRODUIT))),collapse = ";"),
          SIQO = paste0(sort(unique(na.omit(SIQO))),collapse = ";"),
          SIQO_FILIERE = paste0(sort(unique(na.omit(SIQO_FILIERE))),collapse = ";"),
          n_products = n()) %>%
  distinct()

BVIAS_to_RICA_RA_SIQO_milk <- BVIAS_to_RICA_milk %>%
  select(farm_id,org_farming,feed_ha_p_species,BVI_ha_feed,p100_sales,prod_kg,BVI_kg) %>%
  # add RICA_RA data
  inner_join(.,RICA_RA %>%
               rename(farm_id = IDENT),
             by = join_by(farm_id)) %>% # 1679 farms
  # add SIQO
  left_join(., RICA_RA_SIQO_product_milk %>% 
              select(farm_id,NOM_DOSSIER,LIBELLE_PRODUIT,SIQO_FILIERE,SIQO),
            by = join_by(farm_id, NOM_DOSSIER)) %>%
  # define a variable with the most restrictive specifications as: AB > SIQO
  mutate(
    FQS = case_when(
      # first organic farming as it is the most restritive specifications
      org_farming == T ~ "AB",
      # cheese PDO groups
      grepl(c("BEURRE CHARENTESPOITOU"),LIBELLE_PRODUIT) ~ "Beurre de Charentes-Poitou",
      grepl(c("COMTE|MORBIER"),LIBELLE_PRODUIT) ~ "Comte - Morbier",
      grepl(c("ROQUEFORT"),LIBELLE_PRODUIT) ~ "Roquefort",
      grepl(c("BLEU D AUVERGNE|CANTAL "),LIBELLE_PRODUIT) ~ "Bleu d'Auvergne - Cantal", # quality check pour voir le micmac avec le cantal
      grepl(c("MUNSTER"),LIBELLE_PRODUIT) ~ "Munster", # quality check : pas de munster / emmental ensemble ? non c'est bon
      grepl(c("BROCCIU"),LIBELLE_PRODUIT) ~ "Brocciu",
      grepl(c("SAVOIE"),LIBELLE_PRODUIT) ~ "Fromages de Savoie",
      # for other FQS
      org_farming == F & !is.na(SIQO) ~ LIBELLE_PRODUIT,
      # other are considered conventional
      .default = "Conventionnel"
    )) %>%
  # remove farms with NAs
  filter(is.finite(BVI_ha_feed+BVI_kg))
 # 1302 farms

# transfert table
tmp_TT_crops <- readxl::read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops") %>%
  mutate(crop = as.character(RICA_code_number))


BVIAS_to_RICA_RA_SIQO <- bind_rows(
  BVIAS_to_RICA_RA_SIQO_crops %>%
    mutate(
      production_type = case_when(
        land_use_type == "arable" ~ "crop",
        land_use_type == "grassland" ~ "grassland"
      )),
  BVIAS_to_RICA_RA_SIQO_milk %>% 
    mutate(production_type = "milk",
           BVI_ha = BVI_ha_feed)
) %>%
  select(farm_id,NOM_DOSSIER,land_use_type,production_type,crop,BV_loc,BVI_ha,BVI_kg,LIBELLE_PRODUIT,SIQO_FILIERE,SIQO,org_farming,FQS)  %>%
  # add crop Libelle and species
  left_join(.,tmp_TT_crops %>%
              mutate(crop = as.character(crop)) %>%
              select(crop,product_name,species),
            by = join_by(crop))  %>%
  # add product name var
  mutate(
    product_name = case_when(
      production_type =="milk" ~ "Lait",
      .default = product_name
    )) %>%
  # add unique id for product-FQS
  mutate(
    product_FQS = case_when(
      production_type == "milk" ~ paste0("Lait - ",FQS),
      .default = paste0(product_name," - ",FQS)
    ))



length(unique(paste0(BVIAS_to_RICA_RA_SIQO$farm_id,BVIAS_to_RICA_RA_SIQO$crop))) == nrow(BVIAS_to_RICA_RA_SIQO)


rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])








