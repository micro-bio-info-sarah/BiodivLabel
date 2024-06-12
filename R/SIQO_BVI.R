# BVI for SIQO


# SIQO products by farm



# Crops ----

# crop products
RICA_RA_SIQO_product_crops <- RICA_RA_SIQO_product %>%
  filter(production_type == "crop") %>%
  mutate(CODE3 = RICA_var_code) %>%
  filter(!is.na(CODE3)) %>%
  # aggregate SIQO by crop
  group_by(IDENT,NOM_DOSSIER,CODE3) %>%
  reframe(LIBELLE_PRODUIT = paste0(unique(LIBELLE_PRODUIT), collapse = ";"),
          SIQO = paste0(unique(SIQO), collapse = ";"),
          SIQO_FILIERE = paste0(unique(SIQO_FILIERE), collapse = ";"),
          #AGBIO = unique(AGBIO),
          n_products = n()) %>% distinct()
# quality check : one observation per crop per farm => OK
#length(unique(paste0(RICA_RA_SIQO_product_crops$IDENT,RICA_RA_SIQO_product_crops$CODE3)))

# add SIQO products to BVI
BVI_to_RICA_RA_SIQO_crops <- BVI_to_RICA_crops %>%
  select(IDENT,CODE3,
         A.3.1,A.4.5,A.4.5_min,A.4.5_org,A.5.1,
         A.3.1_max,A.4.5_max,A.4.5_min_max,A.4.5_org_max,A.5.1_max,
         A.3.1_norm,A.4.5_norm,A.5.1_norm,
         BVI_ha,yield,BVI_kg) %>%
  # add RICA_RA data
  inner_join(.,RICA_RA) %>% # 5311 farms
  # add organic farming
  inner_join(.,RICA_2020 %>% select(IDENT,AGBIO)) %>%
  # add SIQO
  left_join(.,RICA_RA_SIQO_product_crops) %>%
  # define a variable with the most restrictive specifications as: AB > SIQO
  mutate(FQS = case_when(
    AGBIO == 0 & is.na(SIQO) ~ "Conventionnel",
    AGBIO == 0 & !is.na(SIQO) ~ SIQO,
    AGBIO == 2 ~ "AB",
    AGBIO == 4 ~ "AB (en conversion)"
  ))


# Milk ----


## join products by farm
RICA_RA_SIQO_product_milk <- RICA_RA_SIQO_product %>%
  # filter SIQO linked to milk production
  filter(grepl(c("FROMAGE|BEURRE|CREME"),SIQO_FILIERE)) %>%
  # add fromage variable
  mutate(fromage = case_when(
    SIQO_FILIERE %in% unique(SIQO_FILIERE[grepl(c("FROMAGE"),SIQO_FILIERE)]) ~ LIBELLE_PRODUIT
  )) %>%
  # aggregate SIQO by farms
  group_by(IDENT,NOM_DOSSIER) %>%
  reframe(LIBELLE_PRODUIT = paste0(unique(LIBELLE_PRODUIT), collapse = ";"),
          SIQO_FILIERE = paste0(unique(SIQO_FILIERE), collapse = ";"),
          SIQO = paste0(unique(SIQO), collapse = ";"),
          fromages = paste0(sort(na.omit(unique(fromage))), collapse = ";"),
          #AGBIO = unique(AGBIO),
          n_products = n()) %>%
  distinct()

BVI_to_RICA_RA_SIQO_milk <- BVI_to_RICA_milk %>%
  select(IDENT,species,feed_ha_p_species,BVI_ha_feed,p100_sales,prod,BVI_kg) %>%
  # add RICA_RA data
  inner_join(.,RICA_RA) %>% # 1679 farms
  # add organic farming
  inner_join(., RICA_2020 %>% select(IDENT,AGBIO)) %>%
  # add SIQO
  left_join(., RICA_RA_SIQO_product_milk %>% select(IDENT,NOM_DOSSIER,LIBELLE_PRODUIT,SIQO_FILIERE,SIQO)) %>%
  # define a variable with the most restrictive specifications as: AB > SIQO
  mutate(
    FQS = case_when(
      AGBIO == 0 & is.na(SIQO_FILIERE) ~ "Conventionnel",
      AGBIO == 0 & !is.na(SIQO_FILIERE) ~ SIQO, # extract only label type
      AGBIO == 2 ~ "AB",
      AGBIO == 4 ~ "AB (en conversion)"
    )) %>% # 1679 farms
  filter(
    # remove AB partiel
    AGBIO != 5
    # remove farms with NAs
    & is.finite(BVI_ha_feed+prod+BVI_kg)
    # remove farms above the 95th percentile ???
  ) %>%
  # add RICA variables 
  inner_join(., RICA_2020 %>% select(IDENT,EXTR2,OTEFDD)) %>%
  # adapt libelle produit
  mutate(
    app_fromage = case_when(
      grepl(c("BEURRE CHARENTESPOITOU"),LIBELLE_PRODUIT) ~ "Beurre de Charentes-Poitou",
      grepl(c("COMTE|MORBIER"),LIBELLE_PRODUIT) ~ "Comte - Morbier",
      grepl(c("ROQUEFORT"),LIBELLE_PRODUIT) ~ "Roquefort",
      grepl(c("BLEU D AUVERGNE|CANTAL "),LIBELLE_PRODUIT) ~ "Bleu d'Auvergne - Cantal", # quality check pour voir le micmac avec le cantal
      grepl(c("MUNSTER"),LIBELLE_PRODUIT) ~ "Munster", # quality check : pas de munster / emmental ensemble ? non c'est bon
      grepl(c("BROCCIU"),LIBELLE_PRODUIT) ~ "Brocciu",
      #grepl(c("EMMENTAL DE SAVOIE|RACLETTE DE SAVOIE|TOMME DE SAVOIE"),LIBELLE_PRODUIT) ~ "Fromages de Savoie",
      grepl(c("SAVOIE"),LIBELLE_PRODUIT) ~ "Fromages de Savoie",
      # pour ceux qui ont une autre appelation fromage
      #grepl(c("FROMAGE"),SIQO_FILIERE) ~ paste0("Autres appellations de fromage ",SIQO_FILIERE),
      # pour ceux sans SIQO fromage
      #!grepl(c("FROMAGE"),SIQO_FILIERE) ~ FQS
      # quality check pour l'instant je mets tous les autres siqo ensemble, sans distinguer fromage, beurre etc
      .default = FQS
    )
  ) # 1331 farms

