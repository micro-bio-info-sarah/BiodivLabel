# Matching_RICA-RA-SIQO

##### Set up ----

library(readr)
library(readxl)

library(tibble)
library(dplyr)
library(tidyr)
library(tidyverse)
library(purrr)

library(stringdist)

##### Matching RICA-RA ----

# Import data

## RA

load("~/BiodivLabel/data_CASD/RA_2020.RData")
#tmp_RA2020_EXPLOIT <- read_csv("//casd.fr/casdfs/Projets/EVAPDRH/Data/RGA_RA_2020/Données au format csv/RA2020_EXPLOITATIONS_240112.csv")
#tmp_RA2020_IDADMIN <- read_csv("//casd.fr/casdfs/Projets/EVAPDRH/Data/RGA_RA_2020/Données au format csv/RA2020_IDADMIN_240112.csv")

RA2020 <- tmp_RA2020_EXPLOIT %>%
  # SELECT ALL VARIABLES NEEDED AFTERWARDS (MATCHING WITH RICA AND PROPENSITY SCORE ESTIMATION)
  select(NOM_DOSSIER,
         SIEGE_REG,
         SIEGE_DEP, 
         SIEGE_CODE_COM,
         REGL_1305_2013,
         DIMECO_COEF2017, 
         STATUT, 
         PBSTOT_COEF17, 
         CDEX_COEF2017,
         OTE64_COEF2017, 
         OTEFDA_COEF17,
         OTEFDD_COEF17,
         CIRCOUFIL, 
         EXTERNALISFIL, 
         TRANSFOFIL,  
         SAU_TOT,
         UGBTA.TOT, 
         BIO_FIL,
         BIO_INTEGRAL,
         BIODYNAMIE,
         NATUREPROGRES,
         HVE) %>%
  left_join(.,tmp_RA2020_IDADMIN %>% select(NOM_DOSSIER,SIRET,PACAGE),
            by = join_by(NOM_DOSSIER))


rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])


### Filter 1: exact matching with SIRET
tmp_RICA_RA1 <- RICA_2020 %>%
  inner_join(.,RA2020 %>% select(NOM_DOSSIER,SIRET),
             by = 'SIRET') %>%
  # select variables
  select(IDENT,NOM_DOSSIER,SIRET,PACAGE) %>%
  mutate(retain = "SIRET")
# 7078 farms left by SIRET

### Filter 2: exact matching with PACAGE
tmp_RICA_RA2 <- RICA_2020 %>%
  filter(
    # remove farms already retained with previous filter
    !(IDENT %in% tmp_RICA_RA1$IDENT)
    # remove farms without PACAGE
    & !is.na(PACAGE)) %>%
  # add RA
  inner_join(.,RA2020 %>%
               filter(!(NOM_DOSSIER %in% tmp_RICA_RA1$NOM_DOSSIER)
                      & PACAGE %in% RICA_2020$PACAGE) %>%
               select(NOM_DOSSIER,PACAGE),
             by = 'PACAGE') %>%
  # select variables
  select(IDENT,NOM_DOSSIER,SIRET,PACAGE) %>%
  mutate(retain = "PACAGE")
# 176 farms 


### Filters 3 and 4: exact matching by SIREN, municipality + OTEX + CDEX
# select farms without a match for their SIRET nor PACAGE and add SIREN
tmp_RICA_left <- RICA_2020 %>%
  filter(!(IDENT %in% c(tmp_RICA_RA1$IDENT,tmp_RICA_RA2$IDENT))) %>%
  mutate(SIREN = sapply(strsplit(SIRET,''),function(x){paste(x[1:9],collapse = '')}))
tmp_RA_left = RA2020 %>%
  filter(!(NOM_DOSSIER %in% c(tmp_RICA_RA1$NOM_DOSSIER,tmp_RICA_RA2$NOM_DOSSIER))) %>%
  mutate(SIREN = sapply(strsplit(SIRET,''),function(x){paste(x[1:9],collapse = '')}))
# create a tibble to retained farms
tmp_retain <- tibble()

# retain farm depending on three filters
for (tmp_i in tmp_RICA_left$IDENT) {
  #  tmp_i = tmp_RICA_left$IDENT[1]
  
  ## FILTER 3: by SIRENE
  # extract farm from RICA
  tmp_RICA = tmp_RICA_left %>% filter(IDENT == tmp_i) 
  
  # extract matching farms from RA
  tmp_RA = tmp_RA_left  %>%
    filter(SIREN == tmp_RICA$SIREN) %>%
    mutate(retain = "SIREN")
  
  ## FILTER 4: by commune code, OTEXE and CDEX
  if (nrow(tmp_RA) == 0) {
    tmp_RA = tmp_RA_left %>% 
      filter(
        ## commune
        SIEGE_CODE_COM == tmp_RICA$DEPCOM
        ## OTEx
        & OTEFDD_COEF17 == tmp_RICA$OTE64F
        ## CDEX
        & as.numeric(CDEX_COEF2017) == tmp_RICA$CDEXE
      ) %>%
      mutate(retain = "DEPCOM,OTE64F,CDEXE")
  }
  
  if (nrow(tmp_RA) == 1) {
    tmp_retain <- tmp_retain %>%
      bind_rows(.,
                tibble("IDENT"=tmp_RICA$IDENT,
                       "NOM_DOSSIER"=tmp_RA$NOM_DOSSIER,
                       "retain"=tmp_RA$retain,
                       "SIRET" = paste0(unique(na.omit(c(tmp_RICA$SIRET,tmp_RA$PACAGE))),collapse = ":RICA|RA:"),
                       "PACAGE"= paste0(unique(na.omit(c(tmp_RICA$SIRET,tmp_RA$PACAGE))),collapse = ":RICA|RA:")))
  }
}

RICA_RA <- Reduce(rbind,
                  list(tmp_RICA_RA1,
                       tmp_RICA_RA2,tmp_retain))
# 7292 farms
rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])

##### matching RICA-RA-SIQO ----

# Import data
## Matching RA 2020 - SIQO done by Julie Regolo et al.
load("~/BiodivLabel/data_CASD/RA_SIQO_2020.RData")
## Farms -> 108702 SIRET
#tmp_SIQO_RA_farm <- read_csv("//casd.fr/casdfs/Projets/EVAPDRH/Data/RGA_RA_2020/Complément SIQO/EXPLOIT_SIQO_RA2020_220909.csv")
## Products -> 108702 farms with 772 different product labels
#tmp_SIQO_RA_product <- read_csv("//casd.fr/casdfs/Projets/EVAPDRH/Data/RGA_RA_2020/Complément SIQO/ProduitsSIQO_RA2020_220909.csv")

## transfert table
tmp_TT_RICA_SIQO_product <- read_excel("data_in/supp_data.xlsx",sheet = "TT_RICA_SIQO")
tmp_TT_crops <- read_excel("data_in/supp_data.xlsx",sheet = "TT_crops")

# Match
## join by farm
RICA_RA_SIQO_farm <- RICA_RA %>%
  left_join(.,tmp_SIQO_RA_farm %>% select(NOM_DOSSIER) %>% mutate(is.SIQO = T),
            by = join_by(NOM_DOSSIER))

## join by product

RICA_RA_SIQO_product_all <- RICA_RA %>%
  left_join(.,tmp_SIQO_RA_product,
            by = join_by(NOM_DOSSIER)) %>%
  left_join(.,tmp_TT_RICA_SIQO_product,
            by = join_by(CODE_PRODUIT, LIBELLE_PRODUIT, SIQO_FILIERE)) %>%
  # add organic farming data
  left_join(.,RICA_2020 %>% select(IDENT,AGBIO),
            by = join_by(IDENT)) %>%
  mutate(org_farming = case_when(
    AGBIO %in% c(2,4) ~TRUE ,
    .default = F
  )) %>%
  # add data about biodynamie, HVE, etc.
  left_join(.,RA2020 %>% 
              select(NOM_DOSSIER,BIO_FIL,BIO_INTEGRAL,BIODYNAMIE,NATUREPROGRES,HVE),
            by = join_by(NOM_DOSSIER)) %>%
  # add a variable with only the Label type
  mutate(SIQO = str_extract(SIQO_FILIERE,"^[A-Z]+[[:blank:]]?[[:punct:]]?")) %>%
  ## clean SIQO name
  mutate(
    SIQO = case_when(
      org_farming == T ~"AB",
      SIQO == "LR," ~ "LR",
      SIQO == "AOP &" ~ "AOP - AOC",
      SIQO == "IGP," ~ "IGP",
      SIQO == "IG," ~ "IG",
      SIQO == "IG &" ~ "IG - AOC",
      SIQO == "AOC," ~ "AOC",
      .default = SIQO
    )
  )

RICA_RA_SIQO_product <- RICA_RA_SIQO_product_all %>%
  # remove SIQO with wine
  filter(!grepl(c(" VINS|EAUXDEVIE"),SIQO_FILIERE))

# Check ----

length(unique(paste0(RICA_RA_SIQO_product$IDENT,RICA_RA_SIQO_product$CODE_PRODUIT))) == nrow(RICA_RA_SIQO_product)

# same number of farms than RICA-RA

##### Output ----

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])




