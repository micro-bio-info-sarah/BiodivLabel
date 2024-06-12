
# Etude Propensity Score, BVI BIO/SIQO/Conventionnel
#----------------------------------------------------#
# Julie Regolo
# 11 avril 2024

# Sarah Huet
# 18/04/2024
#-----------------#

#--------------------------------------------------------------------------------------------------------------------#
# 0 - Libraries, fonctions et paramètres ####
#--------------------------------------------------------------------------------------------------------------------#

#rm(list=ls())
library(data.table)
library(stringr)
library(dplyr)
library(tidyverse)
library(MatchIt)
library(lmtest)
library(sandwich)
library(cobalt)

#### PLAN
# Chargement des données: BVI/RGA2020
# Propensity score matching sur exploitations BIO/SIQO/Conventionnel
# Examen des differences de BVI après matching
# Priorité variables de contrôles: Region, Taille, Montagne (RA)


#--------------------------------------------------------------------------------------------------------------------#
# 1 - Chargement des données BVI (envoyées par Sarah) ####
#--------------------------------------------------------------------------------------------------------------------#

library(readr)
tmp_ExplSIQO <- read_csv("RICA_RA_SIQO_farms.csv")
tmp_ProdSIQO <- read_csv("RICA_RA_SIQO_products.csv")

# Ajout des libellés tmp_code RICA
library(readxl)
tmp_TT_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops")
tmp_ProdSIQO <- tmp_ProdSIQO %>%
  left_join(., tmp_TT_crops %>% select(CODE3,LIBELLE,species) %>%
              mutate(CODE3 = as.character(CODE3)), by = c("product_RICA_CODE" = "CODE3")) %>%
  mutate(lib_RICA_CODE = case_when(
    !is.na(LIBELLE) ~ "Produits végétaux",
    is.na(LIBELLE) ~ "Produits Laitiers"
  )) %>%
#table(is.na(tmp_ProdSIQO$lib_RICA_CODE))#Empty
  # Tri de la table: Garde céréales et Produits laitiers
  filter(product_RICA_CODE=="milk"|species == "cereal") %>%
  # j'ajoute les conversions aux AB
  mutate(label = case_when(
    label == "AB (en conversion)" ~ "AB",
    .default = label
  )) %>%
  #creation variable culture/produits laitiers
  mutate(
    culture = case_when(product_RICA_CODE!="milk"~ 1,
                        .default = 0)
  )

#Description rapide
summary(tmp_ProdSIQO)
unique(tmp_ProdSIQO$lib_RICA_CODE)
unique(tmp_ProdSIQO$label)
unique(tmp_ProdSIQO$label[tmp_ProdSIQO$culture == 1])
unique(tmp_ProdSIQO$label[tmp_ProdSIQO$culture == 0])

#

#--------------------------------------------------------------------------------------------------------------------#
# 2 - Chargement des données RA_2020 necessaires ####
#--------------------------------------------------------------------------------------------------------------------#
# Je garde beaucoup de variables au cas ou, mais variables importantes au debut
tmp_RA <- read_csv("data_CASD/RGA_RA_2020/Données au format csv/RA2020_EXPLOITATIONS_240112.csv")
tmp_VariablesRA <- tmp_RA %>%
  select(NOM_DOSSIER, 
         SIEGE_REG, 
         SIEGE_DEP, 
         DIMECO_COEF2017, 
         REGL_1305_2013,
         STATUT, 
         PBSTOT_COEF17, 
         OTEFDA_COEF17,
         OTEFDD_COEF17, 
         OTE64_COEF2017, 
         CIRCOUFIL, 
         EXTERNALISFIL, 
         TRANSFOFIL,  
         UGBTA.TOT, 
         SAU_TOT
         ) %>%
  mutate(
    MNT=case_when(
      REGL_1305_2013=="MNT_ANC" ~ 1,
      .default = 0)#REGL_1305_2013: ICHN. Modalités:MNT_ANC(zone Montagne),LNT_ANC(pas montagne mais contraintes naturelles), OTH_ANC: contraintes specifiques
    ) %>%
  #Ajustement
  replace_na(list(SAU_TOT = 0,UGBA.TOT = 0))
rm(tmp_RA)

### Ajout peut etre plus tard de Formation/Age Exploitant ####

# # Je vais chercher les infos controles sur les exploitants (formation, age)
RA2020_MO_CHEF_COEXPL<-data.table(readRDS("DATA/RGA_RA_2020/Données au format R/RA2020_MO_CHEF_COEXPL_220415.rds"))

tmp_VariablesRA <- tmp_VariablesRA %>%
  # add age and educational level of the manager
  left_join(.,
            RA2020_MO_CHEF_COEXPL_240112 %>%
              # id as num
              mutate(NOM_DOSSIER = as.numeric(NOM_DOSSIER)) %>%
              # keep only manager
              filter(STATUTDIRIG ==1) %>%
              #age
              mutate(AGE = 2020- ANAIS) %>%
              # educational level
              mutate(NivForm = case_when(
                as.numeric(MOFGENE) > as.numeric(MOFAGRI) & !is.na(MOFGENE) ~ MOFGENE,
                .default = MOFAGRI
              )) %>%
              select(NOM_DOSSIER,AGE,NivForm))

#--------------------------------------------------------------------------------------------------------------------#
# 3 - Appariemment RA et BVI ####
#--------------------------------------------------------------------------------------------------------------------#
tmp_basePscore<-merge(tmp_ProdSIQO, tmp_VariablesRA, by="NOM_DOSSIER")# Tout a matché, ok

tmp_stat<- tmp_basePscore %>%
  group_by(label,product_RICA_CODE) %>%
  summarise(
    Grandes=sum(ifelse(substr(DIMECO_COEF2017,1,1)=="4",1,0)),
    Moyennes=sum(ifelse(substr(DIMECO_COEF2017,1,1)=="3",1,0)),
    Petites=sum(ifelse(substr(DIMECO_COEF2017,1,1)=="2",1,0)),
    Micro=sum(ifelse(substr(DIMECO_COEF2017,1,1)=="1",1,0)),
    MNT=sum(MNT))


#--------------------------------------------------------------------------------------------------------------------#
# 4- Analyse économétrique par score de propension ####
#--------------------------------------------------------------------------------------------------------------------#

# data ----
# Choix de méthode pour l'instant: exact matching sur tmp_code RICA, SIEGE region et Dim eco, Nearest sur PBS et MNT
tmp_base<-tmp_basePscore %>%
  # je créé variable label binaire
  mutate(
    label_bool = case_when(
      label !="Conventionnel" ~ 1,
      .default= 0)
  )

# test cereals


# Sur chaque produit pour chaque label ----

tmp_matched_data_all <- tibble()
tmp_pttest_all <- list()

for (tmp_product in sort(unique(tmp_base$product_RICA_CODE[tmp_base$label != "Conventionnel"]))) {
  #tmp_product = sort(unique(tmp_base$product_RICA_CODE[tmp_base$label != "Conventionnel"]))[1]
  
  # subset data
  loop1_data = tmp_base %>% 
    filter(product_RICA_CODE == tmp_product)
  
  for (tmp_code in unique(loop1_data$label[loop1_data$label != "Conventionnel"])){ # bug avec les Fromages de Savoie
    #tmp_code = unique(loop1_data$label[loop1_data$label != "Conventionnel"])[2]
    
    # subset data
    loop2_data = loop1_data %>% 
      filter(label %in% c(tmp_code,"Conventionnel"))
    
    # propensity score & match
    loop2_match_obj<-matchit(label_bool ~ SIEGE_REG + MNT + PBSTOT_COEF17 + AGE + NivForm,
                             data=loop2_data, method='nearest', exact=c('SIEGE_REG'),
                             ratio=3,
                             replace=F)
    
    plot(loop2_match_obj, type="jitter", interactive=FALSE)# observe distribution des scores
    love.plot(loop2_match_obj, binary="std")# observe distribution des scores
    loop2_matched_data <-match.data(loop2_match_obj)
    
    # paired t.test
    
    # BVI hectare
    ## differences between paired observations
    loop2_dif <- loop2_matched_data %>% 
      group_by(label,subclass) %>% 
      summarise(value = mean(BVI_ha)) %>% 
      mutate(ttt = case_when(
        label != "Conventionnel" ~ "treated",
        label == "Conventionnel" ~ "control"
      )) %>%
      pivot_wider(id_cols = subclass,names_from = ttt,values_from = value) %>%
      mutate(diff = treated - control)
    # paired t-test
    loop2_ha <- t.test(loop2_dif$treated,loop2_dif$control,paired = T)
    
    print(tmp_product)
    print(tmp_code)
    print("BVI / ha : is p-value <= 0.05?")
    print(loop2_ha[["p.value"]] <= 0.05)
    rm(loop2_dif)
    
    # BVI kilo
    ## differences between paired observations
    loop2_dif <- loop2_matched_data %>% 
      group_by(label,subclass) %>% 
      summarise(value = mean(BVI_kg)) %>% 
      mutate(ttt = case_when(
        label != "Conventionnel" ~ "treated",
        label == "Conventionnel" ~ "control"
      )) %>%
      pivot_wider(id_cols = subclass,names_from = ttt,values_from = value) %>%
      mutate(diff = treated - control)
    ## paired t test
    loop2_kg <- t.test(loop2_dif$treated,loop2_dif$control,paired = T)
    
    print("BVI / kg : is p-value <= 0.05?")
    print(loop2_kg[["p.value"]] <= 0.05)
    rm(loop2_dif)
    
    # extract tibble & list
    tmp_matched_data_all <- tmp_matched_data_all %>%
      rbind(.,loop2_matched_data %>% mutate(match = paste0(tmp_product,"__",tmp_code)))
    tmp_pttest_all[[tmp_product]][[tmp_code]][["ha"]] <- loop2_ha
    tmp_pttest_all[[tmp_product]][[tmp_code]][["kg"]] <- loop2_kg
    
    rm(list = names(.GlobalEnv)[grep("loop2",names(.GlobalEnv))])
    
  }
  
  rm(list = names(.GlobalEnv)[grep("loop1",names(.GlobalEnv))])
}

# hectares
ggplot(tmp_matched_data_all) +
  aes(x = label, y = BVI_ha) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(vars(match), scales = "free")

view(tmp_basePscore %>% aggregate(NOM_DOSSIER~product_RICA_CODE * label, 
                                  FUN = function(x) length(unique(x))))
view(tmp_matched_data_all %>% aggregate(NOM_DOSSIER~match*product_RICA_CODE*label, 
                                        FUN = function(x) length(unique(x))))
# kilo
ggplot(tmp_matched_data_all) +
  aes(x = label, y = BVI_kg) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(vars(match), scales = "free")


