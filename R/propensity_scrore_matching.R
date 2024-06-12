
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
  left_join(., tmp_TT_crops %>% select(CODE3,LIBELLE,species) %>% mutate(CODE3 = as.character(CODE3)), by = c("product_RICA_CODE" = "CODE3")) %>%
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
# RA2020_MO_CHEF_COEXPL<-data.table(readRDS("DATA/RGA_RA_2020/Données au format R/RA2020_MO_CHEF_COEXPL_220415.rds"))
# RA2020_MO_CHEF_COEXPL<-RA2020_MO_CHEF_COEXPL[STATUTDIRIG==1,.(NOM_DOSSIER, ANAIS, MOFGENE, MOFAGRI)]# je ne garde que les chefs d'exploitation (1 par expl)
# summary(RA2020_MO_CHEF_COEXPL)
# RA2020_MO_CHEF_COEXPL[,AGE:=2020-ANAIS]
# RA2020_MO_CHEF_COEXPL[is.na(MOFGENE),length(NOM_DOSSIER)]#5894
# RA2020_MO_CHEF_COEXPL[is.na(MOFAGRI),length(NOM_DOSSIER)]#0-> mieux rempli
# # Calcul variables pertinentes niveaux de formation
# RA2020_MO_CHEF_COEXPL[, NivForm:=ifelse(MOFGENE>MOFAGRI&!is.na(MOFGENE), MOFGENE,MOFAGRI)]
# RA2020_MO_CHEF_COEXPL[,Formation4M:=ifelse(NivForm %in% c("00", "10"),"0",
#                                  ifelse(NivForm %in% c("11", "23", "24"), "1",
#                                         ifelse(NivForm %in% c("25", "26"), "2","3")
#                                  )
# ) ]
# RA2020_MO_CHEF_COEXPL[,FormationAgri4M:=ifelse(MOFAGRI %in% c("00", "10"),"0",
#                                      ifelse(MOFAGRI %in% c("11", "23", "24"), "1",
#                                             ifelse(MOFAGRI %in% c("25", "26"), "2","3")
#                                      )
# ) ]

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

### SUR AB ####
#----------------------------#

# Choix de méthode pour l'instant: exact matching sur tmp_code RICA, SIEGE region et Dim eco, Nearest sur PBS et MNT
tmp_base<-tmp_basePscore %>%
  ## J'enlève les LR: MAtching sur AB: Treated=AB
  filter(label %in% c("AB", "Conventionnel")) %>%
  # je créé variable AB binaire
  mutate(AB = case_when(
  label=="AB" ~ 1,
  .default= 0)
  )

## MATCHING
tmp_match_obj<-matchit(AB~lib_RICA_CODE + SIEGE_REG + DIMECO_COEF2017 + MNT +PBSTOT_COEF17,
                   data=tmp_base, method='nearest', exact=c('lib_RICA_CODE','SIEGE_REG', 'DIMECO_COEF2017'),
                   ratio=3,
                   replace=F
)

tmp_r2 <- fixest::feglm(AB~lib_RICA_CODE + SIEGE_REG + DIMECO_COEF2017 + MNT +PBSTOT_COEF17,
                data = tmp_base,family = binomial(link='logit'))
tmp_r2 <- fixest::r2(tmp_r2,type='all')

# Resultats matching
tmp_sum<-summary(tmp_match_obj)
tmp_sum

# Graphs Resultats matching: Pour description de l'ECHANTILLON matched/unmatched
plot(tmp_match_obj, type="jitter", interactive=FALSE)# observe distribution des scores
love.plot(tmp_match_obj, binary="std")# observe distribution des scores
tmp_diffDIMECO<-plot(tmp_match_obj, type="density", interactive=FALSE, which.xs="DIMECO_COEF2017")# observe distribution des scores
tmp_diffMontagne<-plot(tmp_match_obj, type="density", interactive=FALSE, which.xs="MNT")
tmp<-bal.tab(tmp_match_obj,un=TRUE)
tmp
#bal.tab(tmp_match_obj,un=TRUE,thresholds = c(m=.1,v=2))
# png(file=paste0("RESULTATS/Lovematched_unmatched.png"),width=1000,height=700)
# love.plot(tmp_match_obj, binary="std")# observe distribution des scores
# dev.off()

# Récupération de l'échantillon matché.
tmp_matched_data<-match.data(tmp_match_obj)

# Regressions Linéaires
tmp_res_ha<-lm(BVI_ha~AB, data=tmp_matched_data, weights=weights)
tmp_t_ha<-coeftest(tmp_res_ha,vcov.=vcovCL,cluster=~SIEGE_REG)# Impact avec Robust-Clustered 
tmp_t_ha
tmp_coefAB_ha<-coefficients(tmp_res_ha)[[2]]
tmp_res_kg<-lm(BVI_kg~AB, data=tmp_matched_data, weights=weights)
tmp_t_kg<-coeftest(tmp_res_kg,vcov.=vcovCL,cluster=~SIEGE_REG)# Impact avec Robust-Clustered 
tmp_t_kg
tmp_coefAB_kg<-coefficients(tmp_res_kg)[[2]]
#save(tmp_match_obj,  tmp_sum, tmp, res, file="RESULTATS/results_global.R")

# yoann version
tmp_ha <- fixest::feols(BVI_ha~AB, data=tmp_matched_data, weights=tmp_matched_data$weights,cluster='SIEGE_REG')
summary(tmp_ha)
tmp_kg <- fixest::feols(BVI_kg~AB, data=tmp_matched_data, weights=tmp_matched_data$weights,cluster='SIEGE_REG')
summary(tmp_kg)
# df
#tmp_df<-coeftest(tmp_ha,vcov.=vcovCL,cluster=~SIEGE_REG,df=fixest::degrees_freedom(tmp_r2,type='t'))# Impact avec Robust-Clustered 
#tmp_df

# Sur chaque culture
for (tmp_code in unique(tmp_matched_data$product_RICA_CODE)){
  print(unique(tmp_matched_data$LIBELLE[tmp_matched_data$product_RICA_CODE==tmp_code]))
  tmp_res_ha<-lm(BVI_ha~AB, data=tmp_matched_data[tmp_matched_data$product_RICA_CODE==tmp_code,], weights=weights)
  tmp_t_ha<-coeftest(tmp_res_ha,vcov.=vcovCL,cluster=~SIEGE_REG)# Impact avec Robust-Clustered
  print(tmp_t_ha)
  tmp_coefAB_ha<-coefficients(tmp_res_ha)[[2]]
}
for (tmp_code in unique(tmp_matched_data$product_RICA_CODE)){
  print(unique(tmp_matched_data$LIBELLE[tmp_matched_data$product_RICA_CODE==tmp_code]))
  tmp_res_kg<-lm(BVI_kg~AB, data=tmp_matched_data[tmp_matched_data$product_RICA_CODE==tmp_code,], weights=weights)
  tmp_t_kg<-coeftest(tmp_res_kg,vcov.=vcovCL,cluster=~SIEGE_REG)# Impact avec Robust-Clustered 
  print(tmp_t_kg)
  tmp_coefAB_kg<-coefficients(tmp_res_kg)[[2]]
}



### SUR BLE TENDRE LR ####
#----------------------------#
tmp_code<-111

# Choix de méthode pour l'instant: exact matching sur tmp_code RICA, SIEGE region et Dim eco, Nearest sur PBS et MNT
tmp_base<-tmp_basePscore %>%
  filter(product_RICA_CODE==tmp_code) %>%
  ## Treated=LR
  filter(label%in%c("LR", "Conventionnel")) %>%
  # je créé variable LR binaire
  mutate(LR = case_when(
    label=="LR" ~ 1,
    .default= 0)
  )

## MATCHING # 3 matche est le mieux si on peut
tmp_match_obj <- matchit(LR~ SIEGE_REG + DIMECO_COEF2017 + MNT +PBSTOT_COEF17,
                   data=tmp_base, method='nearest', exact=c('SIEGE_REG', 'DIMECO_COEF2017'),
                   ratio=3,
                   replace=F
)

# Resultats matching
tmp_sum <- summary(tmp_match_obj)
tmp_sum

# Graphs Resultats matching: Pour description de l'ECHANTILLON matched/unmatched
plot(tmp_match_obj, type="jitter", interactive=FALSE)# observe distribution des scores
love.plot(tmp_match_obj, binary="std")# observe distribution des scores
tmp_diffDIMECO<-plot(tmp_match_obj, type="density", interactive=FALSE, which.xs="DIMECO_COEF2017")# observe distribution des scores
tmp_diffMontagne<-plot(tmp_match_obj, type="density", interactive=FALSE, which.xs="MNT")
tmp<-bal.tab(tmp_match_obj,un=TRUE)
tmp
# png(file=paste0("RESULTATS/Lovematched_unmatched.png"),width=1000,height=700)
# love.plot(tmp_match_obj, binary="std")# observe distribution des scores
# dev.off()

# Récupération de l'échantillon matché. Jointure avec celui de l'AB
tmp_matched_data_LR<-match.data(tmp_match_obj) %>%
  # specific conventionnal that match with LR
  mutate(label = case_when(
    label == "Conventionnel" ~ "Conventionnel-LR",
    .default = label))
tmp_matched_data_group<-rbind(as.data.table(tmp_matched_data), as.data.table(tmp_matched_data_LR),fill=T)

# Regressions Linéaires
tmp_res_ha<-lm(BVI_ha~LR, data=tmp_matched_data_LR, weights=weights)
tmp_t_ha<-coeftest(tmp_res_ha,vcov.=vcovCL,cluster=~SIEGE_REG)# Impact avec Robust-Clustered 
tmp_t_ha
tmp_res_kg<-lm(BVI_kg~LR, data=tmp_matched_data_LR, weights=weights)
tmp_t_kg<-coeftest(tmp_res_kg,vcov.=vcovCL,cluster=~SIEGE_REG)# Impact avec Robust-Clustered 
tmp_t_kg
#save(tmp_match_obj,  tmp_sum, tmp, res, file="RESULTATS/results_global.R")

### SUR LES AOP Laitieres ####
#-------------------#
# Choix de méthode pour l'instant: exact matching sur tmp_code RICA, SIEGE region et Dim eco, Nearest sur PBS et MNT
tmp_base<-tmp_basePscore

## Matching sur AOP: Treated=AOP
tmp_base<-tmp_base[!(label%in%c("AB", "LR"))]
tmp_base<-tmp_base[product_RICA_CODE=="milk"]
tmp_base[,AOP:=ifelse(label!="Conventionnel",1,0)]# je créé variable AOP binaire

## MATCHING
tmp_match_obj<-matchit(AOP~ SIEGE_REG + DIMECO_COEF2017 + MNT +PBSTOT_COEF17,
                   data=tmp_base, method='nearest', exact=c('SIEGE_REG', 'DIMECO_COEF2017'),
                   ratio=1,
                   replace=F
)

# Resultats matching
tmp_sum<-summary(tmp_match_obj)
tmp_sum

# Graphs Resultats matching: Pour description de l'ECHANTILLON matched/unmatched
plot(tmp_match_obj, type="jitter", interactive=FALSE)# observe distribution des scores
love.plot(tmp_match_obj, binary="std")# observe distribution des scores
tmp_diffDIMECO<-plot(tmp_match_obj, type="density", interactive=FALSE, which.xs="DIMECO_COEF2017")# observe distribution des scores
tmp_diffMontagne<-plot(tmp_match_obj, type="density", interactive=FALSE, which.xs="MNT")
tmp<-bal.tab(tmp_match_obj,un=TRUE)
tmp
# png(file=paste0("RESULTATS/Lovematched_unmatched.png"),width=1000,height=700)
# love.plot(tmp_match_obj, binary="std")# observe distribution des scores
# dev.off()

# Récupération de l'échantillon matché.
matched_data_AOP<-match.data(tmp_match_obj)
matched_data_AOP[label=="Conventionnel", label:="Conventionnel-AOP"]
tmp_matched_data_group<-rbind(as.data.table(tmp_matched_data_group), as.data.table(matched_data_AOP),fill=tmp)

# Regressions Linéaires
tmp_res_ha<-lm(BVI_ha~AOP, data=matched_data_AOP, weights=weights)
tmp_t_ha<-coeftest(tmp_res_ha,vcov.=vcovCL,cluster=~SIEGE_REG)# Impact avec Robust-Clustered 
tmp_t_ha
tmp_res_kg<-lm(BVI_kg~AOP, data=matched_data_AOP, weights=weights)
tmp_t_kg<-coeftest(tmp_res_kg,vcov.=vcovCL,cluster=~SIEGE_REG)# Impact avec Robust-Clustered 
tmp_t_kg

# Sur chaque Groupe d'AOP
for (tmp_code in unique(matched_data_AOP$label)){
  print(tmp_code)
  tmp_res_ha<-lm(BVI_ha~AOP, data=matched_data_AOP[label==tmp_code|label=="Conventionnel-AOP"], weights=weights)
  tmp_t_ha<-coeftest(tmp_res_ha,vcov.=vcovCL)# Impact avec Robust-Clustered 
  print(tmp_t_ha)
}
for (tmp_code in unique(matched_data_AOP$label)){
  tmp_res_kg<-lm(BVI_kg~AOP, data=matched_data_AOP[label==tmp_code|label=="Conventionnel-AOP"], weights=weights)
  tmp_t_kg<-coeftest(tmp_res_kg,vcov.=vcovCL)# Impact avec Robust-Clustered 
  print(tmp_t_kg)
}


#------------------------------------#
#  BOXPLOT FINAL ####
#----------------------------------#

# Boxplot AB Cultures - BVI par HA
png(file=paste0("RESULTATS/BOXPLOT_CULTURE_BVIHA.png"),width=1000,height=700)
ggplot(tmp_matched_data_group[culture==1],aes(lib_RICA_CODE,BVI_ha,fill=label)) +
  geom_boxplot()+
  coord_flip()+
  scale_fill_manual(values=c("green","grey","white","red"))#+
  #theme(legend.position="none")
dev.off()


# Boxplot AB Cultures - BVI par KG
png(file=paste0("RESULTATS/BOXPLOT_CULTURE_BVIKG.png"),width=1000,height=700)
ggplot(tmp_matched_data_group[culture==1],aes(lib_RICA_CODE,BVI_kg,fill=label)) +
  geom_boxplot()+
  coord_flip()+
  scale_fill_manual(values=c("green","grey","white","red"))#+
#theme(legend.position="none")
dev.off()


# Boxplot AB et AOP Produits Laitiers - BVI par HA
tmp_matched_data_group<-tmp_matched_data_group[order(label)]
png(file=paste0("RESULTATS/BOXPLOT_ProdLaitiers_BVIHA.png"),width=1000,height=700)
ggplot(tmp_matched_data_group[culture==0],aes(label,BVI_ha,fill=ifelse(label=="AB","AB", ifelse(label=="Conventionnel", "Conventionnel",ifelse(label=="Conventionnel-AOP", "Conventionnel-AOP", "AOP"))))) +
  geom_boxplot()+
  coord_flip()+
  scale_fill_manual(values=c("green","grey","white","blue"))+
theme(legend.position="none")
dev.off()


# Boxplot AB et AOP Produits Laitiers - BVI par kg
tmp_matched_data_group<-tmp_matched_data_group[order(label)]
png(file=paste0("RESULTATS/BOXPLOT_ProdLaitiers_BVIKG.png"),width=1000,height=700)
ggplot(tmp_matched_data_group[culture==0],aes(label,BVI_kg,fill=ifelse(label=="AB","AB", ifelse(label=="Conventionnel", "Conventionnel",ifelse(label=="Conventionnel-AOP", "Conventionnel-AOP", "AOP"))))) +
  geom_boxplot()+
  coord_flip()+
  scale_fill_manual(values=c("green","grey","white","blue"))+
  theme(legend.position="none")
dev.off()











