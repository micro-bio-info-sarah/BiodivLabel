# A.4.5 Intensity of fertilizing: fertilizers ----

library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringi)
library(stringr)

# here we considered that fertilizers are applied both on crops and grasslands

# Input data ----


if (my_DB == "RICA") {
  
  # transfert table
  tmp_TT_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops") %>%
    rename(crop = RICA_code_number)
  
  tmp_input <- RICA_2020_veg %>%
    # summaries areas by crops
    group_by(IDENT,CODE3)%>%
    summarise(
      area_ha = sum(SUPER3*10^-2,na.rm = T)
    ) %>%
    # add mineral nitrogen consumption (kg N)
    left_join(.,RICA_2020 %>% select(IDENT,CONSON,AGBIO)) # 7266 farms
  
  }

if (my_DB == "FADN") {
  
  # transfert table
  tmp_TT_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops") %>%
    rename(crop = FADN_code_letter)
  
  tmp_input <- FADN_18 %>%
    # area in hectares for each crop
    # Quantity of N used in mineral fertilisers
    ## INUSE_Q	H_SC_3031_Q	Quantity of N used in mineral fertilisers Quantity	in tonnes
    select(ID,INUSE_Q, all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_TA"),colnames(FADN_18)))) %>%
    pivot_longer(cols = !c(ID,INUSE_Q),names_to = "crop",values_to = "area_ha") %>%
    mutate(crop = gsub("_TA","",crop)) %>%
    rename(farm_id = ID) %>%
    mutate(CONSON = INUSE_Q*10^3) %>%
    # select variables
    select(farm_id,crop,area_ha,CONSON)
  
}

# EPK averages ----

#source("~/BiodivLabel/scripts/EPK_data_Nferti_avrg.R", encoding = 'UTF-8')
## execute script when EPK 2017 data available
tmp_EPK_Nferti <- read_xlsx("data_in/supp_data.xlsx",sheet = "PKGC_N_ferti")

# assign averages by crop codes (60 different crops)
tmp_N_avrg <- tmp_TT_crops %>%
  select(crop,EPK_ferti_crop_name) %>%
  # select crop code and name
  filter(crop %in% unique(tmp_input$crop)) %>%
  # add average fertilization data from EPK 2017
  rowwise() %>%
  mutate(
    N_min_Conventionel = mean(tmp_EPK_Nferti$N_min[
      tmp_EPK_Nferti$BIO == 0 
      & tmp_EPK_Nferti$EPK_ferti_crop_name %in% unlist(strsplit(EPK_ferti_crop_name,";"))],na.rm = T),
    
    N_org_Conventionel = mean(tmp_EPK_Nferti$N_org[
      tmp_EPK_Nferti$BIO == 0 
      & tmp_EPK_Nferti$EPK_ferti_crop_name %in% unlist(strsplit(EPK_ferti_crop_name,";"))],na.rm = T),
    
    N_min_Bio = 0,
    
    N_org_Bio = mean(tmp_EPK_Nferti$N_org[
      tmp_EPK_Nferti$BIO == 1 
      & tmp_EPK_Nferti$EPK_ferti_crop_name %in% unlist(strsplit(EPK_ferti_crop_name,";"))],na.rm = T)
    ## NB: for crops without specific reference average, we used average of similar crops
  ) %>%
  ## quality check / sensi
  ## if no crops is similar enough, we used the global average ??? for now, I use zeros
  ## ??? i choose mean all for missing values => voir agreste EPK maraichâge pour voir si valeurs ferti: que données sur IFT en public mais dossier EPK dans CASD ...
  replace_na(list(
    N_min_Conventionel = 0,#mean(tmp_EPK_Nferti$N_min[tmp_EPK_Nferti$BIO == 0],na.rm = T),
    N_org_Conventionel = 0,# mean(tmp_EPK_Nferti$N_org[tmp_EPK_Nferti$BIO == 0],na.rm = T),
    N_min_Bio = 0,
    N_org_Bio = 0 #mean(tmp_EPK_Nferti$N_org[tmp_EPK_Nferti$BIO == 1], na.rm = T)
  ))

# Estimate N org threshold for farms with livestock ----

#source("~/BiodivLabel/scripts/EPK_data_Norg_threshold.R", encoding = 'UTF-8')
## execute script when EPK 2017 data available
tmp_EPK_Norg <- read_xlsx("data_in/supp_data.xlsx",sheet = "PKGC_N_ferti_org")

# assign threshold by crop codes (60 different crops)
tmp_Norg_thrhld <- tmp_TT_crops %>%
  select(crop,EPK_ferti_crop_name) %>%
  # select crop code and name
  filter(crop %in% unique(tmp_input$crop)) %>%
  # add average fertilization data from EPK 2017
  rowwise() %>%
  mutate(
    Norg_thrhld_conv = mean(tmp_EPK_Norg$Norg_thrhld[
      tmp_EPK_Norg$BIO == 0 
      & tmp_EPK_Norg$EPK_ferti_crop_name %in% unlist(strsplit(EPK_ferti_crop_name,";"))],na.rm = T),
    
    Norg_thrhld_bio = mean(tmp_EPK_Norg$Norg_thrhld[
      tmp_EPK_Norg$BIO == 1 
      & tmp_EPK_Norg$EPK_ferti_crop_name %in% unlist(strsplit(EPK_ferti_crop_name,";"))],na.rm = T)
    ## NB: for crops without specific reference average, we used average of similar crops
  )

# keep only average and threshold variables
tmp_N_avrg_n_thrhld <- tmp_N_avrg %>%
  left_join(.,tmp_Norg_thrhld %>%
              select(crop,Norg_thrhld_conv,Norg_thrhld_bio)) %>%
  # select threshold as the minimum between valeur forfaitaire et borne inférieure CI 95%
  rowwise() %>%
  mutate(
    Norg_thrhld_min_conv = min(c(N_org_Conventionel,Norg_thrhld_conv),na.rm = T),
    Norg_thrhld_min_bio = min(c(N_org_Bio,Norg_thrhld_bio),na.rm = T)
  )
  

##### Join data ----

tmp_data <- tmp_input %>%
  # add N averages
  left_join(.,tmp_N_avrg_n_thrhld) %>%
  # add N excreted from livestock
  left_join(.,N_excr %>% group_by(farm_id) %>% summarise(Nex_total = sum(Nex_total,na.rm = T))) %>% ungroup()

# 7266 farms

##### Estimate parameter ----

tmp_N_ferti <- tmp_data %>%
  # here we considered that fertilizers are applied both on crops and grasslands
  filter(
    # keep only crops & grasslands with areas
    SUPER3 > 0, # 7264 farms
    # remove organic farms with CONSON>0, as mineral fertilization is supposed to be null in organic farms
    !(AGBIO %in% c(2,4) & CONSON >0)
    ) %>% # 7069 farms
  # calculate parameter
  mutate(
    # x_i
    x_i = CONSON,
    # calculate SAU_c in hectares
    SAU_c = SUPER3 * 10^-2,
    # calculate pseudo-fertilization
    Mmin_SAU_c = case_when(
      AGBIO == 0 ~ N_min_Conventionel * SAU_c,
      AGBIO %in% c(2,4) ~ N_min_Bio * SAU_c
      ),
    Morg_SAU_c = case_when(
      AGBIO == 0 ~ N_org_Conventionel * SAU_c,
      AGBIO %in% c(2,4) ~ N_org_Bio * SAU_c
    )
  ) %>%
  # calculate sum of N_min averages for each farm
  group_by(farm_id) %>%
  mutate(
    sum_Mmin_SAU_c = sum(Mmin_SAU_c),
    sum_Morg_SAU_c = sum(Morg_SAU_c)
  ) %>%
  # calculate Y_c for each crop in each farm
  mutate(
    ## mineral N fertilizer
    ## still zero for organic farms
    A.4.5_min = case_when(
      AGBIO %in% c(2,4) ~ 0,
      x_i == 0 ~0,
      .default = (x_i * (Mmin_SAU_c/sum_Mmin_SAU_c)) / SAU_c
    ),
    ## organic N fertilizer
    ### estimate if enough livestock
    livestock = case_when(
      #### Conventional
      AGBIO == 0 & Nex_total >0 & ((Nex_total * (Morg_SAU_c/sum_Morg_SAU_c)) / SAU_c) >= Norg_thrhld_min_conv  ~ T,
      #### Organic
      AGBIO %in% c(2,4) & Nex_total >0 & ((Nex_total * (Morg_SAU_c/sum_Morg_SAU_c)) / SAU_c) >= Norg_thrhld_min_bio  ~ T,
      .default = F
    ),
    ### estimate N org
    A.4.5_org = case_when(
      ### if livestock: N excreted from livestock
      livestock == T ~ (Nex_total * (Morg_SAU_c/sum_Morg_SAU_c)) / SAU_c,
      ### if no or not enough livestock: average organic fertilizer input
      livestock == F & AGBIO == 0 ~ N_org_Conventionel,
      livestock == F & AGBIO %in% c(2,4) ~ N_org_Bio
    ),
    # A.4.5, kg N ha-1
    A.4.5 = A.4.5_min + A.4.5_org 
  ) %>% ungroup()

# 7069 farms

##### Output ----

BV_A.4.5 = tmp_N_ferti %>%
  filter(!is.na(A.4.5)) %>%
  select(farm_id,crop,A.4.5,A.4.5_min,A.4.5_org )

# 6918 farms 

# quality check : OK
## je dois retrouver mes totaux de quantité d'azote min
#tmp = left_join(tmp_N_ferti %>% group_by(farm_id) %>% summarise(xi = sum(A.4.5_min*SAU_c)),RICA_2020 %>% select(farm_id,AGBIO,CONSON)) %>% mutate(comp = case_when(round(xi,0) == round(CONSON,0) ~T,.default = F))

#view(tmp_N_ferti %>% group_by(crop) %>% summarise(A.4.5_min=mean(A.4.5_min),A.4.5_org=mean(A.4.5_org)))
#view(tmp_N_ferti %>% group_by(crop) %>% summarise(A.4.5_min=mean(A.4.5_min,na.rm = T),A.4.5_org=mean(A.4.5_org,na.rm = T)))

# quality check for organic farms
#tmp = BV_A.4.5 %>% filter(farm_id %in% RICA_2020$farm_id[RICA_2020$AGBIO %in% c(2)])

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])
