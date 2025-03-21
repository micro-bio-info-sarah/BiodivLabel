
# A.4.5 Intensity of fertilizing: fertilizers

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
    # add land use type
    left_join(.,tmp_TT_crops %>% select(crop,land_use_type), by = join_by(CODE3 == crop)) %>%
    # summaries areas by crops
    group_by(IDENT,CODE3,land_use_type)%>%
    summarise(
      area_ha = sum(SUPER3*10^-2,na.rm = T),
      .groups = "keep"
    ) %>% ungroup() %>%
    # add mineral nitrogen consumption (kg N)
    left_join(.,RICA_2020 %>% select(IDENT,CONSON,AGBIO),
              by = join_by(IDENT)) %>%
    mutate(org_farming = case_when(
             AGBIO %in% c(2,4) ~ T,
             .default = F
           )) %>%
    filter(
      # keep only crops & grasslands with areas
      area_ha > 0, # 7264 farms
      # remove organic farms with CONSON>0, as mineral fertilization is supposed to be null in organic farms
      !(org_farming == 1 & CONSON >0)
    ) %>%
    # select variables and obs
    rename(farm_id = IDENT,crop = CODE3) %>%
    select(farm_id,land_use_type,org_farming,crop,area_ha,CONSON)    
  # 7069 farms

  }

if (my_DB == "FADN") {

  # transfert table
  tmp_FADN_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "FADN_crop_code")
  tmp_TT_crops0 <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops")
  tmp_TT_crops <- left_join(
    tmp_FADN_crops %>% rename(crop = code_letter),
    tmp_TT_crops0 %>% rename(crop = FADN_code_letter) %>% select(crop,EPK_ferti_crop_name) %>%
      group_by(crop) %>%
      summarise(
        EPK_ferti_crop_name = paste0(unique(na.omit(unlist(strsplit(EPK_ferti_crop_name,";")))),collapse = ";")
        ))

  tmp_input <- FADN_18 %>%
    # area in hectares for each crop
    # Quantity of N used in mineral fertilisers
    ## INUSE_Q	H_SC_3031_Q	Quantity of N used in mineral fertilisers Quantity	in tonnes
    select(ID,ORGANIC,INUSE_Q, all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_TA"),colnames(FADN_18)))) %>%
    pivot_longer(cols = all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_TA"),colnames(FADN_18))),
                 names_to = "crop",values_to = "area_ha") %>%
    mutate(crop = gsub("_TA","",crop),
           org_farming = case_when(
             ORGANIC %in% c(2,4) ~ T,
             .default = F
             )) %>%
    rename(farm_id = ID) %>%
    mutate(CONSON = INUSE_Q*10^3) %>%
    # select variables and obs
    select(farm_id,org_farming,crop,area_ha,CONSON) %>%
    filter(
      # keep only crops & grasslands with areas
      area_ha > 0,
      # remove organic farms with CONSON>0, as mineral fertilization is supposed to be null in organic farms
      !(org_farming == 1 & CONSON >0)
    )

}

# EPK averages ----

source("~/BiodivLabel/R/EPK_data_Nferti_avrg.R", encoding = 'UTF-8')
## execute script when EPK 2017 data available
#tmp_EPK_Nferti <- read_xlsx("data_in/supp_data.xlsx",sheet = "PKGC_N_ferti")

# assign averages by crop codes (60 different crops)
tmp_N_avrg <- tmp_TT_crops %>%
  select(crop,land_use_type,EPK_ferti_crop_name) %>%
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

source("~/BiodivLabel/R/EPK_data_Norg_threshold.R", encoding = 'UTF-8')
## execute script when EPK 2017 data available
#tmp_EPK_Norg <- read_xlsx("data_in/supp_data.xlsx",sheet = "PKGC_N_ferti_org")

# assign threshold by crop codes (60 different crops)
tmp_Norg_thrhld <- tmp_TT_crops %>%
  select(crop,land_use_type,EPK_ferti_crop_name) %>%
  # select crop code and name
  filter(crop %in% unique(tmp_input$crop)) %>%
  # add N org thresholds calculated from EPK 2017
    ## NB: for crops without specific reference average, we used average of similar crops
  rowwise() %>%
  mutate(
    Norg_thrhld_min_conv = mean(tmp_EPK_Norg$Norg_thrhld_min[
      tmp_EPK_Norg$BIO == 0
      & tmp_EPK_Norg$EPK_ferti_crop_name %in% unlist(strsplit(EPK_ferti_crop_name,";"))],na.rm = T),

    Norg_thrhld_min_bio = mean(tmp_EPK_Norg$Norg_thrhld_min[
      tmp_EPK_Norg$BIO == 1
      & tmp_EPK_Norg$EPK_ferti_crop_name %in% unlist(strsplit(EPK_ferti_crop_name,";"))],na.rm = T),
    
    Norg_thrhld_max_conv = mean(tmp_EPK_Norg$Norg_thrhld_max[
      tmp_EPK_Norg$BIO == 0
      & tmp_EPK_Norg$EPK_ferti_crop_name %in% unlist(strsplit(EPK_ferti_crop_name,";"))],na.rm = T),
    
    Norg_thrhld_max_bio = mean(tmp_EPK_Norg$Norg_thrhld_max[
      tmp_EPK_Norg$BIO == 1
      & tmp_EPK_Norg$EPK_ferti_crop_name %in% unlist(strsplit(EPK_ferti_crop_name,";"))],na.rm = T)
    
  )

# keep only average and threshold variables
tmp_N_avrg_n_thrhld <- tmp_N_avrg %>%
  left_join(.,tmp_Norg_thrhld %>%
              select(crop,land_use_type,Norg_thrhld_min_conv,Norg_thrhld_max_conv,Norg_thrhld_min_bio,Norg_thrhld_max_bio),
            by = join_by(crop, land_use_type)) %>%
  # select threshold as the minimum or maximum between average standard value CI 95% limit
  rowwise() %>%
  mutate(
    Norg_thrhld_min_conv = min(c(N_org_Conventionel,Norg_thrhld_min_conv),na.rm = T),
    Norg_thrhld_max_conv = max(c(N_org_Conventionel,Norg_thrhld_max_conv),na.rm = T),
    Norg_thrhld_min_bio = min(c(N_org_Bio,Norg_thrhld_min_bio),na.rm = T),
    Norg_thrhld_max_bio = max(c(N_org_Bio,Norg_thrhld_max_bio),na.rm = T)
  )



# Join data ----

tmp_data <- tmp_input %>%
  # add N averages
  left_join(.,tmp_N_avrg_n_thrhld,
            by = join_by(land_use_type, crop)) %>%
  # add N excreted from livestock
  left_join(.,N_excr %>% 
              group_by(farm_id) %>% 
              summarise(Nex_total = sum(Nex_total,na.rm = T)),
            by = join_by(farm_id)) %>% ungroup()

# 7266 farms
# 7069 farms

# Estimate parameter ----

tmp_N_ferti <- tmp_data %>%
  # here we considered that fertilizers are applied both on crops and grasslands
  # calculate parameter
  mutate(
    # x_i
    x_i = CONSON,
    # calculate SAU_c in hectares
    SAU_c = area_ha,
    # calculate theoretical fertilization
    Mmin_SAU_c = case_when(
      org_farming == 0 ~ N_min_Conventionel * SAU_c,
      org_farming == 1 ~ N_min_Bio * SAU_c
      ),
    Morg_SAU_c = case_when(
      org_farming == 0 ~ N_org_Conventionel * SAU_c,
      org_farming == 1 ~ N_org_Bio * SAU_c
    )
  ) %>%
  # calculate sum of theoretical fertilization for each farm
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
      org_farming == 1 ~ 0,
      x_i == 0 ~0,
      .default = (x_i * (Mmin_SAU_c/sum_Mmin_SAU_c)) / SAU_c
    ),
    
    ## organic N fertilizer
    ### estimate if enough livestock
    livestock_enough = case_when(
      #### Conventional
      org_farming == 0 & Nex_total >0 & ((Nex_total * (Morg_SAU_c/sum_Morg_SAU_c)) / SAU_c) > Norg_thrhld_min_conv  ~ T,
      #### Organic
      org_farming == 1 & Nex_total >0 & ((Nex_total * (Morg_SAU_c/sum_Morg_SAU_c)) / SAU_c) > Norg_thrhld_min_bio  ~ T,
      .default = F
    ),
    ### estimate if too much livestock
    livestock_toomuch = case_when(
      #### Conventional
      org_farming == 0 & Nex_total >0 & ((Nex_total * (Morg_SAU_c/sum_Morg_SAU_c)) / SAU_c) > Norg_thrhld_max_conv  ~ T,
      #### Organic
      org_farming == 1 & Nex_total >0 & ((Nex_total * (Morg_SAU_c/sum_Morg_SAU_c)) / SAU_c) > Norg_thrhld_max_bio  ~ T,
      .default = F
    ),
    
    ### estimate N org
    A.4.5_org = case_when(
      ### if livestock: N excreted from livestock
      livestock_enough == T & livestock_toomuch == F ~ (Nex_total * (Morg_SAU_c/sum_Morg_SAU_c)) / SAU_c,
      ### if no or not enough livestock: average organic fertilizer input
      livestock_enough == F & livestock_toomuch == F & org_farming == 0 ~ N_org_Conventionel,
      livestock_enough == F & livestock_toomuch == F & org_farming == 1 ~ N_org_Bio,
      ### if too much livestock: average organic fertilizer input
      livestock_enough == T & livestock_toomuch == T & org_farming == 0 ~ Norg_thrhld_max_conv,
      livestock_enough == T & livestock_toomuch == T & org_farming == 1 ~ Norg_thrhld_max_bio
    ),
    # A.4.5, kg N ha-1
    A.4.5 = A.4.5_min + A.4.5_org
  ) %>% ungroup()

print(paste0(length(unique(tmp_N_ferti$farm_id[tmp_N_ferti$livestock_enough == F])),
             " farms have no or not enough livestock. They receive the average national value as a standard value (see Methods). Among them, ",
             length(unique(tmp_N_ferti$farm_id[tmp_N_ferti$livestock_enough == F & tmp_N_ferti$org_farming == T])),
             " are organic farms."
             ))

print(paste0(length(unique(tmp_N_ferti$farm_id[tmp_N_ferti$livestock_toomuch == T])),
             " farms have too much livestock. They receive the maximum organic N threshold as a standard value (see Methods). Among them, ",
             length(unique(tmp_N_ferti$farm_id[tmp_N_ferti$livestock_toomuch == T & tmp_N_ferti$org_farming == T])),
             " are organic farms."
))

print(paste0(length(unique(tmp_N_ferti$farm_id[tmp_N_ferti$livestock_enough == T & tmp_N_ferti$livestock_toomuch == F])),
             " farms have enough and not too much livestock. They spread the total N excreted from their livestock on-farm (see Methods). Among them, ",
             length(unique(tmp_N_ferti$farm_id[tmp_N_ferti$livestock_enough == T & tmp_N_ferti$livestock_toomuch == F & tmp_N_ferti$org_farming == T])),
             " are organic farms."
))



# RICA2020: 7069 farms

# check ----

# compare N averages
tmp_check <- tmp_N_ferti %>%
  filter(crop %in% tmp_N_avrg$crop) %>%
  group_by(crop,org_farming) %>%
  summarise(
    average_Nmin = mean(A.4.5_min,na.rm = T),
    average_Norg = mean(A.4.5_org,na.rm = T),
    .groups = "keep")

quantile(tmp_check$average_Nmin[tmp_check$org_farming == 0],na.rm = T)
quantile(tmp_N_avrg$N_min_Conventionel,na.rm = T)

max(tmp_N_ferti$A.4.5_org[tmp_N_ferti$org_farming == T])

# quantiles OK


# compare sum to original value
sum(RICA_2020$CONSON[RICA_2020$IDENT %in% tmp_N_ferti$farm_id]) == sum(tmp_N_ferti$A.4.5_min*tmp_N_ferti$area_ha)

# quality check : OK

# Output ----

BV_A.4.5 = tmp_N_ferti %>%
  filter(!is.na(A.4.5)) %>%
  select(farm_id,land_use_type,org_farming,area_ha,crop,A.4.5,A.4.5_min,A.4.5_org ) %>%
  ungroup()

# 6918 farms

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])
