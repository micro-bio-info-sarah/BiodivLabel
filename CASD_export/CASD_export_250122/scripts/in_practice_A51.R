# A.5.1 Plant Protection Agents ----

library(tibble)
library(readxl)
library(dplyr)

# we choose a proxy of "Plant protection agents" metric as the value of pesticides (€) applied by crop area
# we considered that no plant protection agents are applied to grasslands

# Input data ----

if (my_DB == "RICA") {

  # transfert table
  tmp_TT_crops <- readxl::read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops") %>%
    rename(crop = RICA_code_number)
  
  tmp_input <- RICA_2020_veg %>%
    # add land use type
    left_join(.,tmp_TT_crops %>% select(crop,land_use_type), by = join_by(CODE3 == crop)) %>%
    # summaries areas by crops
    group_by(IDENT,CODE3,land_use_type)%>%
    summarise(
      area_ha = sum(SUPER3*10^-2,na.rm = T)
    ) %>% ungroup() %>%
    # add mineral nitrogen consumption (kg N)
    left_join(.,RICA_2020 %>% select(IDENT,CHRPH,AGBIO)) %>%
    mutate(org_farming = case_when(
      AGBIO %in% c(2,4) ~ T,
      .default = F
    )) %>%
    filter(
      # keep only crops & grasslands with areas
      area_ha > 0
      ) %>%
    # select variables and obs
    rename(farm_id = IDENT,crop = CODE3,CONSOPEST = CHRPH) %>%
    select(farm_id,org_farming,crop,area_ha,CONSOPEST)
  
}

if (my_DB == "FADN") {

  # transfert table
  tmp_FADN_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "FADN_crop_code")
  tmp_TT_crops0 <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops")
  tmp_TT_crops <- left_join(
    tmp_FADN_crops %>% rename(crop = code_letter),
    tmp_TT_crops0 %>% rename(crop = FADN_code_letter) %>%
      group_by(crop,land_use_type) %>%
      summarise(
        TFI_crop_name = paste0(unique(na.omit(unlist(strsplit(TFI_crop_name,";")))),collapse = ";")
        ))

  tmp_input <- FADN_18 %>%
    # area in hectares for each crop
    # Value of crop protection products
    ## IPROT_V	H_SC_3040_V	Crop protection products Value	in EUR
    select(ID,ORGANIC,IPROT_V, all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_TA"),colnames(FADN_18)))) %>%
    pivot_longer(cols = all_of(intersect(paste0(tmp_FADN_crops$code_letter,"_TA"),colnames(FADN_18))),
                 names_to = "crop",values_to = "area_ha") %>%
    mutate(crop = gsub("_TA","",crop),
           org_farming = case_when(
             ORGANIC %in% c(2,4) ~ T,
             .default = F
           )) %>%
    rename(farm_id = ID) %>%
    mutate(CONSOPEST = IPROT_V) %>%
    # select variables and obs
    select(farm_id,org_farming,crop,area_ha,CONSOPEST) %>%
    filter(
      # keep only crops with areas
      area_ha > 0 #  farms
    )


}

# Calculate reference average ----

# source EPK 2017 (kg ha-1) and others (see raw file)
tmp_ift_ref <- read_excel("data_in/supp_data.xlsx",sheet = "IFT_ref") %>%
  # we will use ift.hbcref = average TFI without biological control agents
  # pk prendre ift hors bc si on ne peut pas déterminer la part de bc dans x_i ???
  mutate(TFI = as.numeric(ifttref))

# assign averages by crop codes (60 different crops)
tmp_TFI <- tmp_TT_crops %>%
  # add average TFI
  # add average ration
  rowwise() %>%
  mutate(
    TFI = mean(tmp_ift_ref$TFI[
      tmp_ift_ref$lib_ift %in% unlist(strsplit(TFI_crop_name,";"))
    ],na.rm = T)
  )

## if no crops is similar enough, we used the global average ???
for (tmp_i in which(is.na(tmp_TFI$TFI))) {
  tmp_TFI$TFI[tmp_i] <- mean(tmp_ift_ref$TFI,
                             na.rm = T)
}

# Join data ----

tmp_data <- tmp_input %>%
  # add TFI
  left_join(.,tmp_TFI)

# Estimate parameter ----

tmp_pesticides <- tmp_data %>%
  # here we considered that no plant protection agents are applied to grasslands
  filter(
    # keep only arable land use type
    land_use_type == "arable"
    ) %>%
    # calculate parameter
  mutate(
    # x_i
    x_i = CONSOPEST,
    # calculate SAU_c in hectares
    SAU_c = area_ha,
    # calculate theoretical TFI for crop area
    TFI_SAU_c = TFI * SAU_c
  ) %>%
  # calculate sum of theoretical TFI for each farm
  group_by(farm_id) %>%
  mutate(
    sum_TFI_SAU_c = sum(TFI_SAU_c,na.rm = T)
  ) %>%
  # calculate Y_c for each crop in each farm
  mutate(
    A.5.1 = case_when(
      # for organic farms
      ## we considered as organic farms those that apply organic farming practices on all their crops: org_farming == 1
      ## we considered that the plant protection agents used in organic farming are 3.82 less impacting for biodiversity than the plant protection agents used in conventional farming (see method)
      org_farming == 1 ~ ((x_i/3.82) * (TFI_SAU_c/sum_TFI_SAU_c)) / SAU_c,
      #AGBIO %in% c(2,4) ~ ((x_i/1) * (TFI_SAU_c/sum_TFI_SAU_c)) / SAU_c,
      # for conventional farms
      .default = (x_i * (TFI_SAU_c/sum_TFI_SAU_c)) / SAU_c
    )
  ) %>% ungroup()

#RICA2020: 6869 farms


# add grassland with zero
tmp_grassland <- tmp_data %>%
  # here we considered that no plant protection agents are applied to grasslands
  filter(
    # keep only grassland land use type
    land_use_type == "grassland"
    ) %>%
  # calculate parameter
  mutate(
    # x_i
    x_i = 0,
    # calculate SAU_c in hectares
    SAU_c = area_ha,
    # calculate theoretical TFI for crop area
    TFI_SAU_c = 0
  ) %>%
  # calculate sum of theoretical TFI for each farm
  group_by(farm_id) %>%
  mutate(
    sum_TFI_SAU_c = 0
  ) %>%
  # calculate Y_c for each crop in each farm
  mutate(A.5.1 = 0) %>%
  ungroup()

tmp_pesticides <- tmp_pesticides %>%
  rbind(tmp_grassland)

# check -----

tmp_check_farm_id <- sort(sample(tmp_pesticides$farm_id[tmp_pesticides$org_farming == 0],size = 15))

# compare sums
tmp_check1 <- tmp_pesticides %>%
  group_by(farm_id,org_farming) %>%
  summarise(sum_pest = sum(A.5.1*SAU_c))
tmp_check2 <- tmp_input
tmp_check <- left_join(
  tmp_check1 %>% filter(farm_id %in% tmp_check_farm_id),
  tmp_check2 %>% filter(farm_id %in% tmp_check_farm_id)
)
table(round(tmp_check$sum_pest) == round(tmp_check$CONSOPEST))

# quality check : OK


## Output ----

BV_A.5.1 = tmp_pesticides %>%
  select(farm_id,land_use_type,crop,A.5.1) %>%
  ungroup()

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])

