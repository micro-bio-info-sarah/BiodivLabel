# A.5.1 Plant Protection Agents ----

library(tibble)
library(readxl)
library(dplyr)

# we choose a proxy of "Plant protection agents" metric as the value of pesticides (€) applied by crop area
# we considered that no plant protection agents are applied to grasslands


##### Calculate reference average ----

# source EPK 2017 (kg ha-1) and others (see raw file)
tmp_ift_ref <- read_excel("data_in/supp_data.xlsx",sheet = "IFT_ref") %>%
  # we will use ift.hbcref = average TFI without biological control agents
  # pk prendre ift hors bc si on ne peut pas déterminer la part de bc dans x_i ???
  mutate(TFI = as.numeric(ifttref))

# transfert table
tmp_TT_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops")

# assign averages by crop codes (60 different crops)
tmp_TFI <- tmp_TT_crops %>%
  # select crop code and name
  filter(CODE3 %in% unique(RICA_2020_veg$CODE3)) %>%
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

##### Join data ----

tmp_data <- RICA_2020_veg %>%
  # summaries areas by crops
  group_by(IDENT,CODE3)%>%
  summarise(
    SUPER3 = sum(SUPER3,na.rm = T)
  ) %>%
  # add pesticide actual cost
  left_join(.,RICA_2020 %>% select(IDENT,CHRPH,AGBIO)) %>% # 7266 farms
  # add TFI
  left_join(.,tmp_TFI)

##### Estimate parameter ----

tmp_pesticides <- tmp_data %>%
  # here we considered that no plant protection agents are applied to grasslands
  filter(
    # keep only arable land use type
    land_use_type == "arable", # 7108 farms
    # keep only crops with areas
    SUPER3 > 0 # 6869 farms
  ) %>%
    # calculate parameter
  mutate(
    # x_i
    x_i = CHRPH,
    # calculate SAU_c in hectares
    SAU_c = SUPER3 * 10^-2,
    # calculate TFI for crop area
    TFI_SAU_c = TFI * SAU_c
  ) %>%
  # calculate sum of TFI for each farm
  group_by(IDENT) %>%
  mutate(
    sum_TFI_SAU_c = sum(TFI_SAU_c,na.rm = T)
  ) %>%
  # calculate Y_c for each crop in each farm
  mutate(
    A.5.1 = case_when(
      # for organic farms
      ## we considered as organic farms those that apply organic farming practices on all their crops: AGBIO %in% c(2,4)
      ## we considered that the plant protection agents used in organic farming are 3.82 less impacting for biodiversity than the plant protection agents used in conventional farming (see method)
      AGBIO %in% c(2,4) ~ ((x_i/3.82) * (TFI_SAU_c/sum_TFI_SAU_c)) / SAU_c,
      #AGBIO %in% c(2,4) ~ ((x_i/1) * (TFI_SAU_c/sum_TFI_SAU_c)) / SAU_c,
      # for conventional farms
      .default = (x_i * (TFI_SAU_c/sum_TFI_SAU_c)) / SAU_c 
    )
  ) %>% ungroup()

# 6869 farms

# quality check : OK
## je dois retrouver mes totaux de charche en pesticide
#tmp = left_join(tmp_pesticides %>% group_by(IDENT) %>% summarise(xi = sum(A.5.1*SAU_c)),RICA_2020 %>% select(IDENT,AGBIO,CHRPH)) %>% mutate(comp = case_when(xi == CHRPH ~T,.default = F))

##### Output ----

BV_A.5.1 = tmp_pesticides %>%
  select(IDENT,CODE3,A.5.1)

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])

