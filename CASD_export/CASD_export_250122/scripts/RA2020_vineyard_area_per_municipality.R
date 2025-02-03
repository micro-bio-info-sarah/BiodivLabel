# Demande Jean-Sau
# 2024-12-03

# vineyard area by municipality

library(tidyr)

# RA data

RA2020_EXPLOITATIONS_240112 <- readRDS("~/BiodivLabel/data_CASD/RA2020_EXPLOITATIONS_240112.rds")
RA2020_PRODVEG_240112 <- readRDS("~/BiodivLabel/data_CASD/RA2020_PRODVEG_240112.rds")

# select, match and summary data
## SIEGE_CODE_COM	- Commune du siège (lieu principal de production)
## VIGNE_SUR_TOT - Surface totale en vigne - hectares

tmp_data <- RA2020_PRODVEG_240112 %>%
  select(NOM_DOSSIER,VIGNE_SUR_TOT) %>%
  left_join(.,
  RA2020_EXPLOITATIONS_240112 %>%
    select(NOM_DOSSIER,SIEGE_CODE_COM),
  by = join_by(NOM_DOSSIER)
            ) %>%
  group_by(SIEGE_CODE_COM) %>%
  summarise(
    sum_area_ha = sum(VIGNE_SUR_TOT,na.rm = T),
    mean_area_ha = mean(VIGNE_SUR_TOT,na.rm = T),
    nb_farm = length(unique(NOM_DOSSIER)),
    .groups = "keep"
  ) %>% ungroup()

# remove value for n<3

tmp_output <- tmp_data %>%
  mutate(
    sum_area_ha = case_when(
      nb_farm <3 ~ NA,
      .default = sum_area_ha
    ),
    mean_area_ha = case_when(
      nb_farm <3 ~ NA,
      .default = mean_area_ha
    ),
    nb_farm = case_when(
      nb_farm <3 ~ NA,
      .default = nb_farm
    )
  )

# proportion of total area of kept farms

sum(tmp_data$sum_area_ha)
#[1] 779903.4
sum(tmp_output$sum_area_ha,na.rm = T)
#[1] 777487.9
sum(tmp_output$sum_area_ha,na.rm = T) / sum(tmp_data$sum_area_ha) * 100
#[1] 99.69028

# extract data

write.csv(tmp_output,"RA2020_vineyard_area_per_municipality.csv",quote = F,row.names = F)