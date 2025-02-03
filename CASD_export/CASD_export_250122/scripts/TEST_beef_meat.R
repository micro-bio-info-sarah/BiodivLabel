# Beef meat

library(dplyr)
library(tidyr)

library(PerformanceAnalytics)
library(modelsummary)

if (my_DB == "RICA") {
  
  # transfert table
  tmp_TT_livestock <- readxl::read_xlsx("data_in/supp_data.xlsx", sheet ="TT_livestock") %>%
    rename(code_livestock = RICA_code_number) %>%
    mutate_at(.vars = vars(code_livestock),.funs = as.character)
  tmp_TT_livestock_product <- readxl::read_xlsx("data_in/supp_data.xlsx",sheet = "TT_livestock_products") %>%
    rename(code_anim_prod = RICA_code_number) %>%
    select(code_anim_prod,LIBELLE,RICA_QPROD7_unit,species, production_type) %>%
    distinct() %>%
    mutate_at(.vars = vars(code_anim_prod),.funs = as.character)
  
  # data
  tmp_anim <- RICA_2020_ani %>%
    filter(CODE6 %in% tmp_TT_livestock$code_livestock[tmp_TT_livestock$species == "cattle"]
           & EFFEC6 >0) %>%
    mutate_at(.vars = vars(IDENT,CODE6,MODE6),.funs = as.character)
  
  tmp_prod_anim <- RICA_2020_pan %>%
    filter(CODE7 %in% tmp_TT_livestock_product$code_anim_prod[tmp_TT_livestock_product$species == "cattle"]) %>%
    mutate_at(.vars = vars(IDENT,CODE7,MODE7),.funs = as.character)
  
  tmp_spe <- RICA_2020_spe %>%
    select(IDENT,
           EFBOV,EFBVI,EFTVO,EFVBA,EFVBU,EFVLA,EFVNO,
           PBBOV,PBBVI,PBTVO) %>%
    mutate_at(.vars = vars(IDENT),.funs = as.character)
  
  tmp_farms_meat <- RICA_2020$IDENT[RICA_2020$OTEFDA == "4600"]
  
  }

# Stat desc ----

# dataset overview

chart.Correlation(tmp_anim[,5:17],histogram = T)
chart.Correlation(tmp_prod_anim[,c(5,10:14,18:22)],histogram = T)


# number of head & production amount
tmp_anim %>% datasummary((EFFEC6 + QVENT6 + QPBRT6 + VVENT6 + VPBRT6) *(N + mean + sd + min + max + median) ~ CODE6, data = .)
tmp_prod_anim %>% datasummary((QPROD7 + QVENT7 + QPBRT7 + VVENT7 + VPBRT7) * (N + mean + sd + min + max + median) ~CODE7, data = .)

# correlation between number of head and product quantity
tmp <- tmp_anim %>%
  group_by(IDENT,CODE6) %>%
  summarise(
    nb_tete = sum(EFFEC6*0.1),
    .groups = "keep"
  ) %>%
  pivot_wider(names_from = CODE6,values_from = c("nb_tete"),
              values_fill = 0) %>%
  # add products
  left_join(.,
            tmp_prod_anim %>%
              filter(CODE7 %in% c(21,22,28) & QPROD7 >0) %>%
              group_by(IDENT,CODE7) %>%
              summarise(QPROD7 = sum(QPROD7,na.rm = T),
                        VVENT7 = sum(VVENT7, na.rm = T),
                        .groups = "keep") %>%
              pivot_wider(names_from = CODE7,values_from = c(QPROD7,VVENT7),values_fill = 0),
            by = join_by(IDENT)
  )

chart.Correlation(tmp[,-1], histogram = T, pch = 19)

# correlation between headcount and specific meat catlle data
tmp <- tmp_anim %>%
  group_by(IDENT,CODE6) %>%
  summarise(
    nb_tete = sum(EFFEC6*0.1),
    .groups = "keep"
  ) %>%
  pivot_wider(names_from = CODE6,values_from = c("nb_tete"),
              values_fill = 0) %>%
  # add meat cattle number and gross production value
  left_join(.,tmp_spe,
            by = join_by(IDENT)
  )

chart.Correlation(tmp[,-1], histogram = T, pch = 19)

# OTEX 4600 ----

tmp_farms_meat_test <- tmp_spe_anim_meat$IDENT[tmp_spe_anim_meat$EFBVI >149 & tmp_spe_anim_meat$EFBVI <151]

# répartition du cheptel et des quantité et valeurs pour l'OTEX 4600 Bovins allaitants
tmp_anim_meat <- tmp_anim %>%
  filter(IDENT %in% tmp_farms_meat_test) %>%
  mutate(
    dur_growing = (EFFEC6*10^-1) / (QVENT6+QSTOF6+ QAUTO6 - QSTOD6- QACHA6),
    cull_rate = case_when(
      CODE6 %in% c(929,930,936) ~ QVENT6 / EFFEC6),
  )

tmp_prod_anim_meat <- tmp_prod_anim %>%
  filter(IDENT %in% tmp_farms_meat_test)

tmp_spe_anim_meat <- tmp_spe %>%
  filter(IDENT %in% tmp_farms_meat_test)

tmp_anim_meat %>% datasummary(CODE6*(EFFEC6 + QACHA6 + QVENT6 + QPBRT6) ~ (N + mean + sd + min + max + median), data = .)
tmp_prod_anim_meat %>% datasummary((QPROD7 + QVENT7 + QPBRT7 + VVENT7 + VPBRT7) * (N + mean + sd + min + max + median) ~CODE7, data = .)
tmp_spe_anim_meat %>% datasummary((EFBOV + EFBVI + EFTVO + EFVBA + EFVBU + EFVLA + EFVNO + PBBOV + PBBVI + PBTVO) ~ (N + mean + sd + min + max + median),data = .)


##### Output ----

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])
