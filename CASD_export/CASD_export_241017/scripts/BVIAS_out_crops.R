# BVIAS for crops

# Set up ----

library(dplyr)
library(tidyr)
library(readxl)

# Input data ----

if (my_DB == "RICA") {
  
  # transfert table
  tmp_TT_crops <- readxl::read_xlsx("~/BiodivLabel/data_in/supp_data.xlsx",sheet = "TT_crops") %>%
    rename(crop = RICA_code_number)
  
  tmp_input_raw <- RICA_2020_veg %>%
    rename(farm_id = IDENT, crop = CODE3) %>%
    # add land use type
    left_join(.,tmp_TT_crops %>% select(crop,product_name,land_use_type), by = join_by(crop)) %>%
    # summaries areas by crops
    group_by(farm_id,crop,product_name,land_use_type)%>%
    summarise(
      area_ha = sum(SUPER3*10^-2,na.rm = T),
      .groups = "keep"
    ) %>% ungroup() %>%
    filter(
      # keep only crops & grasslands with areas
      area_ha > 0) %>%
    # select variables and obs
    select(farm_id,land_use_type,crop,product_name,area_ha)    
}

if (my_DB == "FADN") {
  
  # transfert table
  tmp_FADN_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "FADN_crop_code")
  tmp_TT_crops0 <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops")
  tmp_TT_crops <- left_join(
    tmp_FADN_crops %>% rename(crop = code_letter),
    tmp_TT_crops0 %>% rename(crop = FADN_code_letter) %>% select(crop,species)
  ) %>% distinct()
  
}


# Input variables ----

# keep only farms for which we successfully estimated all variables in arable land use type
tmp_input_arable <- Reduce(full_join,list(
  BV_A.2.1 %>% select(farm_id,land_use_type,crop,A.2.1) %>% filter(land_use_type == "arable"),
  BV_A.2.2 %>% select(farm_id,land_use_type,crop,A.2.2) %>% filter(land_use_type == "arable"),
  BV_A.3.1 %>% select(farm_id,land_use_type,A.3.1) %>% filter(land_use_type == "arable"),
  BV_A.3.2 %>% select(farm_id,land_use_type,crop,A.3.2) %>% filter(land_use_type == "arable"),
  BV_A.3.3 %>% select(farm_id,land_use_type,A.3.3,crop_nb_LU) %>% filter(land_use_type == "arable"),
  BV_A.4.3 %>% select(farm_id,land_use_type,crop,A.4.3) %>% filter(land_use_type == "arable"),
  BV_A.4.5 %>% select(farm_id,land_use_type,crop,A.4.5) %>% filter(land_use_type == "arable"),
  BV_A.5.1 %>% select(farm_id,land_use_type,crop,A.5.1) %>% filter(land_use_type == "arable")
)) %>% ungroup() %>%
  filter(complete.cases(.))

# keep only farms for which we successfully estimated all variables in grassland land use type
tmp_input_grassland <- Reduce(full_join,list(
  BV_A.2.1 %>% select(farm_id,land_use_type,crop,A.2.1) %>% filter(land_use_type == "grassland"),
  BV_A.4.3 %>% select(farm_id,land_use_type,crop,A.4.3) %>% filter(land_use_type == "grassland"),
  BV_A.4.5 %>% select(farm_id,land_use_type,crop,A.4.5) %>% filter(land_use_type == "grassland")
)) %>% ungroup() %>%
  filter(complete.cases(.))

# join data sets of arable and grasslands
tmp_input <- tmp_input_raw %>%
  left_join(.,bind_rows(tmp_input_arable,tmp_input_grassland),
            by = join_by(farm_id, land_use_type, crop)) %>%
  # crop as.character
  mutate(crop = as.character(crop)) %>%
  # for farms with both land use types, keep only those for which we successfully estimated all variables in both land use types
  left_join(.,tmp_input_raw %>%
              group_by(farm_id) %>%
              summarise(nb_LU = length(unique(land_use_type)),
                        .groups = "keep"),
            by = join_by(farm_id)) %>%
  mutate(
    all_var = case_when(
      nb_LU == 1 & land_use_type == "arable" & farm_id %in% tmp_input_arable$farm_id ~ T,
      nb_LU == 1 & land_use_type == "grassland" & farm_id %in% tmp_input_grassland$farm_id ~ T,
      nb_LU == 2 & farm_id %in% intersect(tmp_input_arable$farm_id,tmp_input_grassland$farm_id) ~ T,
      .default = F
    )
  ) %>%
  filter(all_var == TRUE) %>%
  # add yields
  left_join(.,crop_yield,by = join_by(farm_id, crop)) %>%
  # add organic farming
  left_join(.,RICA_2020 %>% select(IDENT,AGBIO,CDEPT) %>%
              mutate(
                org_farming = case_when(
                  AGBIO %in% c(2,4) ~ T,
                  .default = F
                )) %>%
              rename(farm_id = IDENT),
            by = join_by(farm_id)
  ) 

BVIAS_input <- tmp_input
#datasummary((A.2.1+A.2.2+A.3.1+A.3.2+A.3.3+A.4.3+A.4.5+A.5.1)*land_use_type~Mean+SD+P0+P25+P50+P75+P100,data = tmp_input)
#datasummary((A.2.1+A.2.2+A.3.3+A.4.3)*CDEPT~Mean+SD+P0+P25+P50+P75+P100,data = tmp_input)


# Model Parameters ----

## Constants of the biodiversity value contribution functions ----

# import constants of the biodiversity value contribution functions from Lindner 2019 SM
tmp_var_BVC_constant <-  read_excel("data_in/supp_data.xlsx",
                                    sheet = "Lindner_2019_BV_LU_param",
                                    col_types = c("text","text", "text", "numeric",
                                                  "numeric", "numeric", "numeric",
                                                  "numeric", "numeric"))
tmp_var_BVC_constant <- tmp_var_BVC_constant %>%
  filter(metric_number %in% colnames(tmp_input))



## Weights for variable agregation ----

tmp_var_weight <- tibble(
  land_use_type = "arable",
  metric_number = c(
    "A.2.1","A.2.2","A.3.1","A.3.2","A.3.3","A.4.3","A.4.5","A.5.1")
) %>%
  mutate(
    weight = case_when(
      metric_number %in% c("A.2.1") ~ 0.11,
      metric_number %in% c("A.3.3") ~ 0.18,
      metric_number %in% c("A.4.5","A.5.1") ~ 0.35,
      .default = 0.05
      
    )
  ) %>%
  rbind(.,tibble(
    land_use_type = "grassland",
    metric_number = c(
      "A.2.1","A.2.2","A.3.1","A.3.2","A.3.3","A.4.3","A.4.5","A.5.1")
  ) %>%
    mutate(
      weight = case_when(
        metric_number %in% c("A.2.1") ~ 0.11,
        metric_number %in% c("A.4.5") ~ 0.35,
        # variables with no effect on grassland biodiversity (cf. method)
        metric_number %in% c("A.2.2","A.3.1","A.3.2","A.3.3","A.5.1") ~ 0,
        .default = 0.05
        
      )
    ) 
  ) %>%
  filter(weight >0)

sum(tmp_var_weight$weight)/2

## Land use range ----

tmp_param_LU_range <- tibble(
  # set BV_norm min and max according to Lindner et al. (2019) and adapted from Gallego et al., 2022
  land_use_type = c("grassland","arable"),
  LU_min = c(0.44,0.23),
  LU_max = c(0.92,0.52)
)

# Optimization ----

source("R/BVIAS_functions.R")
tmp_id_cols = c("farm_id","crop","land_use_type")

tmp_x_norm <- data_for_BVIAS(input = tmp_input,id_cols = tmp_id_cols, var_constant = tmp_var_BVC_constant)

library(modelsummary)
datasummary_skim(tmp_x_norm %>%
                   filter(land_use_type == "arable") %>%
                   pivot_wider(id_cols = tmp_id_cols,
                               names_from = metric_number,
                               values_from = x_max))
datasummary_skim(tmp_x_norm %>%
                   filter(land_use_type == "grassland") %>%
                   pivot_wider(id_cols = tmp_id_cols,
                               names_from = metric_number,
                               values_from = x_max))
#datasummary((A.2.1+A.2.2+A.3.1+A.3.2+A.3.3+A.4.3+A.4.5+A.5.1)*land_use_type~Mean+SD+P0+P25+P50+P75+P100,data = tmp_input)
#datasummary((A.2.1+A.2.2+A.3.3+A.4.3)*CDEPT~Mean+SD+P0+P25+P50+P75+P100,data = tmp_input)


#datasummary(metric_number*land_use_type*(value+x_max+x_norm)~Mean+SD+P0+P25+P50+P75+P100,data = tmp_x_norm)

## initial MSE ----
tmp_MSE_init = BVIAS_MSE(input = tmp_x_norm,
                         id_cols = tmp_id_cols,
                         var_constant = tmp_var_BVC_constant,
                         var_weight = tmp_var_weight)
tmp_MSE_init$MSE
tmp_MSE_init$distance_table

## Automatized optimization----

#source("R/BVIAS_optim_auto.R")
load("~/BiodivLabel/data_out/optim_2024-10-14.RData")

## Retrieve optimized parameters ----

### Constants
tmp_optim_arable = tmp_optim_ls$arable
tmp_optim_grassland = tmp_optim_ls$grassland

tmp_optim_constants_arable <- tmp_optim_arable[[length(tmp_optim_arable)]]$par[1:(6*8)]
tmp_optim_constants_grassland <- tmp_optim_grassland[[length(tmp_optim_grassland)]]$par[1:(6*8)]

tmp_var_BVC_constant_optim = tmp_var_BVC_constant %>%
  filter(land_use_type == "arable" & metric_number %in% colnames(tmp_input)) %>% 
  select_if(.,is.character) %>%
  cbind(.,matrix(tmp_optim_constants_arable,
                 nrow = nrow(tmp_var_BVC_constant %>% filter(land_use_type == "arable" & metric_number %in% colnames(tmp_input))),
                 ncol = length(tmp_optim_constants_arable) / nrow(tmp_var_BVC_constant %>% filter(land_use_type == "arable" & metric_number %in% colnames(tmp_input))),
                 dimnames = list(1:nrow(tmp_var_BVC_constant %>% filter(land_use_type == "arable" & metric_number %in% colnames(tmp_input))),
                                 colnames(tmp_var_BVC_constant %>% select_if(.,is.numeric))))) %>%
  rbind(.,tmp_var_BVC_constant %>%
          filter(land_use_type == "grassland" & metric_number %in% colnames(tmp_input)) %>% 
          select_if(.,is.character) %>%
          cbind(.,matrix(tmp_optim_constants_grassland,
                         nrow = nrow(tmp_var_BVC_constant %>% filter(land_use_type == "grassland" & metric_number %in% colnames(tmp_input))),
                         ncol = length(tmp_optim_constants_grassland) / nrow(tmp_var_BVC_constant %>% filter(land_use_type == "grassland" & metric_number %in% colnames(tmp_input))),
                         dimnames = list(1:nrow(tmp_var_BVC_constant %>% filter(land_use_type == "grassland" & metric_number %in% colnames(tmp_input))),
                                         colnames(tmp_var_BVC_constant %>% select_if(.,is.numeric))))))

### Weights
tmp_optim_weights_arable <- tmp_optim_arable[[length(tmp_optim_arable)]]$par[49:(49+8-1)]
tmp_optim_weights_grassland <- tmp_optim_grassland[[length(tmp_optim_grassland)]]$par[49:(49+3-1)]

tmp_var_weight_optim <-  tmp_var_weight %>%
  filter(land_use_type == "arable") %>%
  mutate(weight = tmp_optim_weights_arable) %>%
  rbind(.,tmp_var_weight %>%
          filter(land_use_type == "grassland") %>%
          mutate(weight = tmp_optim_weights_grassland))

## compare parameters

(setdiff(tmp_var_BVC_constant,tmp_var_BVC_constant_optim))
(setdiff(tmp_var_BVC_constant_optim,tmp_var_BVC_constant))

tmp_var_weight
tmp_var_weight_optim

# compare initial and optimized BVIAS ----

## MSE ----

tmp_MSE_optim = BVIAS_MSE(input = tmp_x_norm,
                          id_cols = tmp_id_cols,
                          var_constant = tmp_var_BVC_constant_optim,
                          var_weight = tmp_var_weight_optim)
tmp_MSE_init$MSE
tmp_MSE_init$distance_table
tmp_MSE_optim$MSE
tmp_MSE_optim$distance_table


tmp_table <- left_join(
  tmp_MSE_init$distance_table %>%
    rename(BVAS_init = BVAS,
           distance_init = distance),
  tmp_MSE_optim$distance_table %>%
    rename(BVAS_optim = BVAS,
           distance_optim = distance),
  by = join_by(archetype, land_use_type, litt_values)
)
write.csv(tmp_table,paste0("~/BiodivLabel/data_out/table_MSE_distance_table",Sys.Date(),".csv"))


tmp_plot <- rbind(tmp_MSE_init$BVIAS$y %>% mutate(land_use_type = "init"),
                  tmp_MSE_optim$BVIAS$y)
library(ggplot2)
ggplot(tmp_plot) +
  geom_point(aes(x = x_norm, y = y, colour = land_use_type),alpha = 0.25) +
  geom_density(aes(x = x_norm, y = - after_stat(density), fill = land_use_type),alpha = 0.25) +
  #geom_smooth(se = TRUE) +
  theme_minimal() +
  facet_wrap(vars(metric_number),scales = "free",ncol = 4)

## Effect Size ----

tmp_ES_init <- BVIAS_effect_size(tmp_x_norm,tmp_id_cols,tmp_var_BVC_constant,tmp_var_weight)
tmp_ES_optim <- BVIAS_effect_size(tmp_x_norm,tmp_id_cols,tmp_var_BVC_constant_optim,tmp_var_weight_optim)

tmp_table <- bind_rows(
  tmp_ES_init %>%
    mutate(set = "init"),
  tmp_ES_optim %>%
    mutate(set = "optim")
)
write.csv(tmp_table,paste0("~/BiodivLabel/data_out/table_ES_table",Sys.Date(),".csv"))


# BVIAS per ha ----

tmp_BVIAS <- BVIAS(input = tmp_x_norm,id_cols = tmp_id_cols,
                   var_constant = tmp_var_BVC_constant,
                   var_weight = tmp_var_weight)


library(ggplot2)
ggplot(tmp_BVIAS$y) +
  geom_point(aes(x = x_norm, y = y, colour = land_use_type),alpha = 0.25) +
  geom_density(aes(x = x_norm, y = - after_stat(density), fill = land_use_type),alpha = 0.25) +
  #geom_smooth(se = TRUE) +
  theme_minimal() +
  ggtitle("Contribution functions with initial constants") +
  facet_wrap(vars(metric_number),scales = "free",ncol = 4)

tmp_BVIAS_optim <- BVIAS(input = tmp_x_norm,id_cols = tmp_id_cols,
                         var_constant = tmp_var_BVC_constant_optim,
                         var_weight = tmp_var_weight_optim)

ggplot(tmp_BVIAS_optim$y) +
  geom_point(aes(x = x_norm, y = y, colour = land_use_type),alpha = 0.25) +
  geom_density(aes(x = x_norm, y = - after_stat(density), fill = land_use_type),alpha = 0.25) +
  #geom_smooth(se = TRUE) +
  theme_minimal() +
  ggtitle("Contribution functions with optimized constants") +
  facet_wrap(vars(metric_number),scales = "free",ncol = 4)


# BVIAS per kg ----

BVIAS_to_RICA_crops_wo_optim <- tmp_BVIAS$BVIAS %>%
  mutate(crop = as.character(crop)) %>%
  # add yields
  left_join(., crop_yield, by = join_by(farm_id, crop)) %>% # 5952 farms
  mutate(
    BVIAS_kg = BVIAS_ha / yield
  ) %>% ungroup()


BVIAS_to_RICA_crops <- tmp_BVIAS_optim$BVIAS %>%
  mutate(crop = as.character(crop)) %>%
  # add yields
  left_join(., crop_yield, by = join_by(farm_id, crop)) %>% # 5952 farms
  mutate(
    BVIAS_kg = BVIAS_ha / yield
  ) %>% ungroup()

# calculate yield for hay using grassland areas => all productions = 0
# WIP calculate yield for olive oil using olive tree areas ???

# Overview ----


# Output ----


optim_final <- list(
  constants = list(BV_constant_init = tmp_var_BVC_constant,
                   BV_constant_optim = tmp_var_BVC_constant_optim),
  weights = list(weight_init = tmp_var_weight,
                 weight_optim = tmp_var_weight_optim),
  MSE = list(MSE_init = tmp_MSE_init,
             MSE_optim = tmp_MSE_optim)
)

#save(list = "optim_final",file = paste0("data_out/optim_final.RData"))

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])











