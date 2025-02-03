# BVIAS for crops

# Set up ----

library(dplyr)
library(tidyr)
library(readxl)

# Input data ----
tmp_input <- BVIAS_input

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
      metric_number %in% c("A.2.1") ~ 0.20,
      metric_number %in% c("A.3.3") ~ 0.18,
      metric_number %in% c("A.4.5") ~ 0.35/3,
      metric_number %in% c("A.5.1") ~ 0.35,
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
        metric_number %in% c("A.4.3") ~ 0.25,
        metric_number %in% c("A.4.5") ~ 0.75,
        # variables with no effect on grassland biodiversity (cf. method)
        metric_number %in% c("A.2.1","A.2.2","A.3.1","A.3.2","A.3.3","A.5.1") ~ 0,
        .default = 0
        
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

source("R/TEST_SENSI_calibration_low_ferti_BVIAS_functions.R")
tmp_id_cols = c("farm_id","crop","land_use_type")

tmp_x_norm <- data_for_BVIAS(input = tmp_input,id_cols = tmp_id_cols, var_constant = tmp_var_BVC_constant)

## initial MSE ----
tmp_MSE_init = BVIAS_MSE(input = tmp_x_norm,
                         id_cols = tmp_id_cols,
                         var_constant = tmp_var_BVC_constant,
                         var_weight = tmp_var_weight)
tmp_MSE_init$MSE
tmp_MSE_init$distance_table

## Automatized optimization----

#source("R/BVIAS_optim_auto.R")
#save(list = "tmp_optim_ls",file = paste0("data_out/TEST_SENSI_calibration_low_ferti_optim_",Sys.Date(),".RData"))
load("~/BiodivLabel/data_out/TEST_SENSI_calibration_low_ferti_optim_2024-12-18.RData")

## Retrieve optimized parameters ----

tmp_var_BVC_constant_optim <- tmp_optim_ls[["constants"]]
tmp_var_weight_optim <- tmp_optim_ls[["weights"]]

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
tmp_MSE_optim$MSE
tmp_MSE_init$distance_table
tmp_MSE_optim$distance_table

tmp_table <- left_join(
  tmp_MSE_init$distance_table %>%
    rename(BVAS_init = BV_loc,
           distance_init = distance),
  tmp_MSE_optim$distance_table %>%
    rename(BVAS_optim = BV_loc,
           distance_optim = distance),
  by = join_by(archetype, land_use_type, litt_values)
)
write.csv(tmp_table,paste0("~/BiodivLabel/data_out/TEST_SENSI_calibration_low_ferti_table_MSE_distance_table",Sys.Date(),".csv"))

### plot BVC functions ----
library(ggplot2)
tmp_plot <- ggplot(tmp_MSE_init$BVIAS$y) +
  geom_point(aes(x = x_norm, y = y, colour = land_use_type),shape = "circle", size = 1.5) +
  geom_density(aes(x = x_norm, y = -after_stat(density), fill = land_use_type), alpha = 0.5 ,adjust = 1L) + 
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(metric_number)) +
  labs(title = "Before calibration")

#tmp_plot
svg(paste0("~/BiodivLabel/data_out/TEST_SENSI_calibration_low_ferti_plot_BVC_functions_before_optimization_",Sys.Date(),".svg"))
ggsave(filename = paste0("~/BiodivLabel/data_out/TEST_SENSI_calibration_low_ferti_plot_BVC_functions_before_optimization_",Sys.Date(),".svg"), plot = tmp_plot,width = 30,height = 20,units = "cm")

tmp_plot <- ggplot(tmp_MSE_optim$BVIAS$y) +
  geom_point(aes(x = x_norm, y = y, colour = land_use_type),shape = "circle", size = 1.5) +
  geom_density(aes(x = x_norm, y = -after_stat(density), fill = land_use_type), alpha = 0.5 ,adjust = 1L) + 
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(metric_number)) +
  labs(title = "After calibration")

#tmp_plot
svg(paste0("~/BiodivLabel/data_out/TEST_SENSI_calibration_low_ferti_plot_BVC_functions_after_optimization_",Sys.Date(),".svg"))
ggsave(filename = paste0("~/BiodivLabel/data_out/TEST_SENSI_calibration_low_ferti_plot_BVC_functions_after_optimization_",Sys.Date(),".svg"), plot = tmp_plot,width = 30,height = 20,units = "cm")


## Effect Size ----

tmp_ES_init <- BVIAS_effect_size(tmp_x_norm,tmp_id_cols,tmp_var_BVC_constant,tmp_var_weight)
tmp_ES_optim <- BVIAS_effect_size(tmp_x_norm,tmp_id_cols,tmp_var_BVC_constant_optim,tmp_var_weight_optim)

tmp_table <- bind_rows(
  tmp_ES_init %>%
    mutate(set = "init"),
  tmp_ES_optim %>%
    mutate(set = "optim")
)

write.csv(tmp_table,paste0("~/BiodivLabel/data_out/TEST_SENSI_calibration_low_ferti_table_ES_table",Sys.Date(),".csv"))

### plot effect sizes ----

tmp_plot <- ggplot(tmp_table) +
  geom_col(  aes(x = id_col, y = BVIAS_ES_5_95, fill = land_use_type),position = position_dodge()) +
  scale_fill_hue(direction = 1) +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(set))
#tmp_plot
svg(paste0("~/BiodivLabel/data_out/TEST_SENSI_calibration_low_ferti_plot_effect_size",Sys.Date(),".svg"))
ggsave(filename = paste0("~/BiodivLabel/data_out/TEST_SENSI_calibration_low_ferti_plot_effect_size",Sys.Date(),".svg"), plot = tmp_plot,width = 30,height = 20,units = "cm")


# BVIAS per ha ----

tmp_BVIAS_optim <- BVIAS(input = tmp_x_norm,id_cols = tmp_id_cols,
                         var_constant = tmp_var_BVC_constant_optim,
                         var_weight = tmp_var_weight_optim)

# BVIAS per kg ----

TEST_SENSI_calibration_low_ferti_BVIAS_to_RICA_crops <- tmp_BVIAS_optim$BVIAS %>%
  mutate(crop = as.character(crop)) %>%
  # add yields
  left_join(., crop_yield %>% select(farm_id,crop,yield), by = join_by(farm_id, crop)) %>% # 5952 farms
  # remove rows without yields
  filter(!is.na(yield)) %>%
  # conrt impact to kg
  mutate(
    BVI_kg = BVI_ha / yield,
    BVI_t = BVI_ha / (yield*10^-3)
  ) %>% ungroup()

# calculate yield for hay using grassland areas => all productions = 0
# WIP calculate yield for olive oil using olive tree areas ???

# Output ----

TEST_SENSI_calibration_low_ferti_optim_final <- list(
  constants = list(BV_constant_init = tmp_var_BVC_constant,
                   BV_constant_optim = tmp_var_BVC_constant_optim),
  weights = list(weight_init = tmp_var_weight,
                 weight_optim = tmp_var_weight_optim),
  MSE = list(MSE_init = tmp_MSE_init,
             MSE_optim = tmp_MSE_optim)
)

#save(list = "optim_final",file = paste0("data_out/optim_final.RData"))

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])

