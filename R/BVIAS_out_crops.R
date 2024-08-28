# BVIAS for crops

# Set up ----

library(dplyr)
library(tidyr)
library(readxl)

# Input data ----

if (my_DB == "RICA") {

  # transfert table
  tmp_TT_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops") %>%
    rename(crop = RICA_code_number)
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

# WIP est-ce la bonne manière de joindre les df pour ne pas garder les fermes qu'on a retiré à chaque paramètre ???
tmp_farms <- Reduce(intersect,list(
  BV_A.2.1$farm_id,
  BV_A.2.2$farm_id,
  BV_A.3.1$farm_id,
  BV_A.3.2$farm_id,
  BV_A.3.3$farm_id,
  BV_A.4.3$farm_id,
  BV_A.4.5$farm_id,
  BV_A.5.1$farm_id))
# RICA2020: 6091 farms
# FADN2018: 60221 farms / 81288

tmp_input <- Reduce(full_join,list(
  BV_A.4.5 %>% filter(farm_id %in% tmp_farms),
  BV_A.2.1 %>% filter(farm_id %in% tmp_farms),
  BV_A.2.2 %>% filter(farm_id %in% tmp_farms),
  BV_A.3.1 %>% filter(farm_id %in% tmp_farms),
  BV_A.3.2 %>% filter(farm_id %in% tmp_farms),
  BV_A.3.3 %>% filter(farm_id %in% tmp_farms),
  BV_A.4.3 %>% filter(farm_id %in% tmp_farms),
  BV_A.5.1 %>% filter(farm_id %in% tmp_farms)
)) %>% ungroup() %>%
  filter(complete.cases(.)) %>%
  # add land use type
  left_join(.,tmp_TT_crops %>% select(crop,land_use_type,species))

# Model Parameters ----

## Constants of the biodiversity value contribution functions ----

# import constants of the biodiversity value contribution functions from Lindner 2019 SM
tmp_param_BV_constant <-  read_excel("data_in/supp_data.xlsx",
                               sheet = "Lindner_2019_BV_LU_function_con",
                               col_types = c("text", "text", "numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric", "numeric"))
tmp_param_BV_constant <- tmp_param_BV_constant %>%
  filter(metric_number %in% colnames(tmp_input))



## Variable weighting coefficients ----

tmp_param_var_weight <- tibble(
  metric_number = c(
    "A.2.1","A.2.2","A.3.1","A.3.2","A.3.3","A.4.3","A.4.5","A.5.1")
  ) %>%
  mutate(
    weight = case_when(
      metric_number %in% c("A.4.5","A.5.1") ~ 1/3,
      .default = (1/3)/(8-2)

    )
  )
sum(tmp_param_var_weight$weight) == 1

## Land use range ----

tmp_param_LU_range <- tibble(
  # set BV_norm min and max according to Lindner et al. (2019)
  land_use_type = c("grassland","arable"),
  LU_min = c(0.44,0.23),
  LU_max = c(0.92,0.52)
)


# BVIAS ----

source("d:/users/srhuet/documents/BiodivLabel/R/BVIAS_functions.R")

tmp <- head(tmp_input[tmp_input$land_use_type == "arable" & tmp_input$org_farming == F,],n = 70) %>%
  rbind(.,head(tmp_input[tmp_input$land_use_type == "grassland" & tmp_input$org_farming == F,],n = 30)) %>%
  rbind(.,head(tmp_input[tmp_input$land_use_type == "arable" & tmp_input$org_farming == T,],n = 70)) %>%
  rbind(.,head(tmp_input[tmp_input$land_use_type == "grassland" & tmp_input$org_farming == T,],n = 30))

tmp_x_norm <- data_for_BVIAS(tmp_input,c("farm_id","crop","land_use_type"),tmp_param_BV_constant)
tmp_BVIAS <- BVIAS(tmp_x_norm,c("farm_id","crop","land_use_type"),tmp_param_BV_constant,tmp_param_var_weight,T)

# Model optimization ----

## Parameter optimization with optim ----

var_param_v = unlist(tmp_param_BV_constant[,3:8])
tmp_optim = optim(var_param_v,BVIAS_optim1)

## Parameter optimization with sgd ----
library('groundhog')
library('foreach')
library('doParallel')
library('doRNG')
# To optimize the BVIAS model, the convergence to the global minimum of the loss function based on the mean square error is obtained through stochastic gradient descent.

tmp_param_BV_constant_optim <- sgd_param(tmp,c("farm_id","crop","land_use_type"),
                                   tmp_param_BV_constant,tmp_param_var_weight,
                                   learning_rate = 0.1, epochs = 500, test_sample_size = 50,tolerance = 1e-5)

tmp_BVIAS_optim1 <- BVIAS(tmp_x_norm,c("farm_id","crop","land_use_type"),tmp_param_BV_constant_optim,tmp_param_var_weight,T)






# compare MSE
tmp_MSE <- BVIAS_loss_function_LU(tmp,c("farm_id","crop","land_use_type"),tmp_param_BV_constant,tmp_param_var_weight)
tmp_MSE_optim <- BVIAS_loss_function_LU(tmp,c("farm_id","crop","land_use_type"),
                                        tmp_param_BV_constant_optim,
                                        tmp_param_var_weight)
# rerun model with optimized parameters
tmp_BVIAS <- BVIAS(tmp_x_norm,c("farm_id","crop","land_use_type"),
                   tmp_param_BV_constant_optim,
                   tmp_param_var_weight,T)





## Calibration of metric weight ----



















# Test ----
# Calcul des gradients (exemple simplifié)
compute_gradients <- function(input, id_cols, var_param, epsilon = 1e-5) {

  tmp_gradients <- var_param
  tmp_base_cost <- BVIAS_cost_function_LU(input, id_cols, var_param)

  for (i in seq_len(nrow(var_param))) {
    for (j in names(var_param)[-(1)]) { # Ignorer les colonnes non-paramètres
      var_param[i, j] <- var_param[i, j] + epsilon
      tmp_new_cost <-  BVIAS_cost_function_LU(input, id_cols, var_param)
      tmp_gradients[i, j] <- (tmp_new_cost - tmp_base_cost) / epsilon
      var_param[i, j] <- var_param[i, j] - epsilon
    }
  }

  return(tmp_gradients)

}

# Optimisation avec SGD
optimize_BVIAS <- function(input, id_cols, var_param, num_iterations = 1000, learning_rate = 0.01) {
  for (iteration in seq_len(num_iterations)) {
    gradients <- compute_gradients(input, id_cols, var_param, LU_range, y_true)

    for (i in seq_len(nrow(var_param))) {
      for (j in names(var_param)[-(1:3)]) { # Ignorer les colonnes non-paramètres
        var_param[i, j] <- var_param[i, j] - learning_rate * gradients[i, j]
      }
    }

    if (iteration %% 100 == 0) {
      current_cost <- cost_function(input, id_cols, var_param, LU_range, y_true)
      cat("Iteration:", iteration, "Cost:", current_cost, "\n")
    }
  }

  return(var_param)
}














# Calculer la sortie actuelle du modèle
tmp_x_norm <- practice_norm(tmp_input[1:100,],tmp_param_var,c("farm_id","crop","land_use_type"))
tmp_BV_LU <- BVLU_from_practices(tmp_x_norm,"x_norm",tmp_param_var,c("farm_id","crop","land_use_type"),T)

# Calculer l'erreur actuelle
tmp_current_cost <- effect_size(tmp_input[1:100,],c("farm_id","crop","land_use_type"),tmp_param_var)


# Gradient Descent Parameters
tmp_learning_rate <- 0.01
tmp_num_iterations <- 1000

# Gradient Descent Loop
for (tmpl_iter in 1:tmp_num_iterations) {


  # Afficher l'erreur tous les 100 itérations
  if (tmpl_iter %% 100 == 0) {
    cat("Iteration:", tmpl_iter, "Cost:", tmpl_current_cost, "\n")
  }

  # Mettre à jour les paramètres en utilisant le gradient des paramètres
  for (tmpl_nvar in 1:length(tmp_param_var$metric_number)) {
    for (tmpl_nparam in 1:ncol(tmp_param_var)-1) {

      tmpl_params <- tmp_param_var

      # Calculer le gradient (dérivée partielle du coût par rapport au paramètre)
      tmpl_original_value <- tmpl_params[tmpl_nvar, tmpl_nparam+1]
      tmpl_params[tmpl_nvar, tmpl_nparam+1] <- tmpl_original_value + 1e-5
      tmpl_cost_plus <- effect_size(tmp_input[1:100,],c("farm_id","crop","land_use_type"),tmpl_params)

      tmpl_params[tmpl_nvar, tmpl_nparam+1] <- tmpl_original_value - 1e-5
      cost_minus <- cost_function(params, theoretical_output)

      gradient <- (cost_plus - cost_minus) / (2 * 1e-5)

      # Mettre à jour le paramètre
      params[i, j] <- original_value - learning_rate * gradient
    }
  }
}




## LU range ----

tmp <- head(tmp_input[tmp_input$land_use_type == "arable",],n = 100) %>%
  rbind(.,head(tmp_input[tmp_input$land_use_type == "grassland",],n = 100))

tmp_y <- BV_from_practices(tmp,tmp_param_var)

optim_LU_range <- function(parameters,model_function,input) {


}

  tmp_stat_desc <- tmp_input %>%
    select(farm_id,crop,land_use_type,
           all_of(intersect(tmp_param_var$metric_number,colnames(tmp_input)))) %>%
    pivot_longer(cols = all_of(intersect(tmp_param_var$metric_number,colnames(tmp_input))),
                 names_to = "metric_number",values_to = "value") %>%
    group_by(land_use_type,metric_number) %>%
    summarise(
      mean = mean(value,na.rm = T),
      sd = sd(value,na.rm = T),
      median = median(value,na.rm = T),
      q25 = quantile(value,0.25,na.rm = T),
      q75 = quantile(value,0.75,na.rm = T),
      q5 = quantile(value,0.05,na.rm = T),
      q95 = quantile(value,0.95,na.rm = T)
    ) %>% ungroup()




## Optimization ----















# Biodiversity Value Contribution ----


  tmp_x_norm <- practice_norm(tmp_input[1:100,],tmp_param_var,c("farm_id","crop","land_use_type"))
  tmp_BV_LU <- BVLU_from_practices(tmp_x_norm,"x_norm",tmp_param_var,c("farm_id","crop","land_use_type"),T)
  tmp_BV_loc <- BV_loc(tmp_BV_LU,tmp_param_LU_range,c("farm_id","crop","land_use_type"))











# select crops
tmp_BV_contrib <- tmp_input[1:100,]

# loop to normalize all input variables and apply BV functions
for (tmp_var in c(tmp_param_var_weight$input_var,"A.4.5_min","A.4.5_org")) { #tmp_i = c("A.3.1","A.4.5","A.5.1")[1]

  # define the maximum of each parameter to 95% of the positive values (i.e., cut off the 5% highest positive values)
  ## ??? pour labour, valeurs dupliquées => modifie quantiles => valeurs unique => change quantiles pour autre
  #tmp_max = pull(tmp_input,tmp_i)
  #tmp_max = quantile(tmp_max[which(tmp_max>0)],0.95)
  tmp_max = unique(pull(tmp_input,tmp_i))
  tmp_max = quantile(tmp_max,0.95,na.rm = T)
  print(paste0(tmp_i," 95th quantile: ",tmp_max))

  # retrieve constant from Lindner 2019
  tmp_constant <- tmp_BV_constant[tmp_BV_constant$metric_number == str_sub(tmp_i,start = 1,end = 5),]

  # function
  tmp_df <- tmp_input %>%
    ## select param
    select(farm_id,crop,all_of(tmp_i)) %>%
    setNames(c("farm_id","crop","x")) %>%
    mutate(
      ## set max
      x_max =
        case_when(
          x > tmp_max ~ tmp_max,
          .default =  x ),
      ## Normalize data
      x_norm =
        case_when(
          x > tmp_max ~ 1,
          .default =  x / tmp_max ),
      ## calculate BV
      y = case_when(
        x_norm == 0 ~ 1,
        x_norm == 1 ~ 0,
        .default = tmp_constant$gamma + tmp_constant$epsilon * exp(-(
          abs(((x_norm^tmp_constant$delta) - tmp_constant$beta) ^ tmp_constant$alpha) /
            (2*tmp_constant$sigma^tmp_constant$alpha))) ),
      ## constrain BV !!! lots of negative values obtained for A.4.5_y (when A.4.5_x_norm > 0.82)
      ## Lindner 2022: diff gamma => when x_norm = 1 => y = -2 => constrain if x_norm = 1 => y =0
      y = case_when(
        y <0 ~ 0,
        y >1 ~ 1,
        .default = y
      )
    )

  #plot
  hist(tmp_df$x_max,nclass = 1000)
  abline(v=tmp_max)
  plot(tmp_df$x_max,tmp_df$y,
       main=tmp_constant$Metric)


  tmp_df <- tmp_df %>%
    setNames(c("farm_id","crop",tmp_i,paste0(tmp_i,"_max"),paste0(tmp_i,"_norm"),paste0(tmp_i,"_y")))

  # retrieve loop results
  tmp_BV_contrib <- tmp_BV_contrib %>%
    left_join(.,tmp_df)

}


##### Land use specific biodiversity value ----

tmp_BV_LU <- tmp_BV_contrib %>%
  rowwise() %>%
  mutate(
    # average
    BV_LU = mean(c(A.3.1_y,A.4.5_y,A.5.1_y),na.rm=T)
      )

##### Biodiversity value normalization ----


# Normalize BV_LU
tmp_BV_norm <- tmp_BV_LU %>%
  # add land use type
  left_join(.,tmp_TT_crops %>% select(crop,land_use_type)) %>%
  mutate(
    # set BV_norm min and max according to Lindner et al. (2019)
    BV_norm_min = case_when(
      land_use_type == "forest" ~ 1/3,
      land_use_type == "grassland" ~ 1/3,
      land_use_type == "arable" ~ 1/6,
      land_use_type == "mining" ~ 0),
    BV_norm_max = case_when(
      land_use_type == "forest" ~ 1,
      land_use_type == "grassland" ~ 5/6,
      land_use_type == "arable" ~ 2/3,
      land_use_type == "mining" ~ 1/3),
    # Normalize BV_LU
    BV_norm = BV_norm_min+BV_LU*(BV_norm_max-BV_norm_min)
  )

# Local biodiversity value ----

tmp_BV_loc <- tmp_BV_norm %>%
  mutate(BV_loc = 1.017626088*(1-exp(-4.055847776*BV_norm)),
         BVI_ha = 1- BV_loc
         )

# quality check : see in BVI to AGB if similar values for similar crops

# BVI per kg ----

BVI_to_RICA_crops <- tmp_BV_loc %>%
  # add yields
  left_join(., crop_yield) %>% # 5952 farms
  mutate(
    BVI_kg = BVI_ha / yield
  ) %>% ungroup()

# calculate yield for hay using grassland areas => all productions = 0
# WIP calculate yield for olive oil using olive tree areas ???

##### Output ----


rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])











