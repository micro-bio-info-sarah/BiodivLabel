# BVIAS for crops

# Set up ----

library(dplyr)
library(tidyr)
library(readxl)
# BVIAS ----

# first I need to prepare the data
data_for_BVIAS <- function(input,id_cols,var_param) {

  # test
#input = tmp_input
#id_cols = c("farm_id","crop","land_use_type")
#var_param = tmp_param_BV_constant

  tmp_var <- intersect(var_param$metric_number,colnames(input))
  tmp_x <- input %>%
    dplyr::select(all_of(id_cols),all_of(tmp_var)) %>%
    tidyr::pivot_longer(cols = all_of(tmp_var),names_to = "metric_number",values_to = "value")

  # set 95th percentile as max
  tmp_max <- tmp_x %>%
    dplyr::group_by(metric_number) %>%
    dplyr::summarise(max = as.vector(quantile(unique(value),0.95,na.rm = T))) %>% ungroup()

  tmp_x_norm <- tmp_x %>%
    # add max
    dplyr::left_join(.,tmp_max,by = 'metric_number') %>%
    ## calculate BV
    #dplyr::rowwise() %>%
    dplyr::mutate(
      ## set max
      x_max =
        case_when(
          value > max ~ max,
          .default =  value ),
      ## Normalize data
      x_norm =
        case_when(
          value > max ~ 1,
          .default =  value / max )) %>%
    dplyr::ungroup()

  return(tmp_x_norm)

}

# then I can estimate the BVIAS
BVIAS <- function(input,id_cols,var_param,var_weight,print_plot) {

  # test
  #input = tmp_x_norm
  #id_cols = c("farm_id","crop","land_use_type")
  #var_param = tmp_param_BV_constant
  #var_weight = tmp_param_var_weight
  #LU_range = tmp_param_LU_range

  tmp_var <- intersect(var_param$metric_number,colnames(input))

  tmp_y <- input %>%
    # add variable parameters
    dplyr::left_join(.,var_param) %>%
    ## calculate BV
    #dplyr::rowwise() %>%
    dplyr::mutate(
      ## calculate BV
      y = gamma + epsilon * exp(-(
        abs((((x_norm)^delta) - beta) ^ alpha) /
          (2*sigma^alpha)))
    ) %>%
    dplyr::ungroup()

  library(ggplot2)
  tmp_plot <- ggplot(tmp_y) +
    aes(x = x_max, y = y) +
    geom_point(colour = "#112446") +
    geom_smooth(se = TRUE, colour = "#112446") +
    theme_minimal() +
    facet_wrap(vars(metric_number),scales = "free")

  tmp_BVIAS <- tmp_y %>%
    # add variable weights
    dplyr::left_join(.,var_weight) %>%
    # BV LU
    # aggregate variables
    dplyr::group_by_at(all_of(id_cols)) %>%
    dplyr::summarise(
      BV_LU = weighted.mean(y,weight)
    ) %>% ungroup() %>%
    # BV NORM
    # add land use type ranges
    dplyr::left_join(.,tibble(
      # set BV_norm min and max according to Lindner et al. (2019)
      land_use_type = c("grassland","arable"),
      LU_min = c(0.44,0.23),
      LU_max = c(0.92,0.52)
    )) %>%
    # normalize BV
    dplyr::mutate(
      BV_norm = LU_min + BV_LU * (LU_max - LU_min)) %>%
    mutate(
      # BV LOC
      BV_loc = 1.017626088*(1-exp(-4.055847776*BV_norm)),
      # BVAS
      BVAS = BV_norm,
      # BVIAS
      BVIAS_ha = 1- BVAS
    )


  #return(list(tmp_plot,tmp_BVIAS))

  if (print_plot == T) {
    print(tmp_plot) # WIP pour A.2.1, je la trouve décroissante quand x_norm >= 0.635
  }
  return(list(y = tmp_y, BVIAS = tmp_BVIAS))

}

# Model optimization ----

## Calibration of metric function forms ----

# To optimize the BVIAS model, the convergence to the global minimum of the loss function based on the mean square error is obtained through stochastic gradient descent.

### Loss function ----

# Mean Squared Error (MSE) as loss function
## WIP use maximum likelihood ratio
BVIAS_loss_function_LU <- function(var_param,input,id_cols,var_weight) {
  library(dplyr)

  #input = tmp
  #id_cols = c("farm_id","crop","land_use_type")
  var_param = tmp_param
  #var_weight = tmp_param_var_weight

  # model output
  tmp_x_norm <- data_for_BVIAS(input,id_cols,var_param)
  tmp_BVIAS <- BVIAS(tmp_x_norm,id_cols,var_param,var_weight,F)

  # least square difference between model output and average land use type values from Gallego et al., 2022

  ## estimate variable median for each land use type
  tmp_LU_median <- tmp_x_norm %>%
    dplyr::group_by(land_use_type,metric_number) %>%
    dplyr::summarise(
      x_norm = median(x_norm,na.rm = T)
    ) %>% ungroup()


  tmp_BVIAS_median <- BVIAS(tmp_LU_median,c("land_use_type"),var_param,var_weight,F)

  # calculate distance from expected values regarding Gallego et al,. 2022
  tmp_distance <- tmp_BVIAS_median$BVIAS %>%
    dplyr::mutate(
      litt_values = case_when(
        land_use_type == "arable" ~ 0.32,
        land_use_type == "grassland" ~ 0.57
      )) %>%
    mutate(
      distance = case_when(
        land_use_type == "arable" ~ (litt_values - BVAS)^2,
        land_use_type == "grassland" ~ (litt_values - BVAS)^2
      )
    ) %>% filter(land_use_type == "grassland")

  # Erreur quadratique moyenne
  tmp_MSE = sum(tmp_distance$distance)/length(tmp_distance$distance)

  return(list(MSE = tmp_MSE,input = tmp_BVIAS_median$y,distance_table = tmp_distance))

}

# loss function to use in optim function

BVIAS_optim1 <- function(var_param_v) {
  library(dplyr)

  input = tmp_input
  id_cols = c("farm_id","crop","land_use_type")
  #var_param = tmp_param_BV_constant
  #var_param_v = unlist(tmp_param_BV_constant[,3:8])
  var_weight = tmp_param_var_weight

  var_param <- tmp_param_BV_constant %>% select(metric_number) %>%
    cbind(.,matrix(var_param_v,nrow = 8,ncol = 6))
  colnames(var_param) <- colnames(tmp_param_BV_constant[c(1,3:8)])


  # model output
  tmp_x_norm <- data_for_BVIAS(input,id_cols,var_param)
  tmp_BVIAS <- BVIAS(tmp_x_norm,id_cols,var_param,var_weight,F)

  # least square difference between model output and average land use type values from Gallego et al., 2022

  ## estimate variable median for each land use type
  tmp_LU_median <- tmp_x_norm %>%
    dplyr::group_by(land_use_type,metric_number) %>%
    dplyr::summarise(
      x_norm = median(x_norm,na.rm = T)
    ) %>% ungroup()

  tmp_BVIAS_median <- BVIAS(tmp_LU_median,c("land_use_type"),var_param,var_weight,F)

  # calculate distance from expected values regarding Gallego et al,. 2022
  tmp_distance <- tmp_BVIAS_median$BVIAS %>%
    dplyr::mutate(
      litt_values = case_when(
        land_use_type == "arable" ~ 0.32,
        land_use_type == "grassland" ~ 0.57
      )) %>%
    mutate(
      distance = case_when(
        land_use_type == "arable" ~ (litt_values - BVAS)^2,
        land_use_type == "grassland" ~ (litt_values - BVAS)^2
      )
    ) %>% filter(land_use_type == "grassland")

  # Erreur quadratique moyenne
  tmp_MSE = sum(tmp_distance$distance)/length(tmp_distance$distance)

  return(tmp_MSE)

}

### Parameter optimization ----


# Stochastic Gradient Descent (SGD) algorithm
## iteratively update model parameters based on sampled data points
### SGD
## https://www.geeksforgeeks.org/stochastic-gradient-descent-in-r/
## https://towardsdatascience.com/gradient-descent-from-scratch-e8b75fa986cc
## https://cran.r-project.org/web/packages/sgd/sgd.pdf
### foreach and doParallel
## https://www.statology.org/r-foreach/
## https://berkeley-scf.github.io/tutorial-parallelization/parallel-R.html#3-parallel-loops-including-parallel-lapply-via-the-future-package
### Parallel loops
## https://datacolada.org/102

sgd_param <- function(input,id_cols,var_param,var_weight,learning_rate, epochs,test_sample_size,tolerance) {
library('groundhog')
library('foreach')
library('doParallel')
library('doRNG')
#get the core processors ready to go
registerDoParallel(cores = round(parallel::detectCores()*0.8)) #I leave 20% free to do things while R runs

  # test
  #input = tmp
  #id_cols = c("farm_id","crop","land_use_type")
  #var_param = tmp_param_BV_constant
  #var_weight = tmp_param_var_weight
  #learning_rate = 0.01
  #epochs = 10 # nb iterations
  #test_sample_size = 10 # nb of randomly selected obs per iteration
  #tolerance = 1e-5 # relative convergence tolerance


  tmp_base_cost <- BVIAS_loss_function_LU(input, id_cols, var_param,var_weight)
  tmp_var_param = var_param

  var_param_optim = foreach(epoch = 1:epochs,.combine = 'rbind',.options.RNG=1234,
                            .packages=c("dplyr","groundhog","foreach","doParallel")) %dorng% {
    #for (epoch in 1:epochs) {
    #library(dplyr)
    library('groundhog')
    #library('foreach')
    #library('doParallel')
    source("d:/users/srhuet/documents/BiodivLabel/R/BVIAS_functions.R")


    # Randomly sample some data point
    index <- sample(1:nrow(input), test_sample_size)
    input_i <- input[index,]

    # Compute the gradient of the loss function
    ## the gradient is obtained by calculating the partial derivative of the loss function
    tmp_new_cost <-  BVIAS_loss_function_LU(input_i, id_cols, tmp_var_param,var_weight)
    ## WIP I think I should compute one gradient per parameter based on the derivative of the loss function according to this parameter
    tmp_gradient <- -2/test_sample_size * sum(tmp_new_cost$distance_table$litt_values - tmp_new_cost$distance_table$BVAS)

    # Update model parameters
    tmpl_var_param = foreach(r = seq_len(nrow(tmp_var_param)), .combine = 'rbind',.options.RNG=1234,
                             .packages=c("dplyr","groundhog","foreach","doParallel")) %dorng% {

      library('groundhog')
      source("d:/users/srhuet/documents/BiodivLabel/R/BVIAS_functions.R")

      tmp_c = foreach(c = colnames(dplyr::select_if(tmp_var_param,is.numeric)),  # Ignorer les colonnes non-paramètres
                      .combine='cbind',.options.RNG=1234,
                      .packages=c("dplyr","groundhog","foreach","doParallel")) %dorng% {

                        library('groundhog')
                        source("d:/users/srhuet/documents/BiodivLabel/R/BVIAS_functions.R")

                        tmp_c <- tmp_var_param[r,c] - learning_rate * tmp_gradient
                        tmp_c
                      }

      # test if new parameters give 0<y<1; else keep unchanged parameters
      tmp_BV = tmp_c %>% mutate(y = gamma + epsilon * exp(-(abs(((0.5^delta) - beta) ^ alpha) / (2*sigma^alpha))))
      if(!(is.na(tmp_BV$y)) & tmp_BV$y>=0 & tmp_BV$y<=1) {
        tmpl_var_param <- tmp_var_param[r,colnames(dplyr::select_if(tmp_var_param,is.character))] %>% cbind(.,tmp_c)
      } else {
        tmpl_var_param <- tmp_var_param[r,]
      }
      tmpl_var_param
    }

    # test if new parameters reduce MSE; else keep unchanged parameters
    MSE_old <- BVIAS_loss_function_LU(input, id_cols, tmp_var_param,var_weight)
    MSE_new <- BVIAS_loss_function_LU(input, id_cols, tmpl_var_param,var_weight)
    if (!is.na(MSE_new$MSE) & MSE_new$MSE <= MSE_old$MSE - tolerance) {
      tmp_var_param <- tmpl_var_param
      var_param_optim <- tmp_var_param %>% mutate(MSE = MSE_new$MSE)
    } else {
      #var_param_optim <- tmp_var_param %>% mutate(MSE = MSE_old$MSE)
      print(paste0("Model converged at epoch ",epoch))
      #break
      }
    var_param_optim
  }
  var_param_optim <- var_param_optim %>% filter(MSE == min(MSE))

  return(var_param_optim)
}

## Calibration of metric weight ----

### Loss function ----

# Mean Squared Error (MSE) as loss function
## WIP use maximum likelihood ratio
BVIAS_loss_function_weight <- function(input,id_cols,var_param,var_weight,crop_name_cereals) {
  library(dplyr)

  #input = tmp
  #id_cols = c("farm_id","crop","land_use_type","org_farming")
  #var_param = tmp_param_BV_constant_optim
  #var_weight = tmp_param_var_weight
  #crop_name_cereals = unique(na.omit(tmp_TT_crops$crop[tmp_TT_crops$species == "cereal"]))

  # model output
  tmp_x_norm <- data_for_BVIAS(input,id_cols,var_param)
  tmp_BVIAS <- BVIAS(tmp_x_norm,id_cols,var_param,var_weight,F)

  ## effect size (ES) ----

  # least square difference between estimated effect size and values from literature

  ## estimate variable median and quantiles for each land use type
  tmp_LU_median <- tmp_x_norm %>%
    dplyr::group_by(land_use_type,metric_number) %>%
    dplyr::summarise(
      median = median(x_norm,na.rm = T),
      q25 = as.vector(quantile(x_norm,0.25,na.rm = T)),
      q75 = as.vector(quantile(x_norm,0.75,na.rm = T)),
      q95 = as.vector(quantile(x_norm,0.95,na.rm = T))
    ) %>%
    ungroup() %>%
    # keep only crop land use type
    filter(land_use_type == "arable")


  ### pesticides -> 0.168 (Beketov et al., 2013)
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.5.1_q0",
      x_norm = case_when(
        metric_number == "A.5.1" ~ 0,
        .default = median
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.5.1_q95",
          x_norm = case_when(
            metric_number == "A.5.1" ~ q95,
            .default = median
          )
        )
    )
  tmp_BVIAS_A.5.1 <- BVIAS(tmp_ES_input,c("id_col","land_use_type"),var_param,var_weight,F)
  tmp_ES_A.5.1 <- tmp_BVIAS_A.5.1[["BVIAS"]]$BVAS[tmp_BVIAS_A.5.1[["BVIAS"]]$id_col == "A.5.1_q0"] - tmp_BVIAS_A.5.1[["BVIAS"]]$BVAS[tmp_BVIAS_A.5.1[["BVIAS"]]$id_col == "A.5.1_q95"]

  ### fertilizers -> 0.168 (Sánchez-Bayo and Wyckhuys, 2019)
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.4.5_q0",
      x_norm = case_when(
        metric_number == "A.4.5" ~ 0,
        .default = median
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.4.5_q95",
          x_norm = case_when(
            metric_number == "A.4.5" ~ q95,
            .default = median
          )
        )
    )
  tmp_BVIAS_A.4.5 <- BVIAS(tmp_ES_input,c("id_col","land_use_type"),var_param,var_weight,F)
  tmp_ES_A.4.5 <- tmp_BVIAS_A.4.5[["BVIAS"]]$BVAS[tmp_BVIAS_A.4.5[["BVIAS"]]$id_col == "A.4.5_q0"] - tmp_BVIAS_A.4.5[["BVIAS"]]$BVAS[tmp_BVIAS_A.4.5[["BVIAS"]]$id_col == "A.4.5_q95"]

  ### crop diversity -> -0.005 (Sirami et al., 2019)
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.3.3_q25",
      x_norm = case_when(
        metric_number == "A.3.3" ~ q25,
        .default = median
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.3.3_q75",
          x_norm = case_when(
            metric_number == "A.3.3" ~ q75,
            .default = median
          )
        )
    )
  tmp_BVIAS_A.3.3 <- BVIAS(tmp_ES_input,c("id_col","land_use_type"),var_param,var_weight,F)
  tmp_ES_A.3.3 <- (tmp_BVIAS_A.3.3[["BVIAS"]]$BVAS[tmp_BVIAS_A.3.3[["BVIAS"]]$id_col == "A.3.3_q75"]
                   - tmp_BVIAS_A.3.3[["BVIAS"]]$BVAS[tmp_BVIAS_A.3.3[["BVIAS"]]$id_col == "A.3.3_q25"]) /
    tmp_BVIAS_A.3.3[["BVIAS"]]$BVAS[tmp_BVIAS_A.3.3[["BVIAS"]]$id_col == "A.3.3_q75"]

  ### semi natural cover -> 0.11 (Sirami et al., 2019)
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.2.1_q25",
      x_norm = case_when(
        metric_number == "A.2.1" ~ q25,
        .default = median
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.2.1_q75",
          x_norm = case_when(
            metric_number == "A.2.1" ~ q75,
            .default = median
          )
        )
    )
  tmp_BVIAS_A.2.1 <- BVIAS(tmp_ES_input,c("id_col","land_use_type"),var_param,var_weight,F)
  tmp_ES_A.2.1 <- (tmp_BVIAS_A.2.1[["BVIAS"]]$BVAS[tmp_BVIAS_A.2.1[["BVIAS"]]$id_col == "A.2.1_q75"]
                   - tmp_BVIAS_A.2.1[["BVIAS"]]$BVAS[tmp_BVIAS_A.2.1[["BVIAS"]]$id_col == "A.2.1_q25"]) /
    tmp_BVIAS_A.2.1[["BVIAS"]]$BVAS[tmp_BVIAS_A.2.1[["BVIAS"]]$id_col == "A.2.1_q75"]

  ### plot size -> -0.05 (Sirami et al., 2019)
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.2.2_q25",
      x_norm = case_when(
        metric_number == "A.2.2" ~ q25,
        .default = median
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.2.2_q75",
          x_norm = case_when(
            metric_number == "A.2.2" ~ q75,
            .default = median
          )
        )
    )
  tmp_BVIAS_A.2.2 <- BVIAS(tmp_ES_input,c("id_col","land_use_type"),var_param,var_weight,F)
  tmp_ES_A.2.2 <- (tmp_BVIAS_A.2.2[["BVIAS"]]$BVAS[tmp_BVIAS_A.2.2[["BVIAS"]]$id_col == "A.2.2_q75"]
                   - tmp_BVIAS_A.2.2[["BVIAS"]]$BVAS[tmp_BVIAS_A.2.2[["BVIAS"]]$id_col == "A.2.2_q25"]) /
    tmp_BVIAS_A.2.2[["BVIAS"]]$BVAS[tmp_BVIAS_A.2.2[["BVIAS"]]$id_col == "A.2.2_q75"]

  ## organic / conventional farming -> 0.34 (Tuck et al., 2014) ----

  # least square difference between estimated effect size and values from litterature

  ## estimate variable median and quantiles for each land use type

  tmp_org_mean <- tmp_BVIAS$BVIAS %>%
    filter(crop %in% c(crop_name_cereals)) %>% # keep only cereals
    dplyr::group_by(org_farming) %>%
    dplyr::summarise(
      BVAS_mean = mean(BVAS,na.rm =T)) %>%
    ungroup()

  tmp_comp_org <- tmp_org_mean$BVAS_mean[tmp_org_mean$org_farming == T] / tmp_org_mean$BVAS_mean[tmp_org_mean$org_farming == F]

  # Erreur quadratique moyenne ----
  distance_table <- tibble(
    archetype = c("ES_A.2.1","ES_A.2.2","ES_A.3.3","ES_A.4.5","ES_A.5.1","comp_org"),
    litt_values = c(0.11,-0.05,-0.005,0.168,0.168,1.34),
    BVAS = c(tmp_ES_A.2.1,tmp_ES_A.2.2,tmp_ES_A.3.3,tmp_ES_A.4.5,tmp_ES_A.5.1,tmp_comp_org)
  ) %>%
    mutate(
      distance = (litt_values - BVAS)^2
    )
  tmp_MSE = mean(distance_table$distance)

  return(list(MSE = tmp_MSE, distance_table = distance_table))

}

### Weight calibration ----

# WIP sum of weight should ==1 !!!




sgd_weight <- function(input,id_cols,var_param,var_weight,learning_rate, epochs, test_sample_size,tolerance) {
  library('groundhog')
  library('foreach')
  library('doParallel')
  library('doRNG')
  #get the core processors ready to go
  registerDoParallel(cores=round(parallel::detectCores()*0.8)) #I leave 20% free to do things while R runs

  # test
  #input = tmp
  #id_cols = c("farm_id","crop","land_use_type","org_farming")
  #var_param = tmp_param_BV_constant_optim
  #var_weight = tmp_param_var_weight %>% mutate(weight = 1/length(metric_number))
  #crop_name_cereals = unique(na.omit(tmp_TT_crops$crop[tmp_TT_crops$species == "cereal"]))
  #learning_rate = 0.01
  #epochs = 10
  #test_sample_size = 100 # nb of randomly selected obs per iteration
  #tolerance = 1e-5 # relative convergence tolerance


  tmp_base_cost <- BVIAS_loss_function_weight(input,id_cols,var_param,var_weight,crop_name_cereals)
  tmp_var_weight = var_weight

  var_weight_optim = foreach(epoch = 1:epochs,.combine = 'rbind',.options.RNG=1234,
                             .packages=c("dplyr","groundhog","foreach","doParallel")) %dorng% {
    #for (epoch in 1:epochs) {
    source("d:/users/srhuet/documents/BiodivLabel/R/BVIAS_functions.R")


    # Randomly sample some data point
    index <- sample(1:nrow(input), test_sample_size)
    input_i <- input[index,]

    # Compute the gradient of the loss function
    ## the gradient is obtained by calculating the partial derivative of the loss function
    tmp_new_cost <-  BVIAS_loss_function_weight(input_i, id_cols,var_param,var_weight,crop_name_cereals)

    ## WIP I think I should compute one gradient per parameter based on the partial derivative of the loss function according to this parameter
    tmp_gradient <- -2/test_sample_size * sum(tmp_new_cost$distance_table$litt_values - tmp_new_cost$distance_table$BVAS)

    # Update model parameters
      tmpl_var_weight <- tmp_var_weight %>%
        mutate(weight = weight - learning_rate * tmp_gradient)

   # test if new parameters reduce MSE; else keep unchanged parameters
    MSE_old <- BVIAS_loss_function_weight(input, id_cols,var_param,tmp_var_weight,crop_name_cereals)
    MSE_new <- BVIAS_loss_function_weight(input, id_cols,var_param,tmpl_var_weight,crop_name_cereals)
    if (!is.na(MSE_new$MSE) & MSE_new$MSE < MSE_old$MSE) {
      tmp_var_weight <- tmpl_var_weight
      var_weight_optim <- tmp_var_weight %>% mutate(MSE = MSE_new$MSE)
    } else {
      #var_param_optim <- tmp_var_param %>% mutate(MSE = MSE_old$MSE)
      print(paste0("Model converged at epoch ",epoch))
      #break
    }
    var_weight_optim
  }
  var_weight_optim <- var_weight_optim %>% filter(MSE == min(MSE))
  return(var_weight_optim)
}





