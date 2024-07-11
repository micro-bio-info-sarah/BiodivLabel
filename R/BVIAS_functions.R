# BVIAS for crops

# Set up ----

library(dplyr)
library(tidyr)
library(readxl)
# BVIAS ----

# first I need to prepare the data
data_for_BVIAS <- function(input,id_cols,var_param) {

  # test
  #input = tmp
  #id_cols = c("farm_id","crop","land_use_type")
  #var_param = tmp_param_var

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
    dplyr::left_join(.,tmp_max) %>%
    ## calculate BV
    dplyr::rowwise() %>%
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
    print(tmp_plot) # WIP pour A.2.1, je la trouve décroissante au lieu de croissante
  }
  return(list(y = tmp_y, BVIAS = tmp_BVIAS))

}

# Model optimization ----

## Calibration of metric function forms ----

# To optimize the BVIAS model, the convergence to the global minimum of the loss function based on the mean square error is obtained through stochastic gradient descent.

### Loss function ----

# Mean Squared Error (MSE) as loss function
BVIAS_cost_function_LU <- function(input,id_cols,var_param,var_weight) {
  library(dplyr)

  #input = tmp
  #id_cols = c("farm_id","crop","land_use_type")
  #var_param = tmp_param_var
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
      observed_values = case_when(
        land_use_type == "arable" ~ 0.32,
        land_use_type == "grassland" ~ 0.57
      )) %>%
    mutate(
      distance = case_when(
        land_use_type == "arable" ~ (observed_values - BVAS)^2,
        land_use_type == "grassland" ~ (observed_values - BVAS)^2
      )
    )

  # Erreur quadratique moyenne
  tmp_MSE = sum(tmp_distance$distance)/length(tmp_distance$distance)

  return(list(MSE = tmp_MSE,input = tmp_BVIAS_median$y,distance_table = tmp_distance))

}

### Parameter optimization ----


# Stochastic Gradient Descent (SGD) algorithm
## iteratively update model parameters based on sampled data points
### SGD
## https://www.geeksforgeeks.org/stochastic-gradient-descent-in-r/
## https://towardsdatascience.com/gradient-descent-from-scratch-e8b75fa986cc
### foreach and doParallel
## https://www.statology.org/r-foreach/
## https://berkeley-scf.github.io/tutorial-parallelization/parallel-R.html#3-parallel-loops-including-parallel-lapply-via-the-future-package
### Parallel loops
## https://datacolada.org/102

sgd <- function(input,id_cols,var_param,var_weight,learning_rate, epochs, test_sample_size) {
library('groundhog')
library('foreach')
library('doParallel')
#get the core processors ready to go
registerDoParallel(cores=round(parallel::detectCores()*0.8)) #I leave 20% free to do things while R runs

  # test
  #input = tmp
  #id_cols = c("farm_id","crop","land_use_type")
  #var_param = tmp_param_BV_constant
  #var_weight = tmp_param_var_weight
  #learning_rate = 0.01
  #epochs = 10
  #test_sample_size = 10


  tmp_base_cost <- BVIAS_cost_function_LU(input, id_cols, var_param,var_weight)
  tmp_var_param = var_param

  var_param_optim = foreach(epoch = 1:epochs,.combine = 'rbind') %dopar% {
    #for (epoch in 1:epochs) {
    library(dplyr)
    library('groundhog')
    library('foreach')
    library('doParallel')
    source("d:/users/srhuet/documents/BiodivLabel/R/out_BVIAS_crops_functions.R")


    # Randomly sample some data point
    index <- sample(1:nrow(input), test_sample_size)
    input_i <- input[index,]

    # Compute the gradient of the loss function
    ## the gradient is obtained by calculating the partial derivative of the loss function
    tmp_new_cost <-  BVIAS_cost_function_LU(input_i, id_cols, tmp_var_param,var_weight)
    ## WIP I think I should compute one gradient per parameter based on the derivative of the loss function according to this parameter
    tmp_gradient <- -2/test_sample_size * sum(tmp_new_cost$distance_table$observed_values - tmp_new_cost$distance_table$BVAS)

    # Update model parameters
    tmpl_var_param = foreach(r = seq_len(nrow(tmp_var_param)), .combine = 'rbind') %dopar% {
      library('groundhog')
      library('foreach')
      library('doParallel')
      source("d:/users/srhuet/documents/BiodivLabel/R/out_BVIAS_crops_functions.R")

      tmp_c = foreach(c = colnames(dplyr::select_if(tmp_var_param,is.numeric)),  # Ignorer les colonnes non-paramètres
                      .combine='cbind') %dopar% {

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
    MSE_old <- BVIAS_cost_function_LU(input, id_cols, tmp_var_param,var_weight)
    MSE_new <- BVIAS_cost_function_LU(input, id_cols, tmpl_var_param,var_weight)
    if (!is.na(MSE_new$MSE) & MSE_new$MSE < MSE_old$MSE) {
      tmp_var_param <- tmpl_var_param
      var_param_optim <- tmp_var_param %>% mutate(MSE = MSE_new$MSE)
    } else {var_param_optim <- tmp_var_param %>% mutate(MSE = MSE_old$MSE)}
    var_param_optim
  }
  var_param_optim <- var_param_optim %>% filter(MSE == min(MSE))
  return(var_param_optim)
}










## Calibration of metric weight ----

### Loss function ----


# Mean Squared Error (MSE) as loss function
BVIAS_loss_function_weight <- function(input,id_cols,var_param,var_weight) {
  library(dplyr)

  #input = tmp
  #id_cols = c("farm_id","crop","land_use_type")
  #var_param = tmp_param_var
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

  tmp_distance <- tmp_BVIAS_median$BVIAS %>%
    # calculate distance from expected values regarding Gallego et al,. 2022
    dplyr::mutate(
      distance = case_when(
        land_use_type == "arable" ~ (0.32 - BV_loc)^2,
        land_use_type == "grassland" ~ (0.57 - BV_loc)^2
      )
    )

  # Erreur quadratique moyenne
  tmp_MSE = mean(tmp_distance$distance)

  return(tmp_MSE)

}

### Weight calibration ----




