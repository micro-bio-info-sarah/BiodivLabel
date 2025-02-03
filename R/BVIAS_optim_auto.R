## Automatized optimization----
set.seed(1234)
tmp_optim_ls <- list()
tmp_var_BVC_constant_optim <- tmp_var_BVC_constant
tmp_var_weight_optim <- tmp_var_weight

# Cost function ----

BVIAS_optim <- function(var_v) {
  library(dplyr)
  
  input = tmp_x_norm_LU
  id_cols = c("farm_id","crop","land_use_type")
  #tmp_LU = "grassland"
  #var_weight = tmp_var_weight_optim
  #var_weight_v = tmp_var_weight_optim$weight[tmp_var_weight_optim$land_use_type == tmp_LU]
  #var_constant = tmp_var_BVC_constant_optim
  #var_constant_v = unlist(tmp_var_BVC_constant_optim %>% filter(land_use_type == tmp_LU & metric_number %in% unique(input$metric_number)) %>% select_if(.,is.numeric))
  #var_v = c(var_constant_v,var_weight_v)
  var_constant_v = var_v[1:length(tmp_var_constant_v_new)]
  
  var_constant = tmp_var_BVC_constant_optim %>%
    # select description variables
    filter(land_use_type == tmp_LU &
             metric_number %in% unique(input$metric_number)) %>% 
    select_if(.,is.character) %>%
    # add new constants for considered land use
    cbind(.,matrix(var_constant_v,
                   nrow = nrow(tmp_var_BVC_constant_optim %>% 
                                 filter(land_use_type == tmp_LU &
                                          metric_number %in% unique(input$metric_number))),
                   ncol = length(var_constant_v) / nrow(tmp_var_BVC_constant_optim %>%
                                                          filter(land_use_type == tmp_LU &
                                                                   metric_number %in% unique(input$metric_number))),
                   dimnames = list(1:nrow(tmp_var_BVC_constant_optim %>% 
                                            filter(land_use_type == tmp_LU &
                                                     metric_number %in% unique(input$metric_number))),
                                   colnames(tmp_var_BVC_constant_optim %>% 
                                              select_if(.,is.numeric))))) %>%
    # add constants for other land use
    rbind(.,tmp_var_BVC_constant_optim %>%
            filter(land_use_type != tmp_LU & metric_number %in% unique(input$metric_number))) %>%
    # do not change gamma and epsilon parameters as they set the response range as [0;1], nor beta as it changes the direction of the variation
    mutate(
      epsilon = tmp_var_BVC_constant_optim$epsilon[tmp_var_BVC_constant_optim$metric_number %in% unique(input$metric_number)],
      gamma = tmp_var_BVC_constant_optim$gamma[tmp_var_BVC_constant_optim$metric_number %in% unique(input$metric_number)],
      beta = tmp_var_BVC_constant_optim$beta[tmp_var_BVC_constant_optim$metric_number %in% unique(input$metric_number)]      
    )
  
  var_weight_v = var_v[(length(var_v)-length(tmp_var_weight_v_new)+1):length(var_v)]
  var_weight <-  tmp_var_weight_optim %>%
    filter(land_use_type == tmp_LU) %>%
    mutate(weight = var_weight_v) %>%
    rbind(.,tmp_var_weight_optim %>%
            filter(land_use_type != tmp_LU))
  
  # estimate MSE
  
  tmp_MSE = BVIAS_MSE(input = input,
                      id_cols = id_cols,
                      var_constant = var_constant,
                      var_weight = var_weight)
  
  tmp_MSE = tmp_MSE$MSE
  
  
  return(tmp_MSE)
  
}

# Calibration ----

for (tmp_LU in unique(tmp_input$land_use_type)) {
  #tmp_LU = unique(tmp_input$land_use_type)[2]
  
  print(tmp_LU)
  
  #tmp_LU = "grassland"
  tmp_x_norm_LU <- tmp_x_norm %>% filter(land_use_type == tmp_LU)
  
  ### initial MSE
  
  tmp_MSE = 1
  tmp_MSE_new = mean(tmp_MSE_init$distance_table$distance[tmp_MSE_init$distance_table$land_use_type == tmp_LU])
  
  ### init loop
  tmp_n = 0
  tmp_optim_ls_LU = list()
  
  #while (tmp_MSE_new[3] <= tmp_MSE[3]*(1-1e-3)) {
  while (tmp_MSE_new <= tmp_MSE*(1-1e-3)) {
    
    # first loop
    if (tmp_n == 0) {
      tmp_var_constant_v_new = unlist(tmp_var_BVC_constant_optim %>% filter(land_use_type == tmp_LU & metric_number %in% unique(tmp_x_norm_LU$metric_number)) %>% select_if(.,is.numeric))
      tmp_var_weight_v_new = unlist(tmp_var_weight_optim %>% filter(land_use_type == tmp_LU & metric_number %in% unique(tmp_x_norm_LU$metric_number)) %>% select(weight))
      tmp_var_v = c(tmp_var_constant_v_new,tmp_var_weight_v_new)
    }
    
    
    # loop indent
    tmp_n = tmp_n +1
    print("Iteration number:")
    print(tmp_n)
    
    # optimize
    tmp_optim_ls_LU[[tmp_n]] <- optim(tmp_var_v,BVIAS_optim)
    
    tmp_var_v = tmp_optim_ls_LU[[tmp_n]]$par
    
    # MSE
    tmp_MSE <- tmp_MSE_new
    tmp_MSE_new <- tmp_optim_ls_LU[[tmp_n]]$value
    
    
    print("with a MSE =")
    print(tmp_MSE_new)
    
  }
  
  tmp_optim_ls[[tmp_LU]] <- tmp_optim_ls_LU
  
  # retrieve parameters
  tmp_var_constant_v = tmp_optim_ls[[tmp_LU]][[length(tmp_optim_ls[[tmp_LU]])]]$par[1:length(tmp_var_constant_v_new)]
  tmp_var_weight_v = tmp_optim_ls[[tmp_LU]][[length(tmp_optim_ls[[tmp_LU]])]]$par[(length(tmp_var_constant_v_new)+1):(length(tmp_var_constant_v_new)+length(tmp_var_weight_v_new))]
  
  tmp_var_BVC_constant_run = tmp_var_BVC_constant_optim %>%
    # select description variables
    filter(land_use_type == tmp_LU &
             metric_number %in% unique(tmp_x_norm_LU$metric_number)) %>% 
    select_if(.,is.character) %>%
    # add new constants for considered land use
    cbind(.,matrix(tmp_var_constant_v,
                   nrow = nrow(tmp_var_BVC_constant_optim %>% 
                                 filter(land_use_type == tmp_LU &
                                          metric_number %in% unique(tmp_x_norm_LU$metric_number))),
                   ncol = length(tmp_var_constant_v) / nrow(tmp_var_BVC_constant_optim %>%
                                                          filter(land_use_type == tmp_LU &
                                                                   metric_number %in% unique(tmp_x_norm_LU$metric_number))),
                   dimnames = list(1:nrow(tmp_var_BVC_constant_optim %>% 
                                            filter(land_use_type == tmp_LU &
                                                     metric_number %in% unique(tmp_x_norm_LU$metric_number))),
                                   colnames(tmp_var_BVC_constant_optim %>% 
                                              select_if(.,is.numeric))))) %>%
    # do not change gamma and epsilon parameters as they set the response range as [0;1], nor beta as it changes the direction of the variation
    mutate(
      epsilon = unlist(tmp_var_BVC_constant %>% filter(land_use_type == tmp_LU & metric_number %in% unique(tmp_x_norm_LU$metric_number)) %>% select(epsilon)),
      gamma = unlist(tmp_var_BVC_constant %>% filter(land_use_type == tmp_LU & metric_number %in% unique(tmp_x_norm_LU$metric_number)) %>% select(gamma)),
      beta = unlist(tmp_var_BVC_constant %>% filter(land_use_type == tmp_LU & metric_number %in% unique(tmp_x_norm_LU$metric_number)) %>% select(beta))
    ) %>%
    # add constants for other land use
    rbind(.,tmp_var_BVC_constant_optim %>%
            filter(land_use_type != tmp_LU))
  
  tmp_var_weight_run <-  tmp_var_weight_optim %>%
    filter(land_use_type == tmp_LU &
             metric_number %in% unique(tmp_x_norm_LU$metric_number)) %>%
    mutate(weight = tmp_var_weight_v) %>%
    rbind(.,tmp_var_weight_optim %>%
            filter(land_use_type != tmp_LU))
  
  tmp_optim_ls[["constants"]] <- tmp_var_BVC_constant_optim <- tmp_var_BVC_constant_run
  tmp_optim_ls[["weights"]] <- tmp_var_weight_optim <- tmp_var_weight_run
}

#save(list = "tmp_optim_ls",file = paste0("data_out/optim_",Sys.Date(),".RData"))
