## Automatized optimization----
set.seed(1234)
tmp_optim_ls <- list()

for (tmp_LU in unique(tmp_input$land_use_type)) {
  
  print(tmp_LU)
  
  #tmp_LU = "arable"
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
      tmp_var_constant_v_new = unlist(tmp_var_BVC_constant %>% filter(land_use_type == tmp_LU & metric_number %in% colnames(tmp_input)) %>% select_if(.,is.numeric))
      tmp_var_weight_v_new = tmp_var_weight$weight[tmp_var_weight$land_use_type == tmp_LU]
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
  
}

save(list = "tmp_optim_ls",file = paste0("data_out/optim_",Sys.Date(),".RData"))
