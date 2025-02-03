# BVIAS for crops

# Set up ----

library(dplyr)
library(tidyr)
library(readxl)
# BVIAS ----

# first I need to prepare the data
data_for_BVIAS <- function(input,id_cols,var_constant) {
  
  # test
  #input = tmp_input
  #id_cols = c("farm_id","crop","land_use_type")
  #var_constant = tmp_var_BVC_constant
  
  tmp_var <- intersect(var_constant$metric_number,colnames(input))
  tmp_x <- input %>%
    dplyr::select(tidyselect::all_of(id_cols),tidyselect::all_of(tmp_var)) %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(tmp_var),names_to = "metric_number",values_to = "value") %>%
    filter(!is.na(value))
  
  # set 95th percentile as max
  tmp_max <- tmp_x %>%
    dplyr::group_by(metric_number,land_use_type) %>%
    dplyr::summarise(max = as.vector(quantile(unique(value),0.95,na.rm = T)),.groups = "keep") %>% ungroup()
  
  tmp_x_norm <- tmp_x %>%
    # add max
    dplyr::left_join(.,tmp_max,by = join_by(land_use_type,metric_number)) %>%
    ## calculate BV
    #dplyr::rowwise() %>% # no need as we left-join the max variable
    dplyr::mutate(
      ## set max
      x_max =
        case_when(
          value > max ~ max,
          .default =  value ),
      ## Normalize data
      x_norm =
        case_when(
          value <= 0 ~ 0,
          value >= max ~ 1,
          .default =  value / max )) %>%
    dplyr::ungroup() 
  
  # add organic_farming
  
  if ("org_farming" %in% colnames(input)) {
    tmp_x_norm <- tmp_x_norm %>%
      left_join(., input %>%
                  dplyr::select(tidyselect::all_of(id_cols),"org_farming") %>%
                  distinct(),
                by = tidyselect::all_of(id_cols))
    
  }
  
  
  return(tmp_x_norm)
  
}

# then I can estimate the BVIAS
BVIAS <- function(input,id_cols,var_constant,var_weight) {
  
  # test
  #input = tmp_x_norm
  #id_cols = c("farm_id","crop","land_use_type")
  #var_constant = tmp_var_BVC_constant
  #var_weight = tmp_var_weight
  #LU_range = tmp_param_LU_range
  
  tmp_y <- input %>%
    # add variable contants
    dplyr::left_join(.,var_constant,by = join_by(land_use_type, metric_number)) %>%
    ## calculate BV
    #dplyr::rowwise() %>%
    dplyr::mutate(
      ## calculate BV
      y = gamma + epsilon * exp(-(
        abs((((x_norm)^delta) - beta) ^ alpha) /
          (2*sigma^alpha)))
    ) %>%
    dplyr::ungroup() %>%
    # check y range as [0;1]
    mutate(
      y = case_when(
        y <=0 ~ 0,
        y>=1 ~1,
        # optimized constants can produced NAs at limits (i.e., 0 or 1), so I force y
        ## increasing functions
        metric_number %in% c("A.2.1","A.3.3") & is.na(y) & x_norm == 0 ~ 0,
        metric_number %in% c("A.2.1","A.3.3") & is.na(y) & x_norm == 1 ~ 1,
        ## decreasing functions
        !(metric_number %in% c("A.2.1","A.3.3")) & is.na(y) & x_norm == 0 ~ 1,
        !(metric_number %in% c("A.2.1","A.3.3")) & is.na(y) & x_norm == 1 ~ 0,
        .default = y
      )
    )
  
  tmp_BVIAS <- tmp_y %>%
    # add variable weights
    dplyr::left_join(.,var_weight, by= join_by(land_use_type, metric_number)) %>%
    # remove variables without weight
    filter(!is.na(weight)) %>%
    # BV LU
    # aggregate variables
    dplyr::group_by_at(tidyselect::all_of(id_cols)) %>%
    dplyr::summarise(
      BV_LU = weighted.mean(y,weight),
      .groups = "keep"
    ) %>% ungroup() %>%
    # BV NORM
    # add land use type ranges
    dplyr::left_join(.,tibble(
      # set BV_norm min and max according to Lindner et al. (2019)
      land_use_type = c("grassland","arable"),
      LU_min = c(0.44,0.23),
      LU_max = c(0.92,0.52)
    ), by = "land_use_type") %>%
    # normalize BV
    dplyr::mutate(
      BV_norm = LU_min + BV_LU * (LU_max - LU_min)) %>%
    mutate(
      # BV LOC
      #BV_loc = 1.017626088*(1-exp(-4.055847776*BV_norm)),
      BV_loc = BV_norm,
      # BVI
      BVI_ha = 1- BV_loc
    )
  
  # add organic_farming
  
  if ("org_farming" %in% colnames(input)) {
    tmp_BVIAS <- tmp_BVIAS %>%
      left_join(., input %>%
                  dplyr::select(tidyselect::all_of(id_cols),"org_farming") %>%
                  dplyr::distinct(),
                by = tidyselect::all_of(id_cols))
    
  }
  
  return(list(y = tmp_y, BVIAS = tmp_BVIAS))
  
}

# Model optimization ----

# To optimize the BVIAS model, the convergence to the global minimum of the loss function based on the mean square error is obtained through stochastic gradient descent.

# Mean Squared Error (MSE) as loss function

BVIAS_MSE <- function(input,id_cols,var_constant,var_weight) {
  library(dplyr)
  library(tibble)
  library(readr)
  
  #input = tmp_x_norm
  #id_cols = c("farm_id","crop","land_use_type")
  #var_constant = tmp_var_BVC_constant
  #var_weight = tmp_var_weight
  LU = unique(input$land_use_type)
  
  
  # model output
  #tmp_x_norm <- data_for_BVIAS(input,id_cols,var_constant)
  tmp_BVIAS <- BVIAS(input,id_cols,var_constant,var_weight)
  
  
  # median ----
  ## estimate variable median for each land use type
  tmp_LU_median <- input %>%
    dplyr::group_by(land_use_type,metric_number) %>%
    dplyr::summarise(
      as_tibble_row(quantile(x_norm,c(0,0.05,0.10,0.25,0.5,0.75,0.90,0.95,1),na.rm = T),
                    .name_repair = \(x) paste0('q',parse_number(x))),
      .groups = "keep") %>%
    mutate(
      x_norm = q50
    ) %>%
    ungroup()
  
  # median LU types ----
  
  tmp_BVIAS_median <- BVIAS(tmp_LU_median,c("land_use_type"),var_constant,var_weight)
  
  # calculate distance from expected values regarding Gallego et al,. 2022
  tmp_LU_distance <- tmp_BVIAS_median$BVIAS %>%
    dplyr::mutate(
      archetype = case_when(
        land_use_type == "arable" ~ "median_crop",
        land_use_type == "grassland" ~ "median_grassland"
      ),
      litt_values = case_when(
        land_use_type == "arable" ~ 0.32,
        land_use_type == "grassland" ~ 0.57
      )
    ) %>%
    select(archetype,land_use_type,litt_values,BV_loc)
  
  # effect size (ES) ----
  
  ## pesticides -> 0.35 (Beketov et al., 2013) ----
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.5.1_q5",
      x_norm = case_when(
        metric_number == "A.5.1" ~ q5,
        .default = q50
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.5.1_q95",
          x_norm = case_when(
            metric_number == "A.5.1" ~ q95,
            .default = q50
          )
        )
    ) %>%
    select(id_col,land_use_type,metric_number,x_norm)
  
  tmp_BVIAS_A.5.1 <- BVIAS(tmp_ES_input,c("id_col","land_use_type"),var_constant,var_weight)
  tmp_qmin <- mean(c(
    tmp_BVIAS_A.5.1$BVIAS$BV_loc[tmp_BVIAS_A.5.1$BVIAS$id_col == "A.5.1_q5" & tmp_BVIAS_A.5.1$BVIAS$land_use_type == "arable"],
    tmp_BVIAS_A.5.1$BVIAS$BV_loc[tmp_BVIAS_A.5.1$BVIAS$id_col == "A.5.1_q5" & tmp_BVIAS_A.5.1$BVIAS$land_use_type == "grassland"]
  ))
  tmp_qmax <- tmp_BVIAS_A.5.1$BVIAS$BV_loc[tmp_BVIAS_A.5.1$BVIAS$id_col == "A.5.1_q95" & tmp_BVIAS_A.5.1$BVIAS$land_use_type == "arable"]
  
  tmp_ES_A.5.1 <- (tmp_qmin - tmp_qmax) / tmp_qmin
  tmp_ES_A.5.1 <- tibble(archetype = "ES_A.5.1",land_use_type = LU,litt_values = 0.35,BV_loc = tmp_ES_A.5.1) %>%
    filter(land_use_type == "arable")
  
  ## fertilizers -> 0.35 (Sánchez-Bayo and Wyckhuys, 2019) ----
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.4.5_q5",
      x_norm = case_when(
        metric_number == "A.4.5" ~ q5,
        .default = q50
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.4.5_q95",
          x_norm = case_when(
            metric_number == "A.4.5" ~ q95,
            .default = q50
          )
        )
    )
  
  tmp_BVIAS_A.4.5 <- BVIAS(tmp_ES_input,c("id_col","land_use_type"),var_constant,var_weight)
  tmp_qmin <- c(
    tmp_BVIAS_A.4.5$BVIAS$BV_loc[tmp_BVIAS_A.4.5$BVIAS$id_col == "A.4.5_q5" & tmp_BVIAS_A.4.5$BVIAS$land_use_type == "arable"],
    tmp_BVIAS_A.4.5$BVIAS$BV_loc[tmp_BVIAS_A.4.5$BVIAS$id_col == "A.4.5_q5" & tmp_BVIAS_A.4.5$BVIAS$land_use_type == "grassland"]
  )
  tmp_qmax <- c(
    tmp_BVIAS_A.4.5$BVIAS$BV_loc[tmp_BVIAS_A.4.5$BVIAS$id_col == "A.4.5_q95" & tmp_BVIAS_A.4.5$BVIAS$land_use_type == "arable"],
    tmp_BVIAS_A.4.5$BVIAS$BV_loc[tmp_BVIAS_A.4.5$BVIAS$id_col == "A.4.5_q95" & tmp_BVIAS_A.4.5$BVIAS$land_use_type == "grassland"]
  )
  
  tmp_ES_A.4.5 <- (tmp_qmin - tmp_qmax) / tmp_qmin
  tmp_ES_A.4.5 <- tibble(archetype = "ES_A.4.5",land_use_type = LU,litt_values = 0.35,BV_loc = tmp_ES_A.4.5)
  
  
  ## crop diversity -> 0.18(Sirami et al., 2019) ----
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "3_crops",
      x_norm = case_when(
        metric_number == "A.3.3" & land_use_type == "arable" ~ quantile(input$x_norm[input$metric_number == "A.3.3" & input$land_use_type == "arable" & input$farm_id %in% BV_A.3.3$farm_id[BV_A.3.3$crop_nb_LU == 3 & BV_A.3.3$land_use_type == "arable"]],0.75),
        .default = q50
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "1_crop",
          x_norm = case_when(
            metric_number == "A.3.3" & land_use_type == "arable" ~ quantile(input$x_norm[input$metric_number == "A.3.3" & input$land_use_type == "arable" & input$farm_id %in% BV_A.3.3$farm_id[BV_A.3.3$crop_nb_LU == 1 & BV_A.3.3$land_use_type == "arable"]],0.25),
            .default = q50
          )
        )
    )
  tmp_BVIAS_A.3.3 <- BVIAS(tmp_ES_input,c("id_col","land_use_type"),var_constant,var_weight)
  
  tmp_qmin <- tmp_BVIAS_A.3.3$BVIAS$BV_loc[tmp_BVIAS_A.3.3$BVIAS$id_col == "1_crop"]
  tmp_qmax <- tmp_BVIAS_A.3.3$BVIAS$BV_loc[tmp_BVIAS_A.3.3$BVIAS$id_col == "3_crops"]
  tmp_ES_A.3.3 <- (tmp_qmax - tmp_qmin) / tmp_qmax
  tmp_ES_A.3.3 <- tibble(archetype = "ES_A.3.3",land_use_type = LU,litt_values = 0.18,BV_loc = tmp_ES_A.3.3) %>%
    filter(land_use_type == "arable")
  
  ## small woody features -> 0.20 (Valle et al., 2023) ----
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.2.1_q5",
      x_norm = case_when(
        metric_number == "A.2.1" ~ q5,
        .default = q50
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.2.1_q75",
          x_norm = case_when(
            metric_number == "A.2.1" ~ q75,
            .default = q50
          )
        )
    )
  tmp_BVIAS_A.2.1 <- BVIAS(tmp_ES_input,c("id_col","land_use_type"),var_constant,var_weight)
  
  tmp_qmin <- tmp_BVIAS_A.2.1$BVIAS$BV_loc[tmp_BVIAS_A.2.1$BVIAS$id_col == "A.2.1_q5"]
  tmp_qmax <- tmp_BVIAS_A.2.1$BVIAS$BV_loc[tmp_BVIAS_A.2.1$BVIAS$id_col == "A.2.1_q75"]
  tmp_ES_A.2.1 <- (tmp_qmax - tmp_qmin) / tmp_qmax
  tmp_ES_A.2.1 <- tibble(archetype = "ES_A.2.1",land_use_type = LU,litt_values = 0.20,BV_loc = tmp_ES_A.2.1) %>%
    filter(land_use_type == "arable")
  
  ## mean field size -> 0.05 (Sirami et al., 2019) ----
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.2.2_q25",
      x_norm = case_when(
        metric_number == "A.2.2" ~ q25,
        .default = q50
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.2.2_q75",
          x_norm = case_when(
            metric_number == "A.2.2" ~ q75,
            .default = q50
          )
        )
    )
  tmp_BVIAS_A.2.2 <- BVIAS(tmp_ES_input,c("id_col","land_use_type"),var_constant,var_weight)
  
  tmp_qmin <- tmp_BVIAS_A.2.2$BVIAS$BV_loc[tmp_BVIAS_A.2.2$BVIAS$id_col == "A.2.2_q25"]
  tmp_qmax <- tmp_BVIAS_A.2.2$BVIAS$BV_loc[tmp_BVIAS_A.2.2$BVIAS$id_col == "A.2.2_q75"]
  tmp_ES_A.2.2 <- (tmp_qmin - tmp_qmax) / tmp_qmin
  tmp_ES_A.2.2 <- tibble(archetype = "ES_A.2.2",land_use_type = LU,litt_values = 0.05,BV_loc = tmp_ES_A.2.2) %>%
    filter(land_use_type == "arable")
  
  
  ## organic / conventional farming -> 0.30 (wheat) and 0.15 (grassland) (Tuck et al., 2014) ----
  
  # least square difference between estimated effect size and values from litterature
  
  ## estimate variable median and quantiles for each land use type
  
  tmp_org_mean <- tmp_BVIAS$BVIAS %>%
    filter(crop == "111" | land_use_type == "grassland") %>% # keep only wheat and grasslands
    dplyr::group_by(org_farming,land_use_type) %>%
    dplyr::summarise(
      BV_loc_mean = mean(BV_loc,na.rm =T),
      .groups = "keep") %>%
    ungroup()
  
  tmp_org <- c(
    tmp_org_mean$BV_loc_mean[tmp_org_mean$org_farming == T & tmp_org_mean$land_use_type == "arable"],
    tmp_org_mean$BV_loc_mean[tmp_org_mean$org_farming == T & tmp_org_mean$land_use_type == "grassland"]
  )
  tmp_conv <- c(
    tmp_org_mean$BV_loc_mean[tmp_org_mean$org_farming == F & tmp_org_mean$land_use_type == "arable"],
    tmp_org_mean$BV_loc_mean[tmp_org_mean$org_farming == F & tmp_org_mean$land_use_type == "grassland"]
  )
  
  tmp_comp_org <- (tmp_org - tmp_conv) / tmp_org
  
  tmp_comp_org <- tibble(archetype = "comp_org",
                         land_use_type = LU,
                         litt_values = c((1.42-1)/1.42,
                                         (1.17-1)/1.17),
                         BV_loc = tmp_comp_org)
  
  
  #  MSE ----
  
  distance_table <- Reduce(rbind,
                           list(tmp_LU_distance,
                                tmp_ES_A.2.1,tmp_ES_A.2.2,tmp_ES_A.3.3,tmp_ES_A.4.5,tmp_ES_A.5.1,
                                tmp_comp_org)) %>%
    # estimate distance
    mutate(
      ## we choose a fourth power distance to ensure a higher equity among distance minimization as it increase the relative importance of higher distances
      distance = (litt_values - BV_loc)^4
      )
  
  tmp_MSE = mean(distance_table$distance)
  
  tmp_MSE
  
  
  return(list(MSE = tmp_MSE,
              distance_table = distance_table,
              x_norm = input,
              BVIAS = tmp_BVIAS))
  
}

# Effect size ----

BVIAS_effect_size <- function(input,id_cols,var_constant,var_weight) {
  library(dplyr)
  library(tibble)
  library(readr)
  
  #input = tmp_x_norm 
  #input = optim_final$MSE$MSE_optim$x_norm
  #id_cols = c("farm_id","crop","land_use_type")
  #var_constant = tmp_var_BVC_constant_optim
  #var_constant = optim_final$constants$BV_constant_optim
  #var_weight = tmp_var_weight_optim
  #var_weight = optim_final$weights$weight_optim
  LU = unique(input$land_use_type)

  # model output
  #tmp_x_norm <- data_for_BVIAS(input,id_cols,var_constant)
  tmp_BVIAS <- BVIAS(input,id_cols,var_constant,var_weight)
  
  
  # median ----
  ## estimate variable median for each land use type
  tmp_LU_median <- input %>%
    dplyr::group_by(land_use_type,metric_number) %>%
    dplyr::summarise(
      as_tibble_row(quantile(x_norm,c(0,0.05,0.10,0.25,0.5,0.75,0.90,0.95,1),na.rm = T),
                    .name_repair = \(x) paste0('q',parse_number(x))),
      .groups = "keep") %>%
    mutate(
      x_norm = q50
    ) %>%
    ungroup()
  
  # effect size (ES) ----
  
  ## A.2.1 Hedges ----
  
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.2.1",
      quantile = "q25",
      x_norm = case_when(
        metric_number == "A.2.1" ~ q25,
        .default = q50
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.2.1",
          quantile = "q75",
          x_norm = case_when(
            metric_number == "A.2.1" ~ q75,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.2.1",
          quantile = "q5",
          x_norm = case_when(
            metric_number == "A.2.1" ~ q5,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.2.1",
          quantile = "q95",
          x_norm = case_when(
            metric_number == "A.2.1" ~ q95,
            .default = q50
          )
        )
    )
  
  tmp_BVIAS_A.2.1 <- BVIAS(tmp_ES_input,c("id_col","quantile","land_use_type"),var_constant,var_weight)
  
  tmp_ES_A.2.1 <- tmp_BVIAS_A.2.1$BVIAS %>%
    pivot_wider(id_cols = c(id_col,land_use_type),
                names_from = quantile,values_from = BVI_ha) %>%
    mutate(BVIAS_ES_25_75 = (q75-q25), BVIAS_ES_5_95 = (q95-q5))
  
  ## A.2.2 Mean Field Size ----
  
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.2.2",
      quantile = "q25",
      x_norm = case_when(
        metric_number == "A.2.2" ~ q25,
        .default = q50
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.2.2",
          quantile = "q75",
          x_norm = case_when(
            metric_number == "A.2.2" ~ q75,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.2.2",
          quantile = "q5",
          x_norm = case_when(
            metric_number == "A.2.2" ~ q5,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.2.2",
          quantile = "q95",
          x_norm = case_when(
            metric_number == "A.2.2" ~ q95,
            .default = q50
          )
        )
    )
  
  tmp_BVIAS_A.2.2 <- BVIAS(tmp_ES_input,c("id_col","quantile","land_use_type"),var_constant,var_weight)
  
  tmp_ES_A.2.2 <- tmp_BVIAS_A.2.2$BVIAS %>%
    pivot_wider(id_cols = c(id_col,land_use_type),
                names_from = quantile,values_from = BVI_ha) %>%
    mutate(BVIAS_ES_25_75 = (q75-q25), BVIAS_ES_5_95 = (q95-q5))
  
  
  ## A.3.1 Intensity of soil mouvement ----
  
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.3.1",
      quantile = "q25",
      x_norm = case_when(
        metric_number == "A.3.1" ~ q25,
        .default = q50
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.3.1",
          quantile = "q75",
          x_norm = case_when(
            metric_number == "A.3.1" ~ q75,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.3.1",
          quantile = "q5",
          x_norm = case_when(
            metric_number == "A.3.1" ~ q5,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.3.1",
          quantile = "q95",
          x_norm = case_when(
            metric_number == "A.3.1" ~ q95,
            .default = q50
          )
        )
    )
  
  tmp_BVIAS_A.3.1 <- BVIAS(tmp_ES_input,c("id_col","quantile","land_use_type"),var_constant,var_weight)
  
  tmp_ES_A.3.1 <- tmp_BVIAS_A.3.1$BVIAS %>%
    pivot_wider(id_cols = c(id_col,land_use_type),
                names_from = quantile,values_from = BVI_ha) %>%
    mutate(BVIAS_ES_25_75 = (q75-q25), BVIAS_ES_5_95 = (q95-q5))
  
  ## A.3.2 Ground cover ----
  

  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.3.2",
      quantile = "q25",
      x_norm = case_when(
        metric_number == "A.3.2" ~ q25,
        .default = q50
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.3.2",
          quantile = "q75",
          x_norm = case_when(
            metric_number == "A.3.2" ~ q75,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.3.2",
          quantile = "q5",
          x_norm = case_when(
            metric_number == "A.3.2" ~ q5,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.3.2",
          quantile = "q95",
          x_norm = case_when(
            metric_number == "A.3.2" ~ q95,
            .default = q50
          )
        )
    )
  
  tmp_BVIAS_A.3.2 <- BVIAS(tmp_ES_input,c("id_col","quantile","land_use_type"),var_constant,var_weight)
  
  tmp_ES_A.3.2 <- tmp_BVIAS_A.3.2$BVIAS %>%
    pivot_wider(id_cols = c(id_col,land_use_type),
                names_from = quantile,values_from = BVI_ha) %>%
    mutate(BVIAS_ES_25_75 = (q75-q25), BVIAS_ES_5_95 = (q95-q5))
  
  ## A.3.3 Cultural Diversity ----
  
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.3.3",
      quantile = "q25",
      x_norm = case_when(
        metric_number == "A.3.3" ~ q25,
        .default = q50
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.3.3",
          quantile = "q75",
          x_norm = case_when(
            metric_number == "A.3.3" ~ q75,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.3.3",
          quantile = "q5",
          x_norm = case_when(
            metric_number == "A.3.3" ~ q5,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.3.3",
          quantile = "q95",
          x_norm = case_when(
            metric_number == "A.3.3" ~ q95,
            .default = q50
          )
        )
    )
  
  tmp_BVIAS_A.3.3 <- BVIAS(tmp_ES_input,c("id_col","quantile","land_use_type"),var_constant,var_weight)
  
  tmp_ES_A.3.3 <- tmp_BVIAS_A.3.3$BVIAS %>%
    pivot_wider(id_cols = c(id_col,land_use_type),
                names_from = quantile,values_from = BVI_ha) %>%
    mutate(BVIAS_ES_25_75 = (q75-q25), BVIAS_ES_5_95 = (q95-q5))
  
  ## A.4.3 Share of artificial fertilizer ----
  
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.4.3",
      quantile = "q25",
      x_norm = case_when(
        metric_number == "A.4.3" ~ q25,
        .default = q50
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.4.3",
          quantile = "q75",
          x_norm = case_when(
            metric_number == "A.4.3" ~ q75,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.4.3",
          quantile = "q5",
          x_norm = case_when(
            metric_number == "A.4.3" ~ q5,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.4.3",
          quantile = "q95",
          x_norm = case_when(
            metric_number == "A.4.3" ~ q95,
            .default = q50
          )
        )
    )
  
  tmp_BVIAS_A.4.3 <- BVIAS(tmp_ES_input,c("id_col","quantile","land_use_type"),var_constant,var_weight)
  
  tmp_ES_A.4.3 <- tmp_BVIAS_A.4.3$BVIAS %>%
    pivot_wider(id_cols = c(id_col,land_use_type),
                names_from = quantile,values_from = BVI_ha) %>%
    mutate(BVIAS_ES_25_75 = (q75-q25), BVIAS_ES_5_95 = (q95-q5))
  
  ## A.4.5 Fertilization ----
  
  
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.4.5",
      quantile = "q25",
      x_norm = case_when(
        metric_number == "A.4.5" ~ q25,
        .default = q50
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.4.5",
          quantile = "q75",
          x_norm = case_when(
            metric_number == "A.4.5" ~ q75,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.4.5",
          quantile = "q5",
          x_norm = case_when(
            metric_number == "A.4.5" ~ q5,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.4.5",
          quantile = "q95",
          x_norm = case_when(
            metric_number == "A.4.5" ~ q95,
            .default = q50
          )
        )
    )
  
  tmp_BVIAS_A.4.5 <- BVIAS(tmp_ES_input,c("id_col","quantile","land_use_type"),var_constant,var_weight)
  
  tmp_ES_A.4.5 <- tmp_BVIAS_A.4.5$BVIAS %>%
    pivot_wider(id_cols = c(id_col,land_use_type),
                names_from = quantile,values_from = BVI_ha) %>%
    mutate(BVIAS_ES_25_75 = (q75-q25), BVIAS_ES_5_95 = (q95-q5))
  
  ## A.5.1 Pesticides ----
  
  
  tmp_ES_input <- tmp_LU_median %>%
    mutate(
      id_col = "A.5.1",
      quantile = "q25",
      x_norm = case_when(
        metric_number == "A.5.1" ~ q25,
        .default = q50
      )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.5.1",
          quantile = "q75",
          x_norm = case_when(
            metric_number == "A.5.1" ~ q75,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.5.1",
          quantile = "q5",
          x_norm = case_when(
            metric_number == "A.5.1" ~ q5,
            .default = q50
          )
        )
    ) %>%
    rbind(
      tmp_LU_median %>%
        mutate(
          id_col = "A.5.1",
          quantile = "q95",
          x_norm = case_when(
            metric_number == "A.5.1" ~ q95,
            .default = q50
          )
        )
    )
  
  tmp_BVIAS_A.5.1 <- BVIAS(tmp_ES_input,c("id_col","quantile","land_use_type"),var_constant,var_weight)
  
  tmp_ES_A.5.1 <- tmp_BVIAS_A.5.1$BVIAS %>%
    pivot_wider(id_cols = c(id_col,land_use_type),
                names_from = quantile,values_from = BVI_ha) %>%
    mutate(BVIAS_ES_25_75 = (q75-q25), BVIAS_ES_5_95 = (q95-q5))
  
  
  # Table ----
  
  effect_size_table <- Reduce(rbind,
                           list(tmp_ES_A.2.1,tmp_ES_A.2.2,
                                tmp_ES_A.3.1,tmp_ES_A.3.2,tmp_ES_A.3.3,
                                tmp_ES_A.4.3,tmp_ES_A.4.5,
                                tmp_ES_A.5.1))
  
  
  return(effect_size_table)
  
}



