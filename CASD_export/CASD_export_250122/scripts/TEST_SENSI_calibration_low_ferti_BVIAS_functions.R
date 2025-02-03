# BVIAS for crops

# Set up ----

library(dplyr)
library(tidyr)
library(readxl)

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
  #var_constant = TEST_SENSI_calibration_low_ferti_optim_final$constants$BV_constant_optim
  #var_weight = TEST_SENSI_calibration_low_ferti_optim_final$weights$weight_optim
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
  tmp_ES_A.4.5 <- tibble(archetype = "ES_A.4.5",land_use_type = LU,litt_values = 0.35/3,BV_loc = tmp_ES_A.4.5)
  
  
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


