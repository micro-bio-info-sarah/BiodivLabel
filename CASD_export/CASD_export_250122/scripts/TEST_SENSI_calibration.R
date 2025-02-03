# Test sensi calibration

source("R/BVIAS_out_crops.R")

save(list = "optim_final",file = paste0("data_out/optim_final_",Sys.Date(),".RData"))

# Low ferti

source("R/TEST_SENSI_calibration_low_ferti_BVIAS_out_crops.R")

save(list = "TEST_SENSI_calibration_low_ferti_optim_final",file = paste0("data_out/TEST_SENSI_calibration_low_ferti_optim_final_",Sys.Date(),".RData"))

# Without Tuck

source("R/TEST_SENSI_calibration_w0_Tuck_BVIAS_out_crops.R")

save(list = "optim_final",file = paste0("data_out/TEST_SENSI_calibration_w0_Tuck_optim_final_",Sys.Date(),".RData"))

# Comparisons ----

BVIAS_to_RICA_crops %>%
  group_by(crop,org_farming) %>%
  summarise(
    BVI_ha_mean = mean(BVI_ha,na.rm = T),
    BVI_ha_sd = sd(BVI_ha,na.rm = T),
    BVI_t_mean = mean(BVI_t,na.rm = T),
    BVI_t_sd = sd(BVI_t,na.rm = T),
    .groups = "keep"
  )

TEST_SENSI_calibration_low_ferti_BVIAS_to_RICA_crops %>%
  group_by(crop,org_farming) %>%
  summarise(
    BVI_ha_mean = mean(BVI_ha,na.rm = T),
    BVI_ha_sd = sd(BVI_ha,na.rm = T),
    BVI_t_mean = mean(BVI_t,na.rm = T),
    BVI_t_sd = sd(BVI_t,na.rm = T),
    .groups = "keep"
  )

TEST_SENSI_calibration_w0_Tuck_BVIAS_to_RICA_crops %>%
  group_by(crop,org_farming) %>%
  summarise(
    BVI_ha_mean = mean(BVI_ha,na.rm = T),
    BVI_ha_sd = sd(BVI_ha,na.rm = T),
    BVI_t_mean = mean(BVI_t,na.rm = T),
    BVI_t_sd = sd(BVI_t,na.rm = T),
    .groups = "keep"
  )
