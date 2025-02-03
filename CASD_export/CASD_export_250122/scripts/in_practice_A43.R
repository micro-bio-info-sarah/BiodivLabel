
# A.4.3 Share of artificial fertilizer

BV_A.4.3 <- BV_A.4.5 %>%
  mutate(
    A.4.3 = case_when(
      A.4.5_min == 0 ~ 0,
      .default = A.4.5_min / A.4.5
    )
  ) %>%
  select(farm_id,land_use_type,crop,A.4.3)
