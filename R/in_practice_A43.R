
# A.4.3 Share of artificial fertilizer

BV_A.4.3 <- BV_A.4.5 %>%
  mutate(
    A.4.3 = A.4.5_min / A.4.5
  ) %>%
  select(farm_id,crop,A.4.3)
