# Basics
# Sarah Huet
# 26/04/2024

# Paired t.test

# paired t.test

# BVI hectare
## differences between paired observations
tmp_dif <- loop2_data %>% 
  group_by(label,subclass) %>% 
  summarise(value = mean(value,na.rm = T)) %>% 
  mutate(ttt = case_when(
    label != "Conventionnel" ~ "treated",
    label == "Conventionnel" ~ "control"
  )) %>%
  pivot_wider(id_cols = subclass,names_from = ttt,values_from = value) %>%
  mutate(diff = treated - control)
## mean and sd of the differences
tmp_mean_diff = mean(tmp_dif$diff)
tmp_sd_diff = sd(tmp_dif$diff)
## se of the mean difference
tmp_se_diff = tmp_sd_diff / sqrt(length(tmp_dif$diff))
## t statistic
tmp_t_stat = tmp_mean_diff / tmp_se_diff
## degree of freedom
tmp_df = length(tmp_dif$diff) -1
## p-value (two-tailed test)
tmp_pvalue = 2 * pt(abs(tmp_t_stat),tmp_df,lower.tail = F)
tmp_pvalue <= 0.05

t.test(tmp_dif$treated,tmp_dif$control,paired = T)
