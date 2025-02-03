# Practice engineering
# 22/11/2024

library(dplyr)
library(tidyr)
library(stringr)

# Typology ----

tmp_typo_thrhld <- practice_data$BVIAS %>%
  group_by(production_type,product_name) %>%
  summarise(q25_ha = quantile(BVI_ha,0.25),
            q50_ha = quantile(BVI_ha,0.50),
            q75_ha = quantile(BVI_ha,0.75),
            q25_t = quantile(BVI_t,0.25),
            q50_t = quantile(BVI_t,0.50),
            q75_t = quantile(BVI_t,0.75),
            .groups = "keep") %>%
  ungroup()

tmp_typo_grp <- practice_data$BVIAS %>%
  select(farm_id,production_type,product_name,product_FQS,BV_loc,BVI_ha,BVI_t) %>%
  left_join(.,tmp_typo_thrhld, by = join_by(production_type,product_name)) %>%
  mutate(
    typo_grp_ha = case_when(
      BVI_ha <= q25_ha ~ "<=q25_ha",
      BVI_ha <= q50_ha ~ "<=q50_ha",
      BVI_ha <= q75_ha ~ "<=q75_ha",
      BVI_ha >= q75_ha ~ ">=q75_ha"),
    typo_grp_t = case_when(
      BVI_t <= q25_t ~ "<=q25_t",
      BVI_t <= q50_t ~ "<=q50_t",
      BVI_t <= q75_t ~ "<=q75_t",
      BVI_t >= q75_t ~ ">=q75_t")
    ) %>%
  mutate(
    typo_grp = paste0(typo_grp_ha," - ",typo_grp_t)
  )

library(ggplot2)
tmp_plot <- tmp_typo_grp %>%
  filter(product_name %in% c("Ble_tendre","Lait")) %>%
  ggplot() +
  geom_count(aes(x = product_FQS, y = typo_grp)) +
  # theme
  theme(axis.text.x = element_text(angle = 90))

tmp_plot  

tmp_plot <- tmp_typo_grp %>%
  filter(product_name %in% c("Ble_tendre","Lait")
         & typo_grp == "<=q25_ha - <=q25_t") %>%
  ggplot() +
  geom_bar(aes(x = product_FQS)) +
  # theme
  labs(title = "Number of farm with BVI_ha <= q25 and BVI_t <= q25") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 0))

tmp_plot 


  # Tukey HSD on unmatched data ----
library(agricolae)


tmp_stat_data <- tibble()

for (tmp_data_in_ls in names(practice_data)) {
  #tmp_data_in_ls = names(practice_data)[4]
  
  tmp_loop1_practice_names <- practice_names[[tmp_data_in_ls]]
  tmp_loop1_practice_data <- practice_data[[tmp_data_in_ls]] %>%
    # add typo groups
    filter(product_FQS %in% unique(matched_data$product_FQS)) %>%
    left_join(.,tmp_typo_grp %>%
                select(farm_id, production_type, product_name, typo_grp),
              by = join_by(farm_id, production_type, product_name))
  
  for (tmp_var in levels(tmp_loop1_practice_names$practice)) {
    #tmp_var = levels(tmp_loop1_practice_names$practice)[1]
    
    for (tmp_production_type in unique(tmp_loop1_practice_data$production_type)) {
      #tmp_production_type = unique(tmp_loop1_practice_data$production_type)[1]
      
      ## select variable
      tmp_loop2 = tmp_loop1_practice_data %>%
        filter(production_type == tmp_production_type) %>%
        select(farm_id,product_name,FQS,product_FQS,typo_grp,tidyselect::all_of(tmp_var)) %>%
        setNames(c("farm_id","product_name","FQS","product_FQS","typo_grp","my_var"))
      
      if (nrow(tmp_loop2) >0) {
        
        # ANOVA + Tukey HSD test
        ## lm
        if (tmp_production_type == "crop") {
          tmp_loop2_lm = lm(tmp_loop2$my_var ~ tmp_loop2$product_name * tmp_loop2$typo_grp)
          
          ## anova
          tmp_loop2_aov = aov(tmp_loop2_lm)
          summary(tmp_loop2_aov)
          ## post-hoc Tukey HSD test
          tmp_loop2_comp = HSD.test(tmp_loop2_aov,trt = c('tmp_loop2$product_name','tmp_loop2$typo_grp'),alpha = 0.05, group = T)
          
          ## means
          tmp_loop2_stat_means = tibble("trt" = rownames(tmp_loop2_comp[["means"]])) %>%
            cbind(.,tmp_loop2_comp[["means"]])
          
          # stat groups
          tmp_loop2_stat_grp = tibble(
            "trt" = rownames(tmp_loop2_comp[["groups"]]),
            "typo_grp_mean" = tmp_loop2_comp[["groups"]][["tmp_loop2$my_var"]],
            "stat_grp" = unlist(tmp_loop2_comp[["groups"]][["groups"]])) %>%
            # wrangle names
            mutate(
              practice_subset = tmp_data_in_ls,
              practice = tmp_var,
              production_type = tmp_production_type,
              product_name = unlist(str_split(trt,pattern = ":"))[seq(1,nrow(tmp_loop2_comp[["groups"]])*2,2)],
              typo_grp = unlist(str_split(trt,pattern = ":"))[seq(2,nrow(tmp_loop2_comp[["groups"]])*2,2)],
              max_value = max(tmp_loop2$my_var)
            ) %>%
            mutate(
              product_typo_grp = paste0(product_name," - ",typo_grp)
            )
          
          ## p-values
          tmp_loop2_comp_pval = HSD.test(tmp_loop2_aov,trt = c('tmp_loop2$product_name','tmp_loop2$typo_grp'),alpha = 0.05, group = F)
          
          tmp_loop2_stat_pval = tibble(
            "comp" = rownames(tmp_loop2_comp_pval[["comparison"]]),
            "pval" = tmp_loop2_comp_pval[["comparison"]][["pvalue"]]) %>%
            ## split comparison names
            mutate(
              trt1 = unlist(str_split(comp,pattern = " - "))[seq(1,nrow(tmp_loop2_comp_pval[["comparison"]])*2,2)],
              trt2 = unlist(str_split(comp,pattern = " - "))[seq(2,nrow(tmp_loop2_comp_pval[["comparison"]])*2,2)]) %>%
            ## split treatment names
            mutate(
              product_name1 = unlist(str_split(trt1,pattern = ":"))[seq(1,nrow(tmp_loop2_comp_pval[["comparison"]])*2,2)],
              typo_grp1 = unlist(str_split(trt1,pattern = ":"))[seq(2,nrow(tmp_loop2_comp_pval[["comparison"]])*2,2)],
              product_name2 = unlist(str_split(trt2,pattern = ":"))[seq(1,nrow(tmp_loop2_comp_pval[["comparison"]])*2,2)],
              typo_grp2 = unlist(str_split(trt2,pattern = ":"))[seq(2,nrow(tmp_loop2_comp_pval[["comparison"]])*2,2)],
            ) %>%
            rename(product_name = product_name1)  %>%
            pivot_wider(id_cols = product_name,names_from = comp,values_from = pval)
          
          
        } else {
          #tmp_data_in_ls = names(practice_data)[4]
          #tmp_var = levels(tmp_loop1_practice_names$practice)[1]
          #tmp_production_type = unique(tmp_loop1_practice_data$production_type)[2]
          
          tmp_loop2_lm = lm(tmp_loop2$my_var ~ tmp_loop2$typo_grp)
          
          ## anova
          tmp_loop2_aov = aov(tmp_loop2_lm)
          summary(tmp_loop2_aov)
          ## post-hoc Tukey HSD test
          tmp_loop2_comp = HSD.test(tmp_loop2_aov,trt = c('tmp_loop2$typo_grp'),alpha = 0.05, group = T)
          
          ## means
          tmp_loop2_stat_means =  tibble("trt" = rownames(tmp_loop2_comp[["means"]])) %>%
            cbind(.,tmp_loop2_comp[["means"]])
          
          ## stat groups 
          tmp_loop2_stat_grp = tibble(
            "trt" = rownames(tmp_loop2_comp[["groups"]]),
            "typo_grp_mean" = tmp_loop2_comp[["groups"]][["tmp_loop2$my_var"]],
            "stat_grp" = unlist(tmp_loop2_comp[["groups"]][["groups"]])) %>%
            # wrangle names
            mutate(
              practice_subset = tmp_data_in_ls,
              practice = tmp_var,
              production_type = tmp_production_type,
              product_name = "Lait",
              typo_grp = trt,
              max_value = max(tmp_loop2$my_var,na.rm = T)
            ) %>%
            mutate(
              product_typo_grp = paste0(product_name," - ",typo_grp)
            ) 
          
          ## p-values
          tmp_loop2_comp_pval = HSD.test(tmp_loop2_aov,trt = c('tmp_loop2$typo_grp'),alpha = 0.05, group = F)
          
          tmp_loop2_stat_pval = tibble(
            "comp" = rownames(tmp_loop2_comp_pval[["comparison"]]),
            "pval" = tmp_loop2_comp_pval[["comparison"]][["pvalue"]]) %>%
            mutate(product_name = "Lait") %>%
            pivot_wider(id_cols = product_name,names_from = comp,values_from = pval)
          
        }
        
        # extract tibble
        tmp_stat_data <- tmp_stat_data %>%
          bind_rows(.,tmp_loop2_stat_means %>%
                      left_join(.,tmp_loop2_stat_grp,by = join_by(trt)) %>%
                      left_join(.,tmp_loop2_stat_pval,by = join_by(product_name)))
        
        
        tmp_loop2_print = paste0(tmp_data_in_ls," -> ",tmp_production_type," -> ",tmp_var)
        print(tmp_loop2_print)
        
        rm(list = names(.GlobalEnv)[grep("tmp_loop2",names(.GlobalEnv))])
        
      }    
    }
  }
  rm(list = names(.GlobalEnv)[grep("tmp_loop1",names(.GlobalEnv))])
  
}


# extract table ----
stat_tukey_hsd_typo = tmp_stat_data %>%
  rename(trt_mean = `tmp_loop2$my_var`)
write.csv(stat_tukey_hsd_typo,paste0("~/BiodivLabel/data_out/tmp_TukeyHSD_typo_",Sys.Date(),".csv"),quote = F,row.names = F)

# PLOT ----

tmp_plot_data <- stat_tukey_hsd_typo %>%
  filter(production_type == "milk")

ggplot() +
  # mean
  geom_point(data = tmp_plot_data,
             aes(x = trt, y = trt_mean, fill = typo_grp,colour = typo_grp),
             shape = 21, size=2, stroke = 1, position = position_dodge(width = 0.75)) +
  # CI95%
  geom_errorbar(data = tmp_plot_data,
                aes(x = trt, ymin = trt_mean-1.96*(std/sqrt(r)), ymax = trt_mean+1.96*(std/sqrt(r)), colour = typo_grp),
                linewidth=1,width = 0.5,position = position_dodge(width = 0.75)) +
  # sd
  geom_errorbar(data = tmp_plot_data,
                aes(x = trt, ymin = trt_mean-std, ymax = trt_mean+std, colour = typo_grp),
                linewidth=0.5,width = 0.5,linetype = "dashed",position = position_dodge(width = 0.75)) +
  # n
  geom_text(data = tmp_plot_data,
            aes(x = trt, y=(trt_mean+std)*1.1,colour = typo_grp,label = paste0("n = ",as.character(r))),
            size=5,position = position_dodge(width = 1)) +
  # stat groups
  geom_text(data = tmp_plot_data,
            aes(x = trt,y=0,colour = typo_grp,label = stat_grp),
            size=5,position = position_dodge(width = 1))+
  # theme
  coord_flip() +
  #ylim(c(0,max(tmp_plot_data$trt_mean+tmp_plot_data$std)*1.2)) +
  #facet_wrap(vars(production_type),scales = "free_y",ncol = 1) +
  facet_wrap(vars(practice), scales = "free") +
  labs(y = "Practice",x= "Produits") +
  #scale_fill_manual(values = tmp_typo_grp_names$colors,labels = tmp_typo_grp_names$labels,) +
  #scale_colour_manual(values = tmp_typo_grp_names$colors,labels = tmp_typo_grp_names$labels) +
  theme_light() +
  theme(legend.position = c(0,-1),
        legend.direction = "horizontal",legend.title = element_blank())

# Paired t-test on matched data ----



# Sur chaque produit pour chaque FQS ----

tmp_pttest <- tibble()

for (tmp_data_in_ls in names(practice_data)) {
  #tmp_data_in_ls = names(practice_data)[4]
  
  tmp_loop1_practice_names <- practice_names[[tmp_data_in_ls]]
  tmp_loop1_practice_data <- practice_data[[tmp_data_in_ls]] %>%
    filter(product_FQS %in% unique(matched_data$product_FQS))
  
  
  for (tmp_var in levels(tmp_loop1_practice_names$practice)) {
    #tmp_var = levels(tmp_loop1_practice_names$practice)[1]
    
    for (tmp_match in sort(unique(matched_data$match[matched_data$production_type %in%  unique(tmp_loop1_practice_data$production_type)]))) {
      #tmp_match = sort(unique(matched_data$match[matched_data$production_type %in%  unique(tmp_loop1_practice_data$production_type)]))[2]
      
      # subset data
      tmp_loop2_data = matched_data %>% 
        filter(match == tmp_match) %>%
        select(farm_id,production_type,product_name,FQS,match,subclass) %>%
        # add practice data
        left_join(.,tmp_loop1_practice_data,
                  by = join_by(farm_id, production_type, product_name, FQS)) %>%
        # rename
        rename(my_var = tidyselect::all_of(tmp_var))
      
      # paired t.test
      ## differences between paired observations
      tmp_loop2_dif <- tmp_loop2_data %>% 
        group_by(FQS,subclass) %>% 
        summarise(my_var = mean(my_var,na.rm = T),
                  .groups = "keep") %>% 
        ungroup() %>%
        mutate(ttt = case_when(
          FQS != "Conventionnel" ~ "treated",
          FQS == "Conventionnel" ~ "control"
        )) %>%
        pivot_wider(id_cols = subclass,names_from = ttt,values_from = my_var) %>%
        mutate(diff = treated - control)
      # paired t-test
      tmp_loop2_t.test <- t.test(tmp_loop2_dif$treated,tmp_loop2_dif$control,paired = T)
      
      print(tmp_var)
      print(tmp_match)
      print(tmp_loop2_t.test)
      
      # tibble
      tmp_loop2_table <- tmp_loop2_data %>%
        group_by(production_type,product_name,FQS,match) %>%
        summarise(mean = mean(my_var,na.rm = T),
                  sd = sd(my_var,na.rm = T),
                  min = min(my_var,na.rm = T),
                  max = max(my_var,na.rm = T),
                  q25 = quantile(my_var,0.25,na.rm = T),
                  q50 = quantile(my_var,0.50,na.rm = T),
                  q75 = quantile(my_var,0.75,na.rm = T),
                  .groups = "keep") %>%
        ungroup() %>%
        mutate(practice = tmp_var) %>%
        mutate(
          n_matched = case_when(
            FQS == "Conventionnel" ~ length(unique(tmp_loop2_data$farm_id[tmp_loop2_data$FQS == "Conventionnel"])),
            FQS != "Conventionnel" ~ length(unique(tmp_loop2_data$farm_id[tmp_loop2_data$FQS != "Conventionnel"]))),
          
          mean_diff_match = tmp_loop2_t.test[["estimate"]],
          pval_pttest = tmp_loop2_t.test[["p.value"]])
      
      # extract tibble & list
      tmp_pttest <- tmp_pttest %>%
        rbind(.,tmp_loop2_table)
      
      rm(list = names(.GlobalEnv)[grep("tmp_loop2",names(.GlobalEnv))])
      
    }}
  
  rm(list = names(.GlobalEnv)[grep("tmp_loop1",names(.GlobalEnv))])
  
}

# Bonferroni correction on p-values
tmp_pttest[,"padjust_bonf"] <- p.adjust(tmp_pttest$pval_pttest,"bonferroni")

# extract table ----
stat_paired_t_test_typo = tmp_pttest
write.csv(stat_paired_t_test_typo,paste0("~/BiodivLabel/data_out/tmp_paired_ttest_typo_",Sys.Date(),".csv"),quote = F,row.names = F)


