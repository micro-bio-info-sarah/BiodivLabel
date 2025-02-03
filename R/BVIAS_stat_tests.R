# stat tests

library(dplyr)
library(tidyr)
library(tibble)

library(agricolae)

library(stringr)
# Tukey HSD on unmatched data ----


tmp_stat_data <- tibble()

for (tmp_data_in_ls in names(practice_data)) {
#tmp_data_in_ls = names(practice_data)[3]
  
  tmp_loop1_practice_names <- practice_names[[tmp_data_in_ls]]
  tmp_loop1_practice_data <- practice_data[[tmp_data_in_ls]] %>%
    filter(product_FQS %in% unique(matched_data$product_FQS))
  
  for (tmp_var in levels(tmp_loop1_practice_names$practice)) {
#tmp_var = levels(tmp_loop1_practice_names$practice)[1]
    
    for (tmp_production_type in unique(tmp_loop1_practice_data$production_type)) {
#tmp_production_type = unique(tmp_loop1_practice_data$production_type)[1]
      
      ## select variable
      tmp_loop2 = tmp_loop1_practice_data %>%
        filter(production_type == tmp_production_type) %>%
        select(farm_id,product_name,FQS,product_FQS,tidyselect::all_of(tmp_var)) %>%
        setNames(c("farm_id","product_name","FQS","product_FQS","my_var")) %>%
        filter(!is.na(my_var))
      
      # test 
      #tmp_loop2 <- tmp_loop2 %>% filter(product_name == "Ble_tendre" & FQS %in% c("AB","Conventionnel"))
      #tmp_loop2 <- tmp_loop2 %>% filter(FQS %in% c("AB","Conventionnel"))
      
      if (nrow(tmp_loop2) >0) {
        
        # ANOVA + Tukey HSD test
        ## lm
        if (tmp_production_type == "crop") {
          tmp_loop2_lm = lm(tmp_loop2$my_var ~ tmp_loop2$product_name * tmp_loop2$FQS)

          ## anova
          tmp_loop2_aov = aov(tmp_loop2_lm)
          summary(tmp_loop2_aov)
          ## post-hoc Tukey HSD test
          tmp_loop2_comp = HSD.test(tmp_loop2_aov,trt = c('tmp_loop2$product_name','tmp_loop2$FQS'),alpha = 0.05, group = T)
          
          ## means
          tmp_loop2_stat_means = tibble("trt" = rownames(tmp_loop2_comp[["means"]])) %>%
            cbind(.,tmp_loop2_comp[["means"]])
          
          # stat groups
          tmp_loop2_stat_grp = tibble(
            "trt" = rownames(tmp_loop2_comp[["groups"]]),
            "FQS_mean" = tmp_loop2_comp[["groups"]][["tmp_loop2$my_var"]],
            "stat_grp" = unlist(tmp_loop2_comp[["groups"]][["groups"]])) %>%
            # wrangle names
            mutate(
              practice_subset = tmp_data_in_ls,
              practice = tmp_var,
              production_type = tmp_production_type,
              product_name = unlist(str_split(trt,pattern = ":"))[seq(1,nrow(tmp_loop2_comp[["groups"]])*2,2)],
              FQS = unlist(str_split(trt,pattern = ":"))[seq(2,nrow(tmp_loop2_comp[["groups"]])*2,2)],
              max_value = max(tmp_loop2$my_var)
            ) %>%
            mutate(
              product_FQS = paste0(product_name," - ",FQS)
            )
          
          # add conventionnal stat group
          tmp_loop2_stat_grp <- tmp_loop2_stat_grp %>%
            left_join(.,tmp_loop2_stat_grp %>%
                        filter(FQS == "Conventionnel") %>%
                        rename(stat_grp_conv = stat_grp,
                               conv_mean = FQS_mean) %>%
                        select(product_name,stat_grp_conv,conv_mean),
                      by = join_by(product_name))
          
          ## p-values
          tmp_loop2_comp_pval = HSD.test(tmp_loop2_aov,trt = c('tmp_loop2$product_name','tmp_loop2$FQS'),alpha = 0.05, group = F)
          
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
              FQS1 = unlist(str_split(trt1,pattern = ":"))[seq(2,nrow(tmp_loop2_comp_pval[["comparison"]])*2,2)],
              product_name2 = unlist(str_split(trt2,pattern = ":"))[seq(1,nrow(tmp_loop2_comp_pval[["comparison"]])*2,2)],
              FQS2 = unlist(str_split(trt2,pattern = ":"))[seq(2,nrow(tmp_loop2_comp_pval[["comparison"]])*2,2)],
            ) %>%
            ## keep only comparisons between same products
            filter(product_name1 == product_name2) %>%
            ## keep only comparisons between FQS and conventional
            filter(FQS1 == "Conventionnel" | FQS2 == "Conventionnel") %>%
            ## retrieve product name and FQS
            mutate(
              product_name = product_name1,
              FQS = case_when(
                FQS1 == "Conventionnel" ~ FQS2,
                .default = FQS1
              )     ) %>%
            # select variables
            select(product_name,FQS,pval)
          
          
          
          
        } else {
#tmp_data_in_ls = names(practice_data)[4]
#tmp_var = levels(tmp_loop1_practice_names$practice)[1]
#tmp_production_type = unique(tmp_loop1_practice_data$production_type)[2]
              
          tmp_loop2_lm = lm(tmp_loop2$my_var ~ tmp_loop2$FQS)
          
          ## anova
          tmp_loop2_aov = aov(tmp_loop2_lm)
          summary(tmp_loop2_aov)
          ## post-hoc Tukey HSD test
          tmp_loop2_comp = HSD.test(tmp_loop2_aov,trt = c('tmp_loop2$FQS'),alpha = 0.05, group = T)
          
          ## means
          tmp_loop2_stat_means =  tibble("trt" = rownames(tmp_loop2_comp[["means"]])) %>%
            cbind(.,tmp_loop2_comp[["means"]])
          
          ## stat groups 
          tmp_loop2_stat_grp = tibble(
            "trt" = rownames(tmp_loop2_comp[["groups"]]),
            "FQS_mean" = tmp_loop2_comp[["groups"]][["tmp_loop2$my_var"]],
            "stat_grp" = unlist(tmp_loop2_comp[["groups"]][["groups"]])) %>%
            # wrangle names
            mutate(
              practice_subset = tmp_data_in_ls,
              practice = tmp_var,
              production_type = tmp_production_type,
              product_name = "Lait",
              FQS = trt,
              max_value = max(tmp_loop2$my_var,na.rm = T)
            ) %>%
            mutate(
              product_FQS = paste0(product_name," - ",FQS)
            ) %>%
            # add conventionnal stat group
            mutate(stat_grp_conv = stat_grp[FQS == "Conventionnel"],
                   conv_mean = FQS_mean[FQS == "Conventionnel"])
          
          ## p-values
          tmp_loop2_comp_pval = HSD.test(tmp_loop2_aov,trt = c('tmp_loop2$FQS'),alpha = 0.05, group = F)
          
          tmp_loop2_stat_pval = tibble(
            "comp" = rownames(tmp_loop2_comp_pval[["comparison"]]),
            "pval" = tmp_loop2_comp_pval[["comparison"]][["pvalue"]]) %>%
            ## keep only comparisons between FQS and conventional
            filter(grepl(pattern = "Conventionnel",comp)) %>%
            ## retrieve product name and FQS
            mutate(
              product_name = "Lait",
              FQS = gsub(pattern = "Conventionnel - | - Conventionnel","",comp)) %>%
            # select variables
            select(product_name,FQS,pval)
          
        }
        
        # compare with conventionnal
        tmp_loop2_stat_grp <- tmp_loop2_stat_grp %>%
          rowwise() %>%
          mutate(
            # is group letter different than conventionnal?
            stat_grp_conv_diff = length(intersect(unlist(strsplit(stat_grp,"")),
                                                  unlist(strsplit(stat_grp_conv,"")))) == 0) %>%
          mutate(mean_conv_diff = FQS_mean - conv_mean)
        
        
        # extract tibble
        tmp_stat_data <- tmp_stat_data %>%
          bind_rows(.,tmp_loop2_stat_means %>%
                      left_join(.,tmp_loop2_stat_grp,by = join_by(trt)) %>%
                      left_join(.,tmp_loop2_stat_pval,by = join_by(product_name, FQS)))
          
        
        tmp_loop2_print = paste0(tmp_data_in_ls," -> ",tmp_production_type," -> ",tmp_var)
        print(tmp_loop2_print)
        
        rm(list = names(.GlobalEnv)[grep("tmp_loop2",names(.GlobalEnv))])
        
      }    
    }
  }
  rm(list = names(.GlobalEnv)[grep("tmp_loop1",names(.GlobalEnv))])
  
}


# extract table ----
stat_tukey_hsd = tmp_stat_data %>%
  rename(trt_mean = `tmp_loop2$my_var`,pval_tukey = pval)
write.csv(stat_tukey_hsd,paste0("~/BiodivLabel/data_out/tmp_TukeyHSD_",Sys.Date(),".csv"),quote = F,row.names = F)



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
        rename(my_var = tidyselect::all_of(tmp_var)) %>%
        filter(!is.na(my_var))
      
      if (nrow(tmp_loop2_data) >0) {
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
      }
      
      rm(list = names(.GlobalEnv)[grep("tmp_loop2",names(.GlobalEnv))])
      
    }}
  
  rm(list = names(.GlobalEnv)[grep("tmp_loop1",names(.GlobalEnv))])
  
}

# Bonferroni correction on p-values
tmp_pttest[,"padjust_bonf"] <- p.adjust(tmp_pttest$pval_pttest,"bonferroni")

# extract table ----
stat_paired_t_test = tmp_pttest
write.csv(stat_paired_t_test,paste0("~/BiodivLabel/data_out/tmp_paired_ttest_",Sys.Date(),".csv"),quote = F,row.names = F)


