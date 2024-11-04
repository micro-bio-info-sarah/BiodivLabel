# matching

tmp_output_dir = c("~/BiodivLabel/data_out/")

library(MatchIt)
library(cobalt)

# data ----

tmp_input <- BVIAS_to_RICA_RA_SIQO

# how many farms by FQS
tmp_nb_farm <- tmp_input %>%
  filter(org_farming == T | !is.na(SIQO)) %>% 
  group_by(product_name,product_FQS) %>% 
  summarise(nobs = length(unique(farm_id)),
            .groups = "keep")
# select FQS with >= 30 farms
tmp_product_FQS<- sort(c(tmp_nb_farm$product_FQS[tmp_nb_farm$nobs >= 30],
             paste0(unique(tmp_nb_farm$product_name[tmp_nb_farm$nobs >= 30]),
                    " - Conventionnel")))

tmp_ProdSIQO <- tmp_input %>%
  # filter FQS with >=30 farms
  filter(product_FQS %in% tmp_product_FQS) %>%
  # filter milk and cereals
  filter(production_type =="milk"|species %in% c("cereal","oilseeds"))

# Matching ----

## propensity_score data ----

# RA data 
RA2020 <- readRDS("~/BiodivLabel/data_CASD/RA2020_EXPLOITATIONS_240112.rds")
RA2020_MO_CHEF_COEXPL_240112 <- readRDS("~/BiodivLabel/data_CASD/RA2020_MO_CHEF_COEXPL_240112.rds")
# several rows per farm, for manager RA2020_MO_CHEF_COEXPL_240112$STATUTDIRIG == 1

tmp_VariablesRA <- RA2020 %>%
  select(NOM_DOSSIER,SIEGE_REG, REGL_1305_2013,PBSTOT_COEF17,SAU_TOT,UGBAG.TOT) %>%
  # create montain variable
  mutate(
    MNT=case_when(
      REGL_1305_2013=="MNT_ANC" ~ T,
      .default = F)
    #REGL_1305_2013: ICHN. Modalités:MNT_ANC(zone Montagne),LNT_ANC(pas montagne mais contraintes naturelles), OTH_ANC: contraintes specifiques, NNT_ANC pas de contraintes spécifiques
  ) %>%
  #Ajustement
  replace_na(list(SAU_TOT = 0,UGBA.TOT = 0)) %>%
  # add age and educational level of the manager
  left_join(.,
            RA2020_MO_CHEF_COEXPL_240112 %>%
              # keep only manager
              filter(STATUTDIRIG ==1) %>%
              #age
              mutate(AGE = 2020- ANAIS) %>%
              # educational level
              mutate(NivForm = case_when(
                as.numeric(MOFGENE) > as.numeric(MOFAGRI) & !is.na(MOFGENE) ~ MOFGENE,
                .default = MOFAGRI
              )) %>%
              select(NOM_DOSSIER,AGE,NivForm),
            by = join_by(NOM_DOSSIER))

rm(list = c("RA2020","RA2020_MO_CHEF_COEXPL_240112"))

tmp_basePscore<-merge(tmp_ProdSIQO, tmp_VariablesRA,
                      by="NOM_DOSSIER") %>% # Tout a matché, ok
  # je créé variable FQS binaire
  mutate(
    label_bool = case_when(
      FQS !="Conventionnel" ~ T,
      .default= F)
  )

## propensity_score matching ----

tmp_matched_data_all <- tibble()
tmp_matched_obj_all <- list()
library(officer)
print(read_docx(),target = paste0(tmp_output_dir,"matching_summary_",Sys.Date(),".docx"))


for (tmp_product in sort(unique(tmp_nb_farm$product_name[tmp_nb_farm$nobs >=30]))) {
  #tmp_product = sort(unique(tmp_nb_farm$product_name[tmp_nb_farm$nobs >=30]))[1]
  
  # subset data
  tmp_loop1_data = tmp_basePscore %>% 
    filter(product_name == tmp_product)
  
  for (tmp_code in unique(tmp_loop1_data$FQS[tmp_loop1_data$FQS != "Conventionnel"])){
    #tmp_code = unique(tmp_loop1_data$FQS[tmp_loop1_data$FQS != "Conventionnel"])[1]
    
    # subset data
    tmp_loop2_data = tmp_loop1_data %>% 
      filter(FQS %in% c(tmp_code,"Conventionnel"))
    
    # propensity score & match
    tmp_loop2_match_obj<-matchit(label_bool ~ SIEGE_REG + MNT + PBSTOT_COEF17 + AGE + NivForm,
                                 data=tmp_loop2_data, method='nearest', exact=c('SIEGE_REG'),
                                 ratio=3,
                                 replace=F)
    tmp_loop2_model_summary = summary(tmp_loop2_match_obj[["model"]])
    tmp_loop2_model_coef = row.names(tmp_loop2_model_summary[["coefficients"]]) %>%
      cbind(tmp_loop2_model_summary[["coefficients"]])
    
    tmp_loop2_matched_data <-match.data(tmp_loop2_match_obj)
    
    # extract info
    tmp_loop2_docx = read_docx(paste0(tmp_output_dir,"matching_summary_",Sys.Date(),".docx")) |> 
      body_add_par(paste0(tmp_product," - ",tmp_code),style = "heading 1") |>
      # covariate summary before and after matching
      body_add_par("Covariate distribution before matching",style = "heading 2") |>
      body_add_table(as.data.frame(
        datasummary_balance(data = tmp_loop2_data %>% 
                              select(product_FQS,SIEGE_REG,MNT,PBSTOT_COEF17,AGE,NivForm),
                            ~product_FQS,output = "data.frame"))) |>
      body_add_par("Covariate distribution after matching",style = "heading 2") |>
      body_add_table(as.data.frame(
        datasummary_balance(data = tmp_loop2_data %>% 
                              filter(farm_id %in% tmp_loop2_matched_data$farm_id) %>%
                              select(product_FQS,SIEGE_REG,MNT,PBSTOT_COEF17,AGE,NivForm),
                            ~product_FQS,output = "data.frame"))) |>
      # matching model summary
      body_add_par("Matching model coefficients",style = "heading 2") |>
      body_add_table(as.data.frame(tmp_loop2_model_coef)) |>
      # propensity score distribution
      body_add_par("Propensity score distribution",style = "heading 2") |>
      body_add_plot(plot(tmp_loop2_match_obj, type="jitter", interactive=FALSE)) |>
      # covariate balance
      body_add_par("Covariate balance",style = "heading 2") |>
      body_add_plot(plot(love.plot(tmp_loop2_match_obj, binary="std")))
    
    print(tmp_loop2_docx,target = paste0(tmp_output_dir,"matching_summary_",Sys.Date(),".docx"))
    
    # extract tibble & list
    tmp_matched_obj_all[[tmp_product]][[tmp_code]] <- tmp_loop2_match_obj
    tmp_matched_data_all <- tmp_matched_data_all %>%
      rbind(.,tmp_loop2_matched_data %>% mutate(match = paste0(product_name,"__",tmp_code)))
    
    
    rm(list = names(.GlobalEnv)[grep("tmp_loop2",names(.GlobalEnv))])
    
  }
  
  rm(list = names(.GlobalEnv)[grep("tmp_loop1",names(.GlobalEnv))])
}

tmp_nobs = read_docx(paste0(tmp_output_dir,"matching_summary_",Sys.Date(),".docx")) |> 
  body_add_par("Number of matches",style = "heading 1") |>
  body_add_table(as.data.frame(
    tmp_matched_data_all %>% 
      group_by(match,FQS) %>% 
      summarise(nobs = length(unique(farm_id)),
                .groups = "keep")
  ))

print(tmp_nobs,target = paste0(tmp_output_dir,"matching_summary_",Sys.Date(),".docx"))

# Output ----

matched_data = tmp_matched_data_all
matched_obj = tmp_matched_obj_all

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])
