# BVI for crops

##### Set up ----

library(dplyr)


##### parameters ----

# ??? comment je joins les df ???

tmp_farms <- Reduce(intersect,list(BV_A.3.1$IDENT,
                                   BV_A.4.5$IDENT,
                                   BV_A.5.1$IDENT))
# 6091 farms

# transfert table
tmp_TT_crops <- read_xlsx("data_in/supp_data.xlsx",sheet = "TT_crops")

tmp_param <- Reduce(full_join,list(
  BV_A.3.1 %>% filter(IDENT %in% tmp_farms),
  BV_A.4.5 %>% filter(IDENT %in% tmp_farms),
  BV_A.5.1 %>% filter(IDENT %in% tmp_farms)
)) %>% ungroup() 

# quality check : j'ai des NAs pour les phyto autre que dans les prairies !!! à cause trous dans TT_crops ?
#sort(unique(tmp_param$CODE3[is.na(tmp_param$A.5.1)]))
#[1] 324 331 341 342 344 371 731 750 751

# WIP je remove les farm qui ont des valeurs abérantes
#sort(tmp_param$A.4.5,decreasing = T)[1] / sort(tmp_param$A.4.5,decreasing = T)[2]


##### Biodiversity Value Contribution ----

# import constant from Lindner 2019 SM 
tmp_BV_constant <-  read_excel("data_in/supp_data.xlsx", 
                               sheet = "Lindner_2019_BV_LU_function_con", 
                               col_types = c("text", "text", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric"))
# select crops
tmp_BV_contrib <- tmp_param

# loop to normalize all parameters and apply BV functions
for (tmp_i in c("A.3.1","A.4.5","A.4.5_min","A.4.5_org","A.5.1")) { #tmp_i = c("A.3.1","A.4.5","A.5.1")[1]
  
  # define the maximum of each parameter to 95% of the positive values (i.e., cut off the 5% highest positive values)
  ## ??? pour labour, valeurs dupliquées => modifie quantiles => valeurs unique => change quantiles pour autre
  #tmp_max = pull(tmp_param,tmp_i)
  #tmp_max = quantile(tmp_max[which(tmp_max>0)],0.95) 
  tmp_max = unique(pull(tmp_param,tmp_i))
  tmp_max = quantile(tmp_max,0.95,na.rm = T)
  print(paste0(tmp_i," 95th quantile: ",tmp_max))
  
  # retrieve constant from Lindner 2019
  tmp_constant <- tmp_BV_constant[tmp_BV_constant$metric_number == str_sub(tmp_i,start = 1,end = 5),]
  
  # function
  tmp_df <- tmp_param %>%
    ## select param
    select(IDENT,CODE3,all_of(tmp_i)) %>%
    setNames(c("IDENT","CODE3","x")) %>%
    mutate(
      ## set max
      x_max = 
        case_when(
          x > tmp_max ~ tmp_max,
          .default =  x ),
      ## Normalize data
      x_norm = 
        case_when(
          x > tmp_max ~ 1,
          .default =  x / tmp_max ),
      ## calculate BV
      y = case_when(
        x_norm == 0 ~ 1,
        x_norm == 1 ~ 0,
        .default = tmp_constant$gamma + tmp_constant$epsilon * exp(-(
          abs(((x_norm^tmp_constant$delta) - tmp_constant$beta) ^ tmp_constant$alpha) /
            (2*tmp_constant$sigma^tmp_constant$alpha))) ),
      ## constrain BV !!! lots of negative values obtained for A.4.5_y (when A.4.5_x_norm > 0.82)
      ## Lindner 2022: diff gamma => when x_norm = 1 => y = -2 => constrain if x_norm = 1 => y =0
      y = case_when(
        y <0 ~ 0,
        y >1 ~ 1,
        .default = y
      )
    )
  
  #plot
  hist(tmp_df$x_max,nclass = 1000)
  abline(v=tmp_max)
  plot(tmp_df$x_max,tmp_df$y,
       main=tmp_constant$Metric)
  
  
  tmp_df <- tmp_df %>%
    setNames(c("IDENT","CODE3",tmp_i,paste0(tmp_i,"_max"),paste0(tmp_i,"_norm"),paste0(tmp_i,"_y")))
  
  # retrieve loop results
  tmp_BV_contrib <- tmp_BV_contrib %>%
    left_join(.,tmp_df) 
  
}


##### Land use specific biodiversity value ----

tmp_BV_LU <- tmp_BV_contrib %>%
  rowwise() %>%
  mutate(
    # average 
    BV_LU = mean(c(A.3.1_y,A.4.5_y,A.5.1_y),na.rm=T)
      )

##### Biodiversity value normalization ----


# Normalize BV_LU
tmp_BV_norm <- tmp_BV_LU %>%
  # add land use type
  left_join(.,tmp_TT_crops %>% select(CODE3,land_use_type)) %>%
  mutate(
    # set BV_norm min and max according to Lindner et al. (2019)
    BV_norm_min = case_when(
      land_use_type == "forest" ~ 1/3,
      land_use_type == "grassland" ~ 1/3,
      land_use_type == "arable" ~ 1/6,
      land_use_type == "mining" ~ 0),
    BV_norm_max = case_when(
      land_use_type == "forest" ~ 1,
      land_use_type == "grassland" ~ 5/6,
      land_use_type == "arable" ~ 2/3,
      land_use_type == "mining" ~ 1/3),
    # Normalize BV_LU
    BV_norm = BV_norm_min+BV_LU*(BV_norm_max-BV_norm_min)
  )

# Local biodiversity value ----

tmp_BV_loc <- tmp_BV_norm %>%
  mutate(BV_loc = 1.017626088*(1-exp(-4.055847776*BV_norm)),
         BVI_ha = 1- BV_loc
         )

# quality check : see in BVI to AGB if similar values for similar crops

# yields ----

source("~/BiodivLabel/scripts/yield_crop.R")

# BVI per kg ----

BVI_to_RICA_crops <- tmp_BV_loc %>%
  # add yields
  left_join(., tmp_yield) %>% # 5952 farms
  mutate(
    BVI_kg = BVI_ha / yield
  ) %>% ungroup()

# calculate yield for hay using grassland areas => all productions = 0
# WIP calculate yield for olive oil using olive tree areas ???

##### Output ----


rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])











