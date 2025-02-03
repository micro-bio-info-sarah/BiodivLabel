# PCA using FactorMinR to explore heterogeneity of data

library(dplyr)
library(tidyr)
library(ggplot2)

library(FactoMineR)
library(factoextra)

# Data ----

## active variables ----

# here we choose as active variables the BVIAS input variables, i.e., the practice intensity variables

tmp_active_var <- BVIAS_input



## supplementary variables ----

# then, we will add CDEX, PBS, OTEX, FQS as supplementary variables

# RA data 
RA2020 <- readRDS("~/BiodivLabel/data_CASD/RA2020_EXPLOITATIONS_240112.rds")
RA2020_MO_CHEF_COEXPL_240112 <- readRDS("~/BiodivLabel/data_CASD/RA2020_MO_CHEF_COEXPL_240112.rds")

tmp_supp_var <- BVIAS_to_RICA_RA_SIQO %>%
  mutate(NOM_DOSSIER = as.character(NOM_DOSSIER)) %>%
  # add RA data
  left_join(.,
            RA2020 %>%
              select(NOM_DOSSIER,SIEGE_REG, REGL_1305_2013,PBSTOT_COEF17,SAU_TOT,UGBAG.TOT,
                     CDEX_COEF2017,OTEFDD_COEF17),
            by = join_by(NOM_DOSSIER)) %>%
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

rm(RA2020,RA2020_MO_CHEF_COEXPL_240112)

# join data ----

## as the PCA function needs a df with rownames, I have to choose one crop / grassland to avoid duplicated rownames
## or I could use a paste of farm_id and crop as rownames

tmp_PCA_data <- tmp_supp_var %>%
  left_join(.,
             tmp_active_var,
             by = join_by("farm_id","land_use_type","crop","product_name", "org_farming" )) %>%
  filter(crop == "111")

tmp_PCA_df <- data.frame(
  tmp_PCA_data %>%
    select(
      # active var
      A.2.1,A.2.2,A.3.1,A.3.2,A.3.3,A.4.3,A.4.5,A.5.1,
      # supp var quanti
      BVIAS_ha,BVIAS_kg,PBSTOT_COEF17,SAU_TOT,UGBAG.TOT,AGE,
      # supp var quali
      FQS,SIEGE_REG,CDEPT,MNT,NivForm,CDEX_COEF2017,OTEFDD_COEF17
      ) %>%
    # factor quali supp var
    mutate(
      FQS = factor(FQS),
      SIEGE_REG = factor(SIEGE_REG),
      CDEPT = factor(CDEPT),
      MNT = factor(MNT),
      NivForm = factor(NivForm)
    ),
  row.names = tmp_PCA_data$farm_id)

# PCA ----

tmp_PCA <- PCA(tmp_PCA_df,
               quanti.sup = which(names(tmp_PCA_df) %in% c("BVIAS_ha","BVIAS_kg","PBSTOT_COEF17","SAU_TOT","UGBAG.TOT","AGE")),
               quali.sup = which(names(tmp_PCA_df) %in% c("FQS","SIEGE_REG","CDEPT","MNT","NivForm","CDEX_COEF2017","OTEFDD_COEF17")))
summary(tmp_PCA, nbelements = Inf)

# description of the dimensions
dimdesc(tmp_PCA)

# plot individuals by quali var
fviz_pca_ind(tmp_PCA, habillage = "FQS",geom.ind = "point",
             addEllipses = T, ellipse.level = 0.68) +
  scale_color_manual(values = c("palegreen3","darkgrey","indianred")) +
  scale_fill_manual(values = c("palegreen3","darkgrey","indianred"))

fviz_pca_ind(tmp_PCA, habillage = "CDEX_COEF2017",geom.ind = "point",
             addEllipses = T, ellipse.level = 0.68) 


# Correlation ----

library(PerformanceAnalytics)

chart.Correlation(tmp_PCA_df[,1:14], histogram = T, pch = 19)


# box plot ----

tmp_boxplot_df <- tmp_PCA_df %>%
  pivot_longer(cols = c(A.2.1,A.2.2,A.3.1,A.3.2,A.3.3,A.4.3,A.4.5,A.5.1),
               names_to = "practice",values_to = "value")

ggplot(tmp_boxplot_df) +
  aes(x = CDEX_COEF2017, y = value) +
  geom_boxplot(fill = "#112446") +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(practice), scales = "free")






























