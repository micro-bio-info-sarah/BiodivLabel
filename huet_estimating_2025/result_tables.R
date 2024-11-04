# Result tables


# Data ----

#{r import_data} ----

library(readr)

stat_desc = readr::read_csv("~/BiodivLabel/CASD_export/CASD_export_241017/stat_desc_BVIAS_SIQO2024-10-17.csv")

stat_tukey_hsd = readr::read_csv("~/BiodivLabel/CASD_export/CASD_export_241017/tmp_TukeyHSD_2024-10-17.csv")

stat_paired_t_test = readr::read_csv("~/BiodivLabel/CASD_export/CASD_export_241017/tmp_paired_ttest_2024-10-17.csv")

#


#{r product_names, include=FALSE, echo=FALSE} ----
library(tibble)

tmp_product_names <- tibble(
  product_name = c(
    "Ble_tendre",
    "Mais_grain",
    "Triticale",
    "Melange_cereales_d_ete",
    "Autres_cereales",
    "Orge_d_hiver_et_escourgeon",
    "Orge_de_printemps",
    "Tournesol",
    "Lait"
  ),
  labels = c(
    "Soft Wheat",
    "Grain Maize",
    "Triticale",
    "Summer Cereal Mix",
    "Other Cereals",
    "Winter Barley",
    "Spring Barley",
    "Sunflower",
    "Milk"
  )
) %>%
  mutate(
    plot = case_when(
      product_name %in% c("Ble_tendre","Mais_grain","Lait") ~T,
      .default = F
    )
  )

tmp_FQS_names <- tibble(
  FQS = c(
    "Conventionnel",
    "AB",
    "LR",
    "Comte - Morbier",
    "Beurre de Charentes-Poitou",
    "AOP - AOC",
    "Bleu d'Auvergne - Cantal",
    "Fromages de Savoie",
    "Munster",
    "Counterfactual"
  ),
  labels = c(
    "Conventional",
    "Organic Farming",
    "Label Rouge",
    "Comté - Morbier",
    "Charentes-Poitou Butter",
    "AOP - AOC",
    "Bleu d'Auvergne - Cantal",
    "Savoie Cheeses",
    "Munster",
    "Counterfactual"
  ),
  colors = c(
    "darkgrey",
    "palegreen3",
    "indianred",
    "plum4",
    "plum4",
    "plum4",
    "plum4",
    "goldenrod3",
    "plum4",
    "#333333"
  )) %>%
  mutate(
    plot = case_when(
      FQS %in% c("Conventionnel","AB","LR","Beurre de Charentes-Poitou","Comte - Morbier") ~T,
      .default = F
    ))
names(tmp_FQS_names$colors) <- tmp_FQS_names$labels
names(tmp_FQS_names$labels) <- tmp_FQS_names$labels

#

#{r practices_names} ----

library(dplyr)

# crops ----
tmp_practice_names <- tibble(
  practice = rev(c("A.2.1",
                   "A.2.2",
                   "A.3.2",
                   "A.3.3",
                   "A.3.1",
                   "A.4.3",
                   "A.4.5",
                   "A.5.1",
                   "yield")),
  labels = rev(c("Hedge density (linear meter / ha)",
                 "Mean Field Size (ha)",
                 "Soil cover (number of uncovered day)",
                 "Cultural Diversity (Shannon Index)",
                 "Tillage (L diesel / ha)",
                 "Share of mineral fertilizer (%)",
                 "Fertilization (kg N / ha)",
                 "Pesticides (€~TFI~UDNu / ha)",
                 "Yield (kg / ha)")))
tmp_practice_names$practice <- factor(tmp_practice_names$practice)
names(tmp_practice_names$labels) <- tmp_practice_names$practice

if (exists("practice_names")) {
  practice_names[["crops"]] <- tmp_practice_names
} else {
  practice_names = list()
  practice_names[["crops"]] <- tmp_practice_names
}

# Herd ----

tmp_practice_names <- tibble(
  practice = c("yield_l_pha",
               "yield_l_panim",
               "nb_cow_pha",
               "cow_pMFA",
               "feed_autonomy",
               "kg_DM_panim_maize_produced",
               #"kg_DM_panim_soy_meal",
               "share_soybean",
               #"share_concent_purchased",
               "share_concent",
               "protein_crop_ha_pha_pseudofarm",
               "MFA_pcow",
               "MFA_pha_pseudofarm",
               "ha_temp_pasture_pha_pseudofarm",
               "grassland_share_pseudofarm"#,
               #"grassland_share_farm"
  ),
  labels = c("Yield (L milk / ha pseudofarm)",
             "Yield (L milk / dairy cow)",
             "Livestock density (dairy cow / ha pseudofarm)",
             "Livestock density (dairy cow / ha MFA)",
             "Feed autonomy (kg produced feed / kg feed)",
             "Forage maize (kg produced forage maize / dairy cow)",
             #"Soyben meal (kg purchased soybean meal / dairy cow)",
             "Soybean meal (kg purchased soybean meal / kg feed)",
             #"Part des concentrés dans les aliments achetés (kg de concentrés / kg d'aliments achetés)",
             "Concentrate (kg concentrate / kg feed)",
             "Legumes (ha legumes / ha pseudofarm",
             "Main Forage Area (MFA / dairy cow)",
             "Main Forage Area (ha MFA / ha pseudofarm)",
             "Temporary meadows (ha temp. meadow / ha pseudofarm)",
             "Permanent grassland (ha perm. grassland / ha pseudofarm)"#,
             #"Part des prairies permanentes (ha prairie / ha ferme)"
  ))

tmp_practice_names$practice <- factor(tmp_practice_names$practice)
names(tmp_practice_names$labels) <- tmp_practice_names$practice

if (exists("practice_names")) {
  practice_names[["herd"]] <- tmp_practice_names
} else {
  practice_names = list()
  practice_names[["herd"]] <- tmp_practice_names
}

# Feed ----

tmp_practice_names <- practice_names$crop %>%
  mutate(practice = factor(paste0("feed_",practice)))
names(tmp_practice_names$labels) <- tmp_practice_names$practice

if (exists("practice_names")) {
  practice_names[["feed"]] <- tmp_practice_names
} else {
  practice_names = list()
  practice_names[["feed"]] <- tmp_practice_names
}

# BVIAS ----

tmp_practice_names <- tibble(
  "practice" = c("BVIAS_ha","BVIAS_kg","BVIAS_t"),
  "labels" = c("BVIAS (ha)","BVIAS (kg)","BVIAS (t)"))
tmp_practice_names$practice <- factor(tmp_practice_names$practice)
names(tmp_practice_names$labels) <- tmp_practice_names$practice

if (exists("practice_names")) {
  practice_names[["BVIAS"]] <- tmp_practice_names
} else {
  practice_names = list()
  practice_names[["BVIAS"]] <- tmp_practice_names
}

# all ----
# practice names
tmp_practice_names <- Reduce(bind_rows,list(
  practice_names$BVIAS,
  practice_names$crops,
  practice_names$herd,
  practice_names$feed
))

#

#{r tbl-results, include=FALSE, echo=FALSE} ----

library(dplyr)
library(tidyr)

# Table ----

tmp_table0 <- stat_desc %>%
  # add unmatched stat test
  left_join(.,stat_tukey_hsd %>%
              select(production_type, product_name, FQS, product_FQS, practice, practice_subset,
                     stat_grp,stat_grp_conv,stat_grp_conv_diff,mean_conv_diff,pval_tukey) %>%
              mutate(match = "no_match"),
            by = join_by(production_type, product_name, FQS, product_FQS, practice, practice_subset,match)) %>%
  # add matching results
  left_join(.,stat_paired_t_test %>%
              select(production_type, product_name, FQS, practice, match,
                     mean_diff_match,pval_pttest,padjust_bonf),
            by = join_by(production_type, product_name, FQS, practice, match)) %>%
  # order factors
  mutate(
    practice = factor(practice,levels = tmp_practice_names$practice,labels = tmp_practice_names$labels,ordered = T),
    product_name = factor(product_name,levels = tmp_product_names$product_name,labels = tmp_product_names$labels,ordered = T),
    FQS = factor(FQS,levels = tmp_FQS_names$FQS,labels = tmp_FQS_names$labels,ordered = T)
  )

#

