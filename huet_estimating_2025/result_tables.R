# Result tables

library(dplyr)
library(tidyr)
# Data ----

#{r import_data} ----

library(readr)

stat_desc = readr::read_csv("~/BiodivLabel/CASD_export/CASD_export_250122/stat_desc_BVIAS_SIQO_2025-01-22.csv")

stat_tukey_hsd = readr::read_csv("~/BiodivLabel/CASD_export/CASD_export_250122//tmp_TukeyHSD_2025-01-22.csv")

stat_paired_t_test = readr::read_csv("~/BiodivLabel/CASD_export/CASD_export_250122//tmp_paired_ttest_2025-01-22.csv")

#


#{r product_names, include=FALSE, echo=FALSE} ----
library(tibble)

tmp_product_names <- tibble(
  product_name = c(
    "Ble_tendre",
    "Mais_grain",
    "Mais_fourrage",
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
    "Forage maize",
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
                   "A.3.1",
                   "A.3.2",
                   "A.3.3",
                   "A.4.3",
                   "A.4.5",
                   "A.5.1",
                   "yield",
                   "crop_nb_LU")),
  labels = rev(c("Hedge Density (linear m / ha)",
                 "Mean Field Size (ha)",
                 "Tillage (L diesel / ha)",
                 "Soil Cover (Number of uncovered day)",
                 "Crop Diversity (Shannon Index)",
                 "Share of mineral fertilization (%)",
                 "Nitrogen fertilization (kg N / ha)",
                 "Pesticides (€~TFI~UDNu / ha)",
                 "Yield (kg / ha)",
                 "Number of crops")))
tmp_practice_names$practice <- factor(tmp_practice_names$practice)
names(tmp_practice_names$labels) <- tmp_practice_names$practice

if (exists("practice_names")) {
  practice_names[["crops"]] <- tmp_practice_names
} else {
  practice_names <- list()
  practice_names[["crops"]] <- tmp_practice_names
}

# Herd ----

tmp_practice_names <- tibble(
  practice = c("yield_l_pha_ps", # pha_ps = per hectare of pseudofarm
               "yield_l_pha",
               "yield_l_panim",
               "nb_cow_pha_ps",
               "nb_cow_pha",
               "nb_cow_pMFA",
               "MFA_pcow",
               "MFA_pha_ps",
               "MFA_pha",
               "ha_perm_grassland_pha_ps",
               "ha_perm_grassland_pha",
               "ha_temp_grassland_pha_ps",
               "ha_temp_grassland_pha",
               "protein_crop_ha_pha_ps",
               "protein_crop_ha_pha",
               "feed_autonomy",
               "kg_DM_panim_maize_produced",
               "share_soybean",
               "share_concent"
  ),
  labels = c("Yield (L of milk / ha pseudofarm)",
             "Yield (L of milk / ha farm)",
             "Yield (L of milk / dairy cow)",
             "Livestock density (dairy cow / ha pseudofarm)",
             "Livestock density (dairy cow / ha farm)",
             "Livestock density (dairy cow / ha MFA)",
             "Main Forage Area (MFA / dairy cow)",
             "Share of Main Forage Area (ha MFA / ha pseudofarm)",
             "Share of Main Forage Area (ha MFA / ha farm)",
             "Share of permanent grassland (ha permanent grassland / ha pseudofarm)",
             "Share of permanent grassland (ha permanent grassland / ha farm)",
             "Share of temporary grassland (ha temporary grassland / ha pseudofarm)",
             "Share of temporary grassland (ha temporary grassland / ha farm)",
             "Share of legumes (ha legumes / ha pseudofarm)",
             "Share of legumes (ha legumes / ha farm)",
             "Feed autonomy (kg produced feed / kg total feed)",
             "Forage maize (kg of forage maize produced / dairy cow)",
             "Share of purchased soybean meal (kg soybean meal / kg total feed)",
             "Share of concentrate (kg concentrate / kg total feed)"
  ))

tmp_practice_names$practice <- factor(tmp_practice_names$practice)
names(tmp_practice_names$labels) <- tmp_practice_names$practice

if (exists("practice_names")) {
  practice_names[["herd"]] <- tmp_practice_names
} else {
  practice_names <- list()
  practice_names[["herd"]] <- tmp_practice_names
}

# Feed ----
tmp_feed_origin <- tibble(
  practice = c("feed_produced","feed_grassland","feed_purchased"),
  labels = c(" among feed produced at farm"," among grasslands"," among purchased feed")
)

tmp_practice_names <- tibble(
  practice =
    paste0(c("feed",tmp_feed_origin$practice),"_",rep(practice_names$crops$practice, each = length(c("feed",tmp_feed_origin$practice)))),
  labels =
    #rep(practice_names$crops$labels,times = length(c("feed",tmp_feed_origin)))
    paste0(rep(practice_names$crops$labels,each = length(c(" among all feed",tmp_feed_origin$labels))),c(" among all feed",tmp_feed_origin$labels))
) %>%
  mutate(practice = factor(practice))

names(tmp_practice_names$labels) <- tmp_practice_names$practice


if (exists("practice_names")) {
  practice_names[["feed"]] <- tmp_practice_names
} else {
  practice_names <- list()
  practice_names[["feed"]] <- tmp_practice_names
}

# BVIAS ----

tmp_practice_names <- tibble(
  "practice" = factor(x = c("BVI_ha","BVI_kg","BVI_t"),
                      levels = c("BVI_ha","BVI_kg","BVI_t")),
  "labels" = c("BVIAS (ha)","BVIAS (kg)","BVIAS (t)"))
names(tmp_practice_names$labels) <- tmp_practice_names$practice

if (exists("practice_names")) {
  practice_names[["BVIAS"]] <- tmp_practice_names
} else {
  practice_names <- list()
  practice_names[["BVIAS"]] <- tmp_practice_names
}

#  all practice names ----
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
              # estimate % diff to conv
              mutate(p100_diff_conv = -(conv_mean- trt_mean)/conv_mean*100) %>%
              # select
              select(production_type, product_name, FQS, product_FQS, practice, practice_subset,
                     p100_diff_conv,stat_grp,stat_grp_conv,stat_grp_conv_diff,pval_tukey) %>%

              mutate(match = "no_match"),
            by = join_by(production_type, product_name, FQS, product_FQS, practice, practice_subset,match)) %>%
  # add matching results
  left_join(.,stat_paired_t_test %>%
              # estimate % diff to counterfactuals
              left_join(
                stat_paired_t_test %>%
                  filter(FQS == "Conventionnel") %>%
                  select(product_name, match, practice,mean) %>%
                  rename(mean_counterfactual = mean),
                by = join_by(product_name, match, practice)
              ) %>%
              mutate(p100_diff_counterfactuals = -(mean_counterfactual - mean)/mean_counterfactual*100) %>%
              # select
              select(production_type, product_name, FQS, practice, match,
                     p100_diff_counterfactuals,pval_pttest,padjust_bonf),
            by = join_by(production_type, product_name, FQS, practice, match)) %>%
  # order factors
  mutate(
    practice = factor(practice,levels = tmp_practice_names$practice,labels = tmp_practice_names$labels,ordered = T),
    product_name = factor(product_name,levels = tmp_product_names$product_name,labels = tmp_product_names$labels,ordered = T),
    FQS = factor(FQS,levels = tmp_FQS_names$FQS,labels = tmp_FQS_names$labels,ordered = T)
  )

#

