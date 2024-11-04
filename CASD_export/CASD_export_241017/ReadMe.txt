8 fichiers de données issues du CASD en csv qui respectent les règles de confidentialités (n >3 et pas de fermes qui reprèsente >80% de la variable affichée avec n_max_p100 < 0.8) :
 dont 3 fichiers issus de l'enquête pratiques culturales de 2017 :
- "PKGC_N_ferti_2024-10-17.csv" : moyennes pondérées par culture de la variable de fertilisation organique dans l'enquête pratiques culturales de 2017 selon le mode de production et l'origine de la fumure avec la variable "n" pour le nombre d'exploitation
- "PKGC_N_ferti_org_2024-10-17.csv" : moyennes pondérées par culture de la variable de fertilisation organique dans l'enquête pratiques culturales de 2017 selon le mode de production et l'origine de la fumure avec la variable "n" pour le nombre d'exploitation
- "PKGC_N_ferti_org_threshold_2024-10-17.csv" : seuils calculés à partir des moyennes pondérées par culture de la variable de fertilisation organique dans l'enquête pratiques culturales de 2017 selon le mode de production et l'origine de la fumure avec la variable "n" pour le nombre d'exploitation
 dont 5 fichier issus du RICA 2020 :
- "stat_desc_BVIAS_SIQO2024-10-17.csv" : statistiques descriptives des variables estimées par mon modèle avec "nobs" la variable pour le nombre d'exploitation
- "table_ES_table2024-10-17.csv" : table des taille d'effet calculées pour chaque variable sur l'ensemble des données exploitées (nombre d'exploitation > 1300)
- "table_MSE_distance_table2024-10-17.csv" : table des erreurs quadratiques pour certaines variables sur l'ensemble des données exploitées (nombre d'exploitation > 1300)
- "tmp_paired_ttest_2024-10-17.csv" : résultats des t-test appariés avec la colonne "n_matched" donnant le nombre d'exploitation
- "tmp_TukeyHSD_2024-10-17.csv" : résultats des tests de Tukey HSD avec la colonne "r" donnant le nombre d'exploitation

1 fichier docx sans données avec des graphs résumant la procédure de matching
- "matching_summary_2024-10-17.docx"

1 dossier avec les scripts utilisés, sans données :
- "scripts"

1 fichier excel sans données du CASD, uniquement des données importées précedemment qui ont été réarrangées pou servir de table d'allocation dans le modèle :
- "supp_data.xlsx"

1 graphique avec une courbe et une densité pour chaque variable du modèle sur les données exploitées (nombre d'exploitation > 1300) :
- "BVC_functions_optim.svg"
