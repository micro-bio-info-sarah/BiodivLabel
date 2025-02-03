# scriot for figures Huet et al., 2025

source("result_tables.R")

#```{r fig-results_ha, fig.align='center',fig.width=10,fig.fullwidth=TRUE} ----

library(dplyr)
library(tidyr)
library(ggplot2)

#source("~/BiodivLabel/huet_estimating_2025/result_tables.R")

## plot data ----

tmp_plot_data <- tmp_table0 %>%
  # select only one practice for some products
  filter(
    practice == tmp_practice_names$labels[tmp_practice_names$practice == "BVI_ha"]
    & product_name %in% tmp_product_names$labels[tmp_product_names$plot == T]
    & FQS %in% tmp_FQS_names$labels[tmp_FQS_names$plot == T]) %>%
  # change names for counterfactuals
  filter(match == "no_match" | FQS == "Conventional") %>%
  mutate(FQS = factor(case_when(
    match == "no_match" ~ FQS,
    match != "no_match" ~ "Counterfactual"
  ))) %>%
  # add asterisk when signif
  mutate(
    stat_grp = case_when(
      match == "no_match" ~ stat_grp,
      match != "no_match" & padjust_bonf <= 0.05 ~ "*",
      .default = ""
    )) %>%
  # add plot variables
  mutate(
    plot_x = paste0(product_name,match,FQS)
  )


## plot order ----

tmp_plot_order <- tibble(
  plot_x = rev(c("Soft Wheatno_matchConventional",
                 "Soft Wheatno_matchOrganic Farming",
                 "Soft WheatBle_tendre__ABCounterfactual",
                 "Soft Wheatno_matchLabel Rouge",
                 "Soft WheatBle_tendre__LRCounterfactual",
                 "Grain Maizeno_matchConventional",
                 "Grain Maizeno_matchOrganic Farming",
                 "Grain MaizeMais_grain__ABCounterfactual",
                 "Milkno_matchConventional",
                 "Milkno_matchOrganic Farming",
                 "MilkLait__ABCounterfactual",
                 "Milkno_matchCharentes-Poitou Butter",
                 "MilkLait__Beurre de Charentes-PoitouCounterfactual",
                 "Milkno_matchComté - Morbier",
                 "MilkLait__Comte - MorbierCounterfactual"
  )),
  labels = rev(c("Soft Wheat - Conventional",
                 "Soft Wheat - Organic Farming",
                 "Soft Wheat - Counterfactual Organic Farming",
                 "Soft Wheat - Label Rouge",
                 "Soft Wheat - Counterfactual Label Rouge",
                 "Grain Maize - Conventional",
                 "Grain Maize - Organic Farming",
                 "Grain Maize - Counterfactual Organic Farming",
                 "Milk - Conventional",
                 "Milk - Organic Farming",
                 "Milk - Counterfactual Organic Farming",
                 "Milk - Charentes-Poitou Butter",
                 "Milk - Counterfactual Charentes-Poitou Butter",
                 "Milk - Comte - Morbier",
                 "Milk - Counterfactual Comte - Morbier")))

tmp_plot_data$plot_x <- factor(tmp_plot_data$plot_x,
                               levels = tmp_plot_order$plot_x,
                               labels = tmp_plot_order$labels)
tmp_plot_data <- tmp_plot_data %>%
  filter(!is.na(plot_x)) %>%
  left_join(
    tmp_plot_data %>%
      group_by(production_type) %>%
      summarise(plot_y_n = max(mean+sd),
                plot_y_stat_group = min(mean-sd),
                .groups = "keep"),
    by = join_by(production_type)
  )
tmp_plot_data$production_type <- toupper(tmp_plot_data$production_type)

## plot ----

tmp_plot <- ggplot() +
  # mean
  geom_point(data = tmp_plot_data,
             aes(x = plot_x, y = mean, fill = FQS,colour = FQS),
             shape = 21, size=2, stroke = 1, position = position_dodge(width = 0.75)) +
  # CI95%
  geom_errorbar(data = tmp_plot_data,
                aes(x = plot_x, ymin = mean-1.96*(sd/sqrt(nobs)), ymax = mean+1.96*(sd/sqrt(nobs)), colour = FQS),
                linewidth=1,width = 0.5,position = position_dodge(width = 0.75)) +
  # sd
  geom_errorbar(data = tmp_plot_data,
                aes(x = plot_x, ymin = mean-sd, ymax = mean+sd, colour = FQS),
                linewidth=0.5,width = 0.5,linetype = "dashed",position = position_dodge(width = 0.75)) +
  # n
  geom_text(data = tmp_plot_data,
            aes(x = plot_x, y=plot_y_n*1.05,colour = FQS,label = paste0("n = ",as.character(nobs)))) +
  # stat groups
  geom_text(data = tmp_plot_data,
            aes(x = plot_x,y=plot_y_stat_group*0.95,colour = FQS,label = stat_grp))+
  # theme
  coord_flip() +
  #  ylim(c(0.45,0.825)) +
  facet_wrap(vars(production_type),scales = "free",ncol = 1,
             labeller = labeller(CROP = "Crops", MILK = "Milk")) +
  labs(y = "BVI_ha",x= "Products") +
  scale_fill_manual(values = tmp_FQS_names$colors,labels = tmp_FQS_names$labels,) +
  scale_colour_manual(values = tmp_FQS_names$colors,labels = tmp_FQS_names$labels) +
  theme_light() +
  theme(legend.position = "none",
        legend.direction = "horizontal",legend.title = element_blank(),
        strip.background = element_rect(
          color="#cccccc", fill="white", linewidth = 1, linetype="solid"),
        strip.text = element_text(colour = "#333333"),
        text = element_text(size = 8))

ggsave(plot = tmp_plot,
       filename = "images/fig-results_ha.svg",
       width = 190/1,height = 240/2,units = "mm")

#```

#```{r fig-results_t, fig.align='center',fig.width=10,fig.fullwidth=TRUE} ----

library(dplyr)
library(tidyr)
library(ggplot2)

#source("~/BiodivLabel/huet_estimating_2025/result_tables.R")

## plot data ----

tmp_plot_data <- tmp_table0 %>%
  # select only one practice for some products
  filter(
    practice == tmp_practice_names$labels[tmp_practice_names$practice == "BVI_t"]
    & product_name %in% tmp_product_names$labels[tmp_product_names$plot == T]
    & FQS %in% tmp_FQS_names$labels[tmp_FQS_names$plot == T]) %>%
  # change names for counterfactuals
  filter(match == "no_match" | FQS == "Conventional") %>%
  mutate(FQS = factor(case_when(
    match == "no_match" ~ FQS,
    match != "no_match" ~ "Counterfactual"
  ))) %>%
  # add asterisk when signif
  mutate(
    stat_grp = case_when(
      match == "no_match" ~ stat_grp,
      match != "no_match" & padjust_bonf <= 0.05 ~ "*",
      .default = ""
    )) %>%
  # add plot_x
  mutate(
    plot_x = paste0(product_name,match,FQS)
  )


## plot order ----

tmp_plot_order <- tibble(
  plot_x = rev(c("Soft Wheatno_matchConventional",
                 "Soft Wheatno_matchOrganic Farming",
                 "Soft WheatBle_tendre__ABCounterfactual",
                 "Soft Wheatno_matchLabel Rouge",
                 "Soft WheatBle_tendre__LRCounterfactual",
                 "Grain Maizeno_matchConventional",
                 "Grain Maizeno_matchOrganic Farming",
                 "Grain MaizeMais_grain__ABCounterfactual",
                 "Milkno_matchConventional",
                 "Milkno_matchOrganic Farming",
                 "MilkLait__ABCounterfactual",
                 "Milkno_matchCharentes-Poitou Butter",
                 "MilkLait__Beurre de Charentes-PoitouCounterfactual",
                 "Milkno_matchComté - Morbier",
                 "MilkLait__Comte - MorbierCounterfactual"
  )),
  labels = rev(c("Soft Wheat - Conventional",
                 "Soft Wheat - Organic Farming",
                 "Soft Wheat - Counterfactual Organic Farming",
                 "Soft Wheat - Label Rouge",
                 "Soft Wheat - Counterfactual Label Rouge",
                 "Grain Maize - Conventional",
                 "Grain Maize - Organic Farming",
                 "Grain Maize - Counterfactual Organic Farming",
                 "Milk - Conventional",
                 "Milk - Organic Farming",
                 "Milk - Counterfactual Organic Farming",
                 "Milk - Charentes-Poitou Butter",
                 "Milk - Counterfactual Charentes-Poitou Butter",
                 "Milk - Comte - Morbier",
                 "Milk - Counterfactual Comte - Morbier")))

tmp_plot_data$plot_x <- factor(tmp_plot_data$plot_x,
                               levels = tmp_plot_order$plot_x,
                               labels = tmp_plot_order$labels)
tmp_plot_data <- tmp_plot_data %>%
  filter(!is.na(plot_x)) %>%
  left_join(
    tmp_plot_data %>%
      group_by(production_type) %>%
      summarise(plot_y_n = max(mean+sd),
                plot_y_stat_group = min(mean-sd),
                .groups = "keep"),
    by = join_by(production_type)
  )

tmp_plot_data$production_type <- toupper(tmp_plot_data$production_type)

## plot ----

tmp_plot <- ggplot() +
  # mean
  geom_point(data = tmp_plot_data,
             aes(x = plot_x, y = mean, fill = FQS,colour = FQS),
             shape = 21, size=2, stroke = 1, position = position_dodge(width = 0.75)) +
  # CI95%
  geom_errorbar(data = tmp_plot_data,
                aes(x = plot_x, ymin = mean-1.96*(sd/sqrt(nobs)), ymax = mean+1.96*(sd/sqrt(nobs)), colour = FQS),
                linewidth=1,width = 0.5,position = position_dodge(width = 0.75)) +
  # sd
  geom_errorbar(data = tmp_plot_data,
                aes(x = plot_x, ymin = mean-sd, ymax = mean+sd, colour = FQS),
                linewidth=0.5,width = 0.5,linetype = "dashed",position = position_dodge(width = 0.75)) +
  # n
  geom_text(data = tmp_plot_data,
            aes(x = plot_x, y=plot_y_n*1.1,colour = FQS,label = paste0("n = ",as.character(nobs)))) +
  # stat groups
  geom_text(data = tmp_plot_data,
            aes(x = plot_x,y=plot_y_stat_group*0.9,colour = FQS,label = stat_grp))+
  # theme
  coord_flip() +
  #ylim(c(0,0.475)) +
  #ylim(c(0,max(tmp_plot_data$mean+tmp_plot_data$sd)*1.2)) +
  #  ylim(c(min(tmp_plot_data$mean-tmp_plot_data$sd)*1.03,max(tmp_plot_data$mean+tmp_plot_data$sd)*1.2)) +
  facet_wrap(vars(production_type),scales = "free",ncol = 1) +
  labs(y = "BVI_t",x= "Products") +
  scale_fill_manual(values = tmp_FQS_names$colors,labels = tmp_FQS_names$labels,) +
  scale_colour_manual(values = tmp_FQS_names$colors,labels = tmp_FQS_names$labels) +
  theme_light() +
  theme(legend.position = "none",
        legend.direction = "horizontal",legend.title = element_blank(),
        strip.background = element_rect(
          color="#cccccc", fill="white", linewidth = 1, linetype="solid"),
        strip.text = element_text(colour = "#333333"),
        text = element_text(size = 8))

ggsave(plot = tmp_plot,
       filename = "images/fig-results_t.svg",
       width = 190/1,height = 240/2,units = "mm")


#```

#```{r practice_crops, fig.align='center',fig.width=15,fig.height=10,fig.fullwidth=TRUE} ----

library(dplyr)
library(tidyr)
library(ggplot2)

#source("~/BiodivLabel/huet_estimating_2025/result_tables.R")

## plot data ----

tmp_plot_data <- tmp_table0 %>%
  # select only one practice for some products
  filter(
    practice_subset == "crops"
    & production_type == "crop"
    & product_name %in% tmp_product_names$labels[tmp_product_names$plot == T]
    & FQS %in% tmp_FQS_names$labels[tmp_FQS_names$plot == T]) %>%
  # add conventional mean
  inner_join(.,tmp_table0 %>%
               filter(FQS %in% c("Conventional","Counterfactual")) %>%
               group_by(product_name,match,practice) %>%
               reframe(mean_conv = mean,.groups = "keep"),
             by = join_by(product_name, practice, match)) %>%
  # change names for counterfactuals
  filter(match == "no_match" | FQS != "Conventional") %>%
  mutate(FQS = factor(case_when(
    match == "no_match" ~ FQS,
    match != "no_match" ~ "Counterfactual"
  ))) %>%
  # add asterisk when signif
  mutate(
    stat_grp = case_when(
      match == "no_match" ~ stat_grp,
      match != "no_match" & padjust_bonf <= 0.05 ~ "*",
      .default = ""
    )) %>%
  # add plot_x
  mutate(
    plot_x = paste0(product_name,match,FQS)
  )


## plot order ----

tmp_plot_order <- tibble(
  plot_x = rev(c("Soft Wheatno_matchConventional",
                 "Soft Wheatno_matchOrganic Farming",
                 "Soft WheatBle_tendre__ABCounterfactual",
                 "Soft Wheatno_matchLabel Rouge",
                 "Soft WheatBle_tendre__LRCounterfactual",
                 "Grain Maizeno_matchConventional",
                 "Grain Maizeno_matchOrganic Farming",
                 "Grain MaizeMais_grain__ABCounterfactual"
  )),
  labels = rev(c("Soft Wheat - Conventional",
                 "Soft Wheat - Organic Farming",
                 "Soft Wheat - Counterfactual Organic Farming",
                 "Soft Wheat - Label Rouge",
                 "Soft Wheat - Counterfactual Label Rouge",
                 "Grain Maize - Conventional",
                 "Grain Maize - Organic Farming",
                 "Grain Maize - Counterfactual Organic Farming"
  )))

tmp_plot_data$plot_x <- factor(tmp_plot_data$plot_x,
                               levels = tmp_plot_order$plot_x,
                               labels = tmp_plot_order$labels)
tmp_plot_data$production_type <- toupper(tmp_plot_data$production_type)

tmp_plot_data <- tmp_plot_data %>%
  filter(!is.na(plot_x) & !is.na(practice)) %>%
  mutate(
    plot_count_size = case_when(
      pval_tukey <= 0.05 | padjust_bonf <= 0.05 ~ abs((mean_conv - mean)/mean_conv*100),
      .default = NA
    ),
    plot_count_colour = case_when(
      mean/mean_conv > 1 ~ T,
      .default = F
    ),
    plot_mean = case_when(
      FQS == "Counterfactual" ~ mean_conv,
      .default = mean
    )
  ) %>%
  # filter practices
  filter(practice %in% practice_names$crops$labels[-1])

## plot ----

tmp_plot <- ggplot() +
  # circles
  geom_count(data = tmp_plot_data,
             aes(x = practice, y = plot_x,
                 colour = plot_count_colour,
                 size = plot_count_size)
  ) +
  # means
  geom_text(data = tmp_plot_data,
            aes(x = practice, y = plot_x,
                label = format(plot_mean,scientific = T,digits = 2)),
            size = 3.5,angle = 45, colour = "#666666") +
  # theme
  #scale_color_binned(type = "viridis")+
  scale_color_discrete("Significant differences \n \n FQS > Conventional") +
  scale_size_binned("% difference \n with conventional",range = c(1, 10)) +
  labs(x = "Practices (mean)", y = "Products") +
  theme_light() +
  theme(axis.text = element_text(size = 8,angle = 45,vjust = 1,hjust = 1),
        text = element_text(size = 8),
        legend.position = "right",
        legend.box="vertical",
        legend.margin = margin(),
        legend.direction = "vertical",
        legend.justification  = "right"
        )

ggsave(plot = tmp_plot,
       filename = "images/fig-results_practices_crops.svg",
       width = 190/1,height = 240/2,units = "mm")

#```

# ```{r practice_herd, fig.align='center',fig.width=15,fig.height=10,fig.fullwidth=TRUE} ----

library(dplyr)
library(tidyr)
library(ggplot2)

#source("~/BiodivLabel/huet_estimating_2025/result_tables.R")

## plot data ----

tmp_plot_data <- tmp_table0 %>%
  # select only one practice for some products
  filter(
    practice_subset == "herd"
    & production_type == "milk"
    & product_name %in% tmp_product_names$labels[tmp_product_names$plot == T]
    & FQS %in% tmp_FQS_names$labels[tmp_FQS_names$plot == T]) %>%
  # add conventional mean
  inner_join(.,tmp_table0 %>%
               filter(FQS %in% c("Conventional","Counterfactual")) %>%
               group_by(product_name,match,practice) %>%
               reframe(mean_conv = mean,.groups = "keep"),
             by = join_by(product_name, practice, match)) %>%
  # change names for counterfactuals
  filter(match == "no_match" | FQS != "Conventional") %>%
  mutate(FQS = factor(case_when(
    match == "no_match" ~ FQS,
    match != "no_match" ~ "Counterfactual"
  ))) %>%
  # add asterisk when signif
  mutate(
    stat_grp = case_when(
      match == "no_match" ~ stat_grp,
      match != "no_match" & padjust_bonf <= 0.05 ~ "*",
      .default = ""
    )) %>%
  # add plot_x
  mutate(
    plot_x = paste0(product_name,match,FQS)
  )


## plot order ----


tmp_plot_order <- tibble(
  plot_x = rev(c("Milkno_matchConventional",
                 "Milkno_matchOrganic Farming",
                 "MilkLait__ABCounterfactual",
                 "Milkno_matchCharentes-Poitou Butter",
                 "MilkLait__Beurre de Charentes-PoitouCounterfactual",
                 "Milkno_matchComté - Morbier",
                 "MilkLait__Comte - MorbierCounterfactual"
  )),
  labels = rev(c("Milk - Conventional",
                 "Milk - Organic Farming",
                 "Milk - Counterfactual Organic Farming",
                 "Milk - Charentes-Poitou Butter",
                 "Milk - Counterfactual Charentes-Poitou Butter",
                 "Milk - Comte - Morbier",
                 "Milk - Counterfactual Comte - Morbier")))

tmp_plot_data$plot_x <- factor(tmp_plot_data$plot_x,
                               levels = tmp_plot_order$plot_x,
                               labels = tmp_plot_order$labels)
tmp_plot_data$production_type <- toupper(tmp_plot_data$production_type)

tmp_plot_data <- tmp_plot_data %>%
  filter(!is.na(plot_x) & !is.na(practice)) %>%
  mutate(
    plot_count_size = case_when(
      pval_tukey <= 0.05 | padjust_bonf <= 0.05 ~ abs((mean_conv - mean)/mean_conv*100),
      .default = NA
    ),
    plot_count_colour = case_when(
      mean/mean_conv > 1 ~ T,
      .default = F
    ),
    plot_mean = case_when(
      FQS == "Counterfactual" ~ mean_conv,
      .default = mean
    )
  ) %>%
  filter(practice %in% practice_names$herd$labels[c(1,3,6,8,10,12,14,16,17,19)])

## plot ----

tmp_plot <- ggplot() +
  # circles
  geom_count(data = tmp_plot_data,
             aes(x = practice, y = plot_x,
                 colour = plot_count_colour,
                 size = plot_count_size)
  ) +
  # means
  geom_text(data = tmp_plot_data,
            aes(x = practice, y = plot_x,
                label = format(plot_mean,scientific = T,digits = 2)),
            size = 3.5, angle = 45, colour = "#666666") +
  # theme
  #scale_color_binned(type = "viridis")+
  scale_color_discrete("Significant differences \n \n FQS > Conventional") +
  scale_size_binned("% difference \n with conventional",range = c(1, 12.5)) +
  labs(x = "Practices (mean)", y = "Products") +
  theme_light() +
  theme(axis.text = element_text(size = 8,angle = 45,vjust = 1,hjust = 1),
        text = element_text(size = 8),
        legend.position = "right",
        legend.box="vertical",
        legend.margin = margin(),
        legend.direction = "vertical",
        legend.justification  = "right"
  )

ggsave(plot = tmp_plot,
       filename = "images/fig-results_practices_herd.svg",
       width = 190/1,height = 240/1.5,units = "mm")

#```

