
# Import data

We used the RICA 2020 data.

```{r packages}

library(readr)
library(readxl)

library(tidyr)
library(dplyr)
library(tibble)

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])
#rm(list = ls())

# which data base are you using? either "FADN" or "RICA"
my_DB = "FADN"

```

```{r load_data}

load("data_in/FADN_2018.RData")

```

# Import raw data

Run only if you haven't already load the data.
```{r import_data_2004-2018, echo=FALSE,include=FALSE}

# Get the files names
#tmp_ls_files = paste0(getwd(),"/FADN_LIFT_2004-2018/raw_data/",list.files(path = paste0(getwd(),"/FADN_LIFT_2004-2018/raw_data/"),pattern = "*.csv"))
# First apply read.csv, then rbind
#FADN_04_18 = lapply(tmp_ls_files, read_csv) %>% bind_rows()

```

```{r import_data_2018, echo=FALSE,include=FALSE}

# Get the files names
#tmp_ls_files = paste0(getwd(),"/FADN_LIFT_2004-2018/raw_data/",list.files(path = paste0(getwd(),"/FADN_LIFT_2004-2018/raw_data/"),pattern = "*2018.csv"))
# First apply read.csv, then rbind
#FADN_18 = lapply(tmp_ls_files, read_csv) %>% bind_rows() %>% mutate(ID = as.character(ID))

```


# Livestock

To estimate the impact of livestock on biodiversity, we first need to estimates the impact of their feed intake. Specifically, for purchased feed, we need to estimate the practices that were implemented in the farms that produced it. For that purpose, we will:
1. Estimate how much of each feed type have been purchased based on an average livestock ration
2. Estimate the ration nutritional value
2. Estimate the average practices implemented to produce each type of feed

## Estimer la ration (quantité et qualité des aliments)

### Calculer la quantité ingérée d'aliments par ferme

```{r feed_purchased}

source("R/feed_purchased.R",local = knitr::knit_global()) # concentrate + rough

```

```{r feed_produced}

# As input, a table with:
## farm Unique Identifier
## farm region
## crop code
## area in hectares for each crop
## production quantity in kg for each crop
## sales quantity in kg for each crop
source("R/yield_crop.R")

source("R/feed_produced.R",local = knitr::knit_global()) # crops + grassland


```


### Répartir la quantité ingérée d'aliments par type de bétail

As feeding practices differ between livestock categories, we considered that the proportion of feed supplied for a livestock category $l$ on the total value of feed in the farm $i$ is equal to the proportion of the average ration for that livestock category $l$ on the sum of the average ration of all farm herd, such as:

$$\frac{x_{i,l}}{x_i} = \frac{ration_l \cdot pop_{i,l}}{ \sum_{l} (ration_l \cdot pop_{i,l}) }$$

Thus, we estimated the amount of feed supplied to each livestock category $l$ as follow:

$$x_{i,l} = x_i \cdot \frac{ ration_l \cdot pop_{i,l}}{ \sum_{l} \big(ration_l \cdot pop_{i,l} \big) }$$

where:

$x$: the amount of feed provided (kg)
$ration$: the average ration as determined by AROPAJ (ref ???)
$pop$: number of animals
$i$: a farm
$l$: a livestock category

```{r feed_by_livestock_category}

source("R/feed_by_livestock_category.R",local = knitr::knit_global())

```

## Estimer la quantité d'azote excrété par le bétail

```{r N_excretion}

# NB : guidelines IPCC are for one animal, not for one livestock unit

source("R/N_excretion_from_livestock.R",local = knitr::knit_global())

### output => kg N yr-1 

```

# Pratiques culturales

## Intensity of soil movement

We estimated the intensity of soil movement by comparing the off-road diesel volume used to the average consumption on non-tillage farms, as established by Pellerin et al. (2013).

$$Y_{i} = \frac{x_{i}}{SAU_{i}} - \overline{x}$$

Where:

$i$: a farm $Y$: the volume of off-road diesel used for tillage per hectare (L/ha) $x$: the volume of off-road diesel used (L) $SAU$: the useful arable area (ha) $\overline{x}$: the average off-road diesel use without tillage (L ha-1) as determined by Pellerin et al. (2015)

We chose an area allocation on arable land use type.

```{r A.3.1}

source("R/BVI_A31.R",local = knitr::knit_global())

```

## Intensity of Fertilizing

As mineral fertilization practices differ between crops, land use type and farm certification, we considered that the proportion of mineral fertilizer used for the crop $c$ on the total value of mineral fertilizer input in the farm $i$ is equal to the proportion of the average mineral fertilizer input for that crop $c$ on the sum of average mineral fertilizer inputs of all farm crops, such as:

$$\frac{x_{i,c}}{x_i} = \frac{ M_{c} \cdot SAU_{i,c} }{ \sum_{i} (M_{c} \cdot SAU_{i,c} ) }$$

Thus, we estimated the value of mineral fertilizer input used per hectares as follow:

$$ Y_{i,c}^{min} = \frac{x_i \cdot \frac{ M_c \cdot SAU_{i,c}}{ \sum_{c} \big(M_c \cdot SAU_{i,c} \big) }}{SAU_{i,c}} $$

where: $Y^{min}$: the quantity of mineral fertilizer input per hectares (kg N/ha) $x$: the quantity of mineral fertilizer input (kg) $M$: the average mineral fertilizer input as determined by the EPK studies (2017) 
$SAU$: the useful arable area (ha) $i$: a farm $c$: a crop or grassland

with $M_{c} = 0$ for certified organic crops

For organic fertilization, we considered that farms without livestock apply a reference  average of organic fertilizer across both their croplands and grasslands, as determined by the EPK studies (2017). In the case of farms with livestock, we calculated the total nitrogen excretion from their livestock following the IPCC Guidelines. We then estimated the quantity of this organic nitrogen applied to cropland and grassland $Y^{org}$ as for $Y^{min}$.

/!\ WIP add threshold min and max

```{r A.4.5}

source("R/BVI_A45.R",local = knitr::knit_global())

```

## Plant protection agents

As plant protection agent practices differ between crops, we considered that the proportion of pesticides used for the crop $c$ on the total value of pesticides in the farm $i$ is equal to the proportion of the average TFI for that crop $c$ on the sum of the average TFI of all farm crops, such as:

$$\frac{x_{i,c}}{x_i} = \frac{TFI_c \cdot SAU_{i,c}}{ \sum_{c} (TFI_c \cdot SAU_{i,c}) }$$

Thus, we estimated the value of pesticides used per hectares of the crop $c$ as follow:

$$Y_{i,c} = \frac{x_i \cdot \frac{ TFI_c \cdot SAU_{i,c}}{ \sum_{c} \big(TFI_c \cdot SAU_{i,c} \big) }}{SAU_{i,c}}$$

where:

$x$: the value of pesticides used (€) $TFI$: the average treatment frequency index (IFT) as determined by the EPK studies (2017,2018) $SAU$: the useful arable area (ha) used to cultivate crop $c$ $Y$: the value of pesticides used per hectares (€/ha) $i$: a farm $c$: a crop

Consequently, we chose a crop area allocation on arable land use type.

The TFI data were retrieve from Agreste "Pratiques culturales" studies and other local studies (all data source are mentioned in file)

```{r A.5.1}

source("R/BVI_A51.R",local = knitr::knit_global())

```

# GHGE
 To estimate the green house gas emission from cropland, grassland, livestock and manure management, we followed the IPCC Guidelines 2019 Refinements.

## Emissions from Livestock and Manure Management
### Methane Emission from Enteric fermentation

Enteric fermentation is a process by which carbohydrates contained in food are converted to methane. Mostly ruminants, mammals with a multichamber stomach, produce methane through such process. Ruminants include cattle, sheep and goats.
https://agledx.ccafs.cgiar.org/emissions-led-options/sources-sinks/enteric-fermentation/

```{r CH4_from_enteric_fermentation}

source("R/CH4_from_enteric_fermentation.R",local = knitr::knit_global())

```

### Methane Emission from Manure Management

As we do not have data about manure management practices in the FADN, we used the Tier 1 method from the IPCC Guidelines (2006, 2019)

```{r CH4_from_manure_management}

source("R/CH4_from_manure_management.R",local = knitr::knit_global())

```

### N2O Emission from Manure Management
To be completed


## Crop/Feed
cf. Sarah Le Clerc 
https://github.com/sarahkleclerc/GHGfromFADN
To be completed
### Crop/Feed emission
To be completed
### Carbon storage
To be completed

# Water
To be completed

# Biodiversity: BVIAS model

## Productions végétales

```{r BVI_to_RICA_crops}

source("R/BVI_crops.R",local = knitr::knit_global())

```

### Biodiversity Value Contribution

We define biodiversity value contribution functions such as Lindner (Lindner et al., 2019, 2022):

-   $BV \in [0,1]$ with $f(min) = \{0,1\}$ & $f(max) = \{1,0\}$

-   function constants are retained from Lindner 2019 and are identical for arable and pasture land use types

-   function are define based on the 95% of the lowest values for each parameters (i.e. the 5% highest values are cut off).

All input variables are normalized to $[0,1]$.

The generic expression of $y_i(x_i)$ is:

$$y_i = \gamma + \epsilon \cdot \exp(-\frac{|(x_i^\delta - \beta)^\alpha|}{2\sigma^\alpha})$$

### Land use specific biodiversity value

All parameters together constitute a joint biodiversity value contribution which is the arithmetic mean of the individual value contribution.

### Biodiversity value normalization

The normalized biodiversity value $BV_{norm}$ is calculated from the land use specific biodiversity value $BV_{LU}$ by projecting $BV_{LU}$ into the value interval of the respective land use type.

$$BV_{norm} = BV_{norm,j,min} + BV_{LU,j} \centerdot (BV_{norm,j,max} - BV_{norm,j,min})$$

with $j= \{forestry,pasture,arable,mining\}$

### Local biodiversity value

The normalized biodiversity value $BV_{norm}$ is skewed by a function to calculate the local biodiversity value $BV_{loc}$ This function serves to push the less natural land uses further apart and to squeez the more natural land uses closer together.

$$BV_{loc}=a \centerdot (1- e^{-b \centerdot BV_{norm}})$$

with $a = 1.017626088$ and $b = 4.055847776$ (Lindner & Knuepffer 2020)

### BVI per kg

To obtain the BVI/kg, we estimated crop yields using the crop area and the crop production, for each crop in each farm. Then, we calculated the ratio between the BV_loc per ha and the yield.

## Productions animales

WIP maybe I should write a script for estimating the pseudofarm

```{r BVi_per_herd}

source("R/BVI_herd.R",local = knitr::knit_global())

```

$$BVI_{herd} = \sum(DMI_{total} \cdot BVI_{ha} )$$
$$BVI_{ha_{feed}} = \frac{\sum (\frac{DMI_{total}}{yield}  \cdot BVI_{ha} )} {\sum (\frac{DMI_{total}}{yield})  }$$

### Milk

$$BVI_{kg}^{milk} = \frac{BVI^{herd} \cdot GS^{milk}}{production^{milk}_{kg}} $$ 
$GS^{milk}$: part du lait dans le chiffre d'affaire (%)

```{r BVI_to_RICA_milk}

source("R/BVI_milk.R",local = knitr::knit_global())

```

### Meat
 #### Beef
 To be completed

 #### Pork
To be completed
 #### Poultry
To be completed

### Eggs
To be completed

