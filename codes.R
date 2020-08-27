###################################################################
################# load packages####################################
###################################################################
library(readxl)
library(tidyverse)
library(scales)
library(pander)
library(maps)
pander::panderOptions('table.split.table', Inf)


####################################################################
############ Load Data##############################################
####################################################################
index_data <- read_excel("/cloud/project/index_data.xlsx")
pander(index_data) # This represents data more nicely

####################################################################
############ Preliminaries #########################################
####################################################################

normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))} # Normalizing the function


index_norm <- index_data %>%
  mutate_if(is.numeric, funs(normalize)) %>%
  rename('Press Freedom' = press_freedom,
         'Urban Population' = urban_pop,
         'GNI per Capita' = GNI_percapita,
         'Infant Mortality' = infant_mortality,
         'Life Expectancy' = life_expenctancy_total,
         'Adult Literacy' = adult_literacy,
         'Mean School Years' = mean_school_years
  )
pander(index_norm)

attach(index_norm)
index_norm$Education <- (`Adult Literacy` + `Mean School Years`)/2

pander(index_norm)

#########################################################################
############# Assigning Weights #########################################
#########################################################################

im <- 0.15 # Infant Moratlity
up <- 0.2 # Urban Population
edu <- 0.2 # Education
pf <- 0.15 # Press Freedom
le <- 0.15 # Life Expectancy 
gni <- 0.1 # GNI per Capita

weights <- c(im,up,edu,pf,le,gni)

# Sum of weights must be equal to 1
sum(weights)==1
            # Or we can run
im+up+edu+pf+le+gni

# Putting effect of weights in the data

index_w <- index_norm %>%
  mutate('Press Freedom' = `Press Freedom`*pf,
         'Urban Population' = `Urban Population`*up,
         'GNI per Capita' = `GNI per Capita`*gni,
         'Infant Mortality' = `Infant Mortality`*im,
         'Life Expectancy' = `Life Expectancy`*le,
         'Education' = `Education`*edu )  %>%
mutate_if(is.numeric, funs(round(., digits = 2)))

pander(index_w)         


########################################################################
############### Scoring ################################################
########################################################################


Index <- index_w %>%
  group_by(Country) %>%
  summarise(Index = sum(`Infant Mortality`,
                        `Urban Population`,
                        `Life Expectancy`,
                        `GNI per Capita`,
                        `Press Freedom`,
                        `Education`))

pander(Index)


# Score wise Ranking
Rank <- Index %>%
  mutate(Rank = dense_rank(desc(Index)))

pander(Rank)


##### Transpose

Rank <- Rank %>%
  as.matrix() %>%
  t()

pander(Rank)



## Component Wise Index Value

index_trans <- index_w %>%
  as.matrix() %>%
  t()

pander(index_trans)


