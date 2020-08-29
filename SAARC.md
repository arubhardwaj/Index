---
title: "SAARC Development Index"
author: "Aru Bhardwaj"
date: "8/27/2020"
output:
  prettydoc::html_pretty:
    theme: cayman
    highlight: github
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Introduction

In this project, I developed an index of SAARC countries, taking data of 2018. The variables used in developing this index are:

- Infant Mortality
- Urban Population
- Life Expectancy
- Adult Literacy
- GNI Per capita
- Mean School Years
- Press Freedom

This index ranges between 0 and 1. It shows that Maldives is the best performing in all these indicators, among other SAARC members, with index value of 0.64. The worst performing nation on these indicators is Nepal. 

## Installing Libraries

The following libraries are be required for developing this Index.
```{r, message=FALSE}
library(readxl) # For reading xlsx file
library(tidyverse)
library(pander) # For making nice tables

pander::panderOptions('table.split.table', Inf)

```


## Loading Data

I have loaded data in the excel file, which is available [here](https://github.com/arubhardwaj/SAARC-Development-Index-2018/blob/master/Data/index_data.xlsx)

The table below shows the loaded data file in R. I am using `pander()` because of it present tables nicely.

```{r}
index_data <- read_excel("/cloud/project/index_data.xlsx")
pander(index_data) # This represents data more nicely
```


## Some Preprations

First, normaization of function will be required according to the formula below:

$$ = \frac{value - min(value)}{max(value) - min(value)} $$
This formula is used commonly in textbooks. It will help us in normalizing the data

```{r}
normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))} # Normalizing the function

```

The below code will apply this `normalize` in the data loaded before, and change column names to appropriate.
```{r}
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

```





## Assigning Weights

In the next step values of weights are required to be assigned. This will specify our focus on the variables---higher weight will imply more stress on the variable. 

Sum of these weights must be equal to 1.

```{r}
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
```

After putting the effects of weight (by multiplying them with the respective column), weighted index will be there for scoring purpose in the next section.

```{r}
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

```


## Indexing

The index value will show the position of countries in the region. The results show that Maldives is a best performer and Nepal is worst performing on these indicators. Second and third position is held by Bangladesh and India, respectively. 

```{r}
Index <- index_w %>%
  group_by(Country) %>%
  summarise(Index = sum(`Infant Mortality`,
                        `Urban Population`,
                        `Life Expectancy`,
                        `GNI per Capita`,
                        `Press Freedom`,
                        `Education`))

pander(Index)
```


## Ranking

Now we will rank the positions of all the countries, on the basis of their results. 
```{r}

# Ranking
Rank <- Index %>%
  mutate(Rank = dense_rank(desc(Index)))

pander(Rank)
```

```{r}
## Transpose

Rank <- Rank %>%
  as.matrix() %>%
  t()

pander(Rank)

```

Component wise index will show the score of countries on each indicators. 

```{r}

## Component Wise Index Value

index_trans <- index_w %>%
  as.matrix() %>%
  t()

pander(index_trans)



```


