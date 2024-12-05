# -*- coding: utf-8 -*-
# @Author:
# @Date: 2023-07-29 20:50:12
# @Last Modified by: dbc
# @Last Modified time: 2024-11-21 23:01:08
# @Description: Tables 1_3 and Figures 3_6

# loading packages
# pacman::p_load(readr,pedometrics)
library(broom)
library(car)
library(dplyr)
library(effects)
library(ggeffects)
library(ggpubr)
library(glmm.hp)
library(gridExtra)
library(patchwork)
library(pedometrics)
library(readr)
library(sjPlot)
library(tidyverse)
library(performance)
library(MASS)

# set file path
setwd("D:/我的坚果云/2023.liyuan_database/新GADM数据集")
getwd()

# load data
invasion.global <- read_csv("D:/我的坚果云/2023.liyuan_database/新GADM数据集/20241120.iso_upadted_v16.diversity.cleaned.csv")
glimpse(invasion.global)
# str(invasion.global)

# view data
# View(invasion.global)

# 调整代码
invasion.global01 <- invasion.global %>%
filter(!(alpha_3 %in% c("TWN", "MAC", "HKG")))  %>%
filter(!is.na(Observed_SR)) %>%
dplyr::select(-c("HS_06_cpi2016", "HS_07_cpi2016", "HS_08_cpi2016", "HS_12_cpi2016")) %>%
rename(SR  = Observed_SR,
       PD = Observed_PD,
       mpd.obs = Observed_MPD,
       mntd.obs = Observed_MNTD,
       HS_06_cpi2016 = HS_06_total_trade_cpi2016_avg,
       HS_07_cpi2016 = HS_07_total_trade_cpi2016_avg,
       HS_08_cpi2016 = HS_08_total_trade_cpi2016_avg,
       HS_12_cpi2016 = HS_12_total_trade_cpi2016_avg
       )

# 计算平均值
invasion.global01 <- invasion.global01 %>%
  mutate(
    HS_n_importers_avg = rowMeans(
      across(c(HS_06_n_importers_avg,
               HS_07_n_importers_avg,
               HS_08_n_importers_avg,
               HS_12_n_importers_avg)),
      na.rm = TRUE
    ),
    HS_evenness_avg = rowMeans(
      across(c(HS_06_evenness_avg,
               HS_07_evenness_avg,
               HS_08_evenness_avg,
               HS_12_evenness_avg)),
      na.rm = TRUE
    )
  )

glimpse(invasion.global01)

# replace null data with zero
invasion.global01 <- invasion.global01 %>%
mutate_at(vars(airport.n, seaport.n), ~case_when(is.na(.) ~ 0,
                                                 TRUE ~.))

# data description
#
# airport.n number
# seaport.n number
# gdp.mean_cpi2016: thousand US dollars; data extracted from Gravity
# gdp_per_capita: Thousands of dollars per person
# HS: US dollars; data extracted fromUNcomtrade
# mean.population_1k: thousand people; data from the total population of July in the population database of UN
# mean.area_km2: square kilometers; data from UN, estimated based on Population database of UN，i.e., Total population in July/population density in July
# mean.population: density_km2;  Persons per square kilometer, calculated acquisition
# pop.density: persons/km2
# wc2.1_10m_bio_1: mean annual temperature at 10 min, data from WORLDCLIM
# wc2.1_10m_bio_12: mean annual precipitation at 10 min, data from WORLDCLIM

# variable calculation
invasion.global01 <- invasion.global01 %>%
  mutate(airport_density = airport.n / (mean.area_km2 / 100000),
         seaport_density = seaport.n / (mean.area_km2 / 100000),
         gdp_per_capita = gdp.mean_cpi2016 / (mean.population_1k*1000),
         HS_06_per_capita = HS_06_cpi2016 / (mean.population_1k*1000),
         HS_07_per_capita = HS_07_cpi2016 / (mean.population_1k*1000),
         HS_08_per_capita = HS_08_cpi2016 / (mean.population_1k*1000),
         HS_12_per_capita = HS_12_cpi2016 / (mean.population_1k*1000))

# range of data
# range(invasion.global01$island, na.rm = TRUE)
range(invasion.global01$airport_density, na.rm = TRUE)
range(invasion.global01$seaport_density, na.rm = TRUE)
range(invasion.global01$gdp_per_capita, na.rm = TRUE)
range(invasion.global01$mean.area_km2, na.rm = TRUE)
range(invasion.global01$mean.population.density_km2, na.rm = TRUE)
range(invasion.global01$wc2.1_10m_bio_1, na.rm = TRUE)
range(invasion.global01$wc2.1_10m_bio_12, na.rm = TRUE)
range(invasion.global01$HS_06_per_capita, na.rm = TRUE)
range(invasion.global01$HS_07_per_capita, na.rm = TRUE)
range(invasion.global01$HS_08_per_capita, na.rm = TRUE)
range(invasion.global01$HS_12_per_capita, na.rm = TRUE)

# range(invasion.global01$HS_n_importers_avg, na.rm = TRUE)

# 2024-11-08
range(invasion.global01$HS_06_n_importers_avg, na.rm = TRUE)
range(invasion.global01$HS_07_n_importers_avg, na.rm = TRUE)
range(invasion.global01$HS_08_n_importers_avg, na.rm = TRUE)
range(invasion.global01$HS_12_n_importers_avg, na.rm = TRUE)

range(invasion.global01$HS_06_evenness_avg, na.rm = TRUE)
range(invasion.global01$HS_07_evenness_avg, na.rm = TRUE)
range(invasion.global01$HS_08_evenness_avg, na.rm = TRUE)
range(invasion.global01$HS_12_evenness_avg, na.rm = TRUE)

# convert as factor variable
invasion.global01$island <- as.factor(invasion.global01$island)

# data transformation, inlcuidiong log and scale transformation
invasion.global01$airport_density.scaled <- scale(log(invasion.global01$airport_density + 1))
invasion.global01$seaport_density.scaled <- scale(log(invasion.global01$seaport_density + 1))

invasion.global01$gdp_per_capita.scaled <- scale(log(invasion.global01$gdp_per_capita))
invasion.global01$mean.area_km2.scaled <- scale(log(invasion.global01$mean.area_km2))
invasion.global01$mean.population.density_km2.scaled <- scale(log(invasion.global01$mean.population.density_km2))

# wc2.1_10m_bio_1 + 10 to avoid the negative value before scaling
invasion.global01$wc2.1_10m_bio_1.scaled <- scale(log(invasion.global01$wc2.1_10m_bio_1 + 10))
invasion.global01$wc2.1_10m_bio_12.scaled <- scale(log(invasion.global01$wc2.1_10m_bio_12))

# HS Code
# 06	Trees and other plants, live; bulbs, roots and the like; cut flowers and ornamental foliage
# 07	Vegetables and certain roots and tubers; edible
# 08	Fruit and nuts, edible; peel of citrus fruit or melons
# 09	Coffee, tea, mate and spices
# 10 Cereals
# 11 Products of the milling industry; malt, starches, including, wheat gluten
# 12 Oil seeds and oleaginous fruits; miscellaneous grains, seeds and fruit, industrial or medicinal plants; straw and fodder
# 13 Lac; gums, resins and other vegetable saps and extracts
# 14 Vegetable plaiting materials; vegetable products not elsewhere specified or included
# TOTAL	All Commodities

# 09, 10, 11, 13, 14 non-living plant

invasion.global01$HS_06_per_capita.scaled <- scale(log(invasion.global01$HS_06_per_capita))
invasion.global01$HS_07_per_capita.scaled <- scale(log(invasion.global01$HS_07_per_capita))
invasion.global01$HS_08_per_capita.scaled <- scale(log(invasion.global01$HS_08_per_capita))
invasion.global01$HS_12_per_capita.scaled <- scale(log(invasion.global01$HS_12_per_capita))

# 新加数据
# invasion.global01$HS_n_importers_avg.scaled <- scale(log(invasion.global01$HS_n_importers_avg))

invasion.global01$HS_06_n_importers_avg.scaled <- scale(log(invasion.global01$HS_06_n_importers_avg))
invasion.global01$HS_07_n_importers_avg.scaled <- scale(log(invasion.global01$HS_07_n_importers_avg))
invasion.global01$HS_08_n_importers_avg.scaled <- scale(log(invasion.global01$HS_08_n_importers_avg))
invasion.global01$HS_12_n_importers_avg.scaled <- scale(log(invasion.global01$HS_12_n_importers_avg))

invasion.global01$HS_06_evenness_avg.scaled <- scale(invasion.global01$HS_06_evenness_avg)
invasion.global01$HS_07_evenness_avg.scaled <- scale(invasion.global01$HS_07_evenness_avg)
invasion.global01$HS_08_evenness_avg.scaled <- scale(invasion.global01$HS_08_evenness_avg)
invasion.global01$HS_12_evenness_avg.scaled <- scale(invasion.global01$HS_12_evenness_avg)

#
# range(invasion.global01$island, na.rm = TRUE)
range(invasion.global01$airport_density.scaled, na.rm = TRUE)
range(invasion.global01$seaport_density.scaled, na.rm = TRUE)
range(invasion.global01$gdp_per_capita.scaled, na.rm = TRUE)
range(invasion.global01$mean.area_km2.scaled, na.rm = TRUE)
range(invasion.global01$mean.population.density_km2.scaled, na.rm = TRUE)
range(invasion.global01$wc2.1_10m_bio_1.scaled, na.rm = TRUE)
range(invasion.global01$wc2.1_10m_bio_12.scaled, na.rm = TRUE)
range(invasion.global01$HS_06_per_capita.scaled, na.rm = TRUE)
range(invasion.global01$HS_07_per_capita.scaled, na.rm = TRUE)
range(invasion.global01$HS_08_per_capita.scaled, na.rm = TRUE)
range(invasion.global01$HS_12_per_capita.scaled, na.rm = TRUE)

# range(invasion.global01$HS_n_importers_avg.scaled, na.rm = TRUE)

range(invasion.global01$HS_06_n_importers_avg.scaled, na.rm = TRUE)
range(invasion.global01$HS_07_n_importers_avg.scaled, na.rm = TRUE)
range(invasion.global01$HS_08_n_importers_avg.scaled, na.rm = TRUE)
range(invasion.global01$HS_12_n_importers_avg.scaled, na.rm = TRUE)

range(invasion.global01$HS_06_evenness_avg.scaled, na.rm = TRUE)
range(invasion.global01$HS_07_evenness_avg.scaled, na.rm = TRUE)
range(invasion.global01$HS_08_evenness_avg.scaled, na.rm = TRUE)
range(invasion.global01$HS_12_evenness_avg.scaled, na.rm = TRUE)

# ==========================================================================
#  minimum_adequate_models for taxonomic diversity
# ==========================================================================
# variable selection
invasion.global01.SR <- invasion.global01 %>%
dplyr::select(SR,
       island,
       airport_density.scaled,
       seaport_density.scaled,
       gdp_per_capita.scaled,
       mean.area_km2.scaled,
       mean.population.density_km2.scaled,
       wc2.1_10m_bio_1.scaled,
       wc2.1_10m_bio_12.scaled,
       HS_06_per_capita.scaled,
       HS_07_per_capita.scaled,
       HS_08_per_capita.scaled,
       HS_12_per_capita.scaled,
       HS_06_n_importers_avg.scaled,
       HS_07_n_importers_avg.scaled,
       HS_08_n_importers_avg.scaled,
       HS_12_n_importers_avg.scaled,
       HS_06_evenness_avg.scaled,
       HS_07_evenness_avg.scaled,
       HS_08_evenness_avg.scaled,
       HS_12_evenness_avg.scaled) %>%
na.omit() %>%
as.data.frame()

#
str(invasion.global01.SR)

# data transformation of response variable
range(invasion.global01.SR$SR)

# glm full model
# 离散水平

model.SR <- glm.nb(SR ~
               island +
               airport_density.scaled +
               seaport_density.scaled +
               gdp_per_capita.scaled +
               mean.population.density_km2.scaled +
               wc2.1_10m_bio_1.scaled +
               wc2.1_10m_bio_12.scaled +
               HS_06_per_capita.scaled +
               HS_07_per_capita.scaled +
               HS_08_per_capita.scaled +
               HS_12_per_capita.scaled +
               HS_06_n_importers_avg.scaled +
               HS_07_n_importers_avg.scaled +
               HS_08_n_importers_avg.scaled +
               HS_12_n_importers_avg.scaled +
               HS_06_evenness_avg.scaled +
               HS_07_evenness_avg.scaled +
               HS_08_evenness_avg.scaled +
               HS_12_evenness_avg.scaled +
               offset(mean.area_km2.scaled),
               data = invasion.global01.SR,
               control = glm.control(maxit = 1000,  # 增加迭代次数
               epsilon = 1e-8  # 调整收敛标准
               ))

# stepwise model based on  AIC
step.model.SR00 <- step(model.SR)
step.model.SR00
summary(step.model.SR00)

# stepwise model based on VIF threshold
step.model.SR <- stepVIF(step.model.SR00, threshold = 5, verbose = TRUE)

vif(step.model.SR)
summary(step.model.SR)

# update model with manually removing the variable without significant p value

shapiro.test(resid(step.model.SR))
# check_overdispersion(step.model.SR)

# data cleaning
result.step.model.SR <- broom::tidy(step.model.SR)

# Use broom::tidy to organize step.model.SR and handle factor variables
result.step.model.SR <- result.step.model.SR %>%
mutate(term = str_replace(term, "islandyes", "island"))

result.step.model.SR

# Creates a data frame that corresponds to the name of the variable and its full name
var.df <- data.frame(
                     term = c("(Intercept)",
                              "island",
                              "airport_density.scaled",
                              "seaport_density.scaled",
                              "gdp_per_capita.scaled",
                              "mean.area_km2.scaled",
                              "mean.population.density_km2.scaled",
                              "wc2.1_10m_bio_1.scaled",
                              "wc2.1_10m_bio_12.scaled",
                              "HS_06_per_capita.scaled",
                              "HS_07_per_capita.scaled",
                              "HS_08_per_capita.scaled",
                              "HS_12_per_capita.scaled",
                              "HS_06_n_importers_avg.scaled",
                              "HS_07_n_importers_avg.scaled",
                              "HS_08_n_importers_avg.scaled",
                              "HS_12_n_importers_avg.scaled",
                              "HS_06_evenness_avg.scaled",
                              "HS_07_evenness_avg.scaled",
                              "HS_08_evenness_avg.scaled",
                              "HS_12_evenness_avg.scaled"
                              ),
                     full_name = c("Intercept",
                                   "Insularity",
                                   "Airport density",
                                   "Seaport density",
                                   "GDP per capita",
                                   "Area",
                                   "Population density",
                                   "Temperature",
                                   "Precipitation",
                                   "HS-06 per capita",
                                   "HS-07 per capita",
                                   "HS-08 per capita",
                                   "HS-12 per capita",
                                   "HS-06: no. importers",
                                   "HS-07: no. importers",
                                   "HS-08: no. importers",
                                   "HS-12: no. importers",
                                   "HS-06: evenness",
                                   "HS-07: evenness",
                                   "HS-08: evenness",
                                   "HS-12: evenness"
                                   ),
                     stringsAsFactors = FALSE
                     )

# # Contributions to the mixed effects model were calculated and extracted using delta and R2m
# r.step.model.SR <- glmm.hp(step.model.SR)
# r.step.model.SR

r.squaredGLMM(step.model.SR)
# > r.squaredGLMM(step.model.SR)
#                 R2m       R2c
# delta     0.5520449 0.5520449
# lognormal 0.6261526 0.6261526
# trigamma  0.4521406 0.4521406
# Warning message:
# the null model is correct only if all variables used by the original model remain unchanged.

# Integrate data from fixed name variables and format the results
result.step.model.SR03 <- var.df %>%
left_join(result.step.model.SR, by = "term")

result.step.model.SR04 <- result.step.model.SR03 %>%
filter(!is.na(p.value)) %>%
mutate(across(c(estimate:statistic), ~round(., 2))) %>%
mutate(across(c(p.value), ~round(., 3)))

result.step.model.SR04

# Save data
write_csv(result.step.model.SR, "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/NEW20241121v4.result.step.model.SR.csv")
write_csv(result.step.model.SR04, "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/NEW20241121v4.result.step.model.SR.more_info.csv")

# ==========================================================================
#  minimum_adequate_models for phylogenetic diversity
# ==========================================================================
# variable selection
invasion.global01.PD <- invasion.global01 %>%
dplyr::select(PD,
       island,
       airport_density.scaled,
       seaport_density.scaled,
       gdp_per_capita.scaled,
       mean.area_km2.scaled,
       mean.population.density_km2.scaled,
       wc2.1_10m_bio_1.scaled,
       wc2.1_10m_bio_12.scaled,
       HS_06_per_capita.scaled,
       HS_07_per_capita.scaled,
       HS_08_per_capita.scaled,
       HS_12_per_capita.scaled,
       HS_06_n_importers_avg.scaled,
       HS_07_n_importers_avg.scaled,
       HS_08_n_importers_avg.scaled,
       HS_12_n_importers_avg.scaled,
       HS_06_evenness_avg.scaled,
       HS_07_evenness_avg.scaled,
       HS_08_evenness_avg.scaled,
       HS_12_evenness_avg.scaled) %>%
na.omit() %>%
as.data.frame()

str(invasion.global01.PD)

# data transformation of response variable
invasion.global01.PD$PD.log <- log(invasion.global01.PD$PD)

# lm full model
model.PD00 <- lm(PD.log ~
               island +
               airport_density.scaled +
               seaport_density.scaled +
               gdp_per_capita.scaled +
               mean.area_km2.scaled +
               mean.population.density_km2.scaled +
               wc2.1_10m_bio_1.scaled +
               wc2.1_10m_bio_12.scaled +
               HS_06_per_capita.scaled +
               HS_07_per_capita.scaled +
               HS_08_per_capita.scaled +
               HS_12_per_capita.scaled +
               HS_06_n_importers_avg.scaled +
               HS_07_n_importers_avg.scaled +
               HS_08_n_importers_avg.scaled +
               HS_12_n_importers_avg.scaled +
               HS_06_evenness_avg.scaled +
               HS_07_evenness_avg.scaled +
               HS_08_evenness_avg.scaled +
               HS_12_evenness_avg.scaled,
      data = invasion.global01.PD
)

# stepwise model based on  AIC
step.model.PD00 <- step(model.PD00)
step.model.PD00

summary(model.PD00)
summary(step.model.PD00)

# stepwise model based on VIF threshold
step.model.PD <- stepVIF(step.model.PD00, threshold = 5, verbose = TRUE)
summary(step.model.PD)

# update model with manually removing the variable without significant p value
step.model.PD <- update(step.model.PD, . ~ . - gdp_per_capita.scaled)
summary(step.model.PD)

step.model.PD <- update(step.model.PD, . ~ . - HS_12_evenness_avg.scaled)
summary(step.model.PD)

vif(step.model.PD)
shapiro.test(resid(step.model.PD))

# data cleaning
result.step.model.PD <- broom::tidy(step.model.PD)
result.step.model.PD

# Use broom::tidy to organize step.model.SR and handle factor variables
result.step.model.PD <- result.step.model.PD %>%
mutate(term = str_replace(term, "islandyes", "island"))

result.step.model.PD

# Contributions to the mixed effects model were calculated and extracted using hierarchical.partitioning and Total.R2
r.step.model.PD <- glmm.hp(step.model.PD)
r.step.model.PD

r.step.model.PD$Total.R2
# > r.step.model.PD$Total.R2
# [1] 0.3337587

r.delta.step.model.PD <- r.step.model.PD$hierarchical.partitioning %>%
as.data.frame() %>%
rownames_to_column("term")

r.delta.step.model.PD

# Merging glm result data with contribution rate data
result.step.model.PD02 <- result.step.model.PD %>%
left_join(r.delta.step.model.PD, by = "term")

result.step.model.PD02

# Integrate data from fixed name variables and format the results
result.step.model.PD03 <- var.df %>%
left_join(result.step.model.PD02, by = "term")

result.step.model.PD04 <-result.step.model.PD03 %>%
filter(!is.na(p.value)) %>%
mutate(across(c(estimate:statistic), round, 2)) %>%
mutate(across(c(p.value, Individual), round, 3))

result.step.model.PD04

# save data
write_csv(result.step.model.PD, "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/NEW20241121v4.result.step.model.PD.csv")
write_csv(result.step.model.PD04, "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/NEW20241121v4.result.step.model.PD.more_info.csv")

# ==========================================================================
#  minimum_adequate_models for MPD
# ==========================================================================
# variable selection
invasion.global01.mpd.obs <- invasion.global01 %>%
dplyr::select(mpd.obs,
       island,
       airport_density.scaled,
       seaport_density.scaled,
       gdp_per_capita.scaled,
       mean.area_km2.scaled,
       mean.population.density_km2.scaled,
       wc2.1_10m_bio_1.scaled,
       wc2.1_10m_bio_12.scaled,
       HS_06_per_capita.scaled,
       HS_07_per_capita.scaled,
       HS_08_per_capita.scaled,
       HS_12_per_capita.scaled,
       HS_06_n_importers_avg.scaled,
       HS_07_n_importers_avg.scaled,
       HS_08_n_importers_avg.scaled,
       HS_12_n_importers_avg.scaled,
       HS_06_evenness_avg.scaled,
       HS_07_evenness_avg.scaled,
       HS_08_evenness_avg.scaled,
       HS_12_evenness_avg.scaled) %>%
na.omit() %>%
as.data.frame()

# data transformation of response variable
invasion.global01.mpd.obs$mpd.obs.log <- log(invasion.global01.mpd.obs$mpd.obs)

# lm full model
model.mpd.obs00 <- lm(mpd.obs.log ~
               island +
               airport_density.scaled +
               seaport_density.scaled +
               gdp_per_capita.scaled +
               mean.area_km2.scaled +
               mean.population.density_km2.scaled +
               wc2.1_10m_bio_1.scaled +
               wc2.1_10m_bio_12.scaled +
               HS_06_per_capita.scaled +
               HS_07_per_capita.scaled +
               HS_08_per_capita.scaled +
               HS_12_per_capita.scaled +
               HS_06_n_importers_avg.scaled +
               HS_07_n_importers_avg.scaled +
               HS_08_n_importers_avg.scaled +
               HS_12_n_importers_avg.scaled +
               HS_06_evenness_avg.scaled +
               HS_07_evenness_avg.scaled +
               HS_08_evenness_avg.scaled +
               HS_12_evenness_avg.scaled,
               data = invasion.global01.mpd.obs
)

# stepwise model based on AIC
step.model.mpd.obs00 <- step(model.mpd.obs00)

summary(model.mpd.obs00)
summary(step.model.mpd.obs00)

# stepwise model based on VIF threshold
step.model.mpd.obs <- stepVIF(step.model.mpd.obs00, threshold = 5, verbose = TRUE)

vif(step.model.mpd.obs)
summary(step.model.mpd.obs)

#
step.model.mpd.obs <- update(step.model.mpd.obs, . ~ . - wc2.1_10m_bio_1.scaled)
summary(step.model.mpd.obs)

step.model.mpd.obs <- update(step.model.mpd.obs, . ~ . - seaport_density.scaled)
summary(step.model.mpd.obs)

step.model.mpd.obs <- update(step.model.mpd.obs, . ~ . - island)
summary(step.model.mpd.obs)

step.model.mpd.obs <- update(step.model.mpd.obs, . ~ . - HS_07_evenness_avg.scaled)
summary(step.model.mpd.obs)

vif(step.model.mpd.obs)
summary(step.model.mpd.obs)
shapiro.test(resid(step.model.mpd.obs))

# data cleaning
result.step.model.mpd.obs <- broom::tidy(step.model.mpd.obs)
result.step.model.mpd.obs

# # Use broom::tidy to organize step.model.SR and handle factor variables
# result.step.model.mpd.obs <- result.step.model.mpd.obs %>%
# mutate(term = str_replace(term, "islandyes", "island"))

# result.step.model.mpd.obs

# Contributions to the mixed effects model were calculated and extracted using hierarchical.partitioning and Total.R2
r.step.model.mpd.obs <- glmm.hp(step.model.mpd.obs)
r.step.model.mpd.obs
r.step.model.mpd.obs$Total.R2
# > r.step.model.mpd.obs$Total.R2
# [1] 0.1139483

r.delta.step.model.mpd.obs <- r.step.model.mpd.obs$hierarchical.partitioning %>%
as.data.frame() %>%
rownames_to_column("term")

r.delta.step.model.mpd.obs

# Merging glm result data with contribution rate data
result.step.model.mpd.obs02 <- result.step.model.mpd.obs %>%
left_join(r.delta.step.model.mpd.obs, by = "term")

result.step.model.mpd.obs02

# Integrate data from fixed name variables and format the results
result.step.model.mpd.obs03 <- var.df %>%
left_join(result.step.model.mpd.obs02, by = "term")

result.step.model.mpd.obs04 <-result.step.model.mpd.obs03 %>%
filter(!is.na(p.value)) %>%
mutate(across(c(estimate:statistic), round, 2)) %>%
mutate(across(c(p.value, Individual), round, 3))

result.step.model.mpd.obs04

# Save data
write_csv(result.step.model.mpd.obs, "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/NEW20241121v4.result.step.model.mpd.obs.csv")
write_csv(result.step.model.mpd.obs04, "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/NEW20241121v4.result.step.model.mpd.obs.more_info.csv")

# ==========================================================================
#  minimum_adequate_models for MNTD
# ==========================================================================
# variable selection
invasion.global01.mntd.obs <- invasion.global01 %>%
dplyr::select(mntd.obs,
       island,
       airport_density.scaled,
       seaport_density.scaled,
       gdp_per_capita.scaled,
       mean.area_km2.scaled,
       mean.population.density_km2.scaled,
       wc2.1_10m_bio_1.scaled,
       wc2.1_10m_bio_12.scaled,
       HS_06_per_capita.scaled,
       HS_07_per_capita.scaled,
       HS_08_per_capita.scaled,
       HS_12_per_capita.scaled,
       HS_06_n_importers_avg.scaled,
       HS_07_n_importers_avg.scaled,
       HS_08_n_importers_avg.scaled,
       HS_12_n_importers_avg.scaled,
       HS_06_evenness_avg.scaled,
       HS_07_evenness_avg.scaled,
       HS_08_evenness_avg.scaled,
       HS_12_evenness_avg.scaled) %>%
na.omit() %>%
as.data.frame()

# data transformation of response variable
invasion.global01.mntd.obs$mntd.obs.log <- log(invasion.global01.mntd.obs$mntd.obs)

# lm full model
model.mntd.obs00 <- lm(mntd.obs.log ~
               island +
               airport_density.scaled +
               seaport_density.scaled +
               gdp_per_capita.scaled +
               mean.area_km2.scaled +
               mean.population.density_km2.scaled +
               wc2.1_10m_bio_1.scaled +
               wc2.1_10m_bio_12.scaled +
               HS_06_per_capita.scaled +
               HS_07_per_capita.scaled +
               HS_08_per_capita.scaled +
               HS_12_per_capita.scaled +
               HS_06_n_importers_avg.scaled +
               HS_07_n_importers_avg.scaled +
               HS_08_n_importers_avg.scaled +
               HS_12_n_importers_avg.scaled +
               HS_06_evenness_avg.scaled +
               HS_07_evenness_avg.scaled +
               HS_08_evenness_avg.scaled +
               HS_12_evenness_avg.scaled,
      data = invasion.global01.mntd.obs
)

# stepwise model based on  AIC
step.model.mntd.obs00 <- step(model.mntd.obs00)
step.model.mntd.obs00

summary(model.mntd.obs00)
summary(step.model.mntd.obs00)

# stepwise model based on VIF threshold
step.model.mntd.obs <- stepVIF(step.model.mntd.obs00, threshold = 5, verbose = TRUE)
vif(step.model.mntd.obs)

step.model.mntd.obs <- update(step.model.mntd.obs, . ~ . - HS_08_per_capita.scaled)
summary(step.model.mntd.obs)

step.model.mntd.obs <- update(step.model.mntd.obs, . ~ . - HS_12_per_capita.scaled)
summary(step.model.mntd.obs)

vif(step.model.mntd.obs)
summary(step.model.mntd.obs)
shapiro.test(resid(step.model.mntd.obs))

result.step.model.mntd.obs <- broom::tidy(step.model.mntd.obs)
result.step.model.mntd.obs

# Use broom::tidy to organize step.model.SR and handle factor variables
result.step.model.mntd.obs <- result.step.model.mntd.obs %>%
mutate(term = str_replace(term, "islandyes", "island"))

result.step.model.mntd.obs

# Contributions to the mixed effects model were calculated and extracted using hierarchical.partitioning and Total.R2
r.step.model.mntd.obs <- glmm.hp(step.model.mntd.obs)
r.step.model.mntd.obs
r.step.model.mntd.obs$Total.R2
# > r.step.model.mntd.obs$Total.R2
# [1] 0.1987157

r.delta.step.model.mntd.obs <- r.step.model.mntd.obs$hierarchical.partitioning %>%
as.data.frame() %>%
rownames_to_column("term")

r.delta.step.model.mntd.obs

# Merging glm result data with contribution rate data
result.step.model.mntd.obs02 <- result.step.model.mntd.obs %>%
left_join(r.delta.step.model.mntd.obs, by = "term")

result.step.model.mntd.obs02

# Integrate data from fixed name variables and format the results
result.step.model.mntd.obs03 <- var.df %>%
left_join(result.step.model.mntd.obs02, by = "term")

result.step.model.mntd.obs04 <-result.step.model.mntd.obs03 %>%
filter(!is.na(p.value)) %>%
mutate(across(c(estimate:statistic), round, 2)) %>%
mutate(across(c(p.value, Individual), round, 3))

result.step.model.mntd.obs04

# save data
write_csv(result.step.model.mntd.obs, "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/NEW20241121v4.result.step.model.mntd.obs.csv")
write_csv(result.step.model.mntd.obs04, "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/NEW20241121v4.result.step.model.mntd.obs.more_info.csv")

# ==========================================================================
# Plotting code
# taxonomic diversity
# ==========================================================================
library(purrr)
library(sjPlot)
library(ggplot2)

# 获取预测变量名
predictors.SR <- names(coef(step.model.SR))[-1]
predictors.SR <- str_replace(predictors.SR, "islandyes", "island")

# 使用 map 函数创建图形列表
pred.SR <- map(predictors.SR, function(predictor) {
  plot_model(step.model.SR,
             type = "eff",
             terms = predictor,
             show.data = TRUE,
             dot.size = 4)
}) %>%
  set_names(predictors.SR)

do.call("grid.arrange", c(pred.SR, ncol = 2))

# Define a function to create the customized plot for all continuous variables
customized_plot <- function(plot_object, x_label, y_label, title,...) {
  plot_object +
  # geom_line(size = 1, color = "red") +
    theme_classic() +
    theme(
      axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
      axis.text = element_text(family = "serif", colour = "black", size = 14),
      axis.title = element_text(family = "serif", colour = "black", size = 14),
      legend.title = element_text(family = "serif", colour = "black", size = 14),
      legend.text = element_text(family = "serif", colour = "black", size = 14)
    ) +
    labs(x = x_label, y = y_label) +
    ggtitle(title)

}

# Define a function to create the box plot for island variable
create_island_boxplot <- function(step.model, y_limit = 8, y_break = 2) {
  # get data
  plot_data <- get_model_data(step.model, type = "eff", terms = "island")

  plot_data01 <- data.frame(plot_data)
  plot_data01$x <- factor(plot_data01$x, levels = c(1,2))

  rawdata <- attr(plot_data, "rawdata")
  rawdata$x <- factor(rawdata$x, levels = c(1,2))

  y_lab <- attr(plot_data, "title")

  # Creating a Box-and-Line Chart
  boxplot_plot <- ggplot(rawdata, aes(x = x, y = response, fill = x)) +
    geom_violin(color = NA, width = 0.7, alpha = 0.7, trim = TRUE) +
    # geom_boxplot(width = 0.5, alpha = 0.7) +
    scale_fill_manual(values = c("#D95F02", "#7570B3")) +
    scale_y_continuous(limits = c(0, y_limit), breaks = seq(0, y_limit, by = y_break)) +
    scale_x_discrete(breaks = c(1, 2), labels = c("no", "yes")) +
    labs(x = "Island country", y = y_lab, title = "") +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
      axis.text = element_text(family = "serif", colour = "black", size = 14),
      axis.title = element_text(family = "serif", colour = "black", size = 14),
      legend.title = element_text(family = "serif", colour = "black", size = 14),
      legend.text = element_text(family = "serif", colour = "black", size = 14)
    )

  # Add predicted values and confidence intervals
  final_plot <- boxplot_plot +
    geom_pointrange(data = plot_data01, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                    color = "grey30", size = 2, fatten = 2)

  return(final_plot)
}

# > range(invasion.global01.SR$SR)
# [1]    2 1188
# range(invasion.global01$island, na.rm = TRUE)
# range(invasion.global01$airport_density.scaled, na.rm = TRUE)
# [1] -1.778696  2.979436
# range(invasion.global01$seaport_density.scaled, na.rm = TRUE)
# [1] -1.063177  2.684560
# range(invasion.global01$gdp_per_capita.scaled, na.rm = TRUE)
# [1] -2.246206  2.103268
# range(invasion.global01$mean.area_km2.scaled, na.rm = TRUE)
# [1] -2.575237  2.103174
# range(invasion.global01$mean.population.density_km2.scaled, na.rm = TRUE)
# [1] -2.510949  3.681997
# range(invasion.global01$wc2.1_10m_bio_1.scaled, na.rm = TRUE)
# [1] -5.1956191  0.9284558
# range(invasion.global01$wc2.1_10m_bio_12.scaled, na.rm = TRUE)
# [1] -4.116276  1.541122
# range(invasion.global01$HS_06_per_capita.scaled, na.rm = TRUE)
# [1] -2.613132  1.810212
# range(invasion.global01$HS_07_per_capita.scaled, na.rm = TRUE)
# [1] -2.278886  1.728552
# range(invasion.global01$HS_08_per_capita.scaled, na.rm = TRUE)
# [1] -2.81482  1.76974
# range(invasion.global01$HS_12_per_capita.scaled, na.rm = TRUE)
# [1] -3.795134  1.954461

# Create the effect plots using the custom function
# pacman::p_load(ggbreak)
library(ggbreak)

plot.island.SR <- create_island_boxplot(step.model = step.model.SR) +
scale_y_continuous(limits = c(0, 1250), breaks = seq(0, 1200, by = 200)) +
labs(y = "Taxonomic diversity") +
scale_y_break(breaks = c(500, 1000),
                space = 0.5,  # 断点空间大小
                scales = 0.2) +  # 允许上下部分使用不同的缩放
theme(
    axis.text.y.right = element_blank(),    # 移除右侧刻度文本
    axis.ticks.y.right = element_blank(),   # 移除右侧刻度线
    axis.line.y.right = element_blank(),    # 移除右侧轴线
    axis.title.y.right = element_blank()    # 移除右侧y轴标题
  )

plot.island.SR

# Create the effect plots using the custom function
plot.airport_density.scaled.SR <- customized_plot(pred.SR$airport_density.scaled,
                                  "Log(airport density)(scaled)",
                                  "SR",
                                  "") +
scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 3, by = 1)) +
scale_y_continuous(limits = c(0, 1250), breaks = seq(0, 1200, by = 200)) +
labs(y = "Taxonomic diversity") +
scale_y_break(breaks = c(500, 1000),
                space = 0.5,  # 断点空间大小
                scales = 0.2) +  # 允许上下部分使用不同的缩放
theme(
    axis.text.y.right = element_blank(),    # 移除右侧刻度文本
    axis.ticks.y.right = element_blank(),   # 移除右侧刻度线
    axis.line.y.right = element_blank(),    # 移除右侧轴线
    axis.title.y.right = element_blank()    # 移除右侧y轴标题
  )

plot.airport_density.scaled.SR

# Create the effect plots using the custom function
plot.wc2.1_10m_bio_1.scaled.SR <- customized_plot(pred.SR$wc2.1_10m_bio_1.scaled,
                                  "Log(MAT; \u00B0C)(scaled)",
                                  "SR",
                                  "") +
scale_x_continuous(limits = c(-5.5, 1), breaks = seq(-5, 1, by = 1)) +
scale_y_continuous(limits = c(0, 1250), breaks = seq(0, 1200, by = 200)) +
labs(y = "Taxonomic diversity") +
scale_y_break(breaks = c(500, 1000),
                space = 0.5,  # 断点空间大小
                scales = 0.2) +  # 允许上下部分使用不同的缩放
theme(
    axis.text.y.right = element_blank(),    # 移除右侧刻度文本
    axis.ticks.y.right = element_blank(),   # 移除右侧刻度线
    axis.line.y.right = element_blank(),    # 移除右侧轴线
    axis.title.y.right = element_blank()    # 移除右侧y轴标题
  )

plot.wc2.1_10m_bio_1.scaled.SR

# Create the effect plots using the custom function
plot.wc2.1_10m_bio_12.scaled.SR <- customized_plot(pred.SR$wc2.1_10m_bio_12.scaled,
                                  "Log(MAP; mm)(scaled)",
                                  "SR",
                                  "") +
scale_x_continuous(limits = c(-5, 2), breaks = seq(-5, 2, by = 1)) +
scale_y_continuous(limits = c(0, 1250), breaks = seq(0, 1200, by = 200)) +
labs(y = "Taxonomic diversity") +
scale_y_break(breaks = c(500, 1000),
                space = 0.5,  # 断点空间大小
                scales = 0.2) +  # 允许上下部分使用不同的缩放
theme(
    axis.text.y.right = element_blank(),    # 移除右侧刻度文本
    axis.ticks.y.right = element_blank(),   # 移除右侧刻度线
    axis.line.y.right = element_blank(),    # 移除右侧轴线
    axis.title.y.right = element_blank()    # 移除右侧y轴标题
  )

plot.wc2.1_10m_bio_12.scaled.SR

# Create the effect plots using the custom function
plot.HS_08_per_capita.scaled.SR <- customized_plot(pred.SR$HS_08_per_capita.scaled,
                                  "Log(HS-08 per capita)(scaled)",
                                  "SR",
                                  "") +
scale_x_continuous(limits = c(-3, 2), breaks = seq(-3, 2, by = 1)) +
scale_y_continuous(limits = c(0, 1250), breaks = seq(0, 1200, by = 200)) +
labs(y = "Taxonomic diversity") +
scale_y_break(breaks = c(500, 1000),
                space = 0.5,  # 断点空间大小
                scales = 0.2) +  # 允许上下部分使用不同的缩放
theme(
    axis.text.y.right = element_blank(),    # 移除右侧刻度文本
    axis.ticks.y.right = element_blank(),   # 移除右侧刻度线
    axis.line.y.right = element_blank(),    # 移除右侧轴线
    axis.title.y.right = element_blank()    # 移除右侧y轴标题
  )

plot.HS_08_per_capita.scaled.SR

# Create the effect plots using the custom function
plot.HS_12_per_capita.scaled.SR <- customized_plot(pred.SR$HS_12_per_capita.scaled,
                                  "Log(HS-12 per capita)(scaled)",
                                  "SR",
                                  "") +
scale_x_continuous(limits = c(-4, 2), breaks = seq(-4, 2, by = 1)) +
scale_y_continuous(limits = c(0, 1250), breaks = seq(0, 1200, by = 200)) +
labs(y = "Taxonomic diversity") +
scale_y_break(breaks = c(500, 1000),
                space = 0.5,  # 断点空间大小
                scales = 0.2) +  # 允许上下部分使用不同的缩放
theme(
    axis.text.y.right = element_blank(),    # 移除右侧刻度文本
    axis.ticks.y.right = element_blank(),   # 移除右侧刻度线
    axis.line.y.right = element_blank(),    # 移除右侧轴线
    axis.title.y.right = element_blank()    # 移除右侧y轴标题
  )

plot.HS_12_per_capita.scaled.SR

# Create the effect plots using the custom function
plot.HS_07_evenness_avg.scaled.SR <- customized_plot(pred.SR$HS_07_evenness_avg.scaled,
                                  "HS-07: evenness (scaled)",
                                  "SR",
                                  "") +
scale_x_continuous(limits = c(-4, 2), breaks = seq(-4, 2, by = 1)) +
scale_y_continuous(limits = c(0, 1250), breaks = seq(0, 1200, by = 200)) +
labs(y = "Taxonomic diversity") +
scale_y_break(breaks = c(500, 1000),
                space = 0.5,  # 断点空间大小
                scales = 0.2) +  # 允许上下部分使用不同的缩放
theme(
    axis.text.y.right = element_blank(),    # 移除右侧刻度文本
    axis.ticks.y.right = element_blank(),   # 移除右侧刻度线
    axis.line.y.right = element_blank(),    # 移除右侧轴线
    axis.title.y.right = element_blank()    # 移除右侧y轴标题
  )

plot.HS_07_evenness_avg.scaled.SR

# Create the effect plots using the custom function
plot.HS_12_evenness_avg.scaled.SR <- customized_plot(pred.SR$HS_12_evenness_avg.scaled,
                                  "HS-12: evenness (scaled)",
                                  "SR",
                                  "") +
scale_x_continuous(limits = c(-3, 2), breaks = seq(-3, 2, by = 1)) +
scale_y_continuous(limits = c(0, 1250), breaks = seq(0, 1200, by = 200)) +
labs(y = "Taxonomic diversity") +
scale_y_break(breaks = c(500, 1000),
                space = 0.5,  # 断点空间大小
                scales = 0.2) +  # 允许上下部分使用不同的缩放
theme(
    axis.text.y.right = element_blank(),    # 移除右侧刻度文本
    axis.ticks.y.right = element_blank(),   # 移除右侧刻度线
    axis.line.y.right = element_blank(),    # 移除右侧轴线
    axis.title.y.right = element_blank()    # 移除右侧y轴标题
  )

plot.HS_12_evenness_avg.scaled.SR

# wrap single plots
p1 <- plot.island.SR + labs(tag = "A")
p2 <- plot.airport_density.scaled.SR + labs(tag = "B")
p3 <- plot.wc2.1_10m_bio_1.scaled.SR + labs(tag = "C")
p4 <- plot.wc2.1_10m_bio_12.scaled.SR + labs(tag = "D")
p5 <- plot.HS_08_per_capita.scaled.SR + labs(tag = "E")
p6 <- plot.HS_12_per_capita.scaled.SR + labs(tag = "F")
p7 <- plot.HS_07_evenness_avg.scaled.SR + labs(tag = "G")
p8 <- plot.HS_12_evenness_avg.scaled.SR + labs(tag = "H")

plots.SR <- (p1 + p5) / (p2 + p6) / (p3 + p7) / (p4 + p8)
plots.SR

# save image
ggexport(plots.SR, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/NEW20241121v4.vif.plots.step.TD.png",
    width = 2400,
    height = 3600,
    pointsize = 12,
    res = 300)

# ==========================================================================
# phylogenetic diversity
# ==========================================================================
# 获取预测变量名
predictors.PD <- names(coef(step.model.PD))[-1]
predictors.PD <- str_replace(predictors.PD, "islandyes", "island")

# 使用 map 函数创建图形列表
pred.PD <- map(predictors.PD, function(predictor) {
  plot_model(step.model.PD,
             type = "eff",
             terms = predictor,
             show.data = TRUE,
             dot.size = 4)
}) %>%
  set_names(predictors.PD)

do.call("grid.arrange", c(pred.PD, ncol = 2))

# Create the effect plots using the custom function
plot.island.PD <- create_island_boxplot(step.model = step.model.PD) +
labs(y = "Log(phylogenetic diversity; Mya)") +
scale_y_continuous(limits = c(4, 12), breaks = seq(4, 12, by = 2))

plot.island.PD

# Create the effect plots using the custom function
plot.mean.area_km2.scaled.PD <- customized_plot(pred.PD$mean.area_km2.scaled,
                                  "Log(area; km\u00B2)(scaled)",
                                  "Log(phylogenetic diversity; Mya)",
                                  "") +
scale_x_continuous(limits = c(-3, 2.2), breaks = seq(-3, 2, by = 1)) +
scale_y_continuous(limits = c(4, 12), breaks = seq(4, 12, by = 2))

plot.mean.area_km2.scaled.PD

# Create the effect plots using the custom function
plot.wc2.1_10m_bio_1.scaled.PD <- customized_plot(pred.PD$wc2.1_10m_bio_1.scaled,
                                  "Log(MAT; \u00B0C)(scaled)",
                                  "Log(phylogenetic diversity; Mya)",
                                  "") +
scale_x_continuous(limits = c(-6, 1), breaks = seq(-6, 1, by = 1)) +
scale_y_continuous(limits = c(4, 12), breaks = seq(4, 12, by = 2))

plot.wc2.1_10m_bio_1.scaled.PD

# Create the effect plots using the custom function
plot.wc2.1_10m_bio_12.scaled.PD <- customized_plot(pred.PD$wc2.1_10m_bio_12.scaled,
                                  "Log(MAP; mm)(scaled)",
                                  "Log(phylogenetic diversity; Mya)",
                                  "") +
scale_x_continuous(limits = c(-5, 2), breaks = seq(-5, 2, by = 1)) +
scale_y_continuous(limits = c(4, 12), breaks = seq(4, 12, by = 2))

plot.wc2.1_10m_bio_12.scaled.PD

# plot.wc2.1_10m_bio_1.scaled.PD
plot.HS_12_per_capita.scaled.PD <- customized_plot(pred.PD$HS_12_per_capita.scaled,
                                  "Log(HS-12 per capita)(scaled)",
                                  "Log(phylogenetic diversity; Mya)",
                                  "") +
scale_x_continuous(limits = c(-4, 2), breaks = seq(-4, 2, by = 1)) +
scale_y_continuous(limits = c(4, 12), breaks = seq(4, 12, by = 2))

plot.HS_12_per_capita.scaled.PD

# plot.HS_07_evenness_avg.scaled.PD
plot.HS_07_evenness_avg.scaled.PD <- customized_plot(pred.PD$HS_07_evenness_avg.scaled,
                                  "HS-07: evenness (scaled)",
                                  "Log(phylogenetic diversity; Mya)",
                                  "") +
scale_x_continuous(limits = c(-4, 2), breaks = seq(-4, 2, by = 1)) +
scale_y_continuous(limits = c(4, 12), breaks = seq(4, 12, by = 2))

plot.HS_07_evenness_avg.scaled.PD

# wrap single plots
plots.PD <-
plot.mean.area_km2.scaled.PD +
plot.island.PD +
plot.wc2.1_10m_bio_1.scaled.PD +
plot.wc2.1_10m_bio_12.scaled.PD +
plot.HS_12_per_capita.scaled.PD +
plot.HS_07_evenness_avg.scaled.PD +
plot_layout(ncol = 2, byrow = FALSE) +
plot_annotation(tag_levels = 'A')

plots.PD

# Save Image
ggexport(plots.PD, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/NEW20241121v4.vif.plots.step.PD.png",
    width = 2400,
    height = 3000,
    pointsize = 12,
    res = 300)

# ==========================================================================
# MPD
# ==========================================================================
# 获取预测变量名
summary(step.model.mpd.obs)
predictors.mpd.obs <- names(coef(step.model.mpd.obs))[-1]
predictors.mpd.obs <- str_replace(predictors.mpd.obs, "islandyes", "island")

# 使用 map 函数创建图形列表
pred.mpd.obs <- map(predictors.mpd.obs, function(predictor) {
  plot_model(step.model.mpd.obs,
             type = "eff",
             terms = predictor,
             show.data = TRUE,
             dot.size = 4)
}) %>%
  set_names(predictors.mpd.obs)

do.call("grid.arrange", c(pred.mpd.obs, ncol = 2))

# Create the effect plots using the custom function
plot.mean.area_km2.scaled.mpd.obs <- customized_plot(pred.mpd.obs$mean.area_km2.scaled,
                                  "Log(area; km\u00B2)(scaled)",
                                  "Log(MPD; Mya)",
                                  "") +
scale_x_continuous(limits = c(-3, 2.2), breaks = seq(-3, 2, by = 1)) +
scale_y_continuous(limits = c(5, 6.5), breaks = seq(5, 6.5, by = 0.5))

plot.mean.area_km2.scaled.mpd.obs

# Create the effect plots using the custom function
plot.airport_density.scaled.mpd.obs <- customized_plot(pred.mpd.obs$airport_density.scaled,
                                  "Log(airport density)(scaled)",
                                  "Log(MPD; Mya)",
                                  "") +
scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 3, by = 1)) +
scale_y_continuous(limits = c(5, 6.5), breaks = seq(5, 6.5, by = 0.5))

plot.airport_density.scaled.mpd.obs

# plot.wc2.1_10m_bio_1.scaled.mpd.obs
plot.HS_12_n_importers_avg.scaled.mpd.obs <- customized_plot(pred.mpd.obs$HS_12_n_importers_avg.scaled,
                                  "Log(HS-12: no. source countries)\n(scaled)",
                                  "Log(MPD; Mya)",
                                  "") +
scale_x_continuous(limits = c(-4, 2), breaks = seq(-4, 2, by = 1)) +
scale_y_continuous(limits = c(5, 6.5), breaks = seq(5, 6.5, by = 0.5))

plot.HS_12_n_importers_avg.scaled.mpd.obs

# wrap single plots
plots.mpd.obs <-
plot.mean.area_km2.scaled.mpd.obs +
plot.airport_density.scaled.mpd.obs +
plot.HS_12_n_importers_avg.scaled.mpd.obs +
plot_layout(ncol = 2, byrow = FALSE) +
plot_annotation(tag_levels = 'A')

plots.mpd.obs

# save image
ggexport(plots.mpd.obs, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/NEW20241121v4.plots.step.mpd.obs.png",
    width = 2400,
    height = 2400,
    pointsize = 12,
    res = 300)

# ==========================================================================
# MNTD
# ==========================================================================
# 获取预测变量名
summary(step.model.mntd.obs)
predictors.mntd.obs <- names(coef(step.model.mntd.obs))[-1]
predictors.mntd.obs <- str_replace(predictors.mntd.obs, "islandyes", "island")

# 使用 map 函数创建图形列表
pred.mntd.obs <- map(predictors.mntd.obs, function(predictor) {
  plot_model(step.model.mntd.obs,
             type = "eff",
             terms = predictor,
             show.data = TRUE,
             dot.size = 4)
}) %>%
  set_names(predictors.mntd.obs)

do.call("grid.arrange", c(pred.mntd.obs, ncol = 2))

# Create the effect plots using the custom function
plot.island.mntd.obs <- create_island_boxplot(step.model = step.model.mntd.obs) +
labs(y = "Log(MNTD; Mya)") +
scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, by = 1))

plot.island.mntd.obs

# Create the effect plots using the custom function
plot.mean.area_km2.scaled.mntd.obs <- customized_plot(pred.mntd.obs$mean.area_km2.scaled,
                                  "Log(area; km\u00B2)(scaled)",
                                  "Log(MNTD; Mya)",
                                  "") +
scale_x_continuous(limits = c(-3, 2.2), breaks = seq(-3, 2, by = 1)) +
scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, by = 1))

plot.mean.area_km2.scaled.mntd.obs

# Create the effect plots using the custom function
plot.wc2.1_10m_bio_1.scaled.mntd.obs <- customized_plot(pred.mntd.obs$wc2.1_10m_bio_1.scaled,
                                  "Log(MAT; \u00B0C)(scaled)",
                                  "Log(MNTD; Mya)",
                                  "") +
scale_x_continuous(limits = c(-6, 1), breaks = seq(-6, 1, by = 1)) +
scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, by = 1))
plot.wc2.1_10m_bio_1.scaled.mntd.obs

# wrap single plots
plots.mntd.obs <-
plot.mean.area_km2.scaled.mntd.obs +
plot.island.mntd.obs +
plot.wc2.1_10m_bio_1.scaled.mntd.obs +
plot_layout(ncol = 2, byrow = FALSE) +
plot_annotation(tag_levels = 'A')

plots.mntd.obs

# save image
ggexport(plots.mntd.obs, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/NEW20241121v4.plots.step.mntd.obs.png",
    width = 2400,
    height = 2400,
    pointsize = 12,
    res = 300)

# ==========================================================================
# MPD + MNTD
# ==========================================================================
# 定义布局矩阵
layout <- "
AD
BE
CF
"

plots.mpdmntd.obs <-
  plot.mean.area_km2.scaled.mpd.obs +
  plot.airport_density.scaled.mpd.obs +
  plot.HS_12_n_importers_avg.scaled.mpd.obs +
  plot.mean.area_km2.scaled.mntd.obs +
  plot.island.mntd.obs +
  plot.wc2.1_10m_bio_1.scaled.mntd.obs +
  plot_layout(ncol = 2, byrow = FALSE) +
  plot_annotation(tag_levels = 'A')

plots.mpdmntd.obs

# save image
ggexport(plots.mpdmntd.obs, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/NEW20241121v4.plots.step.mpdmntd.obs.png",
    width = 2400,
    height = 3000,
    pointsize = 12,
    res = 300)
