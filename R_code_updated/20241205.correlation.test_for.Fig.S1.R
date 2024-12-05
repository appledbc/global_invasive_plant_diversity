# -*- coding: utf-8 -*-
# @Author:
# @Date: 2023-07-29 20:50:12
# @Last Modified by: dbc
# @Last Modified time: 2024-12-05 15:57:15
# @Description: Figure S1

# loading packages
library(dplyr)
library(ggcorrplot)
library(ggpubr)
library(readr)
library(tidyverse)


# file path
path <- "D:/我的坚果云/2023.liyuan_database/新GADM数据集"

# set file path
setwd(path)
getwd()

# load data
invasion.global <- read_csv("D:/我的坚果云/2023.liyuan_database/新GADM数据集/20241107.iso_upadted_v14.comtrade.updated.csv")
glimpse(invasion.global)
# str(invasion.global)

# view data
# View(invasion.global)

# 调整代码
invasion.global01 <- invasion.global %>%
filter(!(alpha_3 %in% c("TWN", "MAC", "HKG")))  %>%
filter(!is.na(updated_SR)) %>%
dplyr::select(-c("PD", "SR", "mpd.obs", "mntd.obs", "HS_06_cpi2016", "HS_07_cpi2016", "HS_08_cpi2016", "HS_12_cpi2016")) %>%
rename(SR  = updated_SR,
       PD = updated_PD,
       mpd.obs = updated_mpd.obs,
       mntd.obs = updated_mntd.obs,
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

# variable selection
invasion.global01.vars <- invasion.global01 %>%
dplyr::select(airport_density.scaled,
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

str(invasion.global01.vars)

# ==========================================================================
# correlation analysis
# ==========================================================================
# Creates a data frame that corresponds to the name of the variable and its full name
var.fullName <- c(airport_density.scaled = "Airport density",
                              seaport_density.scaled = "Seaport density",
                              gdp_per_capita.scaled = "GDP per capita",
                              mean.area_km2.scaled = "Area",
                              mean.population.density_km2.scaled = "Population density",
                              wc2.1_10m_bio_1.scaled = "Temperature",
                              wc2.1_10m_bio_12.scaled = "Precipitation",
                              HS_06_per_capita.scaled = "HS-06 per capita",
                              HS_07_per_capita.scaled = "HS-07 per capita",
                              HS_08_per_capita.scaled = "HS-08 per capita",
                              HS_12_per_capita.scaled = "HS-12 per capita",
                              HS_06_n_importers_avg.scaled = "HS-06: no. source countries",
                              HS_07_n_importers_avg.scaled = "HS-07: no. source countries",
                              HS_08_n_importers_avg.scaled = "HS-08: no. source countries",
                              HS_12_n_importers_avg.scaled = "HS-12: no. source countries",
                              HS_06_evenness_avg.scaled = "HS-06: evenness",
                              HS_07_evenness_avg.scaled = "HS-07: evenness",
                              HS_08_evenness_avg.scaled = "HS-08: evenness",
                              HS_12_evenness_avg.scaled = "HS-12: evenness"
              )

custom_order <- c(
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
                  "HS_12_evenness_avg.scaled")

# calculate the correlation coefficient matrix and p-value matrix
# 重新排序相关矩阵
corr <- round(cor(invasion.global01.vars), 3)
p.mat <- cor_pmat(invasion.global01.vars)

# create correlation plot with p-values
pair.plot <- ggcorrplot(corr,
                        method = "square",
                        hc.order = FALSE,
                        type = "upper",
                        insig = "blank",
                        p.mat = p.mat,
                        lab = TRUE)

# main plot
pair.plot01 <- pair.plot +
theme_bw() +
theme(panel.background = element_rect(fill = NA),
      panel.grid.major = element_line(colour = "white", linetype = "solid"),
      # axis.line = element_line(size = 1, linetype = "solid"),
      # axis.ticks = element_line(colour = "black", linetype = "solid"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank(),
      axis.text = element_text(family = "serif", colour = "black", size = 14),
      legend.text = element_text(family = "serif", colour = "black", size = 14),
      legend.title = element_text(family = "serif", colour = "black", size = 14),
      legend.position = c(0.9, 0.3)) +
scale_fill_gradient2(low = "#4575B4", mid = "white", high = "#D73027", midpoint = 0, limits = c(-1, 1))+
  labs(fill = "Corr")

# add full names
pair.plot01 <- pair.plot01 +
scale_x_discrete(labels = var.fullName) +
scale_y_discrete(labels = var.fullName)

pair.plot01

# save plot
ggexport(pair.plot01, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241205.correlation.transformed.plot.png",
         width = 3000,
         height = 3000,
         pointsize = 12,
         res = 300)
