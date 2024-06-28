# -*- coding: utf-8 -*-
# @Author:
# @Date: 2023-07-29 20:50:12
# @Last Modified by: dbc
# @Last Modified time: 2024-06-28 13:56:33
# @Description: Figure S1

# loading packages
library(dplyr)
library(ggcorrplot)
library(ggpubr)
library(readr)
library(tidyverse)

# file path
path <- "D:/我的坚果云/2023.liyuan_database"

# set file path
setwd(path)
getwd()

# load data
invasion.asia <- read_csv("./20231125.iso_upadted_v14.gdp.asia.SRgreaterthan1.withIsland.csv")
glimpse(invasion.asia)
str(invasion.asia)

# view data
View(invasion.asia)

# remove unavailable data, including "TWN", "MAC", "HKG"
invasion.asia01 <- invasion.asia %>%
filter(!(alpha_3 %in% c("TWN", "MAC", "HKG")))  # %>%
# filter(!is.na(PD))

str(invasion.asia01)

# replace null data with zero
invasion.asia01 <- invasion.asia01 %>%
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

invasion.asia01 <- invasion.asia01 %>%
  mutate(airport_density = airport.n / (mean.area_km2 / 100000),
         seaport_density = seaport.n / (mean.area_km2 / 100000),
         gdp_per_capita = gdp.mean_cpi2016 / (mean.population_1k*1000),
         HS_06_per_capita = HS_06_cpi2016 / (mean.population_1k*1000),
         HS_07_per_capita = HS_07_cpi2016 / (mean.population_1k*1000),
         HS_08_per_capita = HS_08_cpi2016 / (mean.population_1k*1000),
         HS_12_per_capita = HS_12_cpi2016 / (mean.population_1k*1000))

# range of data
range(invasion.asia01$airport_density, na.rm = TRUE)
range(invasion.asia01$seaport_density, na.rm = TRUE)
range(invasion.asia01$gdp_per_capita, na.rm = TRUE)
range(invasion.asia01$HS_06_per_capita, na.rm = TRUE)
range(invasion.asia01$HS_07_per_capita, na.rm = TRUE)
range(invasion.asia01$HS_08_per_capita, na.rm = TRUE)
range(invasion.asia01$HS_12_per_capita, na.rm = TRUE)

# range(invasion.asia01$island, na.rm = TRUE)
range(invasion.asia01$airport.n, na.rm = TRUE)
range(invasion.asia01$seaport.n, na.rm = TRUE)
range(invasion.asia01$gdp.mean_cpi2016, na.rm = TRUE)
range(invasion.asia01$mean.area_km2, na.rm = TRUE)
range(invasion.asia01$mean.population.density_km2, na.rm = TRUE)
range(invasion.asia01$wc2.1_10m_bio_1, na.rm = TRUE)
range(invasion.asia01$wc2.1_10m_bio_12, na.rm = TRUE)
range(invasion.asia01$HS_06_cpi2016, na.rm = TRUE)
range(invasion.asia01$HS_07_cpi2016, na.rm = TRUE)
range(invasion.asia01$HS_08_cpi2016, na.rm = TRUE)
range(invasion.asia01$HS_12_cpi2016, na.rm = TRUE)

# convert as factor variable
invasion.asia01$island <- as.factor(invasion.asia01$island)

# data transformation, inlcuidiong log and scale transformation
invasion.asia01$airport_density.scaled <- scale(log(invasion.asia01$airport_density + 1))
invasion.asia01$seaport_density.scaled <- scale(log(invasion.asia01$seaport_density + 1))

invasion.asia01$gdp_per_capita.scaled <- scale(log(invasion.asia01$gdp_per_capita))
invasion.asia01$mean.area_km2.scaled <- scale(log(invasion.asia01$mean.area_km2))
invasion.asia01$mean.population.density_km2.scaled <- scale(log(invasion.asia01$mean.population.density_km2))

invasion.asia01$wc2.1_10m_bio_1.scaled <- scale(log(invasion.asia01$wc2.1_10m_bio_1))
invasion.asia01$wc2.1_10m_bio_12.scaled <- scale(log(invasion.asia01$wc2.1_10m_bio_12))

# HS Code
# 06  Trees and other plants, live; bulbs, roots and the like; cut flowers and ornamental foliage
# 07  Vegetables and certain roots and tubers; edible
# 08  Fruit and nuts, edible; peel of citrus fruit or melons
# 09  Coffee, tea, mate and spices
# 10 Cereals
# 11 Products of the milling industry; malt, starches, including, wheat gluten
# 12 Oil seeds and oleaginous fruits; miscellaneous grains, seeds and fruit, industrial or medicinal plants; straw and fodder
# 13 Lac; gums, resins and other vegetable saps and extracts
# 14 Vegetable plaiting materials; vegetable products not elsewhere specified or included
# TOTAL All Commodities

# 09, 10, 11, 13, 14 non-living plant

invasion.asia01$HS_06_per_capita.scaled <- scale(log(invasion.asia01$HS_06_per_capita))
invasion.asia01$HS_07_per_capita.scaled <- scale(log(invasion.asia01$HS_07_per_capita))
invasion.asia01$HS_08_per_capita.scaled <- scale(log(invasion.asia01$HS_08_per_capita))
invasion.asia01$HS_12_per_capita.scaled <- scale(log(invasion.asia01$HS_12_per_capita))

#
range(invasion.asia01$airport_density.scaled, na.rm = TRUE)
range(invasion.asia01$seaport_density.scaled, na.rm = TRUE)
range(invasion.asia01$gdp_per_capita.scaled, na.rm = TRUE)
range(invasion.asia01$mean.area_km2.scaled, na.rm = TRUE)
range(invasion.asia01$mean.population.density_km2.scaled, na.rm = TRUE)
range(invasion.asia01$wc2.1_10m_bio_1.scaled, na.rm = TRUE)
range(invasion.asia01$wc2.1_10m_bio_12.scaled, na.rm = TRUE)
range(invasion.asia01$HS_06_per_capita.scaled, na.rm = TRUE)
range(invasion.asia01$HS_07_per_capita.scaled, na.rm = TRUE)
range(invasion.asia01$HS_08_per_capita.scaled, na.rm = TRUE)
range(invasion.asia01$HS_12_per_capita.scaled, na.rm = TRUE)


# variable selection
invasion.asia01.vars <- invasion.asia01 %>%
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
              HS_12_per_capita.scaled) %>%
na.omit() %>%
as.data.frame()

str(invasion.asia01.vars)

# ==========================================================================
# correlation analysis
# ==========================================================================
# Creates a data frame that corresponds to the name of the variable and its full name
var.fullName <- c(
              airport_density.scaled = "Airport density",
              seaport_density.scaled = "Seaport density",
              gdp_per_capita.scaled = "GDP per capita",
              mean.area_km2.scaled = "Area",
              mean.population.density_km2.scaled = "Population density",
              wc2.1_10m_bio_1.scaled = "Temperature",
              wc2.1_10m_bio_12.scaled = "Precipitation",
              HS_06_per_capita.scaled = "HS-06 per capita",
              HS_07_per_capita.scaled = "HS-07 per capita",
              HS_08_per_capita.scaled = "HS-08 per capita",
              HS_12_per_capita.scaled = "HS-12 per capita"
              )

# calculate the correlation coefficient matrix and p-value matrix
corr <- round(cor(invasion.asia01.vars), 3)
p.mat <- cor_pmat(invasion.asia01.vars)

# create correlation plot with p-values
pair.plot <- ggcorrplot(corr,
                        method = "square",
                        hc.order = TRUE,
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
scale_fill_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, na.value = "grey")

# add full names
pair.plot01 <- pair.plot01 +
scale_x_discrete(labels = var.fullName) +
scale_y_discrete(labels = var.fullName)

pair.plot01

# save plot
ggexport(pair.plot01, filename = "./results/20240625.correlation.transformed.plot.png",
         width = 2000,
         height = 2000,
         pointsize = 12,
         res = 300)
