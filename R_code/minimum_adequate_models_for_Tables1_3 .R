# -*- coding: utf-8 -*-
# @Author:
# @Date: 2023-07-29 20:50:12
# @Last Modified by: dbc
# @Last Modified time: 2024-06-28 13:50:04
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

# variable calculation
invasion.asia01 <- invasion.asia01 %>%
  mutate(airport_density = airport.n / (mean.area_km2 / 100000),
         seaport_density = seaport.n / (mean.area_km2 / 100000),
         gdp_per_capita = gdp.mean_cpi2016 / (mean.population_1k*1000),
         HS_06_per_capita = HS_06_cpi2016 / (mean.population_1k*1000),
         HS_07_per_capita = HS_07_cpi2016 / (mean.population_1k*1000),
         HS_08_per_capita = HS_08_cpi2016 / (mean.population_1k*1000),
         HS_12_per_capita = HS_12_cpi2016 / (mean.population_1k*1000))

# range of data
# range(invasion.asia01$island, na.rm = TRUE)
range(invasion.asia01$airport_density, na.rm = TRUE)
range(invasion.asia01$seaport_density, na.rm = TRUE)
range(invasion.asia01$gdp_per_capita, na.rm = TRUE)
range(invasion.asia01$mean.area_km2, na.rm = TRUE)
range(invasion.asia01$mean.population.density_km2, na.rm = TRUE)
range(invasion.asia01$wc2.1_10m_bio_1, na.rm = TRUE)
range(invasion.asia01$wc2.1_10m_bio_12, na.rm = TRUE)
range(invasion.asia01$HS_06_per_capita, na.rm = TRUE)
range(invasion.asia01$HS_07_per_capita, na.rm = TRUE)
range(invasion.asia01$HS_08_per_capita, na.rm = TRUE)
range(invasion.asia01$HS_12_per_capita, na.rm = TRUE)

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

invasion.asia01$HS_06_per_capita.scaled <- scale(log(invasion.asia01$HS_06_per_capita))
invasion.asia01$HS_07_per_capita.scaled <- scale(log(invasion.asia01$HS_07_per_capita))
invasion.asia01$HS_08_per_capita.scaled <- scale(log(invasion.asia01$HS_08_per_capita))
invasion.asia01$HS_12_per_capita.scaled <- scale(log(invasion.asia01$HS_12_per_capita))

#
# range(invasion.asia01$island, na.rm = TRUE)
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

# ==========================================================================
#  minimum_adequate_models for taxonomic diversity
# ==========================================================================
# variable selection
invasion.asia01.SR <- invasion.asia01 %>%
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
       HS_12_per_capita.scaled) %>%
na.omit() %>%
as.data.frame()

#
str(invasion.asia01.SR)

# glm full model
model.SR <- glm(SR ~
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
               HS_12_per_capita.scaled,
      family = poisson(),
      data = invasion.asia01.SR
)

# stepwise model based on  AIC
step.model.SR00 <- step(model.SR)
step.model.SR00

# stepwise model based on VIF threshold
step.model.SR <- stepVIF(step.model.SR00, threshold = 5, verbose = TRUE)

vif(step.model.SR)
summary(model.SR)
summary(step.model.SR00)
summary(step.model.SR)

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
                              "HS_12_per_capita.scaled"),
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
                                   "HS-12 per capita"),
                     stringsAsFactors = FALSE
                     )


# Contributions to the mixed effects model were calculated and extracted using delta and R2m
r.step.model.SR <- glmm.hp(step.model.SR)
r.step.model.SR
r.step.model.SR$r.squaredGLMM
# > r.step.model.SR$r.squaredGLMM
#                 R2m       R2c
# delta     0.9770370 0.9770370
# lognormal 0.9772751 0.9772751
# trigamma  0.9767938 0.9767938

r.delta.step.model.SR <- r.step.model.SR$delta %>%
as.data.frame() %>%
rownames_to_column("term")

r.delta.step.model.SR

# Merging glm result data with contribution rate data
result.step.model.SR02 <- result.step.model.SR %>%
left_join(r.delta.step.model.SR, by = "term")

result.step.model.SR02

# Integrate data from fixed name variables and format the results
result.step.model.SR03 <- var.df %>%
left_join(result.step.model.SR02, by = "term")

result.step.model.SR04 <-result.step.model.SR03 %>%
filter(!is.na(p.value)) %>%
mutate(across(c(estimate:statistic, Unique:'I.perc(%)'), round, 2)) %>%
mutate(across(p.value, round, 3))

result.step.model.SR04

# Save data
write_csv(result.step.model.SR, "./results/NEW20240626.result.step.model.SR.csv")
write_csv(result.step.model.SR04, "./results/NEW20240626.result.step.model.SR.more_info.csv")

# ==========================================================================
#  minimum_adequate_models for phylogenetic diversity
# ==========================================================================
# variable selection
invasion.asia01.PD <- invasion.asia01 %>%
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
       HS_12_per_capita.scaled) %>%
na.omit() %>%
as.data.frame()

str(invasion.asia01.PD)

# data transformation of response variable
invasion.asia01.PD$PD.log <- log(invasion.asia01.PD$PD)

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
               HS_12_per_capita.scaled,
      data = invasion.asia01.PD
)

# stepwise model based on  AIC
step.model.PD00 <- step(model.PD00)
step.model.PD00

summary(model.PD00)
summary(step.model.PD00)

# stepwise model based on VIF threshold
step.model.PD <- stepVIF(step.model.PD00, threshold = 5, verbose = TRUE)

# update model with manually removing the variable without significant p value
step.model.PD <- update(step.model.PD, . ~ . - mean.population.density_km2.scaled)

vif(step.model.PD)
summary(step.model.PD)

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
# [1] 0.6154521

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
mutate(across(c(estimate:statistic, Unique:'I.perc(%)'), round, 2)) %>%
mutate(across(p.value, round, 3))

result.step.model.PD04

# save data
write_csv(result.step.model.PD, "./results/NEW20240626.result.step.model.PD.csv")
write_csv(result.step.model.PD04, "./results/NEW20240626.result.step.model.PD.more_info.csv")

# ==========================================================================
#  minimum_adequate_models for MPD
# ==========================================================================
# variable selection
invasion.asia01.mpd.obs <- invasion.asia01 %>%
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
       HS_12_per_capita.scaled) %>%
na.omit() %>%
as.data.frame()

# data transformation of response variable
invasion.asia01.mpd.obs$mpd.obs.log <- log(invasion.asia01.mpd.obs$mpd.obs)

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
               HS_12_per_capita.scaled,
      data = invasion.asia01.mpd.obs
)

# stepwise model based on AIC
step.model.mpd.obs00 <- step(model.mpd.obs00)

summary(model.mpd.obs00)
summary(step.model.mpd.obs00)

# stepwise model based on VIF threshold
step.model.mpd.obs <- stepVIF(step.model.mpd.obs00, threshold = 5, verbose = TRUE)

vif(step.model.mpd.obs)
summary(step.model.mpd.obs)

# data cleaning
result.step.model.mpd.obs <- broom::tidy(step.model.mpd.obs)
result.step.model.mpd.obs

# Use broom::tidy to organize step.model.SR and handle factor variables
result.step.model.mpd.obs <- result.step.model.mpd.obs %>%
mutate(term = str_replace(term, "islandyes", "island"))

result.step.model.mpd.obs

# Contributions to the mixed effects model were calculated and extracted using hierarchical.partitioning and Total.R2
r.step.model.mpd.obs <- glmm.hp(step.model.mpd.obs)
r.step.model.mpd.obs
r.step.model.mpd.obs$Total.R2
# > r.step.model.mpd.obs$Total.R2
# [1] 0.1836048

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
mutate(across(c(estimate:statistic, Unique:'I.perc(%)'), round, 2)) %>%
mutate(across(p.value, round, 3))

result.step.model.mpd.obs04

# Save data
write_csv(result.step.model.mpd.obs, "./results/NEW20240626.result.step.model.mpd.obs.csv")
write_csv(result.step.model.mpd.obs04, "./results/NEW20240626.result.step.model.mpd.obs.more_info.csv")

# ==========================================================================
#  minimum_adequate_models for MNTD
# ==========================================================================
# variable selection
invasion.asia01.mntd.obs <- invasion.asia01 %>%
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
       HS_12_per_capita.scaled) %>%
na.omit() %>%
as.data.frame()

# data transformation of response variable
invasion.asia01.mntd.obs$mntd.obs.log <- log(invasion.asia01.mntd.obs$mntd.obs)

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
               HS_12_per_capita.scaled,
      data = invasion.asia01.mntd.obs
)

# stepwise model based on  AIC
step.model.mntd.obs00 <- step(model.mntd.obs00)
step.model.mntd.obs00

summary(model.mntd.obs00)
summary(step.model.mntd.obs00)

# stepwise model based on VIF threshold
step.model.mntd.obs <- stepVIF(step.model.mntd.obs00, threshold = 5, verbose = TRUE)
vif(step.model.mntd.obs)

# Having trouble getting rid of islandyes
# step.model.mntd.obs02 <- update(step.model.mntd.obs02, . ~ . - islandyes - HS_06_per_capita.scaled - HS_12_per_capita.scaled  - airport_density.scaled - wc2.1_10m_bio_1.scaled)
# directly using formula of mntd.obs.log ~ mean.area_km2.scaled
step.model.mntd.obs <- lm(mntd.obs.log ~ mean.area_km2.scaled, data = invasion.asia01.mntd.obs)

# vif(step.model.mntd.obs)
summary(step.model.mntd.obs)

result.step.model.mntd.obs <- broom::tidy(step.model.mntd.obs)
result.step.model.mntd.obs

# Use broom::tidy to organize step.model.SR and handle factor variables
result.step.model.mntd.obs <- result.step.model.mntd.obs %>%
mutate(term = str_replace(term, "islandyes", "island"))

# Calculate R2
r.step.model.mntd.obs <- broom::glance(step.model.mntd.obs) %>%
select(r.squared, adj.r.squared) %>%
mutate(term = "mean.area_km2.scaled")

r.step.model.mntd.obs

# Merging glm result data with contribution rate data
result.step.model.mntd.obs02 <- result.step.model.mntd.obs %>%
left_join(r.step.model.mntd.obs, by = "term")

# Integrate data from fixed name variables and format the results
result.step.model.mntd.obs03 <- var.df %>%
left_join(result.step.model.mntd.obs02, by = "term")

result.step.model.mntd.obs04 <- result.step.model.mntd.obs03 %>%
filter(!is.na(p.value)) %>%
mutate(across(c(estimate:adj.r.squared), round, 2)) %>%
mutate(across(p.value, round, 3))

#   r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
#       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
# 1     0.130         0.103 0.514      4.91  0.0337     1  -25.3  56.6  61.3

# save data
write_csv(result.step.model.mntd.obs, "./results/NEW20240626.result.step.model.mntd.obs.csv")
write_csv(result.step.model.mntd.obs04, "./results/NEW20240626.result.step.model.mntd.obs.more_info.csv")

# ==========================================================================
# Plotting code
# taxonomic diversity
# ==========================================================================
# Extract the marginal mean
pred.SR <-  plot_model(step.model.SR, type = "eff", show.data = TRUE, dot.size = 4)
pred.SR

# > pred.SR
# $island
# $mean.area_km2.scaled
# $mean.population.density_km2.scaled
# $wc2.1_10m_bio_1.scaled
# $HS_07_per_capita.scaled
# $HS_08_per_capita.scaled
# $HS_12_per_capita.scaled

do.call("grid.arrange", c(pred.SR, ncol = 3))

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
create_island_boxplot <- function(step.model, y_limit = 400, y_break = 200) {
  # get data
  plot_data <- get_model_data(step.model, type = "eff", terms = "island")

  plot_data01 <- data.frame(plot_data)
  plot_data01$x <- factor(plot_data01$x, levels = c(1,2))

  rawdata <- attr(plot_data, "rawdata")
  rawdata$x <- factor(rawdata$x, levels = c(1,2))

  y_lab <- attr(plot_data, "title")

  # Creating a Box-and-Line Chart
  boxplot_plot <- ggplot(rawdata, aes(x = x, y = response, fill = x)) +
    geom_boxplot(width = 0.5, alpha = 0.7) +
    scale_fill_manual(values = c("#3498db", "#e74c3c")) +
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
                    color = "red", size = 2, fatten = 2)

  return(final_plot)
}

# Create the effect plots using the custom function
plot.island.SR <- create_island_boxplot(step.model = step.model.SR) + labs(y = "Taxonomic diversity")
plot.island.SR

# Create the effect plots using the custom function
plot.mean.area_km2.scaled.SR <- customized_plot(pred.SR$mean.area_km2.scaled,
                                  "Log(area)(scaled)",
                                  "SR",
                                  "") +
scale_x_continuous(limits = c(-3, 2), breaks = seq(-3, 2, by = 1)) +
scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 100)) +
labs(y = "Taxonomic diversity")

plot.mean.area_km2.scaled.SR

# Create the effect plots using the custom function
plot.mean.population.density_km2.scaled.SR <- customized_plot(pred.SR$mean.population.density_km2.scaled,
                                  "Log(population density)(scaled)",
                                  "SR",
                                  "") +
scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 3, by = 1)) +
scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 100)) +
labs(y = "Taxonomic diversity")

plot.mean.population.density_km2.scaled.SR

# Create the effect plots using the custom function
plot.wc2.1_10m_bio_1.scaled.SR <- customized_plot(pred.SR$wc2.1_10m_bio_1.scaled,
                                  "Log(temperature)(scaled)",
                                  "SR",
                                  "") +
scale_x_continuous(limits = c(-4, 1), breaks = seq(-4, 1, by = 1)) +
scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, by = 200)) +
labs(y = "Taxonomic diversity")

plot.wc2.1_10m_bio_1.scaled.SR

plot.HS_07_per_capita.scaled.SR <- customized_plot(pred.SR$HS_07_per_capita.scaled,
                                  "Log(HS-07 per capita)(scaled)",
                                  "SR",
                                  "") +
scale_x_continuous(limits = c(-2.5, 2), breaks = seq(-2, 2, by = 1)) +
scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 100)) +
labs(y = "Taxonomic diversity")

plot.HS_07_per_capita.scaled.SR

plot.HS_08_per_capita.scaled.SR <- customized_plot(pred.SR$HS_08_per_capita.scaled,
                                  "Log(HS-08 per capita)(scaled)",
                                  "SR",
                                  "") +
scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, by = 1)) +
scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 100)) +
labs(y = "Taxonomic diversity")

plot.HS_08_per_capita.scaled.SR

plot.HS_12_per_capita.scaled.SR <- customized_plot(pred.SR$HS_12_per_capita.scaled,
                                  "Log(HS-12 per capita)(scaled)",
                                  "SR",
                                  "") +
scale_x_continuous(limits = c(-2.5, 2.5), breaks = seq(-2, 2, by = 1)) +
scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 100)) +
labs(y = "Taxonomic diversity")

plot.HS_12_per_capita.scaled.SR

# wrap single plots
plots.SR <-
plot.island.SR +
plot.wc2.1_10m_bio_1.scaled.SR +
plot.mean.population.density_km2.scaled.SR +
plot.mean.area_km2.scaled.SR +
plot.HS_07_per_capita.scaled.SR +
plot.HS_08_per_capita.scaled.SR +
plot.HS_12_per_capita.scaled.SR +
plot_layout(ncol = 2) +
plot_annotation(tag_levels = 'A')

plots.SR

# save image
ggexport(plots.SR, filename = "./results/NEW20240626vif.plots.step.TD.png",
    width = 2400,
    height = 3600,
    pointsize = 12,
    res = 300)

# ==========================================================================
# phylogenetic diversity
# ==========================================================================
# Extract the marginal mean
pred.PD <- plot_model(step.model.PD, type = "eff", show.data = TRUE, colors = "red", dot.size = 4)
pred.PD

do.call("grid.arrange", c(pred.PD, ncol = 3))

# Create the effect plots using the custom function
plot.island.PD <- create_island_boxplot(step.model = step.model.PD) +
labs(y = "Log(PD, Mya)") +
scale_y_continuous(limits = c(5, 10), breaks = seq(5, 10, by = 1))

plot.island.PD

# Create the effect plots using the custom function
plot.airport_density.scaled.PD <- customized_plot(pred.PD$airport_density.scaled,
                                  "Log(airport density)(scaled)",
                                  "Log(PD, Mya)",
                                  "") +
scale_x_continuous(limits = c(-2, 3), breaks = seq(-2, 3, by = 1)) +
scale_y_continuous(limits = c(5, 11), breaks = seq(5, 11, by = 1))

plot.airport_density.scaled.PD

# Create the effect plots using the custom function
plot.mean.area_km2.scaled.PD <- customized_plot(pred.PD$mean.area_km2.scaled,
                                  "Log(area)(scaled)",
                                  "Log(PD, Mya)",
                                  "") +
scale_x_continuous(limits = c(-3, 2), breaks = seq(-3, 2, by = 1)) +
scale_y_continuous(limits = c(4, 10), breaks = seq(4, 10, by = 1))

plot.mean.area_km2.scaled.PD

# Create the effect plots using the custom function
plot.wc2.1_10m_bio_1.scaled.PD <- customized_plot(pred.PD$wc2.1_10m_bio_1.scaled,
                                  "Log(temperature)(scaled)",
                                  "Log(PD, Mya)",
                                  "") +
scale_x_continuous(limits = c(-4, 1), breaks = seq(-4, 1, by = 1)) +
scale_y_continuous(limits = c(5, 11), breaks = seq(5, 11, by = 1))

plot.wc2.1_10m_bio_1.scaled.PD

# plot.wc2.1_10m_bio_1.scaled.PD
plot.HS_08_per_capita.scaled.PD <- customized_plot(pred.PD$HS_08_per_capita.scaled,
                                  "Log(HS-08 per capita)(scaled)",
                                  "Log(PD, Mya)",
                                  "") +
scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, by = 1)) +
scale_y_continuous(limits = c(5, 10), breaks = seq(5, 10, by = 1))

plot.HS_08_per_capita.scaled.PD

# wrap single plots
plots.PD <-
plot.island.PD +
plot.wc2.1_10m_bio_1.scaled.PD +
plot.mean.area_km2.scaled.PD +
plot.airport_density.scaled.PD +
plot.HS_08_per_capita.scaled.PD +
plot_layout(ncol = 2) +
plot_annotation(tag_levels = 'A')

plots.PD

# Save Image
ggexport(plots.PD, filename = "./results/NEW20240626.plots.step.PD.png",
    width = 2400,
    height = 2700,
    pointsize = 12,
    res = 300)

# ==========================================================================
# MPD
# ==========================================================================
# Extract the marginal mean
pred.mpd.obs <- plot_model(step.model.mpd.obs, type = "eff", show.data = TRUE, colors = "red", dot.size = 4)
pred.mpd.obs

do.call("grid.arrange", c(pred.mpd.obs, ncol = 3))

# Create the effect plots using the custom function
plot.wc2.1_10m_bio_12.scaled.mpd.obs <- customized_plot(pred.mpd.obs$wc2.1_10m_bio_12.scaled,
                                  "Log(precipitation)(scaled)",
                                  "Log(MPD, Mya)",
                                  "") +
scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, by = 1)) +
scale_y_continuous(limits = c(5, 6.5), breaks = seq(5, 6.5, by = 0.5))

plot.wc2.1_10m_bio_12.scaled.mpd.obs

# Create the effect plots using the custom function
plot.HS_12_per_capita.scaled.mpd.obs <- customized_plot(pred.mpd.obs$HS_12_per_capita.scaled,
                                  "Log(HS-12 per capita)(scaled)",
                                  "Log(MPD, Mya)",
                                  "") +
scale_x_continuous(limits = c(-2.5, 2.5), breaks = seq(-2, 2, by = 1)) +
scale_y_continuous(limits = c(5, 6.5), breaks = seq(5, 6.5, by = 0.5))

plot.HS_12_per_capita.scaled.mpd.obs

# wrap single plots
plots.mpd.obs <-
plot.wc2.1_10m_bio_12.scaled.mpd.obs +
plot.HS_12_per_capita.scaled.mpd.obs +
plot_layout(ncol = 2) +
plot_annotation(tag_levels = 'A')

plots.mpd.obs

# save image
ggexport(plots.mpd.obs, filename = "./results/NEW20240626.plots.step.mpd.obs.png",
    width = 2400,
    height = 900,
    pointsize = 12,
    res = 300)

# ==========================================================================
# MNTD
# ==========================================================================
pred.mntd.obs <- plot_model(step.model.mntd.obs, type = "eff", show.data = TRUE, colors = "red", dot.size = 4)
pred.mntd.obs

# Create the effect plots using the custom function
plot.mean.area_km2.scaled.mntd.obs <- customized_plot(pred.mntd.obs,
                                  "Log(area)(scaled)",
                                  "Log(MNTD, Mya)",
                                  "") +
scale_x_continuous(limits = c(-3, 2), breaks = seq(-3, 2, by = 1)) +
scale_y_continuous(limits = c(3, 7), breaks = seq(3, 7, by = 1))

plot.mean.area_km2.scaled.mntd.obs

# wrap single plots
plots.mntd.obs <-
plot.mean.area_km2.scaled.mntd.obs

plots.mntd.obs

# save image
ggexport(plots.mntd.obs, filename = "./results/NEW20240626vif.plots.step.mntd.obs.png",
    width = 1200,
    height = 900,
    pointsize = 12,
    res = 300)