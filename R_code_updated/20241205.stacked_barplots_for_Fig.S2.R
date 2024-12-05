# -*- coding: utf-8 -*-
# @Author:
# @Date: 2023-07-29 20:50:12
# @Last Modified by: dbc
# @Last Modified time: 2024-12-04 23:44:17
# @Description: Figures 1_6

# Load required R packages
library(ape)
library(dplyr)  # for filter
library(ggtree)
library(picante)
library(ggplot2)
library(readr)  # for read_csv
library(readxl)  # for read_excel
library(stringr)
library(tidyverse)
library(ggrepel)
library(patchwork)
library(ggpubr)
library(forcats)
library(countrycode)

# # Load “PhyloMeasures” package
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/PhyloMeasures/PhyloMeasures_2.1.tar.gz"
# install.packages(packageurl, repos = NULL, type = "source")

# clean memory
cat("\014")
rm(list = ls())
gc()

# file path
path <- "D:/我的坚果云/2023.liyuan_database/新GADM数据集"

# set file path
setwd(path)
getwd()


# 导入原始数据，具有大陆岛屿信息
invasion.global <- read_csv("D:/我的坚果云/2023.liyuan_database/新GADM数据集/20241120.iso_upadted_v16.diversity.cleaned.csv")
glimpse(invasion.global)
# str(invasion.global)

# view data
str(invasion.global)

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

# 没有缺失大于信息
table(invasion.global01$region)
table(invasion.global01$sub_region)

# ==========================================================================
# 根据iso_3 提取国家所属的大洲，以及提取岛屿
# ==========================================================================
# 使用 countrycode 将 ISO3 转换为大陆名称
# 假设 invasion.global01 已包含 'iso3' 列
invasion.global01 <- invasion.global01 %>%
  mutate(continent = countrycode(sourcevar = alpha_3,
                                 origin = "iso3c",
                                 destination = "continent")) %>%
  mutate(region = countrycode(sourcevar = alpha_3,
    	                      origin = "iso3c",
    	                      destination = "region")) %>%
  mutate(continent01 = case_when(continent == "Americas" & region == "North America" ~ "North America",
                                 continent == "Americas" & region != "North America" ~ "South America",
                                 TRUE ~ continent))


table(invasion.global01$continent01)


# 汇总大陆信息
invasion.global02 <- invasion.global01 %>%
dplyr::select(alpha_3, continent01, continent, region, island, P_value_PD, P_value_MPD, P_value_MNTD)

# ==========================================================================
# 导入系统发育信息
# ==========================================================================
# load data
checklist.ses.pd01 <- read.csv("D:/我的坚果云/2023.liyuan_database/新GADM数据集/20241015updated.GRIIS.phylo_tree.ses.pd.SRgreaterthan1.csv")
checklist.ses.mpd01 <- read.csv("D:/我的坚果云/2023.liyuan_database/新GADM数据集/20241015updated.GRIIS.phylo_tree.ses.mpd.SRgreaterthan1.csv")
checklist.ses.mntd01 <- read.csv("D:/我的坚果云/2023.liyuan_database/新GADM数据集/20241015updated.GRIIS.phylo_tree.ses.mntd.SRgreaterthan1.csv")
checklist.ses.pd01
checklist.ses.mpd01
checklist.ses.mntd01

# ==========================================================================
# 分别对岛屿和大陆国家创建 PD 标签的比例图
# ==========================================================================
invasion.global02 <- invasion.global02 %>%
mutate(label.pd = ifelse(P_value_PD < 0.025, "Overdispersed", ifelse(P_value_PD > 0.975, "Clustered", "Random")),
       label.mpd = ifelse(P_value_MPD < 0.025, "Overdispersed", ifelse(P_value_MPD > 0.975, "Clustered", "Random")),
       label.mntd = ifelse(P_value_MNTD < 0.025, "Overdispersed", ifelse(P_value_MNTD > 0.975, "Clustered", "Random")))

# 转换为因子变量并定义水平顺序
invasion.global03 <- invasion.global02 %>%
  mutate(
    label.pd = factor(label.pd, levels = c("Overdispersed", "Random", "Clustered")),
    label.mpd = factor(label.mpd, levels = c("Overdispersed", "Random", "Clustered")),
    label.mntd = factor(label.mntd, levels = c("Overdispersed", "Random", "Clustered")),
    island = factor(island, levels = c("no", "yes")),
    continent01 = factor(continent01, levels = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"))
  ) %>%
  mutate(continent01 = fct_reorder(
      continent01,
      label.pd == "Clustered",
      function(x) mean(x),
      .desc = FALSE))

levels(invasion.global03$continent01)

# 3. 创建独立的比例图
plot_continent_pd <- ggplot(invasion.global03, aes(x = continent01, fill = label.pd)) +
  geom_bar(position = "fill", alpha = 0.7) +
  scale_y_continuous(labels = function(x) paste0(x * 100)) +
  scale_fill_manual(values = c("Clustered" = "#D73027", "Random" = "grey", "Overdispersed" = "#4575B4")) +
  # ggsci::scale_fill_npg() +
  coord_flip() +
  labs(
    x = "",
    y = "Proportion (%)",
    fill = "PD:"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16),
    panel.grid = element_blank(),
    axis.ticks.x = element_line(size = 1),
    axis.line.x = element_line(size = 1),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "top"
  )

# 显示大陆国家的图形
print(plot_continent_pd)

plot_island_pd <- ggplot(invasion.global03, aes(x = island, fill = label.pd)) +
  geom_bar(position = "fill", alpha = 0.7) +
  scale_y_continuous(labels = function(x) paste0(x * 100)) +
  scale_fill_manual(values = c("Clustered" = "#D73027", "Random" = "grey", "Overdispersed" = "#4575B4")) +
  # ggsci::scale_fill_npg() +
  coord_flip() +
  labs(
    x = "Island",
    y = "Proportion (%)",
    fill = "PD:"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16),
    panel.grid = element_blank(),
    axis.ticks.x = element_line(size = 1),
    axis.line.x = element_line(size = 1),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "none"
  )

# 显示大陆国家的图形
print(plot_island_pd)

# 合并的系统发育指标
combined_maps_pd <-
plot_continent_pd +
plot_island_pd +
plot_layout(ncol = 2) +
plot_annotation(tag_levels = 'A')

combined_maps_pd

# 保存图像
ggexport(combined_maps_pd, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/2024112.stacked.bar.plot.pd.png",
    width = 3600,
    height = 1000,
    pointsize = 12,
    res = 300)

# ==========================================================================
# 分别对岛屿和大陆国家创建 MPD 标签的比例图
# ==========================================================================
# 3. 创建独立的比例图
plot_continent_mpd <- ggplot(invasion.global03, aes(x = continent01, fill = label.mpd)) +
  geom_bar(position = "fill", alpha = 0.7) +
  scale_y_continuous(labels = function(x) paste0(x * 100)) +
  scale_fill_manual(values = c("Clustered" = "#D73027", "Random" = "grey", "Overdispersed" = "#4575B4")) +
  # ggsci::scale_fill_npg() +
  coord_flip() +
  labs(
    x = "",
    y = "Proportion (%)",
    fill = "MPD:"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16),
    panel.grid = element_blank(),
    axis.ticks.x = element_line(size = 1),
    axis.line.x = element_line(size = 1),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "top"
  )

# 显示大陆国家的图形
print(plot_continent_mpd)

plot_island_mpd <- ggplot(invasion.global03, aes(x = island, fill = label.mpd)) +
  geom_bar(position = "fill", alpha = 0.7) +
  scale_y_continuous(labels = function(x) paste0(x * 100)) +
  scale_fill_manual(values = c("Clustered" = "#D73027", "Random" = "grey", "Overdispersed" = "#4575B4")) +
  # ggsci::scale_fill_npg() +
  coord_flip() +
  labs(
    x = "Island",
    y = "Proportion (%)",
    fill = "MPD:"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16),
    panel.grid = element_blank(),
    axis.ticks.x = element_line(size = 1),
    axis.line.x = element_line(size = 1),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "none"
  )

# 显示大陆国家的图形
print(plot_island_mpd)

# 合并的系统发育指标
combined_maps_mpd <-
plot_continent_mpd +
plot_island_mpd +
plot_layout(ncol = 2) +
plot_annotation(tag_levels = 'A')

combined_maps_mpd

# 保存图像
ggexport(combined_maps_mpd, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241126.stacked.bar.plot.mpd.png",
    width = 3600,
    height = 1000,
    pointsize = 12,
    res = 300)

# ==========================================================================
# 分别对岛屿和大陆国家创建 MNTD 标签的比例图
# ==========================================================================
# 3. 创建独立的比例图
plot_continent_mntd <- ggplot(invasion.global03, aes(x = continent01, fill = label.mntd)) +
  geom_bar(position = "fill", alpha = 0.7) +
  scale_y_continuous(labels = function(x) paste0(x * 100)) +
  scale_fill_manual(values = c("Clustered" = "#D73027", "Random" = "grey", "Overdispersed" = "#4575B4")) +
  # ggsci::scale_fill_npg() +
  coord_flip() +
  labs(
    x = "",
    y = "Proportion (%)",
    fill = "MNTD:"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16),
    panel.grid = element_blank(),
    axis.ticks.x = element_line(size = 1),
    axis.line.x = element_line(size = 1),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "top"
  )

# 显示大陆国家的图形
print(plot_continent_mntd)

plot_island_mntd <- ggplot(invasion.global03, aes(x = island, fill = label.mntd)) +
  geom_bar(position = "fill", alpha = 0.7) +
  scale_y_continuous(labels = function(x) paste0(x * 100)) +
  scale_fill_manual(values = c("Clustered" = "#D73027", "Random" = "grey", "Overdispersed" = "#4575B4")) +
  # ggsci::scale_fill_npg() +
  coord_flip() +
  labs(
    x = "Island",
    y = "Proportion (%)",
    fill = "MNTD:"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16),
    panel.grid = element_blank(),
    axis.ticks.x = element_line(size = 1),
    axis.line.x = element_line(size = 1),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "none"
  )

# 显示大陆国家的图形
print(plot_island_mntd)

# 合并的系统发育指标
combined_maps_mntd <-
plot_continent_mntd +
plot_island_mntd +
plot_layout(ncol = 2) +
plot_annotation(tag_levels = 'A')

combined_maps_mntd

# 保存图像
ggexport(combined_maps_mntd, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241126.stacked.bar.plot.mntd.png",
    width = 3600,
    height = 1000,
    pointsize = 12,
    res = 300)

# ==========================================================================
# 汇总所有数据
# ==========================================================================
# 合并的系统发育指标
combined_maps_pdmpdmntd <-
plot_continent_pd +
plot_island_pd +
plot_continent_mpd +
plot_island_mpd +
plot_continent_mntd +
plot_island_mntd +
plot_layout(ncol = 2) +
plot_annotation(tag_levels = 'A')

combined_maps_pdmpdmntd

# 保存图像
ggexport(combined_maps_pdmpdmntd, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241126.stacked.bar.plot.pdmpdmntd.png",
    width = 3600,
    height = 3000,
    pointsize = 12,
    res = 300)
