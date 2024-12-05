# -*- coding: utf-8 -*-
# @Author: bcdon
# @Date:   2023-06-23 00:06:57
# @Last Modified by: dbc
# @Last Modified time: 2024-12-03 23:54:25
# @description: 根据各国，计算平均PCA值

# library(devtools)
# install_github("bleutner/RStoolbox")
# pacman::p_load(readr)
# loading packages
library(readr)
library(tidyverse)
library(dplyr)
library(sf)
library(ggpubr)
library(raster)
library(RColorBrewer)
library(patchwork)

# Specify the full paths directly
folder_path <- "D:/我的坚果云/2023.liyuan_database/新GADM数据集"

setwd(folder_path)
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

glimpse(invasion.global01)

invasion.global02 <- invasion.global01 %>%
dplyr::select(alpha_3, SR, PD, mpd.obs, mntd.obs, P_value_PD, P_value_MPD, P_value_MNTD, island)

range(invasion.global02$mntd.obs)
range(invasion.global02$mpd.obs)
range(invasion.global02$PD)

# ==========================================================================
# 加载世界地图
# ==========================================================================
# # Load world map
# world.map <- sf::st_read("D:/我的坚果云/2023.liyuan_database/新GADM数据集/gadm_410-levels/gadm_410_levels_adm_0.shp")
# world.map

# version 3.6
library(geodata)
gadm.map <- world(level = 0, path = tempdir())
gadm.map01 <- st_as_sf(gadm.map)

# 去除GADM地图中的中国部分
gadm.map02 <- gadm.map01 %>% filter(! GID_0 %in% c("CHN", "TWN", "MAC", "HKG"))
plot(gadm.map02)

# ==========================================================================
# 加载中国地图
# ==========================================================================
# 加载China
china_mainland <- sf::st_read("D:/我的坚果云/2023.liyuan_database/新GADM数据集/China_map/bou1_4p.shp", options = "ENCODING=GB2312")
class(china_mainland)

china_mainland01 <- china_mainland %>%
                    summarise(geometry = st_union(geometry)) %>%
                    mutate(GID_0 = "CHN", NAME_0 = "China")

china_mainland01 <- st_as_sf(china_mainland01)
st_crs(china_mainland01) <- st_crs(gadm.map)

plot(china_mainland01)

# 合并中国地图到世界地图
global_map <- rbind(china_mainland01, gadm.map02)

# 进一步添加sub_region的信息
global_map.diversity <- global_map %>% left_join(invasion.global02, by = c("GID_0" = "alpha_3"))
plot(global_map.diversity)

# 提取地图
st_write(global_map.diversity, "D:/我的坚果云/2023.liyuan_database/新GADM数据集/global_map.diversity.shp", append = FALSE)

# ==========================================================================
# 作图
# ==========================================================================
# 创建新的对数变量，避免 log(0) 的问题
global_map.diversity <- global_map.diversity %>%
  mutate(SR_log = log10(SR + 1),
         PD_log = log10(PD + 1),
         mpd.obs_log = log10(mpd.obs + 1),
         mntd.obs_log = log10(mntd.obs + 1))

# 获取 SR_log 的最小值和最大值
SR_log_min <- min(global_map.diversity$SR_log, na.rm = TRUE)
SR_log_max <- max(global_map.diversity$SR_log, na.rm = TRUE)
SR_log_min
SR_log_max

# 获取 PD_log 的最小值和最大值
PD_log_min <- min(global_map.diversity$PD_log, na.rm = TRUE)
PD_log_max <- max(global_map.diversity$PD_log, na.rm = TRUE)
PD_log_min
PD_log_max

# 获取 mpd.obs_log 的最小值和最大值
mpd.obs_log_min <- min(global_map.diversity$mpd.obs_log, na.rm = TRUE)
mpd.obs_log_max <- max(global_map.diversity$mpd.obs_log, na.rm = TRUE)
mpd.obs_log_min
mpd.obs_log_max

# 获取 mntd.obs_log 的最小值和最大值
mntd.obs_log_min <- min(global_map.diversity$mntd.obs_log, na.rm = TRUE)
mntd.obs_log_max <- max(global_map.diversity$mntd.obs_log, na.rm = TRUE)
mntd.obs_log_min
mntd.obs_log_max

#
global_map.diversity <- global_map.diversity %>%
  mutate(pd_significance = case_when(
    P_value_PD > 0.975 ~ "Clustered",
    P_value_PD < 0.025 ~ "Overdispersed",
    P_value_PD <= 0.975 & P_value_PD >= 0.025 ~ "Random"
  )) %>%
  mutate(mpd_significance = case_when(
    P_value_MPD > 0.975 ~ "Clustered",
    P_value_MPD < 0.025 ~ "Overdispersed",
    P_value_MPD <= 0.975 & P_value_MPD >= 0.025 ~ "Random"
  )) %>%
  mutate(mntd_significance = case_when(
    P_value_MNTD > 0.975 ~ "Clustered",
    P_value_MNTD < 0.025 ~ "Overdispersed",
    P_value_MNTD <= 0.975 & P_value_MNTD >= 0.025 ~ "Random"
  )) %>%
  mutate(pd_significance = factor(pd_significance, levels = c("Clustered", "Overdispersed", "Random")),
         mpd_significance = factor(mpd_significance, levels = c("Clustered", "Overdispersed", "Random")),
         mntd_significance = factor(mntd_significance, levels = c("Clustered", "Overdispersed", "Random"))
         )

table(global_map.diversity$pd_significance)
table(global_map.diversity$mpd_significance)
table(global_map.diversity$mntd_significance)

# ==========================================================================
# 岛屿设置
# ==========================================================================
# 岛屿位置
island_points <- global_map.diversity %>%
  filter(island == "yes")

# 修复几何形状中的不合法几何
island_points_a_valid <- island_points %>%
  mutate(geometry = st_make_valid(geometry))%>%
  mutate(area = st_area(geometry))

  # 计算岛屿区域的几何中心
threshold_area <- units::set_units(1e10, "m^2")
island_points_a <- island_points_a_valid %>%
  st_transform(crs = st_crs(global_map.diversity)) %>%  # 确保坐标系一致
  filter(area < threshold_area) %>%
  st_centroid()

# ==========================================================================
# 自定义
# ==========================================================================
# 定义通用渐变色方案
color_palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(50)
color_palette <- alpha(color_palette, 0.8)  # 添加透明度

# 定义通用主题设置
theme_map <- theme_minimal() +
  theme(
    # 清除不必要的背景元素
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_line(linewidth = 0.3),
    axis.title = element_text(family = "serif", size = 12),
    axis.text = element_text(family = "serif", size = 12),
    legend.text = element_text(family = "serif", size = 12),
    legend.title = element_text(family = "serif", size = 12),
    # 设置图例位置
    legend.position = c(0.05, 0.15),
    legend.justification = c(0, 0)
  )

# ==========================================================================
# SR
# ==========================================================================
# 绘制图形
plot.SR <- ggplot(data = global_map.diversity) +
  geom_sf(aes(fill = SR_log)) +  # 使用对数转化后的 SR 进行填充
  geom_sf(data = island_points_a, aes(geometry = geometry, color = SR_log),
          size = 2, shape = 21, fill = "transparent", stroke = 2) +  #添加岛屿信息 按照SR_log映射颜色
  theme_map +
  scale_color_gradientn(
    colors = color_palette,
    name = "Taxonomic diversity",
    na.value = "white",
    limits = c(SR_log_min, SR_log_max),
    breaks = c(SR_log_min, SR_log_max),
    labels = c(paste0("Min: ", round(10^SR_log_min - 1, 0)),
               paste0("Max: ", round(10^SR_log_max - 1, 0)))
  ) +
  scale_fill_gradientn(
    colors = color_palette,
    name = "Taxonomic diversity",
    na.value = "white",
    limits = c(SR_log_min, SR_log_max),
    breaks = c(SR_log_min, SR_log_max),
    labels = c(paste0("Min: ", round(10^SR_log_min - 1, 0)),
               paste0("Max: ", round(10^SR_log_max - 1, 0)))
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 0.5,   # 颜色条宽度
      barheight = 5,   # 颜色条高度
      title.position = "top",
      label.position = "right"
    )
  )

# 显示图形
print(plot.SR)

ggexport(plot.SR, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241130.global_map.plot.SR.png",
    width = 2400,
    height = 2000,
    pointsize = 12,
    res = 300)

# ==========================================================================
# PD
# ==========================================================================
plot.PD <- ggplot(data = global_map.diversity) +
  geom_sf(aes(fill = PD_log)) +  # 使用对数转化后的 pd 进行填充
  geom_sf(data = island_points_a, aes(geometry = geometry, color = PD_log),
          size = 2, shape = 21, fill = "transparent", stroke = 2) +  #添加岛屿信息 按照SR_log映射颜色
  theme_map +
  scale_color_gradientn(
    colors = color_palette,
    name = "Phylogenetic diversity\n(Mya)",
    na.value = "white",
    limits = c(PD_log_min, PD_log_max),
    breaks = c(PD_log_min, PD_log_max),
    labels = c(paste0("Min: ", round(10^PD_log_min - 1, 0)),
               paste0("Max: ", round(10^PD_log_max - 1, 0)))
  ) +
  scale_fill_gradientn(
    colors = color_palette,
    name = "Phylogenetic diversity\n(Mya)",
    na.value = "white",
    limits = c(PD_log_min, PD_log_max),
    breaks = c(PD_log_min, PD_log_max),
    labels = c(paste0("Min: ", round(10^PD_log_min - 1, 0)),
               paste0("Max: ", round(10^PD_log_max - 1, 0)))
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 0.5,   # 颜色条宽度
      barheight = 5,   # 颜色条高度
      title.position = "top",
      label.position = "right"
    )
  )

# 显示图形
print(plot.PD)

ggexport(plot.PD, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241130.global_map.plot.PD.png",
    width = 2400,
    height = 2000,
    pointsize = 12,
    res = 300)

# ==========================================================================
# mpd.obs
# ==========================================================================
plot.mpd.obs <- ggplot(data = global_map.diversity) +
  geom_sf(aes(fill = mpd.obs_log)) +  # 使用对数转化后的 pd 进行填充
  geom_sf(data = island_points_a, aes(geometry = geometry, color = mpd.obs_log),
          size = 2, shape = 21, fill = "transparent", stroke = 2) +  #添加岛屿信息 按照SR_log映射颜色
  theme_map +
  scale_color_gradientn(
    colors = color_palette,
    name = "MPD (Mya)",
    na.value = "white",
    limits = c(mpd.obs_log_min, mpd.obs_log_max),
    breaks = c(mpd.obs_log_min, mpd.obs_log_max),
    labels = c(paste0("Min: ", round(10^mpd.obs_log_min - 1, 0)),
               paste0("Max: ", round(10^mpd.obs_log_max - 1, 0)))
  ) +
  scale_fill_gradientn(
    colors = color_palette,
    name = "MPD (Mya)",
    na.value = "white",
    limits = c(mpd.obs_log_min, mpd.obs_log_max),
    breaks = c(mpd.obs_log_min, mpd.obs_log_max),
    labels = c(paste0("Min: ", round(10^mpd.obs_log_min - 1, 0)),
               paste0("Max: ", round(10^mpd.obs_log_max - 1, 0)))
  ) +
  guides(
      fill = guide_colorbar(
      barwidth = 0.5,   # 颜色条宽度
      barheight = 5,   # 颜色条高度
      title.position = "top",
      label.position = "right"
    )
  )

# 显示图形
print(plot.mpd.obs)

ggexport(plot.mpd.obs, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241130.global_map.plot.mpd.obs.png",
    width = 2400,
    height = 2000,
    pointsize = 12,
    res = 300)

# ==========================================================================
# mntd.obs
# ==========================================================================
plot.mntd.obs <- ggplot(data = global_map.diversity) +
  geom_sf(aes(fill = mntd.obs_log)) +  # 使用对数转化后的 pd 进行填充
  geom_sf(data = island_points_a, aes(geometry = geometry, color = mntd.obs_log),
          size = 2, shape = 21, fill = "transparent", stroke = 2) +  #添加岛屿信息 按照SR_log映射颜色
  theme_map +
  scale_color_gradientn(
    colors = color_palette,
    name = "MNTD (Mya)",
    na.value = "white",
    limits = c(mntd.obs_log_min, mntd.obs_log_max),
    breaks = c(mntd.obs_log_min, mntd.obs_log_max),
    labels = c(paste0("Min: ", round(10^mntd.obs_log_min - 1, 0)),
               paste0("Max: ", round(10^mntd.obs_log_max - 1, 0)))
  ) +
  scale_fill_gradientn(
    colors = color_palette,
    name = "MNTD (Mya)",
    na.value = "white",
    limits = c(mntd.obs_log_min, mntd.obs_log_max),
    breaks = c(mntd.obs_log_min, mntd.obs_log_max),
    labels = c(paste0("Min: ", round(10^mntd.obs_log_min - 1, 0)),
               paste0("Max: ", round(10^mntd.obs_log_max - 1, 0)))
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 0.5,   # 颜色条宽度
      barheight = 5,   # 颜色条高度
      title.position = "top",
      label.position = "right"
    )
  )

# 显示图形
print(plot.mntd.obs)

ggexport(plot.mntd.obs, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241130.global_map.plot.mntd.obs.png",
    width = 2400,
    height = 2000,
    pointsize = 12,
    res = 300)

# ==========================================================================
# 整合图
# ==========================================================================
# 合并标准化后的多样性指标
combined_maps01 <-
plot.SR +
plot.PD +
plot_layout(ncol = 1) +
plot_annotation(tag_levels = 'A')

combined_maps01

# 保存图像
ggexport(combined_maps01, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241130.global_map.plot.SRPD.png",
    width = 2400,
    height = 4000,
    pointsize = 12,
    res = 300)

# 合并的系统发育指标
combined_maps02 <-
plot.mpd.obs +
plot.mntd.obs +
plot_layout(ncol = 1) +
plot_annotation(tag_levels = 'A')

combined_maps02

# 保存图像
ggexport(combined_maps02, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241130.global_map.plot.mpdmntd.obs.png",
    width = 2400,
    height = 4000,
    pointsize = 12,
    res = 300)

# ==========================================================================
# 作图
# ==========================================================================
# ==========================================================================
# plot.pd.pvalue
# ==========================================================================
plot.pd.pvalue <-
ggplot(data = global_map.diversity) +
  geom_sf(aes(fill = pd_significance)) +
  geom_sf(data = island_points_a, aes(geometry = geometry, color = pd_significance),
          size = 2, shape = 21, fill = "transparent", stroke = 2) +  #添加岛屿信息 按照SR_log映射颜色
  theme_map +
  scale_color_manual(values = c("Clustered" = alpha("red", 0.6),
                                "Overdispersed" = alpha("blue", 0.6),
                                "Random" = alpha("grey", 0.9)),
                    name = "Pattern of\nphylogenetic diversity",
                    na.value = "white",
                    guide = "none") +
  scale_fill_manual(values = c("Clustered" = alpha("red", 0.6),
                               "Overdispersed" = alpha("blue", 0.6),
                              "Random" = alpha("grey", 0.9)),
                    name = "Pattern of\nphylogenetic diversity",
                    na.value = "white") +
guides(
      fill = guide_legend(
      ncol = 1,  # 设置图例列数
      byrow = TRUE,  # 按行填充
      override.aes = list(size = 5),  # 增加图例符号的大小
      keywidth = 0.5,  # 调整图例键的宽度
      keyheight = 0.5  # 调整图例键的高度
    ))

plot.pd.pvalue

ggexport(plot.pd.pvalue, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241130.global_map.plot.pd.pvalue.png",
    width = 2400,
    height = 2000,
    pointsize = 12,
    res = 300)

# ==========================================================================
# plot.mpd.pvalue
# ==========================================================================
# global_map.diversity <- global_map.diversity %>%
#   mutate(mpd_significance = case_when(
#     P_value_MPD > 0.975 ~ "Clustered",
#     P_value_MPD < 0.025 ~ "Overdispersed",
#     P_value_MPD <= 0.975 & P_value_MPD >= 0.025 ~ "Random"
#   )) %>%
#   mutate(mpd_significance = factor(mpd_significance, levels = c("Clustered", "Overdispersed", "Random")))

# table(global_map.diversity$mpd_significance)

plot.mpd.pvalue <-
ggplot(data = global_map.diversity) +
  geom_sf(aes(fill = mpd_significance)) +
  geom_sf(data = island_points_a, aes(geometry = geometry, color = mpd_significance),
          size = 2, shape = 21, fill = "transparent", stroke = 2) +  #添加岛屿信息 按照SR_log映射颜色
  theme_map +
  scale_color_manual(values = c("Clustered" = alpha("red", 0.6),
                                "Overdispersed" = alpha("blue", 0.6),
                                "Random" = alpha("grey", 0.9)),
                    name = "Pattern of MPD",
                    na.value = "white",
                    guide = "none") +
  scale_fill_manual(values = c("Clustered" = alpha("red", 0.6),
                               "Overdispersed" = alpha("blue", 0.6),
                               "Random" = alpha("grey", 0.9)),
                    name = "Pattern of MPD",
                    na.value = "white") +
guides(
    fill = guide_legend(
      ncol = 1,  # 设置图例列数
      byrow = TRUE,  # 按行填充
      override.aes = list(size = 5),  # 增加图例符号的大小
      keywidth = 0.5,  # 调整图例键的宽度
      keyheight = 0.5  # 调整图例键的高度
    )
  )

plot.mpd.pvalue

ggexport(plot.mpd.pvalue, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241130.global_map.plot.mpd.pvalue.png",
    width = 2400,
    height = 2000,
    pointsize = 12,
    res = 300)

# ==========================================================================
# plot.mntd.pvalue
# ==========================================================================
# global_map.diversity <- global_map.diversity %>%
#   mutate(mntd_significance = case_when(
#     P_value_MNTD > 0.975 ~ "Clustered",
#     P_value_MNTD < 0.025 ~ "Overdispersed",
#     P_value_MNTD <= 0.975 & P_value_MNTD >= 0.025 ~ "Random"
#   )) %>%
#   mutate(mntd_significance = factor(mntd_significance, levels = c("Clustered", "Overdispersed", "Random")))

# table(global_map.diversity$mntd_significance)

plot.mntd.pvalue <-
ggplot(data = global_map.diversity) +
  geom_sf(aes(fill = mntd_significance)) +
  geom_sf(data = island_points_a, aes(geometry = geometry, color = mntd_significance),
          size = 2, shape = 21, fill = "transparent", stroke = 2) +  #添加岛屿信息 按照SR_log映射颜色
  theme_map +
  scale_color_manual(values = c("Clustered" = alpha("red", 0.6),
                               "Overdispersed" = alpha("blue", 0.6),
                              "Random" = alpha("grey", 0.9)),
                    name = "Pattern of MNTD",
                    na.value = "white",
                    guide = "none") +
  scale_fill_manual(values = c("Clustered" = alpha("red", 0.6),
                               "Overdispersed" = alpha("blue", 0.6),
                              "Random" = alpha("grey", 0.9)),
                    name = "Pattern of MNTD",
                    na.value = "white") +
  guides(
    fill = guide_legend(
      ncol = 1,  # 设置图例列数
      byrow = TRUE,  # 按行填充
      override.aes = list(size = 5),  # 增加图例符号的大小
      keywidth = 0.5,  # 调整图例键的宽度
      keyheight = 0.5  # 调整图例键的高度
    )
  )


plot.mntd.pvalue

ggexport(plot.mntd.pvalue, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241130.global_map.plot.mntd.pvalue.png",
    width = 2400,
    height = 2000,
    pointsize = 12,
    res = 300)

# ==========================================================================
# 整合图
# ==========================================================================
# 合并标准化后的系统发育指标
combined_maps03 <-
plot.pd.pvalue +
plot.mpd.pvalue +
plot.mntd.pvalue +
plot_layout(ncol = 1) +
plot_annotation(tag_levels = 'A')

combined_maps03

# 保存图像
ggexport(combined_maps03, filename = "D:/我的坚果云/2023.liyuan_database/新GADM数据集/results/20241130.global_map.plot.pdmpdmntd.pvalue.png",
    width = 2400,
    height = 6000,
    pointsize = 12,
    res = 300)
