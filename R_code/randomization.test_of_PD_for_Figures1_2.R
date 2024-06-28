# -*- coding: utf-8 -*-
# @Author:
# @Date: 2023-07-29 20:50:12
# @Last Modified by: dbc
# @Last Modified time: 2024-06-28 14:04:05
# @Description: Figures 1_6

# Load required R packages
library(ape)
library(dplyr)  # for filter
library(ggtree)
library(picante)
library(readr)  # for read_csv
library(readxl)  # for read_excel
library(stringr)
library(tidyverse)

# # Load “PhyloMeasures” package
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/PhyloMeasures/PhyloMeasures_2.1.tar.gz"
# install.packages(packageurl, repos = NULL, type = "source")

# clear memory
cat("\014")
rm(list = ls())
gc()

# file path
path <- "D:/我的坚果云/2023.liyuan_database"

# set file path
setwd(path)
getwd()

# load data
checklist <- read_csv("./GRIIS_checklist/GRIIS.iso.asia.20230729a.csv")
str(checklist)
View(checklist)

# filter available Asian countries with SR that is greater than 1
avail.country <- checklist %>% count(alpha_3) %>% filter(n > 1) %>% pull(alpha_3)
avail.country

#
avail.checklist <- checklist %>% filter(alpha_3 %in% avail.country)

# save data
write_excel_csv(avail.checklist, "avail.checklist.csv")

# Create a 0-1 matrix based on alpha_3 and tip.label
checklist.matrix <- avail.checklist %>%
mutate(presence = 1) %>%
pivot_wider(id_cols = alpha_3, names_from = rgbif.tip.label, values_from = presence) %>%
mutate_at(vars(-alpha_3), ~ ifelse(is.na(.), 0, 1))

View(checklist.matrix)

# save data
write_excel_csv(checklist.matrix, "./GRIIS_checklist/checklist.matrix.SRgreaterthan1.csv")

# use alpha_3 as the row name for the matrix
checklist.matrix01 <- checklist.matrix %>% column_to_rownames(var = "alpha_3")
head(checklist.matrix01)
view(checklist.matrix01)

# ==========================================================================
# Load tree data
# ==========================================================================
# Load tree data
phylo.tree <- read.tree("./GRIIS_checklist/GRIIS.phylo_tree.SRgreaterthan1.tre")
phylo.tree

# load iso 3166
checklist.tree00 <- read_csv("./GRIIS_checklist/GRIIS.phylo_tree.csv")
str(checklist.tree00)
View(checklist.tree00)

# filter data with available info of countries with SR greater than 1
checklist.tree <- checklist.tree00 %>%
filter(avail.country.status.SRgreaterthan1 == "yes") %>%  # Filter available country data
select(species, genus, family, species.relative, genus.relative)

# view data
glimpse(checklist.tree)
View(checklist.tree)

# simple plot of phylogenetic tree
ggtree(tr = phylo.tree, layout = "fan", size = 0.1, col = "grey30") +
  theme_tree()

# ==========================================================================
# calculate pd, mpd, mntd
# ==========================================================================
# pd
checklist.pd <- pd(checklist.matrix01, phylo.tree, include.root = FALSE)
checklist.pd01 <- checklist.pd %>% rownames_to_column(var = "alpha_3")
checklist.pd01

# mpd
phylo.matrix <- cophenetic(phylo.tree)
checklist.mpd <- mpd(samp = checklist.matrix01, dis = phylo.matrix, abundance.weighted = FALSE)
checklist.mpd01 <- data.frame(alpha_3 = rownames(checklist.matrix01), mpd.obs = checklist.mpd)
checklist.mpd01

# mntd
checklist.mntd <- mntd(samp = checklist.matrix01, dis = phylo.matrix, abundance.weighted = FALSE)
checklist.mntd01 <- data.frame(alpha_3 = rownames(checklist.matrix01), mntd.obs = checklist.mntd)
checklist.mntd01

# ==========================================================================
# randomization test of pd, mpd and mntd
# ==========================================================================
# randomization test of pd
set.seed(2023)
checklist.ses.pd <- ses.pd(samp = checklist.matrix01, tree = phylo.tree, null.model = "richness")
checklist.ses.pd

checklist.ses.pd01 <- checklist.ses.pd %>% rownames_to_column(var = "alpha_3")
checklist.ses.pd01

# randomization test of mpd
set.seed(2023)
checklist.ses.mpd <- ses.mpd(samp = checklist.matrix01, dis = phylo.matrix, null.model = "richness")
checklist.ses.mpd01 <- checklist.ses.mpd %>% rownames_to_column(var = "alpha_3")
checklist.ses.mpd01

# randomization test of mntd
set.seed(2023)
checklist.ses.mntd <- ses.mntd(samp = checklist.matrix01, dis = phylo.matrix, null.model = "richness")
checklist.ses.mntd01 <- checklist.ses.mntd %>% rownames_to_column(var = "alpha_3")
checklist.ses.mntd01

# save data
write_excel_csv(checklist.pd01, "./GRIIS_checklist/GRIIS.phylo_tree.pd.SRgreaterthan1.csv")
write_excel_csv(checklist.mpd01, "./GRIIS_checklist/GRIIS.phylo_tree.mpd.SRgreaterthan1.csv")
write_excel_csv(checklist.mntd01, "./GRIIS_checklist/GRIIS.phylo_tree.mntd.SRgreaterthan1.csv")

write_excel_csv(checklist.ses.pd01, "./GRIIS_checklist/GRIIS.phylo_tree.ses.pd.SRgreaterthan1.csv")
write_excel_csv(checklist.ses.mpd01, "./GRIIS_checklist/GRIIS.phylo_tree.ses.mpd.SRgreaterthan1.csv")
write_excel_csv(checklist.ses.mntd01, "./GRIIS_checklist/GRIIS.phylo_tree.ses.mntd.SRgreaterthan1.csv")

# Merge SR, PD, MPD, and MNTD data into alpha_3 data
# Merge data
checklist.phylo.metrics01 <- checklist.pd01 %>%
left_join(checklist.mpd01, by = "alpha_3") %>%
left_join(checklist.mntd01, by = "alpha_3")

checklist.phylo.metrics01

# Save data
write_excel_csv(checklist.phylo.metrics01, "./GRIIS_checklist/GRIIS.phylo.metrics.SRgreaterthan1.csv")

