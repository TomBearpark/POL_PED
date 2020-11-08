# Code for producing plots and statistics used in democracy essay 
# review essay. 

# Contents:
#  0. Set up environment
#  1. Basic maps

##################################################################
# 0. Set up environment
##################################################################
rm(list = ls())

library(tidyverse)
library(readxl)
library(rnaturalearth)


theme_set(theme_bw())
base = "/Users/tombearpark/Documents/princeton/1st_year/POL523/essays/"
dir = paste0(base, "data/")
output = paste0(base, "figs/")

df = read_xlsx(paste0(dir, "Bjørnskov-Rode-integrated-dataset-v3.2.xlsx"))
length(unique(df$country))

plot_df = df %>% 
  filter(year == 2019) %>% 
  rename(iso_a3 = `country isocode`)

head(plot_df)

world <- ne_countries(scale = "medium", returnclass = "sf")
plot = left_join(world, plot_df, by = "iso_a3")

ggplot(data = plot) +
  geom_sf(aes(fill = as.factor(Democracy)) ) + ggtitle("2019")
  