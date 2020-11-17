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
library(sf)

theme_set(theme_bw())

base = "/Users/tombearpark/Documents/princeton/1st_year/POL523/essays/"
dir = paste0(base, "data/")
output = paste0(base, "figs/")

df = read_xlsx(paste0(dir, "Bjørnskov-Rode-integrated-dataset-v3.2.xlsx")) %>% 
  mutate(`country isocode` = ifelse(`country isocode` == "GER", "DEU", `country isocode`))

plot_df = df %>% 
  filter(year == 2019) %>% 
  rename(iso_a3 = `country isocode`)

world <- ne_countries(scale = "medium", returnclass = "sf")
plot = anti_join(plot_df, world, by = "iso_a3") %>% sf::st_as_sf()

ggplot(data = plot) +
  geom_sf(aes(fill = as.factor(Democracy))) + ggtitle("2019")
  


# Teorell data... 
devtools::install_github("vdeminstitute/vdemdata")
library(vdemdata)
df = vdem
plot_df = df %>% 
  select(c("country_name", "year", "e_migdpgro", "v2x_partipdem", "e_migdppc"))


find_var("gdp")

ggplot(plot_df) + 
  geom_point(aes(x= v2x_partipdem, y = log(e_migdppc), 
                 color = year, alpha = 0.4))

ggplot(filter(plot_df, year %in% c(2010, 1950)), group = year) + 
  geom_density(aes(x = v2x_partipdem, fill = year))

lm(data = plot_df, v2x_partipdem ~ e_migdppc  ) %>% 
  summary()

c_df = plot_df %>% filter(country_name %in% c("China", "South Korea", 
                                              "Nigeria", "Brazil", "India"))
c_df %>% 
  ggplot() + 
  geom_line(aes(x = year, y  = v2x_partipdem, color = country_name)) +
  geom_line(aes(x = year, y  = e_migdppc, color = country_name)) + 
  ggtitle("Teoral Participatory Democracy Index") + 
  facet_wrap(~country_name)

lm(data = c_df,v2x_partipdem  ~ e_migdppc + e_migdppc^2) %>% summary()

plot = left_join()



