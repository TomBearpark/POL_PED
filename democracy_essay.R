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
# 
# theme_set(theme_bw())
# 
# base = "/Users/tombearpark/Documents/princeton/1st_year/POL523/essays/"
# dir = paste0(base, "data/")
# output = paste0(base, "figs/")
# 
# df = read_xlsx(paste0(dir, "Bjørnskov-Rode-integrated-dataset-v3.2.xlsx")) %>% 
#   mutate(`country isocode` = ifelse(`country isocode` == "GER", "DEU", `country isocode`))
# 
# plot_df = df %>% 
#   filter(year == 2019) %>% 
#   rename(iso_a3 = `country isocode`)
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# plot = anti_join(plot_df, world, by = "iso_a3") %>% sf::st_as_sf()
# 
# ggplot(data = plot) +
#   geom_sf(aes(fill = as.factor(Democracy))) + ggtitle("2019")
#   
# 

# Teorell data... 
# https://www.v-dem.net/en/analysis/CountryGraph/

devtools::install_github("vdeminstitute/vdemdata")
library(vdemdata)
library(gganimate)
library(countrycode)

df = vdem

plot_df = df %>% 
  select(c("country_name", "year", "e_migdpgro", 
           "v2x_partipdem", "e_migdppc", "e_total_oil_income_pc", 
           "e_regiongeo", "e_peedgini", "country_id", "COWcode")) %>% 
  filter(year %in% c(1930, 1950, 1970, 1990, 2010, 2015)) %>% 
  mutate(region = ifelse(e_regiongeo %in% c(1, 2, 3, 4), "Europe", ""))%>% 
  mutate(region = ifelse(e_regiongeo %in% c(5,6,7,8,9), "Africa", region)) %>%
  mutate(region = ifelse(e_regiongeo %in% c(10,11,12,13,14), "Asia", region)) %>%
  mutate(region = ifelse(e_regiongeo %in% c(15), "Oceana", region)) %>%
  mutate(region = ifelse(e_regiongeo %in% c(16), "North America", region)) %>%
  mutate(region = ifelse(e_regiongeo %in% c(17,18,19), "Other America", region))




ggplot(data = plot_df ) + 
  geom_point(aes(y = v2x_partipdem, x = log(e_migdppc), color = e_peedgini)) + 
  facet_wrap(~year, ncol = 3) + xlab("Log GDPPC") + ylab("Democracy Index")


library(wbstats)
dfdb <- wb( indicator = "SI.POV.GINI", startdate = 1973, enddate = 2015) %>% 
  mutate(COWcode = countrycode(iso3c, origin = 'iso3c', destination = 'cown')) %>%
  rename(year = date) %>% mutate(year = as.numeric(year)) %>% 
  rename(gini = value)

plot_df1 = left_join(plot_df, dfdb, by = c("COWcode", "year"))


ggplot(data = plot_df1 %>% filter(year == 2015) ) + 
  geom_point(aes(y = v2x_partipdem, x = log(e_migdppc), color = gini)) + 
  xlab("Log GDPPC") + ylab("Democracy Index")






wb_search("inequality")


plot_df %>% filter(year == 2015, log(e_migdppc) > 10, )

reg_df = df 

# Everying in the regression... 
lm(data =reg_df, v2x_partipdem ~ log(e_migdppc) ) %>% summary()

# More recent
lm(data =reg_df, v2x_partipdem ~ log(e_migdppc) ) %>% summary()



# Animated version
p = ggplot(data = plot_df %>% filter(year %in% c(1930, 1950, 1970, 1990, 2010, 2015))) + 
  geom_point(aes(y = v2x_partipdem, x = log(e_migdppc)))

anim <- p +
  transition_states(year,
                    transition_length = 2,
                    state_length = 1)



library(gapminder)
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) 
  # # Here comes the gganimate specific bits
  # labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  # transition_time(year) +
  # ease_aes('linear')



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



