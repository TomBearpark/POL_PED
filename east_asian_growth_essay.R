

##################################################################
# 0. Set up environment
##################################################################
rm(list = ls())
library(tidyverse)
library(wbstats)
library(ggplot2)
library(sf)
library(viridis)

theme_set(theme_bw())
path <- "/Users/tombearpark/Documents/princeton/1st_year/POL523/essays/EA growth/"

##################################################################
# 1. Growth rate plots
##################################################################

# EA country list: 
EA <- c("Japan", "Korea, Rep.", "Vietnam" , "China", 
       "Malaysia", "Philippines", "Thailand")

# indicators of intereste
my_indicators <- c(
  life_exp = "SP.DYN.LE00.IN", 
  gdp_capita ="NY.GDP.PCAP.CD", 
  pop = "SP.POP.TOTL", 
  gdp_growth_rate = "NY.GDP.MKTP.KD.ZG", 
  ag_gdp_growth_rate = "NV.AGR.TOTL.ZG", 
  gini = "SI.POV.GINI"
)
d <- wb_data(my_indicators, start_date = 1960, end_date = 2020)

df <- d %>% filter(country %in% EA) %>% 
  pivot_longer(cols = c("gdp_growth_rate", 
                        "gdp_capita", "life_exp", "pop", "gini"))

ggplot(data = df , aes(group = country)) + 
  geom_line(aes(x = date, y = value, color = country))  + 
  facet_wrap(~name, scales = "free")

ggplot(data = df %>% filter(name == "gdp_growth_rate", country != "Japan") , 
       aes(group = country)) + 
  geom_line(aes(x = date, y = value, color = country)) +   
  facet_wrap(~country) + 
  geom_hline(yintercept = 0)
ggsave(paste0(path, "GDP_growth_time_series.png"), height = 8, width = 10)


# Summary stats;
filter(df, name == "gdp_growth_rate") %>% group_by(country) %>% 
  summarise(gdp_growth_rate = mean(value, na.rm = TRUE))
filter(df, name == "gdp_capita", date == 1960)
filter(df, name == "gdp_capita", date == 2019)
d %>% 
  summarise(gdp_growth_rate = mean(gdp_growth_rate, na.rm = TRUE))


plot_df = d %>% filter(country %in% c("Korea, Rep.", "Chile", "Brazil", "Argentina")) %>% 
  mutate(period = ifelse(date < 1985, "Pre 1985", "Post 1985"), 
         period = factor(period, levels = c("Pre 1985", "Post 1985"))) 

plot_df %>% 
  ggplot() + 
  geom_line(aes(x = date, y= gdp_capita, color = country), alpha = 1) +
    facet_wrap(vars(period), scales = "free") + 
  ggtitle("GDP Per Capita: South Korea vs Selected Latin American Countries") + 
  ylab("USD Per Person") + xlab("Year")

ggsave(paste0(path, "SK_vs_LA_growth.png"), height = 5, width = 10)

# Maps...
# https://fred.stlouisfed.org/series/RGDPNATWA666NRUG
# calculate Taiwan... 
g60 = 22085
d2010 = 862474 
g = exp((1/(2010-1960)) * log(d2010 / g60))

EA <- c("Japan", "Korea, Rep.", "Vietnam" , "China", 
        "Malaysia", "Philippines", "Thailand", "Indonesia", "Taiwan", "Laos", "Myanmar", "Cambodia", 
        "Papua New Guinea", "Hong Kong SAR, China")

world_geo <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")

d2019 <- d %>% filter(date >1960) %>% filter(date < 2010) %>%group_by(country, iso2c) %>% 
  summarise(gdp_growth_rate = mean(gdp_growth_rate, na.rm = TRUE)) %>% 
  arrange(gdp_growth_rate) %>% filter(!is.na(gdp_growth_rate)) %>% ungroup() %>% 
  add_row(country = "Taiwan", iso2c = "TW", gdp_growth_rate = 7.7)
  
pop_geo <- left_join(world_geo, d2019, by = c("iso_a2" = "iso2c"))%>% filter(country %in% EA)

ggplot(pop_geo %>% rename(`GDP Growth Rate (%)` = gdp_growth_rate)) +
  geom_sf(aes(fill = `GDP Growth Rate (%)` )) +
  scale_fill_viridis("% per year") + ggtitle("Average GDP Growth Rate, 1960 - 2010") +
  theme_bw()  +  geom_sf_label(aes(label = country), nudge_y = 2) + xlab("") + ylab("") + 
  labs(caption = "Sources: WB Data, Data for Taiwan from St Louis Fed Data") 

ggsave(paste0(path, "GDP_growth_map.png"), height = 8, width = 8)

##################################################################
# 2. # Democracy vs Income
##################################################################
# devtools::install_github("vdeminstitute/vdemdata")
library(vdemdata)
library(ggrepel)

df = vdem
EA <- c( "South Korea", "Taiwan", "China")

# Clean plotting df
plot_df = df %>% 
  select(c("country_name", "year", "e_migdpgro", 
           "v2x_partipdem", "e_migdppc", "e_total_oil_income_pc", 
           "e_regiongeo", "e_peedgini", "country_id", "COWcode")) %>% 
  mutate(EA_Country = ifelse(country_name %in% EA, 1, 0)) %>%
  mutate(lab = ifelse(EA_Country ==1, country_name, "" )) %>% 
  filter(year %in% c(1930, 1950,1970, 1990, 2010))

ggplot(data = plot_df, aes(color = as.factor(EA_Country))) + 
  geom_point(aes(y = v2x_partipdem, x = log(e_migdppc))) + 
  facet_wrap(~year, ncol = 3) +
  geom_label_repel(data=plot_df[plot_df$EA_Country == 1,], segment.color = "red",
                   aes(y = v2x_partipdem, x = log(e_migdppc), label = lab))+
                   # ,nudge_y       = 1 - plot_df[plot_df$EA_Country == 1,]$v2x_partipdem) + 
  xlab("Log GDPPC") + ylab("Democracy Index") + 
  scale_colour_manual(values = c("black", "red")) + 
  guides(color = FALSE) + 
  labs(caption = "Sources: Teorell et al. 2020 Varieties of Democracy Dataset") 

ggsave(paste0(path, "GDP_democracy_relationship.png"), height = 6, width = 8)

##################################################################
# 2.# Trade data...
##################################################################

library(comtradr)

ct_commodity_lookup("steel", 
                    return_char = TRUE)
q <- ct_search(reporters = "Japan", 
               partners = c("World"), 
               trade_direction = "imports", commod_codes = "720690") 

ggplot(data = q) + 
  geom_line(aes(x = year, y = trade_value_usd)) + 
  geom_vline(xintercept = 2017)








