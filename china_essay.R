# Code for producing plots and statistics used in China long development 
# review essay. 

# Contents:
#  0. Set up environment
#  1. Long run share of global gdp figure 
#  2. Growth under Mao
#  3. Modern economic growth

##################################################################
# 0. Set up environment
##################################################################
rm(list = ls())

library(tidyverse)
library(readxl)

theme_set(theme_bw())
base = "/Users/tombearpark/Documents/princeton/1st_year/POL523/essays/"
dir = paste0(base, "data/")
output = paste0(base, "figs/")
sup = suppressWarnings

##################################################################
# 1. Chinese share of global GDP in the very long run
##################################################################

# Include a 2019 value: 
# https://data.worldbank.org/?locations=CN-1W-EU-US
world_gdp = 87.6
chn_ratio = 14.3 / world_gdp
eu_ratio= 15.6 / world_gdp
us_ratio = 21.4 / world_gdp

lr_gdp_share = read_xlsx(paste0(dir, 
                                "long_run_china_share_global_gdp.xlsx")) %>% 
  filter(Country %in% c("China", "Europe", "US"))

lr_gdp_share$`2019` = c(chn_ratio, eu_ratio, us_ratio)

lr_gdp_share = lr_gdp_share%>% 
  mutate(`2019` = 100* `2019`) %>% 
  pivot_longer(!Country, names_to = "year", 
                                     values_to = "percent_global_gdp") %>% 
  mutate(year = as.numeric(year)) %>%
  rename(Region = Country)

ggplot(lr_gdp_share) + 
  geom_line(aes(x = year, y = percent_global_gdp, color = Region)) +
  xlab("") + ylab("Percent of Global GDP") +
  labs(title = "Long run global GDP Shares", 
       caption = "Source: Maddison estimates and calculations from WB data")
ggsave(paste0(output, "LR_shares_global_GDP.png"), height = 5, width = 7)

##################################################################
# 2. Growth under Mao
##################################################################

get_wb_data = function(string, ind_name){
  
  gdp_pc = read_csv(paste0(dir, string,"/", string, ".csv"), skip = 4)
  
  df = gdp_pc %>% 
    select(-c(`Country Code`,	`Indicator Name`,	`Indicator Code`, X66)) %>%
    pivot_longer(!`Country Name`, names_to = "year", values_to = ind_name) %>%
    mutate(year = as.numeric(year)) %>% 
    rename(Region = `Country Name`)
  
  return(df)
}

df_PC = sup(get_wb_data("API_NY.GDP.PCAP.CD_DS2_en_csv_v2_1495171", 
                        "GDP_PC")) 


df_PC = df_PC %>% 
  filter(Region %in% c("World", "China"))

df_growth = sup(get_wb_data("API_NY.GDP.PCAP.KD.ZG_DS2_en_csv_v2_1495281", 
                     "GDP_PC_GROWTH")) 
# calcualte variance 
df_growth %>% filter(year <1979) %>% group_by(Region) %>% 
  summarise(sd = sd(GDP_PC_GROWTH, na.rm  =TRUE)) %>% 
  filter(sd>10)

df_growth = df_growth %>% 
  filter(Region %in% c("World", "China")) 

plot_df = bind_rows(
  df_PC %>% mutate(var = "GDP_PC") %>% rename(value = GDP_PC), 
  df_growth %>% mutate(var = "GDP_PC_growth") %>% rename(value = GDP_PC_GROWTH)        
  ) 

ggplot(data = plot_df %>% filter(year < 1979) ) + 
  geom_line(aes(x = year, y = value, color = Region)) +
  facet_wrap(.~var, scales = "free") + xlab("") + ylab("") +
  geom_hline(yintercept= 0, color = "black", alpha = 0.7) +
  labs(title = "Growth Under Mao", 
       caption = "Source: WB data")
ggsave(paste0(output, "growth_mao.png"), height = 5, width = 8)

##################################################################
# 3. Rapid modern growth
##################################################################

ggplot(data = plot_df %>% filter(year >= 1979) ) + 
  geom_line(aes(x = year, y = value, color = Region)) +
  facet_wrap(.~var, scales = "free") + xlab("") + ylab("") +
  geom_hline(yintercept= 0, color = "black", alpha = 0.7) +
  labs(title = "Modern Chinese Growth", 
       caption = "Source: WB data")
ggsave(paste0(output, "growth_modern.png"), height = 5, width = 8)





