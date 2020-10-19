# Code for producing plots and statistics used in China long development 
# review essay. 

# Contents:
#  0. Set up environment
#  1. Long run share of global gdp figure 
#  2. 
#  3. 

##################################################################
# 0. Set up environment
##################################################################

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

chn_ratio = 14.3 / 87.6
eu_ratio= 15.6 / 87.6
us_ratio = 21.4 / 87.6
lr_gdp_share = read_xlsx(paste0(dir, 
                                "long_run_china_share_global_gdp.xlsx")) %>% 
  filter(Country %in% c("China", "Europe", "US"))

lr_gdp_share$`2019` = c(chn_ratio, eu_ratio, us_ratio)

lr_gdp_share = lr_gdp_share%>% 
  mutate(`2019` = 100* `2019`) %>% 
  pivot_longer(!Country, names_to = "year", 
                                     values_to = "percent_global_gdp") %>% 
  mutate(year = as.numeric(year)) 

ggplot(lr_gdp_share) + 
  geom_line(aes(x = year, y = percent_global_gdp, color = Country)) +
  xlab("") + ylab("Percent of Global GDP") +
  labs(title = "Long run global GDP Shares", 
       caption = "Source: Maddison estimates and calculations from WB data")
ggsave(paste0(output, "LR_shares_global_GDP.png"))














# 2. Growth under Mao

get_wb_data = function(string, ind_name){
  
  gdp_pc = read_csv(
    paste0(dir, string,"/", 
            string, ".csv"), 
                    skip = 4)
  
  df = gdp_pc %>% 
    select(-c(`Country Code`,	`Indicator Name`,	`Indicator Code`, X66)) %>%
    pivot_longer(!`Country Name`, names_to = "year", values_to = ind_name) %>%
    mutate(year = as.numeric(year)) %>% 
    rename(region = `Country Name`)
  
  return(df)
}

df_PC = sup(get_wb_data("API_NY.GDP.PCAP.CD_DS2_en_csv_v2_1495171", "GDP_PC")) %>% 
  filter(region %in% c("World", "China"))

df_growth = sup(get_wb_data("API_NY.GDP.PCAP.KD.ZG_DS2_en_csv_v2_1495281", 
                     "GDP_PC_GROWTH")) %>% 
  filter(region %in% c("World", "China"))

plot_df_mao = bind_rows(
  df_PC %>% mutate(var = "GDP_PC") %>% rename(value = GDP_PC), 
  df_growth %>% mutate(var = "GDP_PC_growth") %>% rename(value = GDP_PC_GROWTH)        
  ) %>% filter(year < 1979)


ggplot(data = plot_df_mao) + 
  geom_line(aes(x = year, y = value, color = region)) +
  facet_wrap(.~var, scales = "free")


  
# 3. GDP growth



df_CHN$GDP_PC_GROWTH[df_CHN$year > 1990 & df_CHN$country_name == "China"] %>% 
  mean(na.rm = TRUE)

df_CHN$GDP_PC_GROWTH[df_CHN$year > 1990 & df_CHN$country_name == "World"] %>% 
  mean(na.rm = TRUE)

ggplot(data = df_CHN) +
  geom_line(aes(x = year, y = GDP_PC_GROWTH, color = country_name)) +
  xlab("") + geom_hline(yintercept =0)




# Outlier?

library(wbstats)

# All country growth rates
df = get_wb_data("API_NY.GDP.PCAP.KD.ZG_DS2_en_csv_v2_1495281", 
                 "GDP_PC_GROWTH") %>% filter(year > 1990) %>% 
  group_by(country_name) %>% 
  summarise(mean_GDPPC_gr = mean(GDP_PC_GROWTH, na.rm = TRUE))

# Population
mydf <- wb(country = "all",
           indicator = "SP.POP.TOTL", 
           startdate = 2016,
           enddate = 2019)

# clean population data
pop = mydf %>% filter(date == 2019) %>% 
  select(country,  value) %>% 
  rename(country_name = country, population = value)

# Chinese population
pop %>% filter(country_name %in% c("China", "World"))

# join 
plot_df = left_join(pop, df, by = "country_name")

ggplot(data =plot_df ) + 
  geom_point(aes(x = population, y = mean_GDPPC_gr))

df 
