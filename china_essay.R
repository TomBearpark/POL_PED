library(tidyverse)
library(readxl)

theme_set(theme_bw())
dir = "/Users/tombearpark/Documents/princeton/1st_year/POL523/essays/data/"

# 1. Plot GDP PC 

get_wb_data = function(string, ind_name){
  
  gdp_pc = read_csv(
    paste0(dir, string,"/", 
            string, ".csv"), 
                    skip = 4)
  
  df = gdp_pc %>% 
    select(-c(`Country Code`,	`Indicator Name`,	`Indicator Code`, X66)) %>%
    pivot_longer(!`Country Name`, names_to = "year", values_to = ind_name) %>%
    mutate(year = as.numeric(year)) %>% 
    rename(country_name = `Country Name`)
  
  return(df)
}

df_CHN = get_wb_data("API_NY.GDP.PCAP.CD_DS2_en_csv_v2_1495171", "GDP_PC") %>% 
  filter(country_name %in% c("World", "China"))

ggplot(data = df_CHN) + 
  geom_line(aes(x = year, y = GDP_PC, color = country_name))


# 2. Chinese share of global GDP

# Include a 2019 value: 
# https://data.worldbank.org/?locations=CN-1W
ratio_2019 = 14.3 / 87.6

lr_gdp_share = read_xlsx(paste0(dir, 
                                "long_run_china_share_global_gdp.xlsx")) %>% 
  filter(Country == "China") %>% 
  mutate(`2019` = 100* ratio_2019) 

df_1 = lr_gdp_share %>% pivot_longer(!Country, names_to = "year", 
                      values_to = "percent_global_gdp") %>% 
  mutate(year = as.numeric(year)) 

ggplot(df_1) + 
  geom_point(aes(x = year, y = percent_global_gdp))
  
# 3. GDP growth

df_CHN = get_wb_data("API_NY.GDP.PCAP.KD.ZG_DS2_en_csv_v2_1495281", 
                     "GDP_PC_GROWTH") %>% 
  filter(country_name %in% c("World", "China"))

df_CHN$GDP_PC_GROWTH[df_CHN$year > 1990 & df_CHN$country_name == "China"] %>% 
  mean(na.rm = TRUE)

df_CHN$GDP_PC_GROWTH[df_CHN$year > 1990 & df_CHN$country_name == "World"] %>% 
  mean(na.rm = TRUE)

ggplot(data = df_CHN) +
  geom_line(aes(x = year, y = GDP_PC_GROWTH, color = country_name))




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
