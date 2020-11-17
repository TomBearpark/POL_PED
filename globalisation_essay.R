# Code to produce graph used in globalistion essy
rm(list = ls())
library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

# Set path strings
base <- "/Users/tombearpark/Documents/princeton/1st_year/POL523/essays/"
dir <- paste0(base, "data/")
output <- paste0(base, "figs/")

# Trade as proportion of global GDP, source:
##    https://ourworldindata.org/trade-and-globalization

df <- read_csv(paste0(dir, "merchandise-exports-gdp-cepii.csv"))
names(df)[4] <- "Exports_Percent_GDP"

ggplot(data = df%>% filter(Entity == "World")) + 
  geom_line(aes(x = Year, y = Exports_Percent_GDP)) + 
  labs(title = "Value of Global Exports as Share of World GDP", 
       caption = "Source: Fouquin and Hugot; CEPII 2016; National data")
ggsave(paste0(output, "exports_percent_global_GDP.png"), height = 5, width = 7)
