library(tidyverse)
Sys.setlocale("LC_ALL", "UTF-8")

select_countries_year = function(data = "vdem71.csv",
                                 countries = c("Estonia", "Latvia" ,"Lithuania"),
                                 year = 1990)
{
  d = data.table::fread(data) %>%
    filter(country_name %in% countries,
           as.numeric(year) >= 1990) %>%
    rename(country = country_name) %>%
    select(everything(), -contains("_"))
  d[colSums(is.na(d)) < nrow(d)/10]
}

vdem = select_countries_year() %>%
  select(country, year, v2psparban, v2elembaut, v2juhcind, v2juncind, v2lginvstp,
         v2dlreason, v2cscnsult, v2csprtcpt, v2elreggov, v2psprbrch, v2psprlnks)
