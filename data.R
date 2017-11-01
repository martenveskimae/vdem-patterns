library(tidyverse)
library(dtwclust)
library(reshape2)
Sys.setlocale("LC_ALL", "UTF-8")

# List of vars
vars = read.csv("vars.csv",stringsAsFactors=F)

# Data function
vdem_data = function(loc = "vdem71.csv",
                     countries = c("Estonia","Latvia","Lithuania"),
                     cols = NULL,
                     y = 1990:2017,
                     std = T,
                     input = T,
                     dif = T,
                     smooth = T)
{
  d = data.table::fread(loc) %>%
    filter(country_name %in% countries,
           as.numeric(year) %in% y) %>%
    .[c("country_name", "year", cols)]
  s = function(x)smooth(x)
  di = function(x) c(0,tail(x,-1) - head(x,-1))
  if(std==T) d[cols] = scale(d[cols])
  if(input==T) d = fill(d,cols)
  if(smooth==T) d[cols] = by(d[cols],d[1],function(x)apply(x,2,s)) %>% do.call(rbind.data.frame,.)
  if(dif==T) d[cols] = by(d[cols],d[1],function(x)apply(x,2,di)) %>% do.call(rbind.data.frame,.)
  return(d)
}

# Get data
sC = unique(vars$index)
cntry = c("Estonia","Latvia","Lithuania","Poland","Czech Republic","Slovakia","Slovenia","Bulgaria","Romania","Croatia","Cyprus")
vdem = vdem_data(cols=sC,countries=cntry,y=1991:2000)

# Viz vars
vdem %>%
  reshape2::melt(c("country_name", "year")) %>%
  ggplot() +
  geom_line(aes(year,value,color=country_name)) +
  facet_wrap(~variable)

# Reshape data for clustering
vdem_rs = vdem %>%
  melt(c("country_name","year")) %>%
  mutate(cntry_var = paste0(country_name,"-",variable)) %>%
  dcast(year~cntry_var,value.var="value")
vdem_rs[is.na(vdem_rs)] = 0

#############################
## CLUSTERING
#############################

# Clusters
vdem_clust = tsclust(t(vdem_rs[-1]),type="fuzzy",k=5,distance="gak",seed=100)
cntry_clust_max = apply(vdem_clust@fcluster,1,which.max) %>%
  data.frame(cntry_var = as.character(names(.)), cluster=.,
             row.names=NULL,stringsAsFactors=F) %>%
  mutate(country_name = sub("-.*$","",cntry_var),
         variable = sub(".*-","",cntry_var),
         cntry_var=NULL)

# Color boxes
cntry_clust_max %>%
  ggplot() +
  geom_tile(aes(country_name,variable,fill=factor(cluster))) +
  coord_flip() +
  theme_bw() +
  labs(x="",y="") +
  theme(axis.text.x = element_text(angle=90),
        aspect=length(unique(cntry_clust_max$country_name))/
          length(unique(cntry_clust_max$variable)))

# Prototypes
plot(vdem_clust,type="sc")
