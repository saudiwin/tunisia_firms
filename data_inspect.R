# Initial data analysis
# Robert Kubinec

require(dplyr)
require(tidyr)
require(tibble)
require(ggplot2)
require(magrittr)
require(plotly)

class2016 <- read_in('data/tabula-Classement 2016.tsv',shiftrow=TRUE,range_col=TRUE,group1=40,
                     group2=518,group3=160,data_year='2016',year1='year_2013',year2='year_2014')

class2016 %>% filter(money_type=='groupes') %>% plot_ly(y=~year_2014,x=~year_2013) %>% add_markers()

class2016 %>% filter(money_type=='groupes') %>%  mutate(winloss=year_2014 - year_2013) %>% plot_ly(y=~winloss,x=~reorder(firm_name,winloss)) %>% 
  add_bars() %>% layout(xaxis=list(title="",showticklabels=FALSE),
                        yaxis=list(title=""))

class2015 <- read_in('data/tabula-Classement 2015.tsv',shiftrow=FALSE,range_col=FALSE,
                     group1=40,group2=501,group3=166,data_year='2015',year1='year_2012',
                     year2='year_2013')
