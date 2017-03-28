# Initial data analysis
# Robert Kubinec

require(dplyr)
require(tidyr)
require(tibble)
require(ggplot2)
require(magrittr)
require(plotly)
require(stringr)
require(readr)
require(stringdist)

source('data_functions.R')

class2016 <- read_in('data/tabula-Classement 2016.tsv',shiftrow=TRUE,range_col=TRUE,group1=40,
                     group2=518,group3=160,data_year='2016',year1='year_2013',year2='year_2014')

class2016 %>% filter(money_type=='groupes') %>% plot_ly(y=~year_2014,x=~year_2013) %>% add_markers()

class2016 %>% filter(money_type=='groupes') %>%  mutate(winloss=year_2014 - year_2013) %>% plot_ly(y=~winloss,x=~reorder(firm_name,winloss)) %>% 
  add_bars() %>% layout(xaxis=list(title="",showticklabels=FALSE),
                        yaxis=list(title=""))

class2015 <- read_in('data/tabula-Classement 2015.tsv',shiftrow=FALSE,range_col=FALSE,
                     group1=40,group2=501,group3=166,data_year='2015',year1='year_2012',
                     year2='year_2013')

class2014 <- read.fwf('data/class2014_firststep.txt',c(3,-3,55,-2,28,-2,11,-2,7)) %>% as_data_frame
names(class2014) <- c('rang','firm_name','sector','year_2011','year_2012')
class2014 <- class2014 %>% mutate(data_year='2014',money_type=c(rep('groupes',43),rep('revenues',501),
                                                                rep('exports',150)))

class2014 %>% filter(money_type=='groupes') %>%  mutate(winloss=year_2012 - year_2011) %>% plot_ly(y=~winloss,x=~reorder(firm_name,winloss)) %>% 
  add_bars() %>% layout(xaxis=list(title="",showticklabels=FALSE),
                        yaxis=list(title=""))

# Merge data

merged <- bind_rows(filter(class2016,money_type=='revenues'),
                     filter(class2015,money_type=='revenues'),
                     filter(class2014,money_type=='revenues'))

# compute term distance to see how to match firms

#dist_matrix <- adist(merged$firm_name,merged$firm_name,ignore.case=TRUE,partial = FALSE)
dist_matrix <- lapply(trimws(merged$firm_name),function(x) 
  {
  out <- stringsim(x,trimws(merged$firm_name),method='qgram')

}) 
# need to look at the data word by word
names(dist_matrix) <- paste0('V',1:length(merged$firm_name))

dist_matrix <- as_data_frame(dist_matrix) %>% mutate(firm_names=merged$firm_name,data_year=merged$data_year,
                                                     firm_names=trimws(firm_names))
dist_matrix <- gather(dist_matrix,key=word,count,-firm_names,-data_year) %>% arrange(word,desc(count)) %>% group_by(word) %>% slice(1:3)
distinct(dist_matrix,word,data_year,.keep_all = TRUE) %>% write_csv(path='firstyears_crosswalk.csv')
