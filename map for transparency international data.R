# Create map of transparency international data

require(ggmap)
require(maps)
require(mapdata)
require(readxl)
require(readr)
require(ggplot2)
require(dplyr)
require(extrafont)

tpi <- read_excel("C:/Users/bobku/Documents/R projects/tunisia_firms/data/CPI2016_FullDataSetWithRegionalTables.xlsx") %>% 
  mutate(Country=ifelse(Country=='The FYR of Macedonia','Macedonia',Country))

# We'll load a data frame of all the administrative boundaries in the world, and then build a custom map by just 
# keeping the states we want

states <- map_data('world') %>% as_data_frame


medit <- filter(states,region %in% c('Tunisia','Algeria','Egypt','Italy','Spain','France','Libya','Morocco','Greece',
                                     'Albania','Bulgaria','Bosnia and Herzegovina','Montenegro','Croatia',
                                     'Slovenia','Israel','Lebanon','Turkey','Syria','Switzerland','Austria','Jordan',
                                     'Germany','Macedonia','Serbia','Hungary','Kosovo')) %>% 
  filter(!((region %in% c('Spain','France','Italy','Greece')) & !(is.na(subregion))))

medit <- left_join(medit,tpi,by=c('region'='Country'))

# I manually created a CSV file with lon/lat locations for country labels (used Google Maps)

country_loc <- read_csv('data/country_labels.csv') %>% mutate(colored=ifelse(Country %in% c('Tunisia','Egypt'),FALSE,TRUE))

# Custom red/yellow color scale that Transparency International uses

colors <- grDevices::colorRampPalette(c('red','yellow'))(n=length(unique(medit$region)))

# The map has several layers, first a polygon for the boundaries and filled with Transparency data
# Then text labels for countries, and finallly an annotation layer for the lines connecting Egypt and Tunisia
# I used the Gentium font, you can change it if your system doesn't have it
# Transparency International Corruption Index 2016
  medit %>% ggplot() + geom_polygon(aes(x=long,y=lat,group=group,fill=CPI2016)) + coord_fixed(1.3) + theme_minimal() +
    scale_fill_gradientn(colors=colors,name='Transparency\nScore')  +
    theme(panel.grid=element_blank(),axis.text=element_blank(),text=element_text(family='Gentium Basic'),
          legend.position = c(.85,.8),legend.title.align=0,legend.direction='vertical') + xlab("") + ylab("") + 
    ggtitle('TRANSPARENCY INTERNATIONAL CORRUPTION INDEX 2016') +
    geom_text(data=country_loc,aes(y=lat,x=long,label=Country,color=colored,size=1-colored),family='Gentium Basic') + scale_color_grey(start=0.2,end=1) + 
    guides(colour='none',size='none') +
    annotate('segment',x=c(9.203367,27.720131),y=c(38.144133,32.68911),xend=c(9.203367,27.720131),yend=c(36.5,30.260214),
             colour='grey30') + scale_size(range=c(3.5,5.5)) 

ggsave('transparency_international.pdf',units='in',width=16,height=9,scale=0.7)

ggsave('transparency_international_beamer_withTE.pdf',units='in',width=12,height=9,scale=0.56)

country_loc <- filter(country_loc,colored==TRUE)

medit %>% ggplot() + geom_polygon(aes(x=long,y=lat,group=group,fill=CPI2016)) + coord_fixed(1.3) + theme_minimal() +
  scale_fill_gradientn(colors=colors,name='Transparency\nScore')  +
  theme(panel.grid=element_blank(),axis.text=element_blank(),text=element_text(family='Gentium Basic'),
        legend.position = c(.85,.8),legend.title.align=0,legend.direction='vertical') + xlab("") + ylab("") + 
  ggtitle('TRANSPARENCY INTERNATIONAL CORRUPTION INDEX 2016') +
  geom_text(data=country_loc,aes(y=lat,x=long,label=Country),color='white',family='Gentium Basic') + scale_color_grey(start=0.2,end=1) + 
  guides(colour='none',size='none')

ggsave('transparency_international_beamer_withoutTE.pdf',units='in',width=12,height=9,scale=0.56)
