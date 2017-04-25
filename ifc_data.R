# REad in and analyze IFC data for Egypt and Tunisia

require(haven)
require(readr)
require(dplyr)
require(magrittr)
require(ggplot2)
require(forcats)
require(purrr)
require(FactoMineR)


egypt_2013 <-
  read_dta('IFC Enterprise Data/Egypt-2013-full data.dta') %>% haven::as_factor(.)
tun_2013 <-
  read_dta('IFC Enterprise Data/Tunisia-2013-full data.dta') %>% haven::as_factor(.)

questions_to_keep <-
  c(
    'c9a',
    'd2',
    'd4',
    'd3c',
    'd3b',
    'd3a',
    'd14',
    'MNAd31b1',
    'MNAq5',
    'b2c',
    'b2b',
    'b2a',
    'b3',
    'b1',
    'e1',
    'e2b',
    'MNAe3a',
    'MNAe3b',
    'MNAq53',
    'MNAe20',
    'h1',
    'MNAf4',
    'k1c',
    'k3c',
    'k3a',
    'k8',
    'MNAq46f',
    'MNAq46d',
    'k15c',
    'k20a',
    'k21',
    'h7a',
    'MNAj1a',
    'MNAj1d',
    'j2',
    'j3',
    'j6a',
    'j6',
    'j7a',
    'j12',
    'j15',
    'j30a',
    'j30b',
    'j30c',
    'MNAj31c1',
    'j30f',
    'MNAj31f1',
    'h30',
    'MNAj30g',
    'l1',
    'MNAl4c',
    'l5',
    'l10',
    'l30a',
    'l30b',
    'n6a',
    'n6b',
    'MNAa20',
    'id'
  )

egypt_2013_rec <- select(egypt_2013,
                         a4b,
                         one_of(questions_to_keep)) %>% mutate(
                           a4b = fct_collapse(
                             a4b,
                             `Don't\nKnow` = c(
                               "Don't know (SPONTANEOUS)",
                               "Refusal (SPONTANEOUS)",
                               "Does not apply (SPONTANEOUS)"
                             ),
                             `Clothing\nManufacturing` = c("Garments", "Textiles", "Leather"),
                             `Consumer\nGoods` = c("Retail", "Wholesale", "Motor vehicles", "Furniture"),
                             `Tourism\nTransportation` = c("Hotels+Restaurants", "Transport + Telecom"),
                             `Raw\nMaterials` = c(
                               "Non-metallic minerals",
                               "Fabricated metals",
                               "Wood",
                               "Recycling",
                               "Minerals"
                             ),
                             `Heavy\nIndustry` = c("Chemicals", "Rubber +  plastics"),
                             `Other\nServices` = c("IT", "Publishing, printing and recorded media"),
                             "Other\nManufacturing" = "Other Manuf"
                           ),
                           a4b = fct_drop(a4b),
                           country = 'Egypt',
                           num_obs = n(),
                           l10=factor(l10,levels=c(NA,1,2),labels=c("Don't Know",'Yes','No'),exclude=NULL),
                           MNAa20=factor(MNAa20,levels=c(NA,1,2),labels=c("Don't Know",'Yes','No'),exclude=NULL)
                         )



tun_2013_rec <- select(tun_2013, a4b,
                       one_of(questions_to_keep)) %>% mutate(
                         a4b = fct_collapse(
                           a4b,
                           "Tourism\nTransportation" = c(
                             "Supporting transport activities (incl travel agencies)",
                             "Transport  (60-62)",
                             "Other transport equipment",
                             "Hotel and restaurants: section H"
                           ),
                           `Clothing\nManufacturing` = c("Textiles", "Garments", "Leather"),
                           "Consumer\nGoods" = c(
                             "Wholesale",
                             "Retail",
                             "Services of motor vehicles",
                             "Motor vehicles",
                             "Furniture"
                           ),
                           "Raw\nMaterials" = c(
                             "Fabricated metal products",
                             "Non metallic mineral products",
                             "Basic metals",
                             "Wood",
                             "Paper",
                             "Recycling"
                           ),
                           "Heavy\nIndustry" = c(
                             "Machinery and equipment",
                             "Electronics (31 & 32)",
                             "Precision instruments",
                             "Chemicals",
                             "Plastics & rubber"
                           ),
                           "Other\nServices" = c("IT", "Publishing printing and recorded media"),
                           "Construction" = "Construction Section F:",
                           "Other\nManufacturing" = "Other manufacturing"
                         ),
                         a4b = fct_drop(a4b),
                         country = 'Tunisia',
                         num_obs = n()
                       )

egypt_2013_rec <- map(egypt_2013_rec,function(x) {
  if(is.numeric(x)) {
    x[x<0] <- NA
  } else if(is.factor(x)) {
    x <- fct_collapse(x,"0"=c('NONE',"LESS THAN ONE DAY","Less than one day","NO PAYMENTS","LESS THAN ONE 1%","NO PAYMENTS OR GIFTS ARE PAID"))
    
    x <- fct_relabel(x,(function(l) {
      to_na <- grepl("[Dd]on't? [Kk]now|[Rr]efusal|NO TIME WAS SPENT|-6|NaN|[Nn]o time was spent|[Dd]oes not apply|DOES NOT APPLY|TOO MANY TO COUNT|REFUS|DON'T KNOW",l)
      l[to_na] <- NA
      l[l=='yes'] <- 'Yes'
      l[l=='no'] <- 'No'
      return(l)
    }))
    
    if(!is.logical(tryCatch(as.numeric(as.character(x)),warning = function(w) return(TRUE)))) {
      x <- as.numeric(as.character(x))
      x[x<0] <- NA
      return(x)
    } 
    
    x <- fct_relabel(x, (function(l) {
      l <- rapportools::tocamel(l,sep=' ')}))
  }  
  return(x)
}) %>% as_data_frame()

tun_2013_rec <- map(tun_2013_rec,function(x) {
  if(is.numeric(x)) {
    x[x<0] <- NA
  } else if(is.factor(x)) {
    x <- fct_collapse(x,"0"=c('NONE',"LESS THAN ONE DAY","Less than one day","NO PAYMENTS","LESS THAN ONE 1%","NO PAYMENTS OR GIFTS ARE PAID"))
    
    x <- fct_relabel(x,(function(l) {
      to_na <- grepl("[Dd]on't? [Kk]now|[Rr]efusal|NO TIME WAS SPENT|-6|NaN|[Nn]o time was spent|[Dd]oes not apply|DOES NOT APPLY|TOO MANY TO COUNT|REFUS|DON'T KNOW",l)
      l[to_na] <- NA
      l[l=='yes'] <- 'Yes'
      l[l=='no'] <- 'No'
      return(l)
    }))
    
    if(!is.logical(tryCatch(as.numeric(as.character(x)),warning = function(w) return(TRUE)))) {
      x <- as.numeric(as.character(x))
      x[x<0] <- NA
      return(x)
    } 
    
    x <- fct_relabel(x, (function(l) {
      l <- rapportools::tocamel(l,sep=' ')}))
  }  
  return(x)
}) %>% as_data_frame()
combined_data <-
  bind_rows(
    # select(egypt_2013_rec, id, a4b, country, num_obs),
    # select(tun_2013_rec, id, a4b, country, num_obs)
    egypt_2013_rec,tun_2013_rec
  ) %>%
  group_by(country, a4b) %>% mutate(cat_count = n() / num_obs)

combined_data %>% ggplot(aes(x = a4b, y = cat_count,
                             fill = country)) + geom_col(position = 'dodge', width =
                                                           0.5) + theme_minimal() +
  xlab("") + ylab("Proportion of Firms in Sample") + scale_fill_brewer(guide =
                                                                         guide_legend(title = ""))

ggsave(
  'combined_sectors.pdf',
  width = 7,
  height = 4,
  scale = 1.3
)


#Let's run some IRT

# need to separate variables into ordinal/numeric. It's mostly already done because of how I coded the factors

# Do some further data coding to simplify some variables
# Check if the variables look right sapply(combined_data,function(x) {if(!is.numeric(x)) table(x)})

combined_data <- mutate(combined_data,
                        # convert to dollars at 2013 prices
                        k15c=if_else(country=='Tunisia',k15c/0.6,k15c/.15),
                        n6a=if_else(country=='Tunisia',n6a/0.6,n6a/.15),
                        n6b=if_else(country=='Tunisia',n6b/0.6,n6b/.15),
                        d2=if_else(country=='Tunisia',d2/0.6,d2/.15),
                        k20a=na_if(k20a,"-6"),
                        k20a=fct_collapse(k20a,"Approved"=c("Application Was Approved"),
                                            'Partly Approved'="Application Was Granted Only In Part",
                                            "Rejected"=c("Application Was Rejected",
                                                         "Application Was Withdrawn",
                                                         "APPLICATION STILL IN PROCESS")),
                        k20a=fct_relevel(k20a,c('Rejected','Partly Approved','Approved'))
                        )

# Dirty hack

combined_data$e1 <- factor(combined_data$e1)
levels(combined_data$e1) <- c('International','Local',"Local","Local","Local")
to_mirt <- ungroup(combined_data) %>% select(-id,-num_obs,-cat_count)
all_classes <- sapply(to_mirt,class)[-1]

model_strings <- ifelse(all_classes=='numeric','2PL','graded')
model_strings <- c('nominal',model_strings)

# Convert character to factor for FAMD

to_mirt <- map(to_mirt,(function(x) {
  if(is.character(x)) {
    x <- as.factor(x)
  }
  return(x)
})) %>% as_data_frame()

# try FAMD
all_miss <- sapply(to_mirt,function(x) sum(is.na(x))) / nrow(to_mirt)
require(missMDA)
#impute data first

famd_impute <- imputeFAMD(as.data.frame(to_mirt),coeff.ridge = 2)
famd_estimate <- FAMD(as.data.frame(to_mirt),tab.comp=famd_impute$tab.disj,ncp=2)

firm_vals <- famd_estimate$ind$coord %>% as_data_frame

combined_data <- ungroup(combined_data) %>% mutate(dim1=firm_vals$Dim.1,
                        dim2=firm_vals$Dim.2) %>% filter(l1>10)

employees <- ggplot(combined_data,aes(x=dim1,y=dim2,color=log(l1))) + geom_point(size=3) + theme_minimal() +
  xlab('') + ylab('') + scale_color_continuous(guide=guide_colourbar(title='Log\nEmployees'))

exports <- ggplot(combined_data,aes(x=dim1,y=dim2,color=d3c)) + geom_point(size=3) + theme_minimal() +
  xlab('') + ylab('') + scale_color_continuous(guide=guide_colourbar(title='Percent\nSales\nExports')) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)

corrupt <- ggplot(combined_data,aes(x=dim1,y=dim2,color=j30f)) + geom_point(size=3) + theme_minimal() +
  xlab('') + ylab('') + scale_color_brewer(palette='RdBu',guide=guide_legend(title=''),
                                           breaks=c('No Obstacle',
                                           'Minor Obstacle',
                                           'Moderate Obstacle',
                                           'Major Obstacle',
                                           'Very Severe Obstacle')) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_x_continuous(breaks=c(-4,5),labels=c('Low Access to Credit','High Access to Credit'),
                     limits=c(-6,8)) +
  scale_y_continuous(breaks=c(-2,4),labels=c('Low\nAccess\nto\nForeign\nMarkets',
                                             'High\nAccess\nto\nForeign\nMarkets'),
                     limits=c(-4,6)) + labs(title='Is Corruption an Obstacle to Current Operations?',
                                               caption='IFC Enterprise Surveys for Egypt & Tunisia (2013)') +
  theme(plot.title = element_text(hjust = 0.5),text=element_text(family='serif'))

ggsave('all_corrupt_firms.png',plot=corrupt,scale=1.2)

facet_exports <- ggplot(combined_data,aes(x=dim1,y=dim2,color=d3c)) + geom_point(size=3) + theme_minimal() +
  xlab('Credit Constraints (Low to High)') + ylab('Difficulties with Government (High to Low)') + scale_color_continuous(guide=guide_colourbar(title='Percent\nSales\nExports')) +
  facet_wrap(~a4b,ncol=2) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
  
facet_bribes <- filter(combined_data,!is.na(j6)) %>%  ggplot(aes(x=dim1,y=dim2,color=log(j6+.1))) + geom_point(size=3) + theme_minimal() +
  xlab('Credit Constraints (Low to High)') + ylab('Difficulties with Government (High to Low)') + scale_color_continuous(guide=guide_colourbar(title='Percent\nSales\nExports')) +
  facet_wrap(~a4b,ncol=2) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)

facet_corrupt <- filter(combined_data,!is.na(MNAj31c1)) %>%  ggplot(aes(x=dim1,y=dim2,color=factor(MNAj31c1))) + geom_point(size=3) + theme_minimal() +
  xlab('Credit Constraints (Low to High)') + ylab('Difficulties with Government (High to Low)') + scale_color_brewer(palette='RdBu',guide=guide_legend(title='Percent\nSales\nExports')) +
  facet_wrap(~a4b,ncol=2) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)

facet_contract <- filter(combined_data,!is.na(j6a)) %>%  ggplot(aes(x=dim1,y=dim2,shape=factor(j6a),color=d3c)) + geom_point(size=3) + theme_minimal() +
  xlab('Access to Capital') + ylab('Access to Export Markets') + scale_color_continuous(guide=guide_colourbar(title='Percent\nSales\nExports')) +
  facet_wrap(~a4b,ncol=2) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + scale_shape(guide=guide_legend(title='Sought\nGovernment\nContract'))


quant_names <- names(to_mirt)[sapply(to_mirt,is.numeric)]
quant_variables <- famd_estimate$quanti.var$coord %>% as_data_frame %>% mutate(var_names=quant_names) %>% 
  mutate(var_labels=c('Power Outage\nLoss','Sales','Days\nExports in Customs','% Exports','% Indirect\nExports','% National\nSales',
                      'Days\nImports in Customs','% State\nOwnership','% Foreign\nOwnership','% Domestic\nOwnership','% Largest\nOwner',
                      '# Competitors','% Purchases\nCredit','% State\nBank Credit',
                      '% Internal\nCapital','Interest\nRate','Total\nLoans','Management\nLobbying','% Bribes',
                      'Bribes as\n% Sales','Employment','Female\nWorkers','Machinery\nCapital','Land\nCapital'))
qual_vars <- famd_estimate$quali.var$coord %>% as_data_frame %>% mutate(var_names=row.names(famd_estimate$quali.var$coord))

ggplot(qual_vars,aes(y=Dim.1,x=reorder(var_names,Dim.1))) + geom_col() + theme_minimal() + coord_flip()

ggplot(qual_vars,aes(y=Dim.2,x=reorder(var_names,Dim.2))) + geom_col() + theme_minimal() + coord_flip()

ggplot(quant_variables,aes(y=Dim.1,x=reorder(var_labels,Dim.1))) + geom_col() + theme_minimal() + coord_flip() +
  scale_x_discrete(expand=c(0,2)) + theme(panel.grid = element_blank()) + xlab('') + ylab('')

ggsave('dimension1.png',scale=1.6,units='in',width=4,height=5)

ggplot(quant_variables,aes(y=Dim.2,x=reorder(var_labels,Dim.2))) + geom_col() + theme_minimal() + coord_flip() +
  scale_x_discrete(expand=c(0,2)) + theme(panel.grid = element_blank()) + xlab('') + ylab('')
ggsave('dimension2.png',scale=1.6,units='in',width=4,height=5)

# Plot the sectoral location along the axes

sectors <- filter(qual_vars,var_names %in% c("Food",
                                  "Clothing Manufacturing",
                                  "Raw Materials",
                                  "Other Services",
                                  "Heavy Industry",
                                  "Consumer Goods",
                                  "Construction",
                                  "Tourism Transportation",
                                  "Other Manufacturing")) %>% 
  ggplot(aes(y=Dim.2,x=Dim.1)) + geom_text(aes(label=var_names),family='serif') +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_x_continuous(breaks=c(-1,1),labels=c('Low Access to Credit','High Access to Credit'),
                     limits=c(-1.5,2)) +
  scale_y_continuous(breaks=c(-.6,.6),labels=c('Low\nAccess\nto\nForeign\nMarkets',
                                             'High\nAccess\nto\nForeign\nMarkets'),
                     limits=c(-0.7,.7)) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),text=element_text(family='serif'))

ggsave('all_sectors.png',plot=sectors)


# Now we can facet by sector

facet_corrupt <- ggplot(combined_data,aes(x=dim1,y=dim2,color=j30f)) + geom_point(size=3) + theme_minimal() +
  xlab('') + ylab('') + scale_color_brewer(palette='RdBu',guide=guide_legend(title=''),
                                           breaks=c('No Obstacle',
                                                    'Minor Obstacle',
                                                    'Moderate Obstacle',
                                                    'Major Obstacle',
                                                    'Very Severe Obstacle')) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_x_continuous(breaks=c(-4,5),labels=c('Low Access to Credit','High Access to Credit'),
                     limits=c(-6,8)) +
  scale_y_continuous(breaks=c(-2,4),labels=c('Low\nAccess\nto\nForeign\nMarkets',
                                             'High\nAccess\nto\nForeign\nMarkets'),
                     limits=c(-4,6)) +
  theme(plot.title = element_text(hjust = 0.5),text=element_text(family='serif')) +
  facet_wrap(~a4b,ncol=2)

ggsave('corrupt_by_sectors.png',plot=facet_corrupt,height=8,width=6,units='in',scale=1.4)
