# Analyze qualitative survey data

require(readxl)
require(dplyr)
require(tidyr)
require(ggplot2)


Survey_Results <- read_excel("C:/Users/bobku/Documents/R projects/tunisia_firms/Survey Results.xlsx")

Survey_Results <- slice(Survey_Results,1:34)

prop.table(table(Survey_Results$Employees))
prop.table(table(Survey_Results$Party_Best))
prop.table(table(Survey_Results$President_Best))
prop.table(table(Survey_Results$Situation_Pol))
prop.table(table(Survey_Results$Party_Contact))
prop.table(table(Survey_Results$Firm_Support))
prop.table(table(Survey_Results$Type_Support1))
prop.table(table(Survey_Results$Type_Support2))
prop.table(table(Survey_Results$Type_Support3))

bill_names <- read_excel('ARP_votes_final.xlsx',sheet = 'Sheet1')
sum(table(bill_names$bill.labels))
prop.table(table(bill_names$bill.labels))

filter(Survey_Results,!is.na(Firm_Support)) %>% ggplot(aes(x=factor(Firm_Support,levels = c('A','B'),
                                   labels=c('Yes','No')))) + geom_bar(width=0.5,fill='blue',alpha=0.5,colour=NA) + theme_minimal() +
  theme(panel.grid=element_blank()) + xlab("") + ylab('Number of Firms')

filter(Survey_Results,!is.na(Party_Best)) %>% mutate(Party_Best=factor(Party_Best,levels=c('None','A',"B",'M','J','F','G','H'),
                                              labels=c('None',
                                                       'Nidaa\nTounes',
                                                        'Nahda',
                                                       'Jamhuri',
                                                       'Moubadira',
                                                       'Afek\nTounes',
                                                       'CPR',
                                                       'Tayar'))) %>% 
  ggplot(aes(x=Party_Best)) + geom_bar(width=0.5,fill='blue',alpha=0.5,colour=NA) + theme_minimal() +
  theme(panel.grid=element_blank()) + xlab("") + ylab('Number of Firms')


