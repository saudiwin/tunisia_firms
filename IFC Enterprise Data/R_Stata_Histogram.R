# Required libraries -- haven is for importing data from Stata

library(ggplot2)
library(haven)

# Load data saved in the same folder as this script
stata_dataset <- read_dta('Egypt-2013-full data.dta')

# change missing stata values to R missing values

stata_dataset <- zap_missing(stata_dataset)

# dataset still has variable labels, but value labels are lost
View(stata_dataset)

# add back value labels, must be listed in original order in Stata dataset
stata_dataset$a3b <- factor(as.numeric(stata_dataset$a3b),labels=c('Yes','No'))

# standard histogram
# change the value of x= to the variable to be counted
# the as.numeric() function around b3 ensures that b3 is treated as a continuous instead of a categorical variable

ggplot(stata_dataset,aes(x=as.numeric(b3))) +
  geom_histogram() + theme_minimal() +
  xlab('% Firm Owned by Manager') + ylab('')

ggsave(filename = 'standard_hist.png',scale=1.3)

# Another version, this time colouring the bars by whether
# the firm is in the capital city.
# To do so I add a variabale to the fill= option in aes() using factor() to ensure it is treated as categorical
# scale_fill_brewer() allows me to adjust the title of the legend and also pick different color palettes
# for a list of palettes enter ?scale_color_brewer in the console


ggplot(stata_dataset,aes(x=as.numeric(b3),
                         fill=factor(a3b))) +
  geom_histogram() + theme_minimal() +
  xlab('% Firm Owned by Manager, in Capital versus Outside Capital') + ylab('') +
  scale_fill_brewer(guide=guide_legend(title='In Capital City?'),palette='Paired')

ggsave(filename = 'hist_overlay.png',scale=1.3)

# ggplot can also do density plots that are useful for continuous variables by changing geom_histogram() to geom_density()
# all other options the same

ggplot(stata_dataset,aes(x=as.numeric(b3))) +
  geom_density(fill='blue',colour=NA) + theme_minimal() +
  xlab('% Firm Owned by Manager') + ylab('')

ggsave(filename = 'standard_dens.png',scale=1.3)

# and similarly can do colour overlays
# I set alpha=0.5 to make the overlay partially transparent (0 equals fully transparent, 1 equals opaque)

ggplot(stata_dataset,aes(x=as.numeric(b3),
                         fill=factor(a3b))) +
  geom_density(alpha=0.5,colour=NA) + theme_minimal() +
  xlab('% Firm Owned by Manager, in Capital versus Outside Capital') + ylab('') +
  scale_fill_brewer(guide=guide_legend(title='In Capital City?'),palette='Paired')

ggsave(filename = 'dens_overlay.png',scale=1.3)

