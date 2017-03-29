# REad in and analyze IFC data for Egypt and Tunisia

require(haven)
require(readr)
require(dplyr)
require(magrittr)
require(ggplot2)


egypt_2013 <- read_dta('IFC Enterprise Data/Egypt-2013-full data.dta')
tun_2013 <- read_dta('IFC Enterprise Data/Tunisia-2013-full data.dta')
