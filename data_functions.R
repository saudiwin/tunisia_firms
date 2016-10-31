# Functions to import & clean Promedia data
# Robert Kubinec

#' @import dplyr
#' @import magrittr
read_in <- function(data_loc,shiftrow=FALSE,range_col=TRUE,group1=NULL,
                    group2=NULL,group3=NULL,data_year=NULL,year1=NULL,year2=NULL) {
 out_data <-  read.delim(data_loc,stringsAsFactors = FALSE,encoding = 'UTF-8') %>% 
    as_data_frame
 if(shiftrow==TRUE) {
 out_data %<>% mutate(shiftrow=ifelse(Rang=="" & grepl('[0-9]+',Nom.de.l.entreprise),
                                             TRUE,FALSE))
   if(sum(out_data$shiftrow)>0) {
     warning('There are probably shifted rows you should fix in the dataset.')
   }
 }
 
 
 # Drop extra columns
 
 if(range_col==TRUE) {
   out_data %<>% select(1:5)
   names(out_data) <- c('range','firm_name','sector',year1,year2)
   # Get rid of blank lines
   
   out_data %<>% filter(range!='',range!='Rang' )
   
 } else {
   out_data %<>% select(1:4)
   names(out_data) <- c('firm_name','sector',year1,year2)
 }
 
 # Change years to a numeric variable. Blanks become NAs
 
 mutate_years <-  list(lazyeval::interp(~as.numeric(a), a = as.name(year1)),
                       lazyeval::interp(~as.numeric(b),b=as.name(year2)))
 
 # Label each section of the data by type of revenue
 
 out_data %<>% mutate(money_type=c(rep('groupes',group1),rep('revenues',group2),rep('exports',group3)),
                      data_year=data_year) %>% mutate_(.dots=setNames(mutate_years,c(year1,year2)))
  return(out_data)
}