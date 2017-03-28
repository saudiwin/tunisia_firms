# convert PDFs to editable data using ABBY

require(abbyyR)

setapp(c("Tunisia Firms", "m+w3xiZFDw0D/4BMQSORs0E6"))

getAppInfo()

processImage(file_path = 'C:\\Users\\bobku\\Box Sync\\Dissertation\\Quantitative\\archieve classement\\Classement 2008.pdf',
             imageSource = 'scanner',description = 'Classement2008')

# all 2010 individuals 

all_2010s <- list.files(path = 'C:\\Users\\bobku\\Box Sync\\Dissertation\\Quantitative\\archieve classement\\',pattern='[0-9]+-[0-9].pdf')


lapply(all_2010s, function(x) 
  processImage(file_path = paste0('C:\\Users\\bobku\\Box Sync\\Dissertation\\Quantitative\\archieve classement\\',x),
               imageSource = 'scanner',description = x)
  )

results <- getResults(output='C:\\Users\\bobku\\Box Sync\\Dissertation\\Quantitative\\archieve classement\\')

# Load text files and process them with regexp

class2014 <- readLines('data/classement2014.txt',encoding='UTF-8')
class2014 <- class2014[grepl('\\h?[0-9]+',class2014,perl=TRUE)]
class2014 <- trimws(class2014)

writeLines(class2014,'data/class2014_firststep.txt')
#class2014 <- class2014[grepl('[0-9]+\\h+[0-9]+',class2014,perl=TRUE)]
