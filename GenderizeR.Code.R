setwd("~/Dropbox/Honours/RawData/CSV")
library("genderizeR", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

Master <- read.csv("Master.csv") #Imports with 9 NA columns
Master[,18:27] = NULL #Delete NA columns
Master$Name = as.character(Master$Name)


Name <- c(Master$Name) #Create a new sheet
unique(Name) #Get rid of multiples

Gender <- c(findGivenNames(Name))

#Merge with a left join to avoid multiples of rows
NameGender2 <- merge(x = Symposia, y = NameGender, by = "Name", all.x = TRUE)

