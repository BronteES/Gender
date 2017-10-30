setwd("~/Dropbox/Honours/RawData/CSV")
library("genderizeR", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

Master <- read.csv("Master.csv") #Imports with 9 NA columns
Master[,18:27] = NULL #Delete NA columns
Master$Name = as.character(Master$Name)


Name <- c(Master$Name) #Create a new sheet
Name <- unique(Name) #Get rid of multiples

Gender <- c(findGivenNames(Name))
gdf = data.frame(Name = toupper(Gender$Name), Gender = Gender$Gender, Probability = Gender$Probability) #gdf means gender data frame; toupper changes names to upper case
gdf$Name = as.character(gdf$Name) #Changes names to characters

#Merge the Master data with the gdf data with a left join to avoid multiples of rows
NameGender <- merge(x = Master, y = gdf, by = "Name", all.x = TRUE)
NameGender$Probability = as.numeric(as.character(NameGender$Probability)) #Changes probabilities to numeric

#Visualise the distribution of probabilities of names (where 1 means the function is 100% sure its name-gender estimate is correct, and 0 means it is 0% sure)
hist(NameGender$Probability)
