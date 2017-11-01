setwd("~/Dropbox/Honours/RawData/CSV")

if (!require(genderizeR)){
 install.packages("genderizeR")
 library(genderizeR)
}
if (!require(plyr)){
 install.packages("plyr")
 library(plyr)
}


Master = read.csv("Master.csv") #Imports with 9 NA columns
Master[,18:27] = NULL #Delete NA columns
Master$Name = as.character(Master$Name)


Name = c(Master$Name) #Create a new sheet
Name = unique(Name) #Get rid of multiples

Gender = c(findGivenNames(Name)) #As we have over 1000 names, we have to run this function over two days or pay
gdf = data.frame(Name = toupper(Gender$name), Gender = Gender$gender, Probability = Gender$probability) #gdf = data.frame(Gender) #gdf means gender data frame; toupper changes names to upper case
gdf$Name = as.character(gdf$Name) #Change names into characters

#Merge the Master data with the gdf data with a left join to avoid multiples of rows
NameGender = merge(x = Master, y = gdf, by = "Name", all.x = TRUE, all.y = TRUE)
NameGender$Probability = as.numeric(as.character(NameGender$Probability)) #Changes probabilities to numeric

#Visualise the distribution of probabilities of names (where 1 means the function is 100% sure its name-gender estimate is correct, and 0 means it is 0% sure)
hist(NameGender$Probability)

sdf = split(NameGender, NameGender$Gender) #sdf means split (NameGender) data frame !check if it has been split into two columns!
female = sdf[[1]] #creates a data frame using only the female data
male = sdf [[2]] #creates a data frame using on the male data
male$invProbability = NA #New column
male$invProbability = 1-male$Probability #invProbability = inverse probability of male name beign correct; inverts the probability of male name-gender

#To run analysis, we need to unsplit the data: combine the male and female probabilities into one column with the corresponding year for each data point
rbind(female$Probability, male$invProbability) #Doesn't work; need to combine year and probability for both male and female dataframes

scatter.smooth(female$Year, female$Probability) #Scatter plot of female name-gender probability
scatter.smooth(male$Year, male$invProbability) #Scatter plot of male name-gender probability

Years = split(NameGender, NameGender$Year)
y2007 = Years [[1]]
y2011 = Years [[2]]
y2013 = Years [[3]]
y2015 = Years [[4]]
y2017 = Years [[5]]

prop.y2007 = prop.table(table(y2007$Gender)) #Proportion of males and females in 2007
prop.y2011 = prop.table(table(y2011$Gender))
prop.y2013 = prop.table(table(y2013$Gender))
prop.y2015 = prop.table(table(y2015$Gender))
prop.y2017 = prop.table(table(y2017$Gender))

#Put results for female proportion into graph


#Not needed
femaleyear = split(female, female$Year) #split female data by year
f2007 = femaleyear[[1]]
f2011 = femaleyear[[2]]
f2013 = femaleyear[[3]]
f2015 = femaleyear[[4]]
f2017 = femaleyear[[5]]
count(f2007$Gender) #233

f2007 = mean (female)