setwd("~/Dropbox/Honours/RawData/CSV")

if (!require(genderizeR)){
 install.packages("genderizeR")
 library(genderizeR)
}
if (!require(plyr)){
 install.packages("plyr")
 library(plyr)
}
if (!require(purrr)){
 install.packages("purrr")
 library(purrr)
}

Master = read.csv("Master.csv") #Imports with 9 NA columns
Master[,18:27] = NULL #Delete NA columns
Master$Name = as.character(Master$Name)


Name = c(Master$Name) #Create a new sheet
Name = unique(Name) #Get rid of multiples

Gender = c(findGivenNames(Name)) #As we have over 1000 names, we have to run this function over two days or pay. Will need to loop for 1-1000, 1001-2000, 2001-3000
gdf = data.frame(Name = toupper(Gender$name), Gender = Gender$gender, Probability = Gender$probability) #gdf = data.frame(Gender) #gdf means gender data frame; toupper changes names to upper case
gdf$Name = as.character(gdf$Name) #Change names into characters

#Merge the Master data with the gdf data with a left join to avoid multiples of rows
NameGender = merge(x = Master, y = gdf, by = "Name", all.x = TRUE, all.y = TRUE)
NameGender$Probability = as.numeric(as.character(NameGender$Probability)) #Changes probabilities to numeric
NameGender$Year = as.numeric(as.character((NameGender$Year)))

#Visualise the distribution of probabilities of names (where 1 means the function is 100% sure its name-gender estimate is correct, and 0 means it is 0% sure)
hist(NameGender$Probability)

sdf = split(NameGender, NameGender$Gender) #sdf means split (NameGender) data frame !check if it has been split into two columns!
female = sdf[[1]] #creates a data frame using only the female data
female$prob.female = female$Probability
male = sdf [[2]] #creates a data frame using on the male data
male$prob.female = NA #New column
male$prob.female = 1-male$Probability #invProbability = inverse probability of male name beign correct; inverts the probability of male name-gender
male.hist = hist(male$prob.female) #histogram of probability male
female.hist = hist(female$prob.female) #histogram of probability female

PrF = rbind(male, female)
PrF.hist = hist(PrF$prob.female) #histogram of Probability female
View(PrF.hist)

scatter.smooth(female$Year, female$Probability) #Scatter plot of female name-gender probability
scatter.smooth(male$Year, male$invProbability) #Scatter plot of male name-gender probability

Years = split(PrF, PrF$Year)
y2007 = Years [[1]]
y2011 = Years [[2]]
y2013 = Years [[3]]
y2015 = Years [[4]]
y2017 = Years [[5]]

prop.y2007 = as.data.frame(prop.table(table(y2007$Gender))) #Proportion of males and females in 2007
prop.y2007$Year = c(2007,2007)
prop.y2011 = as.data.frame(prop.table(table(y2011$Gender)))
prop.y2011$Year = c(2011,2011)
prop.y2013 = as.data.frame(prop.table(table(y2013$Gender)))
prop.y2013$Year = c(2013,2013)
prop.y2015 = as.data.frame(prop.table(table(y2015$Gender)))
prop.y2015$Year = c(2015,2015)
prop.y2017 = as.data.frame(prop.table(table(y2017$Gender)))
prop.y2017$Year = c(2017,2017)

prop = rbind(prop.y2007, prop.y2011, prop.y2013, prop.y2015, prop.y2017)
View(prop)

PrF.2007.hist = hist(y2007$prob.female)
PrF.2011.hist = hist(y2011$prob.female)
PrF.2013.hist = hist(y2013$prob.female)
PrF.2015.hist = hist(y2015$prob.female)
PrF.2017.hist = hist(y2017$prob.female)

#Trying out Violin Plots
#Making a data set with Year and Prob.female
VYear = PrF$Year
VProb = as.character(PrF$prob.female)
V = as.data.frame(VYear, VProb, row.names = NULL, optional = FALSE)

subset = PrF[PrF$Year,PrF$prob.female] #Don't think this is right
ggplot(PrF$Year, PrF$prob.female)


+ geom_line()+geom_violin(aes(group=year),alpha=0.5)










#Not needed. The number of females each year
femaleyear = split(female, female$Year) #split female data by year
f2007 = femaleyear[[1]]
f2011 = femaleyear[[2]]
f2013 = femaleyear[[3]]
f2015 = femaleyear[[4]]
f2017 = femaleyear[[5]]
count(f2007$Gender) #233
count(f2011$Gender) #203
count(f2013$Gender) #332
count(f2015$Gender) #152
count(f2017$Gender) #311
#Doesn't say much as the overall participant numbers fluctuate too.