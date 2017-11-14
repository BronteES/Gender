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

if (!require(ggplot2)){
 install.packages("ggplot2")
 library(ggplot2)
}

Master = read.csv("Master.csv") #Imports with 9 NA columns
Master[,18:27] = NULL #Delete these 9 NA columns
Master$Name = as.character(Master$Name)
Master$Year = as.character(Master$Year)


Name = c(Master$Name) #Create a new sheet
Name = unique(Name) #Get rid of multiples

#genderizeR code
#Gender = c(findGivenNames(Name)) #As we have over 1000 names, we have to run this function over two days or pay. Will need to loop for 1-1000, 1001-2000, 2001-3000
#gdf = data.frame(Name = toupper(Gender$name), Gender = Gender$gender, Probability = Gender$probability) #gdf = data.frame(Gender) #gdf means gender data frame; toupper changes names to upper case
#gdf$Name = as.character(gdf$Name) #Change names into characters

#Merge the Master data with the gdf data with a left join to avoid multiples of rows
NameGender = merge(x = Master, y = gdf, by = "Name", all.x = TRUE, all.y = TRUE)
NameGender$Probability = as.numeric(as.character(NameGender$Probability)) #Changes probabilities to numeric
NameGender$Year = as.character((NameGender$Year))

sdf = split(NameGender, NameGender$Gender) #sdf means split (NameGender) data frame !check if it has been split into two columns!
female = sdf[[1]] #creates a data frame using only the female data
female$prob.female = female$Probability
male = sdf [[2]] #creates a data frame using on the male data
male$prob.female = NA #New column
male$prob.female = 1-male$Probability #invProbability = inverse probability of male name beign correct; inverts the probability of male name-gender
male.hist = hist(male$prob.female) #histogram of probability male
female.hist = hist(female$prob.female) #histogram of probability female

PrF = rbind(male, female)

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
props = split(prop, prop$Var1)
fem.prop = props [[1]]
fem.prop = fem.prop[,-1]

#Smooth graph
ggplot(data = fem.prop, aes(x = Year, y = Freq, colour = Year)) + 
 geom_smooth()

#Violin plots for each year

PrF$Year = as.character(PrF$Year)
ggplot(data = PrF, aes(x = Year, y = prob.female, colour = Year)) + #Setting up the data
 geom_violin(aes(fill = Year, alpha = 0)) + #Create a violin plot
 geom_jitter(width = 0.05) + #Show the data points spaced-out
 scale_x_discrete(breaks = c("2007", "2011", "2013", "2015", "2017"), labels = c("2007", "2011", "2013", "2015", "2017")) + #Year labels
 xlab("Conference Year") + #x-label
 ylab("Probability of Female") + #y-label
 ggtitle("Gender Composition at Evolution Meetings Conferences", subtitle = "Subtitle") + #Titles
 theme(legend.position = "none") #Remove legend
#Need to remove the NA violin
 
class(PrF$Year) #character
class(PrF$prob.female) #numeric
 



PrF.2007.hist = hist(y2007$prob.female)
PrF.2011.hist = hist(y2011$prob.female)
PrF.2013.hist = hist(y2013$prob.female)
PrF.2015.hist = hist(y2015$prob.female)
PrF.2017.hist = hist(y2017$prob.female)



