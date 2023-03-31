install.packages("psych")
library("psych")
install.packages("readr")
library("readr")
library("utils")
install.packages('tidyverse')
library('tidyverse')
install.packages('corrplot')
library('corrplot')
install.packages('caTools')
library('caTools')
install.packages("car")   #for vif
library('car')
install.packages('MASS')  #for stepAIC
library('MASS')
library('dplyr')
install.packages("forecast")
library("forecast")



mydata <- read_csv("C:\Users\Admin\OneDrive\Desktop\School\SEMESTER 2\Statistical analysis\prj\housing.csv")
view(mydata)
head(mydata)


describe(mydata)     #Basic info about the data
n_distinct(mydata$Address)
mydata_new = subset(mydata, select = -c(Address))                               #Drop column Address since it contains 499 unique values
view(mydata_new)                                                                

#Distribution Plot and Box Plot and Correlation Matrix
hist(mydata_new$Price,main = 'House Price Data',xlab = 'House Prices in USD',col='cyan') 

boxplot(mydata_new$`Avg. Area House Age`,mydata_new$`Avg. Area Number of Rooms`,mydata_new$`Avg. Area Number of Bedrooms`, main="Box Plot 1",names = c("Avg. House Age","Avg. No of Rooms","Avg. No of Bedrooms"),col = c('red','orange','yellow')) #Split to 3 different plot because the ranges are too different
boxplot(mydata_new$`Avg. Area Income`,mydata_new$`Area Population`, main = "Box Plot 2", names = c("Avg. Area Income","Area Population"),col = c('green','blue'))
boxplot(mydata_new$Price, main = "Box Plot 3", names = ("Price"),col = 'darkorchid')



cor(mydata_new)
a <- cor(mydata_new)
pairs(~mydata_new$`Avg. Area Income`+ mydata_new$`Avg. Area House Age`+ mydata_new$`Avg. Area Number of Rooms`+ mydata_new$`Avg. Area Number of Bedrooms`+ mydata_new$`Area Population` + mydata_new$Price, main = "Pairplot",labels = colnames(mydata_new))
corrplot(a, method = 'circle')                                                                                                                                                                                                                                                                    

#Train and Test Split
install.packages('caTools')
library('caTools')
set.seed(123)
split <- sample.split(mydata_new$Price, SplitRatio = 0.70)
train_data <- subset(mydata, split == T)                                        #Create training data for analysis
test_data <- subset(mydata, split == F)                                         #Create test data for final verification


#Check for linearity between each independent variable and dependent variable
plot(x = mydata_new$`Avg. Area Income`, y = mydata_new$Price) 
plot(x = mydata_new$`Avg. Area House Age`, y = mydata_new$Price)
plot(x = mydata_new$`Avg. Area Number of Rooms`, y = mydata_new$Price)
plot(x = mydata_new$`Avg. Area Number of Bedrooms`, y = mydata_new$Price)
plot(x = mydata_new$`Area Population`, y = mydata_new$Price)


#Create Model1 
model1 <- lm(Price~`Avg. Area Income`+ `Avg. Area House Age`+ `Avg. Area Number of Rooms`+ `Avg. Area Number of Bedrooms`+ `Area Population`, data = train_data)
summary(model1)
stepAIC(model1,direction = 'both')

vif(model1)


#Create Model2
model2 <- lm(Price~`Avg. Area Income`+ `Avg. Area House Age`+ `Avg. Area Number of Rooms`+ `Area Population`, data = train_data)
summary(model2)
vif(model2)                                                                    
plot(model2)                                            # 4 types of plot
infIndexPlot(model2)
res.aov <- aov(model2)
summary(res.aov)

a <- aov(model3)
summary(a)
p <- predict(model2, test_data)
plot(p)
summary(p)
plot(x=p, y= test_data$Price,  xlab='Predicted Values', ylab='Actual Values', main='Predicted vs. Actual Values')
abline(a=0, b=1)

accuracy(p,test_data$Price)
rsq()

