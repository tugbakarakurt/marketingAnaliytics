projectdata= read.csv("C:\\Users\\tugba.karakurt\\Desktop\\BlackFriday.csv", header=TRUE, sep=",")
header= c("User_ID","Product_ID","Gender","Age","Occupation","City_Category","Stay_In_Current_City_Years","Marital_Status","Product_Category_1","Product_Category_2","Product_Category_3","Purchase")


#NA values
sum(is.na(projectdata))
projectdata[is.na(projectdata)]<-0

#dummy variables for Gender
projectdata$Gender=ifelse(projectdata$Gender=="F", 1, 0)

#Coding Age values
projectdata$Age=factor(projectdata$Age, 
                       levels = c("0-17", "18-25", "26-35", "36-45", "46-50", "51-55", "55+"), labels = c(1,2,3,4,5,6,7) )
projectdata$Age<-as.integer(projectdata$Age)


#Coding City_Category
projectdata$City_Category=factor(projectdata$City_Category, 
                       levels = c("A", "B", "C"), labels = c(1,2,3) )
projectdata$City_Category<-as.integer(projectdata$City_Category)

#Coding Stay_In_Current_City_Years
projectdata$Stay_In_Current_City_Years=factor(projectdata$Stay_In_Current_City_Years, 
                                 levels = c("1", "2", "3", "4+"), labels = c(1,2,3,4) )
projectdata$Stay_In_Current_City_Years<-as.integer(projectdata$Stay_In_Current_City_Years)

projectdata$Product_Category_2<-as.integer(projectdata$Product_Category_2)

projectdata$Product_Category_3 <-as.integer(projectdata$Product_Category_3)


head(projectdata)
str(projectdata)

#PCA
install.packages("factoextra")
library(factoextra)
data_pca <- prcomp(projectdata[,3:11], center= TRUE ,scale = TRUE)
data_pca = data_pca[,1:3]
summary(data_pca)
head(data_pca)
data <- data_pca$x
data = data[,1:9]
head(data)
head(data_pca)
data_pca
str(data)
str(projectdata)
lm_fit <- lm(projectdata$Purchase~data$PC1+data$PC2+data$PC3+data$PC4+data$PC5+data$PC6+data$PC7+data$PC8+data$PC9, data = data)
lm_fit <- lm(projectdata$Purchase~data)
summary(lm_fit)



lm_fit3 <- lm(projectdata$Purchase~projectdata$Age+projectdata$Gender+projectdata$Marital_Status+projectdata$Occupation+projectdata$Stay_In_Current_City_Years+projectdata$City_Category+projectdata$Product_Category_1+projectdata$Product_Category_2+projectdata$Product_Category_3, data = projectdata)
summary(lm_fit3)  

lm_fit2 <- lm(projectdata$Purchase~projectdata$Product_Category_1+projectdata$Product_Category_2+projectdata$Product_Category_3, data = projectdata)
summary(lm_fit2)


lm_fit3

projectdata[,3:4]

install.packages("caret")

library(caret) 
table()
confusionMatrix(lm_fit)

#RMSE
(sqrt(mean((train$Purchase-train$train_prob_purchase)**2)))


#gplot2
install.packages("ggplot2")
library(ggplot2)

install.packages("randomForest")
require(randomForest)
randomForest <- randomForest(formula =  projectdata$Purchase ~ projectdata$Age+projectdata$Gender+projectdata$Marital_Status+projectdata$Occupation+projectdata$Stay_In_Current_City_Years+projectdata$City_Category+projectdata$Product_Category_1+projectdata$Product_Category_2+projectdata$Product_Category_3, data = projectdata,ntree=10)


#k-means
clusters <- kmeans(projectdata[,2:3], 5)
