print("welcome the future data scientist- vaishali sagar")
##############DATA PREPROCESSING######################
dataset<- read.csv("Position_Salaries.csv")
View(dataset)
dataset<- dataset[,2:3]

##linear regression
regressor<- lm(formula = Salary~.,
               data=dataset)
summary(regressor)
###polynomial regression, y= b0+ x1b1+ x1^2*b1+ x1^3*b2.......
dataset$Level2<- (dataset$Level)^2
dataset$Level3<- (dataset$Level)^3
dataset$Level4<- (dataset$Level)^4
 poly_reg<- lm(formula = Salary ~.,
               data = dataset)
 summary(poly_reg)
 #######visualising the linear model result
 library(ggplot2)
 ggplot()+
   geom_point(aes(x= dataset$Level, y= dataset$Salary),
              colour="sky blue") +
   geom_line(aes(x= dataset$Level, y= predict(regressor, newdata = dataset))
             ,colour= "blue") +
   ggtitle("truth or bluff") +
   xlab("level") +
   ylab("salary")
 ##visualising polynimial regression result
 ggplot()+
   geom_point(aes(x= dataset$Level, y= dataset$Salary),
              colour="sky blue") +
   geom_line(aes(x= dataset$Level, y= predict(poly_reg, newdata = dataset))
             ,colour= "blue") +
   ggtitle("truth or bluff") +
   xlab("level") +
   ylab("salary")
 