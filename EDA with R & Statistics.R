library(tidyverse)
view(melanoma)

#Summarising the melanoma dataset to get the five summary
summary(melanoma) 

#How many columns?
nrow(melanoma)

#Variance
var(melanoma)

#Melanoma - changing these as they are categories and it the summary does not make sense so it counts and summarises them into categories 
melanoma$sex=factor(melanoma$sex)
melanoma$status=factor(melanoma$status)
melanoma$ulcer=factor(melanoma$ulcer)
 
#Added the years column from time to be able to quickly work out the amount if time passed
melanoma$Years <- melanoma$time / 365

#Standard Deviation 
sd(melanoma$time) 
sd(melanoma$Years) 
sd(melanoma$age)
sd(melanoma$thickness)


#Histograms to look at the spread of data
par(mfrow=c(2,2))
hist(melanoma$time,main="Days of Survival After Tumor Removal", xlab = "Days(time)", col ="blue")
hist(melanoma$thickness,main= "Thickness of Tumor",xlab= "Thickness(mm)", col="blue")
hist(melanoma$year, main = "Year of Surgery", xlab = "Year",col="blue")
hist(melanoma$age, main= "Age of Patients", xlab = "Age",col="blue")

#Box plots to look at the spread of data
par(mfrow=c(2,2))
boxplot(melanoma$time ~ melanoma$sex,main="Sex by Time (Days Survived)", xlab ="sex", ylab="time")
boxplot(melanoma$time ~ melanoma$ulcer,main ="Ulcer by Time (Days Survived)", xlab = "ulcer", ylab="time")


boxplot(melanoma$thickness ~ melanoma$sex, main="Sex by Thickness ", xlab = "sex", ylab ="thickness")
boxplot(melanoma$thickness ~ melanoma$ulcer,main="Ulcer by Thickness", xlab = "ulcer", ylab="thickness")  

#1. Linear Regression and Correlation for time and thickness 
#pearsons correlation coeefficent and scatterplot to identify strength and direction of correlation
attach(melanoma)
cor(thickness,time, method="pearson")
plot(time,thickness, col="black", main="Numbers of Days Survived vs Thickness of Tumor", xlab="time (number of days survived)", ylab= "thickness of tumor(mm)")

#creating my linear regression model summary for equation of a straight line y=mx+c. /remember its y,x
my_model=lm(formula = thickness~time)
my_model
summary(my_model)

#Adding the line of best fit - linear regression to see the trend line (graphical representation of the linear regression)
abline(my_model, col = "black", lwd = 2) 

#2 Linear Regression for time and age 
attach(melanoma) 
cor(time,age, method = "pearson")
plot(age,time,col="black", main= "Age of Patients vs Number of Days Survived",xlab="Age of Patients", ylab="Number of Days Survived")

#creating my linear regression model to see the correlation - remember its y~x
my_model=lm(formula = time~age)
my_model
summary(my_model)

#Adding the line of best fit - linear regression to see the trend line (graphical representation of the linear regression)
abline(my_model, col = "black", lwd = 2) 

#3 Linear Regression for thickness and age
attach(melanoma) 
cor(thickness,age,method = "pearson")
plot(age,thickness,col="black", main= "Age of Patients vs Thickness of Tumor",xlab="Age of Patients", ylab="Thickness(mm)")

#creating my linear regression model to see the correlation - remember its y~x just like the equation of a line
my_model=lm(formula = thickness~age)
my_model
summary(my_model)

#Adding the line of best fit - linear regression to see the trend line (graphical representation of the linear regression)
abline(my_model, col = "black", lwd = 2) 


#Hypothesis testing - steps below! 
head(melanoma)

#1. Change 0 and 1 to women and men
melanoma <- melanoma %>%
mutate(sex = recode_factor(sex,`0` = "women",`1` = "men"),
             ulcer= recode_factor(ulcer,"0" = "absent", "1" = "present"),
            status=recode_factor(status, "died from melanoma" ="1", "still alive" = "2", "died from other" = "3"))
head(melanoma)

#2.Group by gender and summarise  
melanoma_by_gender <-melanoma %>%
group_by(sex) %>%
summarize( 
num.obs = n(), 
mean_age = round(mean(age),0), 
sd_age = round(sd(age),0), 
mean_time = round(mean(time),0),
sd_time = round(sd(time),0),
mean_thickness= round(mean(thickness),0),
sd_thickness=round(sd(thickness),0)
)


#View the new table by gender 
head(melanoma_by_gender) 

#Start the significance testing for variable 'time' and 'sex' =  H0: the mean times are the same / H1: the mean time weights is different 
melanoma_time_t_test <- t.test(time ~ sex, data = melanoma)
melanoma_time_t_test

#Start the significance testing for variable 'thickness' and 'sex' =  H0: the mean times are the same / H1: the mean time weights is different  
melanoma_time_t_test <- t.test(thickness ~ sex, data = melanoma)
melanoma_time_t_test

#Start the significance testing for variable 'age' and 'sex' =  H0: the mean times are the same / H1: the mean time weights is different  
melanoma_time_t_test <- t.test(age ~ sex, data = melanoma)
melanoma_time_t_test


#QQ PLOTS! Variable - time/ Tests for normality - to see if the population is normal distribution (using the qq plot stat_qq|() function)
p_time <- ggplot(data = melanoma, aes(sample = time))
p_time + stat_qq() + stat_qq_line(color="blue") + facet_grid(. ~ sex) +
ggtitle("QQ Plots") +
xlab("Theoretical Quantile") +
ylab("Time")  

#QQ PLOTS! Variable - thickness/ Tests for normality - to see if the population is normal distribution (using the qq plot stat_qq|() function)
p_thickness <- ggplot(data = melanoma, aes(sample = thickness))
p_thickness + stat_qq() + stat_qq_line(color="blue") + facet_grid(. ~ sex) +
  ggtitle("QQ Plots") +
  xlab("Theoretical Quantile") +
  ylab("Thickness") 
  

#QQ PLOTS! Variable - thickness/ Tests for normality - to see if the population is normal distribution (using the qq plot stat_qq|() function)
p_age <- ggplot(data = melanoma, aes(sample = age))
p_age + stat_qq() + stat_qq_line(color="blue") + facet_grid(. ~ sex) +
  ggtitle("QQ Plots") +
  xlab("Theoretical Quantile") +
  ylab("Age")  



