library(lmtest)
library(car)
library(Hmisc)
library(corrplot)
library(stargazer)

#data prep.
data <- read.csv("Dataset.csv", header=TRUE)
#age 18-65
data.cln_1 <- subset(data, RIDAGEYR>=18 & RIDAGEYR <=65)
#RIAGENDR 1=male 2=female
data.fem <- subset(data.cln_1, RIAGENDR==0)
data.male <- subset(data.cln_1, RIAGENDR==1)
#no pregnant female
data.female <- subset(data.fem, RIDEXPRG != 1)
#combine male and female
data.clean.fem <- data.female[,c(2,3,5,6,9,11,12,13)]
data.clean.male <- data.male[,c(2,3,5,6,9,11,12,13)]
data.ready <- rbind(data.clean.fem, data.clean.male)
#omit missing data
data.final <- na.omit(data.ready)
#exclude responses that are refuse, don't know, something not relevant
set.final <- subset(data.final, DMDHREDU<6)
set.final <- subset(set.final, INDHHIN2<16 & INDHHIN2!=12 & INDHHIN2!=13)
set.final <- subset(set.final, RIDRETH3 != 7)
set.final <- subset(set.final, DBD900<22)

#make factors
set.final$gender <- factor(set.final$RIAGENDR)
set.final$race <- factor(set.final$RIDRETH3)
colnames(set.final) <- c("gender","age","race","income","education","fastfood","totalsugar","BMI","gender.c","race.c")


#descriptive stats
stargazer(set.final, type="text")

#Correlation plot
res2 <- rcorr(as.matrix(set.final[,1:8]))
corrplot(res2$r, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


#model with factor
model.f <- lm(BMI ~ gender.c+age+race.c+income+education+fastfood+totalsugar, data=set.final)
summary(model.f)
model.f2 <- lm(BMI ~ gender.c+age+income+education+fastfood+totalsugar, data=set.final)
summary(model.f2)
model.f3 <- lm(BMI ~ gender.c+age+income+fastfood, data=set.final)
summary(model.f3)
stargazer(model.f,model.f2,model.f3, type="text")
ctest1 <- coeftest(model.f)
ctest2 <- coeftest(model.f2)
ctest3 <- coeftest(model.f3)
stargazer(ctest1,ctest2,ctest3,type="text")



