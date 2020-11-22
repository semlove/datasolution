# Loading Packages 

pacman::p_load(tidyverse, 
               esquisse,
               qgraph,
               MVN,
               forecast,
               formattable,
               MLmetrics,
               corrplot,
               moments,
               psych,
               car,
               nFactors,
               GPArotation,
               ppcor,
               DataExplorer,
               broom,
               ggfortify,
               factoextra
)

# Calling data 
O_Data=read.csv("C:/gskim/SSA/r/datasolution/data/Factor-Hair-Revised.csv", header = TRUE, sep = ",")
head(O_Data)
dim(O_Data)
str(O_Data)
names(O_Data)



# Basic statistics
library(psych)
describe(O_Data)

# Removing ID variable
Data <- subset(O_Data, select = -c(1))
head(Data)
summary(Data)

# Histogram
library(ggplot2)
options(repr.plot.width = 15, repr.plot.height = 10)
ggplot(gather(Data), aes(value,fill=key)) + 
  geom_histogram(colour = "black",bins = '30') + 
  facet_wrap(~key)+
  theme_classic()

# Boxplot
options(repr.plot.width = 15, repr.plot.height = 10)
ggplot(gather(Data), aes(value,fill=key)) + 
  geom_boxplot(colour = "black") + 
  facet_wrap(~key)+
  theme_light()

# Normality 
MVN::mvn(Data)

#Correlation ----------------------------------------------------------
correl=cor(Data, method = "pearson")
formattable::percent(Correlation <- cor(Data))

corrplot(Correlation, method = "circle")

# Pairwise correlation
library(ppcor)
pcor(Data, method = "pearson")

# Regression and Evidence for Multicollinearity
library(car)
model0 = lm(Satisfaction~., Data)
summary(model0)
vif(model0)

#Simple linear Regression--------------------------
for (i in 1:ncol(Data)) {
  print("------------------------------------------------")
  print(paste0("Model for combination - ",i))
  field <- as.data.frame(Data[,c(i,12)])
  formula <- paste0("Satisfaction ~ ",names(field)[1])
  print(formula)
  mod0 <- lm(formula = formula, data = field)
  print(summary(mod0))
  
}

#Plot of Simple linear Regression--------------------------
attach(Data)
par(mfrow=c(3,4))

plot(lm(Satisfaction~ProdQual),col="darkblue",main="Product Quality")
plot(lm(Satisfaction~Ecom),col="darkblue",main="Ecommerce")
plot(lm(Satisfaction~TechSup),col="darkblue",main="Technical Support")
plot(lm(Satisfaction~CompRes),col="darkblue",main="Complaint Resolution")
plot(lm(Satisfaction~Advertising),col="darkblue",main="Advertising")
plot(lm(Satisfaction~ProdLine),col="darkblue",main="Product Line")
plot(lm(Satisfaction~SalesFImage),col="darkblue",main="Sales Force Image")
plot(lm(Satisfaction~ComPricing),col="darkblue",main="Competitive Price")
plot(lm(Satisfaction~WartyClaim),col="darkblue",main="Warrenty Claim")
plot(lm(Satisfaction~OrdBilling),col="darkblue",main="Oreder Bill")
plot(lm(Satisfaction~DelSpeed),col="darkblue",main="Delivery Speed")

# modelling the Response variable against the predictors using linear Regression
Model1 = lm(Satisfaction ~ . , data = Data)
summary(Model1)
Ev=t(data.frame(vif(Model1)))

Ev

## Ploting scree plot and adding lines.

plot(EV, main = "Scree Plot", xlab = "Factors", ylab = "Eigen Values", pch = 20, col = "blue")
lines(EV, col = "red")
abline(h = 1, col = "green", lty = 2)

#Checking KMO
data2 <- subset(Data, select = -c(12))
datamatrix<-cor(data2)
KMO(r=datamatrix)

#Ploting factor
scree(Data)

#PCA
data2 <- subset(Data, select = -c(12))
PCA_Un=psych::principal(r=scale(data2),nfactors = 4,rotate="none")
print(PCA_Un)

PCA_rotated=psych::principal(r=scale(data2),nfactors = 4,rotate="varimax")
print(PCA_rotated)

PCA_Data=PCA_rotated$scores
head(PCA_Data)

#FA
library(psych)
parallel <- fa.parallel(data2, fm = 'minres', fa = 'fa', n.iter=1)

fa1<- fa(r=data2, nfactors = 4, rotate="varimax",fm="pa")
print(fa1)
fa.diagram(fa1)


regdata <- cbind(Data[12], fa1$scores)

head(regdata)

#Factor loadings
colnames(regdata) <- c("Satisfaction", "Purchanse", "Marketing", "Post_purchase", "Prod_positioning")
regdata=data.frame(regdata)
regdata

#Regression with factors created
Hair_Data=lm(Satisfaction~., data = regdata)
summary(Hair_Data)


data(regdata)
describe(regdata)
mod3 <- setCor(Satisfaction ~ Purchanse + Marketing + Post_purchase + Prod_positioning, data=regdata)
