```{r medical cost, echo=FALSE}
insurance <- read.csv("insurance.csv")

#Number of people by region
subsetofSE <- subset(insurance, region == "southeast")
subsetofSW <- subset(insurance, region == "southwest")
subsetofNE <- subset(insurance, region == "northeast")
subsetofNW <- subset(insurance, region == "northwest")

rowofSE<- nrow(subsetofSE)
rowofSW<- nrow(subsetofSW)
rowofNE<- nrow(subsetofNE)
rowofNW<- nrow(subsetofNW)

round(2.123456, digits=2)

```

Summary of the data:

```{r}
insurance <- read.csv("insurance.csv")
summary(insurance)
sum(is.na(insurance$children))
```


Percentage of males and female patients:

```{r}

#mean charges by gender
subsetMale <- subset(insurance, sex == "male")
subsetFemale <- subset(insurance, sex == "female")

male <- nrow(subsetMale)
female <- nrow(subsetFemale)

#Prepare graph parameters
x <- c(male, female)
labels <- c()
piepercent<- round(100*x/sum(x), 1)
labels <- paste(labels, piepercent)
labels <- paste(labels,"%",sep="")

pie(x, labels=labels, main = "Male and Female Patients", col=terrain.colors(length(x)))
legend("topright", c("Males", "Females"), cex = 0.9, fill = terrain.colors(length(x)))


```


Percentage of smokers and non-smokers:

```{r}
subsetSmoker<- subset(insurance, smoker == "yes")
subsetnonSmoker<- subset(insurance, smoker == "no")

numberofSmokers <- nrow(subsetSmoker)
numberofnonSmokers <- nrow(subsetnonSmoker)

#Prepare graph parameters
x <- c(numberofSmokers, numberofnonSmokers)
labels <- c()
piepercent<- round(100*x/sum(x), 1)
labels <- paste(labels, piepercent)
labels <- paste(labels,"%",sep="")

#Plot the chart
pie(x, labels=labels, main = "Smokers and Non-Smokers", col=terrain.colors(length(x)))
legend("topright", c("Smokers", "Non-Smokers"), cex = 0.9, fill = terrain.colors(length(x)))

```

Percentage of patients in each region:

```{r}
subsetSE<- subset(insurance, region == "southeast")
subsetSW<- subset(insurance, region == "southwest")
subsetNE<- subset(insurance, region == "northeast")
subsetNW<- subset(insurance, region == "northwest")

numSE <- nrow(subsetSE)
numSW <- nrow(subsetSW)
numNE <- nrow(subsetNE)
numNW <- nrow(subsetNW)

#Prepare graph parameters
x <- c(numSE, numSW, numNE, numNW)
labels <- c()
piepercent<- round(100*x/sum(x), 1)
labels <- paste(labels, piepercent)
labels <- paste(labels,"%",sep="")

#Plot the chart
pie(x, labels=labels, main = "Percentage of Patients in Each Region", col=terrain.colors(length(x)))
legend("topright", c("SouthEast", "SouthWest", "NorthEast", "NorthWest"),
       cex = 0.9, fill = terrain.colors(length(x)))

```


The average cost:

```{r}
mean(insurance$charges)
```

The average cost in each region:

```{r}
se <- round((mean(subsetSE$charges)), 2)
sw <- round((mean(subsetSW$charges)), 2)
ne <- round((mean(subsetNE$charges)), 2)
nw <- round((mean(subsetNW$charges)), 2)

regionCosts <- c(se, sw, ne, nw)

myBarChart <- barplot(regionCosts, ylab = "Average Cost", xlab = "Region", col = c(3, 4, 7, 2),
        names.arg = c("South East", "South West", "North East", "North West"),
        main = "Average Cost in Each Region")
text(myBarChart, 0, regionCosts, cex=1, pos=3)
legend("topright", c("South East", "South West", "North East", "North West"),
       cex = 0.9, fill = c(3, 4, 7, 2))

```

The average BMI for smokers and non-smokers:

```{r}
smokerBMI <- round(mean(subsetSmoker$bmi), 2)
nonSmokerBMI <- round(mean(subsetnonSmoker$bmi), 2)

x <- c(smokerBMI, nonSmokerBMI)

myBarChart <- barplot(x, ylab = "Average BMI", col = c(3, 4),
        names.arg = c("Smokers", "Non-Smokers"),
        main = "Average BMI for Smokers and Non-Smokers")
text(myBarChart, 0, x, cex=1, pos=3)

```

Womens Average BMI by Number of Children:

```{r}
noChildFemale <- subset(subsetFemale, children == 0)
oneChildFemale <- subset(subsetFemale, children == 1)
twoChildFemale <- subset(subsetFemale, children == 2)
threeChildFemale <- subset(subsetFemale, children == 3)
fourChildFemale <- subset(subsetFemale, children == 4)
fiveChildFemale <- subset(subsetFemale, children == 5)

zero <- round(mean(noChildFemale$bmi), 2)
one <- round(mean(oneChildFemale$bmi), 2)
two <- round(mean(twoChildFemale$bmi), 2)
three <- round(mean(threeChildFemale$bmi), 2)
four <- round(mean(fourChildFemale$bmi), 2)
five <- round(mean(fiveChildFemale$bmi), 2)

x <- c(zero, one, two, three, four, five)

myBarChart <- barplot(x, ylab = "Average BMI", xlab="Number of Children", col = c(3, 4, 7, 2, 5, 8),
        names.arg = c("0 kids", "1 kid", "2 kids", "3 kids", "4 kids", "5 kids"),
        main = "Womens Average BMI by Number of Children")
text(myBarChart, 0, x, cex=1, pos=3)
legend("topright", c("0", "1", "2", "3", "4", "5"),
       cex = 0.9, fill = c(3, 4, 7, 2, 5, 8))

```



```{r}
#1) Smoker kadınların ortalama BMI non smoker kadınlardan fazla mı?
smokingladies<- subset(subsetFemale, smoker == "yes")
nonsmokingladies <- subset(subsetFemale, smoker == "no")

femaleSmoker <- round(mean(smokingladies$bmi), 2)
femalenonSmoker <- round(mean(nonsmokingladies$bmi), 2)

x <- c(femaleSmoker,femalenonSmoker)

myBarChart <- barplot(x, ylab = "Average BMI", xlab="Smoker", col = c(3, 4, 7, 2, 5, 8),
        names.arg = c("Yes","No"),
        main = "Womens Average BMI by Smoking Habits")
text(myBarChart, 0, x, cex=1, pos=3)
legend("topright", c("smokers", "nonSmokers"),
       cex = 0.9, fill = c(3, 4))

```

Charges by Region

```{r}
#2) Southeast ortalama ücret Southwest'den fazla mı?
chargesNW <- round(mean(subsetNW$charges), 2)
chargesNE <- round(mean(subsetNE$charges), 2)
chargesSE <- round(mean(subsetSE$charges), 2)
chargesSW <- round(mean(subsetSW$charges), 2)
x <- c(chargesNW, chargesNE, chargesSE, chargesSW)

myBarChart <- barplot(x, ylab = "Average BMI", xlab="Region", col = c(3, 4, 7, 2),
        names.arg = c("Northwest", "Northeast", "Southeast", "Southwest"),
        main = "Mean Charges by Region")
text(myBarChart, 0, x, cex=1, pos=3)
legend("topright", c("NW", "NE", "SE", "SW"),
       cex = 0.9, fill = c(3, 4, 7, 2))

```



```{r}

# Switch string values with numerical values to use in regression

# female -> 0, male -> 1 (changes the csv)

insurance$sex <- as.character(insurance$sex)
insurance$sex[which(insurance$sex=="female")] <- "0"
insurance$sex[which(insurance$sex=="male")] <- "1"
insurance$sex <- as.numeric(insurance$sex)

# smoker no -> 0, smoker yes -> 1

insurance$smoker <- as.character(insurance$smoker)
insurance$smoker[which(insurance$smoker=="no")] <- "0"
insurance$smoker[which(insurance$smoker=="yes")] <- "1"
insurance$smoker <- as.numeric(insurance$smoker)

# northeast -> 0, northwest -> 1, southeast -> 2, southwest -> 3

insurance$region <- as.character(insurance$region)
insurance$region[which(insurance$region=="northeast")] <- "0"
insurance$region[which(insurance$region=="northwest")] <- "1"
insurance$region[which(insurance$region=="southeast")] <- "2"
insurance$region[which(insurance$region=="southwest")] <- "3"
insurance$region <- as.numeric(insurance$region)


```


```{r}
#3) Predict charges for new patient

#Fit multiple linear regression model
model <- lm(charges ~ age + sex + bmi + children + smoker + region, data=insurance)

#View model summary
summary(model)

#Define new patient
new <- data.frame(age=c(63), sex=c(1), bmi=c(22.8), children=c(3), smoker=c(1), region=c(3))

#Use the fitted model to predict the rating for the new patient
predict(model, newdata=new)


plot(insurance$bmi, insurance$charges, col='red', pch=20)
abline(lm(charges ~ bmi, data = insurance), col = "blue")

smoothScatter(insurance$age, insurance$charges)

```

```{r}
library(igraph)
library(corrplot)
library(dplyr)
options(repr.plot.width=7, repr.plot.height=7)
corr <- cor(insurance %>% mutate_if(is.factor, as.numeric))

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=45, 
         sig.level = 0.01, insig = "blank", 
         diag=FALSE)
```
```{r}
set.seed(1881)
samp <- sample(1:nrow(insurance), ceiling(0.80*nrow(insurance)))
train <- insurance[samp,]
test <- insurance[-samp,]

options(scipen = 3131)
l <- lm(charges ~ age + sex + bmi + children + smoker + region, data = train)
summary(l)

library(Metrics)
l_pred <- predict(l, test)
radj <- summary(l)$adj.r.squared
rse <- sqrt(sum(residuals(l)^2) / l$df.residual ) 
rmse <- rmse(l_pred, test$charges)
aic <- AIC(l)
l_reg <- cbind("Adjusted R sq"=radj, "RSE"=rse, "RMSE"=rmse, "AIC"=aic)


```