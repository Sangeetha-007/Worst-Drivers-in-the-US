---
title: "606-Final Presentation"
author: "Sangeetha Sasikumar"
date: "12/5/2022"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(openintro)
library(DATA606)
library(wordcloud)
library(tidyr)
library(ggplot2)
drivers <- read.csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/bad-drivers/bad-drivers.csv')
```

### **Part 1 - Introduction**
#### **Abstract/Intro**
During this holiday season, Americans will be driving to see new places, Christmas decorations and visit their loved ones. As a frequent driver myself, I get insulted by people I know from other states about how I am a terrible driver because I am a New Yorker. According to NY Magazine some of the reasons that New Yorkers get the label of being a "bad driver" is because “people that drive in very congested areas lose sight of the rules of the road” or “people in states with large metropolitan areas tend to take public transit more often and lose some of their driving skills.” [...] We’re just not driving as much as everyone else”. Fivethirtyeight uses three determining factors to see if a state’s driver is terrible or not: the number of car crashes, how much insurance companies pay out, and how much insurance companies charge drivers. In this analysis, I will be checking to see if driver behavior predicts the cost of car insurance. My prediction is, the higher the rate of collisions in a state, the higher insurance premiums would be there. 

**Context on the data collection:** Data is collected by Mona Chalabi from FiveThirtyEight (National Highway Traffic Safety Administration 2009- 2012 (NHTSA) (https://www-fars.nhtsa.dot.gov/Main/index.aspx) and National Association of Insurance Commissioners 2010 & 2011): https://github.com/fivethirtyeight/data/tree/master/bad-drivers This is an observational study. 

**Description of the dependent variable (what is being measured):** For this linear regression analysis, the dependent variable is car insurance premiums. Car insurance is different for each state/area and there are various factors that determine what the average insurance premium is. 

**Description of the independent variable:** The independent variable is the number of drivers involved in fatal collisions per billion miles. Even though this isn’t actually an independent variable in a bigger picture because fatal collisions can depend on multiple factors such as driving after drinking alcohol and driving with other distractions. However, for this analysis I am looking into it as a possible cause for high car insurance rates. 

**Research Question:** Does driver behavior predict car insurance? The worse the driver (higher collisions) a state has, would the car insurance premium be higher? 


### **Part 2 - Data**
```{r}
head(drivers, n=1)
```
```{r}
dim(drivers)
```

```{r}
colnames(drivers)
```

### **Part 3 - Exploratory data analysis**

```{r}
#premium<-drivers$Car.Insurance.Premiums
#print(premium)
#fatalCollisions<-drivers$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles
#state<-drivers$State

drivers <- drivers %>%
  rename(premium= Car.Insurance.Premiums....)
colnames(drivers)

#grades_updated <- grades_updated %>%
#  rename(Term = Col2)

drivers <- drivers %>%
  rename(NumOfDriversFatalCollisions= Number.of.drivers.involved.in.fatal.collisions.per.billion.miles)
head(drivers)
```

```{r}
plot(x = drivers$NumOfDriversFatalCollisions, y = drivers$Car.Insurance.Premiums,
   xlab = "Car Insurance",
   ylab = "Collisions",
   main = "Car Insurance Vs. Collisions",
   col="purple"
   )

```


```{r}

#Syntax: cor(x, y, method)
drivers %>%
  summarise(cor(drivers$premium, drivers$NumOfDriversFatalCollisions, use = "complete.obs"))

```

The correlation coefficient of -0.1997 shows how there is a weak negative linear relationship (out of 1). A negative correlation indicates two variables that tend to move in opposite directions: a positive change in one variable will be accompanied by a negative change in the other variable. A positive correlation indicates that the variables move in the same direction: a positive change in one variable will tend to accompany a positive change in the other variable. Since it is 0.2 out of 1, it is not considered as "strong". 


```{r}
#plot_ss(x, y, data, showSquares = FALSE, leastSquares = FALSE)
DATA606::plot_ss(x = drivers$NumOfDriversFatalCollisions, y = drivers$premium , showSquares = TRUE)  

```
The showsquares=TRUE part, shows the residuals are the difference between the observed values and the values predicted by the line. The residual sum of squares shows the level of variance in the error term (residuals), of a regression model. The smaller the residual sum of squares, the better your model will fit your data. When the residual sum of squares gets large, the poorer your model fits your data. A value of zero means your model is a perfect fit.

### **Part 4 - Inference**

```{r}
#lm( fitting_formula, dataframe )
#(Y~X)
m1 <- lm(drivers$premium~drivers$NumOfDriversFatalCollisions, data = drivers)

print(m1)

```
```{r}
summary(m1)

```

**Equation: **y=1023.35 - 8.638* premium 

**Slope:** Each additional amount of fatal collisions that have happened in a state, we would expect the premium to increase by -8.638.  
**Intercept:** The amount of car insurance premium with no amount of change based on fatal accidents is 1023.354 (y intercept). Multiple R squared tells us the measure of how well observed outcomes are replicated by the model. My multiple R squared value is 0.0398. 

**Residual:** Having a negative residual means that the predicted value is too high, similarly if you have a positive residual it means that the predicted value was too low. The aim of a regression line is to minimise the sum of residuals.

My independent variable does not seem to predict my dependent one better as my R Square value is less than premium and NumOfDriversFatalCollisions.



```{r}
ggplot(data = drivers, aes(x = drivers$NumOfDriversFatalCollisions, y = drivers$premium)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)
```

geom_smooth creates the line by fitting a linear model, which is used in the above code. 

This line can be used to predict y at any value of x. When predictions are made for values of x that are beyond the range of the observed data, it is referred to as extrapolation and is not usually recommended. However, predictions made within the range of the data are more reliable. They’re also used to compute the residuals.

```{r}
ggplot(data = m1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

```

We saw above if the relationship is linear or not with the scatterplot. We should also verify this condition with a plot of the residuals vs. fitted (predicted) values.

```{r}
#Residuals per count
ggplot(data = m1, aes(x = .resid)) +
  geom_histogram(binwidth = 20) +
  xlab("Residuals")

```

```{r}

#graphical technique for determining if two data sets come from populations with a common distribution
ggplot(data = m1, aes(sample = .resid)) +
  stat_qq()

```

We see both the normal probability plot and histogram show that the distribution of these data are nearly normal (almost a normal curve).

```{r}
#Sorting states in ascending order of collisions
drivers %>%
  select(State) %>%
  arrange((drivers$NumOfDriversFatalCollisions))


```

```{r}
#NJ is the highest, Idaho is the lowest
drivers %>% 
  mutate(state = fct_reorder(drivers$State, drivers$premium)) %>%
  ggplot(., aes(x = drivers$State,y = drivers$premium, fill = state)) + 
  geom_bar(position = "stack", stat="identity") + 
  ylab("Car Insurance Premium Per State in the United States") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
 coord_flip()

```


```{r}
#Reading in the data I collected
survey_data<-read.csv('https://raw.githubusercontent.com/Sangeetha-007/Worst-Drivers-in-the-US/main/NY%20Drivers_Road%20Experience%20(Responses)%20-%20Form%20Responses%201%20(1).csv')
head(survey_data, n=2)

```

```{r}
#Bar graph shows age groups
ggplot(data = survey_data, aes(x = Age)) + geom_bar(fill ='#9F2B68')

```

```{r}
#Word cloud on their reviews
thoughts<-survey_data$Whether.you.chose..yes..or..no...why.do.you.feel.that.way.

wordcloud(words = thoughts, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))


```


### **Part 5 - Conclusion**

Although I believed car insurance rates should increase according to driver behavior (more collisions), the data shows otherwise. The sum of squares was extremely high (1,526,088), and the higher the sum of squares, the worse the fit of the model on the data. My standard error was 98.75 for the intercept and 6.1 for NumOfDriversFatalCollisions. A high standard error shows that the sample means are widely spread apart from the population mean, thus my sample does not represent the population. My residual sum also was too large, therefore my model didn’t fit the data well either. I think the reason my numbers were so high is because the number of collisions in a state doesn't have a direct correlation to how high car insurance premiums are (thus my weak negative correlation). There are many determining factors towards car insurance premiums such as demography, how close you are to a major city, gender, your driving background, your age, how many people are attached to your premium, the brand of your car, etc. 


### References
* https://www.investopedia.com/ask/answers/041015/what-does-negative-correlation-coefficient-mean.asp

* https://www.investopedia.com/terms/r/residual-sum-of-squares.asp#

* https://www.ncl.ac.uk/webtemplate/ask-assets/external/maths-resources/statistics/regression-and-correlation/residuals.html

* https://www.scribbr.com/statistics/standard-error/

* https://nymag.com/intelligencer/2010/06/why_are_new_yorkers_such_bad_d.html




