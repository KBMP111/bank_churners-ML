---
title: "Bank Marketing Campaign"
author: "Kabwe Mpundu"
date: "27/08/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction
A Portugues bank collected marketing information for its customers after a campaign to encourage the uptake of term deposits. This data is available to the public through Kaggle and was the centre of an academic paper related to data mining. 

The dataset includes information on savers and non-savers all of whom were customers of the bank. In addition to this key information the following variables were included.

 1. Age : The age of the customer 
 2. Job : A job category which may include:"admin.","unknown","unemployed","management","housemaid","entrepreneur","student","blue-collar","self-employed","retired","technician","services") 
 3. Marital : The marital status of the customer; "married","divorced","single"; divorced also included widowed.
 4. Education: The level of education of the customer: "unknown","secondary","primary","tertiary")
 5. Default:Did the client have any credit in default? : "yes","no"
 6. Balance: The average yearly balance, in euros (numeric) 
 7. Housing: Did the client have a housing loan? : "yes","no"
 8. Loan: Did the client have a personal loan? :"yes","no"
 
 The following were related with the last contact of the marketing campaign:
 
 1. Contact: The type of contact communication type: "unknown","telephone","cellular"
 2. Day: The last contact day of the month (numeric)
 3. Month: The last contact month of year "jan",..., "dec"
 4. Duration: last contact duration, in seconds (numeric) 
 5. Campaign: The number of contacts performed during this campaign and for this client (numeric, includes last contact)
 6. Pdays: The number of days between contact from a previous campaigns (numeric, -1 means client was not previously contacted)
 15. Previous: The number of contacts performed before the campaign and for this client.
 16. Poutcome: The outcome of the previous marketing campaign:"unknown","other","failure","success".
 
The target variable was the clients who subscribed for a term deposit as a result of the marketing campaign.

To solve this problem we shall begin by processing the data to ensure the it is clean and compatitable for Exploration, Visualisation, Training and Testing. The Methods Section details how the dataset was downloaded and how it was transformed to have gained insights into customer saving. It has also shown which Machine Learning tools were used.  

## Methods and Analysis
The dataset was first downloaded with the required packages, the following section of the r script demonstrates a part of the process:
```{r, include=FALSE}
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")

```
```{r Loading the Data,echo=TRUE,include=TRUE, results = 'hide'}
dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip", dl)


bank <- fread(text = gsub(";", "\t", readLines(unzip(dl, "bank.csv"))),
               header=TRUE)
set.seed(1)
ind <- createDataPartition(bank$y,p=0.1, list = FALSE)
bank_test <- bank[ind,]
bank_train <- bank[-ind,]

```
```{r cleaning and organising the data}
#We ensure the colnames are Capitalised.
colnames(bank_train) <- str_to_title(colnames(bank_train))
colnames(bank_test) <- str_to_title(colnames(bank_test))

#We then ensure the factor variables as stored as factors
bank_test <- bank_test %>% 
  mutate_at(c('Job','Marital','Education','Default','Housing','Loan','Contact','Month','Poutcome','Y'),
            as_factor)
bank_train <- bank_train %>% 
  mutate_at(c('Job','Marital','Education','Default','Housing','Loan','Contact','Month','Poutcome','Y'),
            as_factor)
```
The data has 17 variables and over a 45, 211 observations for the train set and 4, 521 for the test set. We then ensure then code the following variables as categorical. A summary of the data shows the distributions of the variables we find the some variables will be more useful than others at informing our decisions due to the qualities they present such as spread and distribution. 

```{r echo=FALSE}


bank_train %>% summary()

```
The analysis above shows most customers were contacted in May, they had low average balances, were married and between the ages 33 and 48. It also showed most clients had taken a pay_day loan, It appears the marketing campaign was mostly targeted at struglling low income families. This was supported by the fact that the majority of our population had no tertiary education. It was also noted most customers were contacted by phone and the quality of the information was poor given, with many blanks or irrelvant varibles.

The findings are visualised as follows:
```{r Intitial Visualisation, echo=FALSE}
#Visualising the findings from the summary
ggplot(data=bank_train)+
  geom_histogram(aes(x=Age,fill = Marital),binwidth = 5, position = 'nudge', col = 'tomato3')+
  geom_vline(xintercept =33, col='violetred')+
  geom_vline(xintercept =48, col= 'violetred')+
  labs(title='Age Distribution Histogram with Marital Status', y = 'Frequency', x='Age')

ggplot(data=bank_train)+
  geom_histogram(aes(x=Age,fill = Education),binwidth = 5, position = 'nudge', col = 'wheat3')+
  geom_vline(xintercept =33, col='coral4')+
  geom_vline(xintercept =48, col= 'coral4')+
  labs(title='Age Distribution Histogram with Education Levels', y = 'Frequency', x='Age')

ggplot(data=bank_train)+
  geom_histogram(aes(x=Balance), col='cyan',fill='Blue')+
  scale_x_continuous(limits = c(-10000,50000))+
  scale_y_log10()+
  labs(title='Average Balances Histogram on a logarithmic scale', y = 'Frequency', x='Average Balance')

```

The data required further analysis for modeling, a decision tree was trained on to the data to highlight the key determinants for saving.
# Decision Tree
```{r Decision Tree }
tree_fit <- train(Y~., 
                  data=bank_train, 
                  method='rpart',
                  tuneGrid=expand.grid(cp=seq(0.0012,0.003,0.0002)))
tree_Y <- predict(tree_fit, bank_test)
ggplot(tree_fit, highlight = TRUE)+
  labs(title='Parmamter-Accuracy Plot')
#The decision tree analysis
#fancyRpartPlot(tree_fit$finalModel)

```

The decision tree had many branches, but would acheived an accruacy over 90%, for the purposes of illustration the less significant branches were chopped off. The tree below illustrated the most important discriminants.

```{r Simpler decisionn tree}
#Showing a simpler tree
fancyRpartPlot(prune(tree_fit$finalModel, cp=0.02))
```

The decision tree showed the key discriminants were the outcomes of previous marketing campaigns and duration of the current  marketing campaign.

Further explorations revealed the following about succesful outcomes;

 1. Customers who subscribed previously were more likely to subscribe again.
 2. Short campaings were not effective, escpecially under three minutes.
 3. Campaigns under 15 minutes long were most effective

 
```{r Decision Tree insights, echo=FALSE}
ggplot(bank_train)+
  geom_histogram(bins= 30, aes(x=Duration/60, fill=Y))+
  facet_wrap(Poutcome~., scales = 'free')+
  labs(title = 'Campaign length by the outcome of the previous campaign',
       x='Minutes',
       y='Frequency')
```
```{r , echo=FALSE}
med <-function(u){ median(u,na.rm = TRUE)}

```

Besides the decision tree the Generalised Linear Model (GLM) was used to predict the marketing outcomes. It was found to have a high accuracy and specificity, unfortunately the sensitivity of the model was below 50%. The similar obsevations were noted with KNN neighbours and the decision tree analysis

```{r , GLM training}
#The GLM method is used for training
glm_fit <- train(Y~.,
                  method='glm',
                  data=bank_train)

glm_Y <- predict(glm_fit,bank_test)
```
```{r, knn training}
#A knn model was also trained and fit 
knn_fit <- train(Y~.,
                 method='knn',
                 data=bank_train)

knn_Y <- predict(knn_fit,bank_test)
```
```{r, plotting the confusion matri}

####After the chosen models were fitted we analysed theire confusion matices.
tree_cm_test <- confusionMatrix(data = tree_Y, reference= bank_test$Y)
tree_cm_test

glm_cm_test <- confusionMatrix(data = glm_Y, reference= bank_test$Y)
glm_cm_test

knn_cm_test <- confusionMatrix(data = knn_Y, reference= bank_test$Y)
knn_cm_test
# Reviews of these models showed low specificity, which in  the banks case is not helpful to anyone.

```


The models above add little value to the bank as they would turn away most prospective valuable candidates, and also a significant proportion of people predicted to save had been wronlgy classified. That is there are many true negatives and false positivies.
```{r}

```
## Results
The sensitivity of the models varied ranging from 15% to 50%, thus the models above add little value to the bank as they would turn away most prospective valuable candidates, and also a significant proportion of people predicted to save had been wrongly classified. That is there are many true negatives and false positives.


## Conclusion

Because of the low prevalence about 10%, the accuracy was high at around 90%, but the sensitivity was low, ranging between 10% and 60%. The predictions from  K-Nearest Neighbours had the lowest sensitivity at around 15% ,the Generalised Linear Model followed around 24% percent,and the most helpful is the decision tree with a sensitivity around 38%.

Nonetheless, The test still showed Previous Customers were very likely to subscribe, it also showed the time spent on a customer had a non-linear relationship with savings. The customers age also played a factor with young and elderly people saving more than their middle aged counterparts. 


## Recommendations

 1. To target younger and older people in future campaigns.
 2. To build a polynomial regression model to predict marketing outcomes.
 3, Collect more statistics, as many variables reported blanks.
 3. Conducting the campaigns in March, September and December as there was a higher uptake during these months, and the current campaign was mostly conducted in May.
 4. Given the low prevalence, and the lack of relevant variables the Decision tree should not be used, unless corrected for prevalence.
 




