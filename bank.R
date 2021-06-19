##################
#Bank Marketing Campaign
#################

#The required packages are installed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")

#The dataset is then downloaded
dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip", dl)

#A trian and test have been obtained from this data

bank <- fread(text = gsub(";", "\t", readLines(unzip(dl, "bank.csv"))),
                   header=TRUE)
set.seed(1)
ind <- createDataPartition(bank$y,p=0.1, list = FALSE)
bank_test <- bank[ind,]
bank_train <- bank[-ind,]
#We ensure the colnames are Capitalised.
colnames(bank_train) <- str_to_title(colnames(bank_train))
colnames(bank_test) <- str_to_title(colnames(bank_test))

#We then ensure the factor varibles as stored as factors
bank_test <- bank_test %>% 
  mutate_at(c('Job','Marital','Education','Default','Housing','Loan','Contact','Month','Poutcome','Y'),
            as_factor)
bank_train <- bank_train %>% 
  mutate_at(c('Job','Marital','Education','Default','Housing','Loan','Contact','Month','Poutcome','Y'),
            as_factor)
#We then working exclusively With the train set to explore and visualise the data.
bank_train %>% summary()

#Visualising the findings from the summary
ggplot(data=bank_train)+
  geom_histogram(aes(x=Age,fill = Marital),binwidth = 5, position = 'nudge', col = 'tomato3')+
  labs(title='Age Distribution Histogram with Marital Status', y = 'Frequency', x='Age')

ggplot(data=bank_train)+
  geom_histogram(aes(x=Age,fill = Education),binwidth = 5, position = 'nudge', col = 'wheat3')+
  labs(title='Age Distribution Histogram with Education Levels', y = 'Frequency', x='Age')
#The plot below shows the distribution of the yearly income
ggplot(data=bank_train)+
  geom_histogram(aes(x=Balance), col='cyan',fill='Blue')+
  scale_x_continuous(limits = c(-10000,50000))+
  scale_y_log10()+
  labs(title='Average Balances Histogram on a logarithmic scale', y = 'Frequency', x='Average Balance')

#We now train a decision tree to the data
library(caret)
tree_fit <- train(Y~., 
                  data=bank_train, 
                  method='rpart',
                  tuneGrid=expand.grid(cp=seq(0.0012,0.003,0.0002)))
tree_Y <- predict(tree_fit,bank_test, )
ggplot(tree_fit, highlight = TRUE)+
  labs(title='Parmamter-Accuracy Plot')
#The decision tree analysis
fancyRpartPlot(tree_fit$finalModel)
#Showing a simpler tree
fancyRpartPlot(prune(tree_fit$finalModel, cp=0.02))

#Plotting the insights gained from the decision tree
ggplot(bank_train)+
  geom_histogram(bins= 30, aes(x=Duration/60, fill=Y))+
  facet_wrap(Poutcome~., scales = 'free')+
  labs(title = 'Campaign length by the outcome of the previous campaign',
       x='Minutes',
       y='Frequency')

#The GLM method is used for training
glm_fit <- train(Y~.,
                 method='glm',
                 data=bank_train)
glm_Y <- predict(glm_fit,bank_train)

#A knn model was also trained and fit 
knn_fit <- train(Y~.,
                 method='knn',
                 data=bank_train)

knn_Y <- predict(knn_fit,bank_train)

####After the chosen models were fitted we analysed theire confusion matices.
tree_cm_train <- confusionMatrix(data = tree_Y, reference= bank_train$Y)
tree_cm_train

glm_cm_train <- confusionMatrix(data = glm_Y, reference= bank_train$Y)
glm_cm_train

knn_cm_train <- confusionMatrix(data = knn_Y, reference= bank_train$Y)
knn_cm_train
# Reviews of these models showed low specificity, which in  the banks case is not helpful to anyone.