## ---- warning=FALSE, message=FALSE-------------------------------------------
setwd("C:/Users/joshi/Desktop/hotel/")
install.packages("tidyverse")
library(tidyverse)
library(readr)


## ---- warning=FALSE, message=FALSE-------------------------------------------

lasVegas <- read_delim("LasVegasTripAdvisorReviews-Dataset.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)
#dimensions of the data
dim(lasVegas)
#Hotel information
knitr::kable( head(lasVegas[, c(8:13,15:16)]), 
              caption = "Hotel Attributes")

#User information
knitr::kable( head(lasVegas[,c(1:7,17,18)]), 
              caption = "User Attributes" )

knitr::kable(table(lasVegas$`Traveler type`), caption = "Traveler Type")



## ---- warning=FALSE, message=FALSE-------------------------------------------
#no missing data
sum(is.na(lasVegas))


## ---- warning=FALSE, message=FALSE-------------------------------------------
#Checking unique values
unique(lasVegas$`Member years`)# "Unique `Member years` values"

#Inconsistent values
knitr::kable( lasVegas[lasVegas$`Member years`==-1806,c(1:5,18)], 
              caption = "Inconcistent values")


## ---- message=FALSE, warning=FALSE-------------------------------------------
lasVegas %>% 
  group_by(`User country`) %>% 
  summarise(n = n()) %>% 
  mutate( percent = n/sum(n)*100)  %>% 
  arrange(desc(n)) %>% 
  slice_max(n, n =5)


## ---- warning=FALSE, message=FALSE, fig.align='center', fig.cap="Average Reviews"----
lasVegas %>% 
  group_by(`User country`) %>% 
  summarise(`User continent` = `User continent`,
            total_review = sum(`Nr. reviews`),
            average_review = mean(`Nr. reviews`)) %>% 
  arrange(desc(total_review)) %>% 
  ggplot(aes(y = `User country`, x = average_review, 
             fill = `User continent`))+
  geom_col() +  labs(x = "Average number of Reviews") +
  theme_bw() + 
  theme(axis.title.y = element_blank(),
        legend.position = c(.85,.5),
        legend.title = element_blank())+
  theme(legend.background =  element_blank())



## ---- message=FALSE, warning=FALSE-------------------------------------------
lasVegas %>% 
  group_by(`User country`) %>% 
  summarise(total_score = sum(Score),
            average_score = mean(Score),
            n = n()) %>% 
  arrange(desc(average_score)) %>% 
  slice_max(average_score, n =5) %>% 
  knitr::kable(caption = "Countries with higher scores for the hotels")


## ---- warning=FALSE, message=FALSE, fig.align='center', fig.cap="Average Hotel Scores"----
#average of scores for hotels by the country of user and count of users
lasVegas %>% 
  group_by(`User country`) %>% 
  summarise(total_score = sum(Score),
            average_score = mean(Score),
            n = n()) %>% 
  slice_max(average_score, n = 10) %>% 
  mutate(country = if (length(`User country`) <= 3) {`User country`} 
         else {str_sub(`User country`,1,4)}) %>% 
  ggplot(aes(x = reorder(country, total_score), y = average_score)) +
  geom_col() + labs(y= "Average number of Scores") +
  coord_flip() + 
  
  geom_text(aes(label = n, hjust = -1)) +
  theme_bw() + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8))


## ---- warning=FALSE, message=FALSE, fig.align='center', fig.cap="Total Hotel Scores"----
#total Score for hotels
lasVegas %>% 
  group_by(`Hotel name`) %>% 
  summarise(total_score = sum(Score),
            rating  = mean("Hotel stars")) %>% 
  ggplot(aes(y = reorder(`Hotel name`, total_score), x = total_score)) +
  geom_col() + labs(x = "Total Scores") +
  theme_bw() + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8))


## ---- warning=FALSE,message=FALSE,fig.align='center',fig.cap="Votes by Period of Stay"----
#total helpful votes for hotels by the stay
lasVegas %>% 
  group_by(`Period of stay`) %>% 
  summarise(total_votes = sum(`Helpful votes`)) %>% 
  ggplot(aes(x = `Period of stay`, y = total_votes, fill = `Period of stay`)) +
  geom_col() + labs(y= "Total Votes") +
  scale_x_discrete(limits = c("Dec-Feb","Mar-May","Jun-Aug","Sep-Nov")) +
  theme_bw() + guides(fill = FALSE) + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8))



## ---- warning=FALSE,message=FALSE,fig.align='center',fig.cap="Reviews by Period of Stay"----
#total helpful votes for hotels by number of hotel reviews
lasVegas %>% 
  group_by(`Period of stay`) %>% 
  summarise(total_review = sum(`Nr. hotel reviews`)) %>% 
  ggplot(aes(x = `Period of stay`, y = total_review, fill = `Period of stay`)) +
  geom_col() + labs(y= "Total Hotel Reviews") +
  scale_x_discrete(limits = c("Dec-Feb","Mar-May","Jun-Aug","Sep-Nov")) +
  theme_bw() + guides(fill = FALSE) + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8))



## ---- warning=FALSE, message=FALSE-------------------------------------------
lasVegas <- 
  lasVegas %>% 
  filter(!grepl("-1806",`Member years`)) %>% 
  select(-`Nr. reviews`,-`Nr. hotel reviews`,-`Helpful votes`, -`Hotel name`,
         -`Review month`,-`Review weekday`)


## ----warning=FALSE, message=FALSE--------------------------------------------
str(lasVegas)
lasVegas$`User country` <- as.factor(lasVegas$`User country`)
lasVegas$`Period of stay` <- as.factor(lasVegas$`Period of stay`)
lasVegas$`Traveler type` <- as.factor(lasVegas$`Traveler type`)
lasVegas$Pool <- as.factor(lasVegas$Pool)
lasVegas$Gym <- as.factor(lasVegas$Gym)
lasVegas$`Tennis court` <- as.factor(lasVegas$`Tennis court`)
lasVegas$Spa <- as.factor(lasVegas$Spa)
lasVegas$Casino <- as.factor(lasVegas$Casino)
lasVegas$`Free internet` <- as.factor(lasVegas$`Free internet`)
lasVegas$`User continent` <- as.factor(lasVegas$`User continent`)
lasVegas$Score <- as.factor(lasVegas$Score)
lasVegas$`Nr. rooms` <- as.factor(lasVegas$`Nr. rooms`)
lasVegas$`Hotel stars` <- as.factor(lasVegas$`Hotel stars`)
str(lasVegas)



## ----------------------------------------------------------------------------
set.seed(1)


## ---- warning=FALSE, message=FALSE-------------------------------------------
library(e1071)
#Naive Bayes
train_index <- sample(c(1:dim(lasVegas)[1]), dim(lasVegas)[1]*0.6)
train_LA <- lasVegas[train_index, ]
valid_LA <- lasVegas[-train_index,]


## ---- warning = FALSE, message=FALSE-----------------------------------------
# run naive bayes
lA_nb <- naiveBayes(Score ~ ., data = train_LA)
lA_nb


## ----------------------------------------------------------------------------
summary(lA_nb)


## ---- message=FALSE, warning=FALSE-------------------------------------------
#Perfomance evaluation
library(caret)
# training
pred_class <- predict(lA_nb, newdata = train_LA)
confusionMatrix(pred_class, train_LA$Score)


## ---- message=FALSE, warning = FALSE-----------------------------------------
# validation
pred_class <- predict(lA_nb, newdata = valid_LA)
confusionMatrix(pred_class, valid_LA$Score)


## ---- warning=TRUE, message=FALSE, fig.align='center', fig.cap="Naive Bayes Lift Tree"----
# predict probabilities
pred_prob <- predict(lA_nb, newdata = valid_LA, type = "raw")
#Lift Chart
install.packages("gains")
library(gains)
gain <- gains(ifelse(valid_LA$Score == "5",1,0), pred_prob[,1], groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid_LA$Score=="5"))~c(0,gain$cume.obs),
     xlab="# high recommended", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid_LA$Score=="5"))~c(0, dim(valid_LA)[1]), lty=2)


## ---- warning=FALSE, message=FALSE-------------------------------------------

#partition
set.seed(1)
names(lasVegas) <- make.names(names(lasVegas))
train_index <- sample(c(1:dim(lasVegas)[1]), dim(lasVegas)[1]*0.6)
train_la <- lasVegas[train_index, ]
valid_la <- lasVegas[-train_index, ]



## ---- warning=FALSE, message=FALSE-------------------------------------------
install.packages("randomForest")
library(randomForest)
## random forest
rf <- randomForest(Score ~ . , data = train_la, ntree = 500,
                   mtry = 13, nodesize = 5, importance = TRUE)
rf


## ---- warning=FALSE, message=FALSE, fig.align='center', fig.cap="Variance Importance"----
## variable importance plot
varImpPlot(rf, type = 2)
## confusion matrix
rf_pred <- predict(rf, valid_la)
confusionMatrix(rf_pred, valid_la$Score)



## ----------------------------------------------------------------------------
plot(margin(rf, testData$Species))


## ---- fig.align='center', fig.cap="Error rates Vs Number of Trees"-----------
#Error rate
# Plot the error rate against the number of trees.

plot(rf, main="")
legend("topright", c("OOB", "1", "2", "3", "4", "5"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest")


## ---- message=FALSE, warning=FALSE, fig.align='center', fig.cap="random forest lift"----
# Plot the OOB ROC curve.
install.packages("verification")
library(verification)
aucc <- verification::roc.area(as.integer(as.factor(train_la$Score))-1,
                               rf$votes[,2])$A
verification::roc.plot(as.integer(as.factor(train_la$Score))-1,
                       rf$votes[,2], main="")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc))
title(main="ROC Curve Random Forest")

