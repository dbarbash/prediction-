#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("ggpubr")
#install.packages("rpart.plot")
#install.packages("neuralnet")
#install.packages("randomForest")
#install.packages("rpart")
#install.packages("caTools")
library(tidyverse)
library(ggplot2)
library(corrplot)
library(ggpubr)
library(rpart.plot)
library(neuralnet)
library(randomForest)
library(rpart)
library(caTools)

df <- read_csv("2021.csv")
str(df)
summary(df)
# some of the variable names are not clear enough and I decided to change the name of several of them a little bit. 
# Also, I will remove whisker low, whisker high and Ladder score in Dystopia variables from my dataset because
# there is no need to use them for visualization and prediction.
df<-df[-c(5,6,13)]
colnames (df) <- c("Country","Region", "Score", "SE",
                   "GDP", "Social_sup","Life_Expectancy", "Freedom",
                   "Generosity","Corruption", "Exp.GDP","Exp.Soc",
                   "Exp.Life","Exp.Free","Exp.Gen","Exp.Corr","Dystopia"
                   )

# correlation between numerical variables in our dataset
Num.cols <- sapply(df, is.numeric)
Cor.data <- cor(df[, Num.cols])
corrplot(Cor.data, method = 'number')

# Average value of happiness score variables for different regions
df_plot1 <- df %>% 
  group_by(Region) %>%
  summarise(avg_happiness = mean(Score)) %>%
  arrange(desc(avg_happiness))
  
ggplot(df_plot1, aes(y=avg_happiness, x=Region, color=Region, fill=Region)) + 
  geom_bar( stat="identity")+ theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Average value of happiness score variables for different regions", 
       y = "Average value") 

# happiness score distribution in different regions
ggplot(df , aes(x = Region, y = Score)) +
  geom_boxplot(aes(fill=Region)) + theme_bw() +
  theme(axis.text.x=element_text(angle = 90))

# spliting dataset into training and test set. Dependent variable is happiness score
set.seed(123)
dataset <- df[3:17]
split = sample.split(dataset$Score, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Multiple Linear Regression
regressor_lm = lm(formula = Score ~ .,
                  data = training_set)

summary(regressor_lm)

# Predicting 
y_pred_lm = predict(regressor_lm, newdata = test_set)

Pred_Actual_lm <- as.data.frame(cbind(Prediction = y_pred_lm, Actual = test_set$Score))

# Plot
gg.lm <- ggplot(Pred_Actual_lm, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Multiple Linear Regression", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(face = "bold", size = (15)), 
        axis.title = element_text(size = (10)))
gg.lm

# Decision Tree Regression 
regressor_dt = rpart(formula = Score ~ .,
                     data = dataset,
                     control = rpart.control(minsplit = 10))

# Predicting
y_pred_dt = predict(regressor_dt, newdata = test_set)

Pred_Actual_dt <- as.data.frame(cbind(Prediction = y_pred_dt, Actual = test_set$Score))

# Plot
gg.dt <- ggplot(Pred_Actual_dt, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Decision Tree Regression", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(face = "bold", size = (15)), 
        axis.title = element_text(size = (10)))
gg.dt

# Plotting the tree
prp(regressor_dt)

# Random Forest Regression 
regressor_rf = randomForest(x = dataset[-1],
                            y = dataset$Score,
                            ntree = 500)

# Predicting
y_pred_rf = predict(regressor_rf, newdata = test_set)

Pred_Actual_rf <- as.data.frame(cbind(Prediction = y_pred_rf, Actual = test_set$Score))

# Plot
gg.rf <- ggplot(Pred_Actual_rf, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Random Forest Regression", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(face = "bold", size = (15)), 
        axis.title = element_text(size = (10)))
gg.rf



# Neural Net
nn <- neuralnet(Score ~ SE + GDP + Social_sup + Life_Expectancy + Freedom + Generosity + Corruption
                + Exp.GDP + Exp.Soc + Exp.Life + Exp.Free + Exp.Gen + Exp.Corr + Dystopia,
                data=training_set,hidden=10,linear.output=TRUE)
# NN plot
plot(nn)

# Predicting
predicted.nn.values <- compute(nn,test_set[,2:15])

Pred_Actual_nn <- as.data.frame(cbind(Prediction = predicted.nn.values$net.result, Actual = test_set$Score))

# Plot
gg.nn <- ggplot(Pred_Actual_nn, aes(Actual, V1 )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Neural Net", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(face = "bold", size = (15)), 
        axis.title = element_text(size = (10)))
gg.nn
# Neural Net and Multiple Linear Regression are the best predictors 

# The mean squared error for Multiple Linear Regression and Neural Net model
MSE.lm <- sum((test_set$Score - y_pred_lm)^2)/nrow(test_set)
MSE.nn <- sum((test_set$Score - predicted.nn.values$net.result)^2)/nrow(test_set)

print(paste("Mean Squared Error (Multiple Linear Regression):", MSE.lm))
print(paste("Mean Squared Error (Neural Net):", MSE.nn))
# As expected, mean squared error for Multiple Linear Regression is smaller that Neural Net.

ggarrange(gg.lm, gg.dt, gg.rf, gg.nn, ncol = 2, nrow = 2)



