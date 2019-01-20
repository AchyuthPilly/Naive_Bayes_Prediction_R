# Libraries
install.packages("naivebayes")
install.packages("psych")
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Data
data <- read.csv(file.choose(), header = T)
View(data)
str(data)
xtabs(~admit+rank, data = data)
data$rank <- as.factor(data$rank)
data$admit <- as.factor(data$admit)

# Visualization
data %>%
  ggplot(aes(x=admit, y=gpa, fill = admit)) +
  geom_boxplot() +
  ggtitle("Box Plot")
data %>%
  ggplot(aes(x=admit, y=gre, fill = admit)) +
  geom_boxplot() +
  ggtitle("Box Plot")

data %>% ggplot(aes(x=gpa, fill = admit)) +
  geom_density(alpha=0.8, color= 'black') +
  ggtitle("Density Plot")
data %>% ggplot(aes(x=gre, fill = admit)) +
  geom_density(alpha=0.8, color= 'black') +
  ggtitle("Density Plot")

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

# Naive Bayes Model
model <- naive_bayes(admit ~ ., data = train, usekernel = T)
model

plot(model)

# Predict
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$admit))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$admit))
1 - sum(diag(tab2)) / sum(tab2)
