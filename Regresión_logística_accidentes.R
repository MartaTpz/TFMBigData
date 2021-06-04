library(corrplot)
library(rpart.plot)
library(rpart)
library(randomForest)
library(magrittr)
library(dplyr)
library(ggplot2)
library(MASS)
library(ISLR)
library(caTools)
library(corrplot)
library(caret)
library(e1071)
library(tidyverse)

# Modelo de regresión logístca: con Lesividad ---------------------------------------------------------

# Acomodamos los datos al logaritmo binomial: AG = Accidentes graves & AL = Accidentes leves
df$LESIVIDAD <- factor(df$LESIVIDAD, levels = c("AG", "IL", "HL"))
df$LESIVIDAD[df$LESIVIDAD == "HG" | df$LESIVIDAD == "MT"] <- "AG"
df$LESIVIDAD[is.na(df$LESIVIDAD)] <- "AG"
df$LESIVIDAD <- factor(df$LESIVIDAD, levels = c("AG", "AL"))
df$LESIVIDAD[df$LESIVIDAD == "IL" | df$LESIVIDAD == "HL"] <- "AL"
df$LESIVIDAD[is.na(df$LESIVIDAD)] <- "AL"

table(df$LESIVIDAD)

# División de los datos
df$LESIVIDAD <- as.factor(df$LESIVIDAD)

set.seed(123)
split <- sample.split(df$LESIVIDAD, SplitRatio = 0.7)
df_train <- subset(df, split == T)
df_test <- subset(df, split == F)

# Regresión:
log_model <- glm(LESIVIDAD ~., family = binomial(), df_train)
summary(log_model)

# Predicción:
prediction <- predict(log_model, df_test, type = "response")

# Matriz de confusión:
confusionMatrix(df_test$LESIVIDAD, prediction)
table(df_test$LESIVIDAD, prediction >= 0.5)

#   FALSE  TRUE
#AG    23  3020
#AL    83 82058

# Accuracy: 
(23 + 82058)/nrow(df_test)# 0.96%

# Recall o sensibilidad:
(83 + 82058)# 82141
(82058 / 82141)# 99%

# Especifidad:
(23 + 3020)# 3043
(23 / 3043)# 7% 
