# Librerías y datos -------------------------------------------------------

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


df <- read.csv("accidentes_estado_meteorologico.csv", sep = ";", header = T, stringsAsFactors = T)
summary(df)
levels(df$LESIVIDAD)

# •	Presentación de los datos: los datos utilizados para este modelo constan de 24 variables con un
# total de 283947 observaciones. Las variables son de dos tipos: integer y factor. Para poder realizar 
# el modelo tuvimos que acomodar el dataframe original y hacer un data cleaning en el que quitamos variables
# repetidas como fecha; posteriormente quitamos las variables número de parte y CPFA.Desconocido debido a que
# no era unas variables que aportara una información relevante para el modelo final.

variables_borrar <- c("FECHA", "NUMERO.PARTE", "CPFA.Desconocido")
df <- df[, !(names(df) %in% variables_borrar)]
ncol(df)

# Modelo Random Forest con lesividad: --------------------------------------

# • Elaboración del modelo: decidimos hacer un modelo predictivo con Random Forest debido a la facilidad que
# presenta este algoritmo a la hora de elaborar arboles de decisión, lo que facilita encontrar puntos de corte
# en las variables independientes. Al establecer estos puntos de corte podemos aproximar aquellas variables que
# tengan mayor correlación con la variable dependiente. Por lo tanto, el algoritmo tendrá más fácil promediar
# varios árboles de decisión entrenados con distintas características.

# •	Modelo Random Forest: tanto en la parte del Split como en la aplicación del algoritmo tenemos que ponerle una
# seed para hacer que los procedimientos anteriores tengan un cierto azar, pero que pueda ser reproducible y obtenga
# los mismos valores en los que se ha basado el modelo para establecer las predicciones.

# Split: train y test
set.seed(123)
sample <- sample.split(df, SplitRatio = 0.8)
train_df <- subset(df, sample == T)
test_df <- subset(df, sample == F)

# •	Modelo Random Forest: tanto en la parte del Split como en la aplicación del algoritmo tenemos que ponerle una
# seed para hacer que los procedimientos anteriores tengan un cierto azar, pero que pueda ser reproducible y obtenga 
# los mismos valores en los que se ha basado el modelo para establecer las predicciones.

set.seed(123)
RF_df <- randomForest(LESIVIDAD ~., data = train_df, ntree = 100)

# Predicción del modelo:
predictRF_df <- predict(RF_df, newdata = test_df)
predictRF_df

# Matriz de confusión: : nos da la información necesaria sobre los resultados del modelo y sus futuras predicciones
# que nosotros utilizaremos para establecer si hemos elaborado un buen o mal modelo de predicción. A continuación, hablaremos de ello:
confusionMatrix(test_df$LESIVIDAD, predictRF_df)



#                        Reference
#                  HG    HL    IL    MT
#            HG    28  1915   162     1
#Prediction  HL    22 18273  7421     1
#            IL     5  3380 27862     0
#            MT     3    79     2     1

#Overall Statistics

#Accuracy : 0.7804         
#95% CI : (0.777, 0.7837)
#No Information Rate : 0.5992         
#P-Value [Acc > NIR] : < 2.2e-16      

#Kappa : 0.5691         

#Mcnemar's Test P-Value : < 2.2e-16      

#Statistics by Class:

#                     Class: HG Class: HL Class: IL Class: MT
#Sensitivity          0.4827586    0.7727    0.7860 3.333e-01
#Specificity          0.9648375    0.7904    0.8572 9.986e-01
#Pos Pred Value       0.0132953    0.7105    0.8917 1.176e-02
#Neg Pred Value       0.9994741    0.8393    0.7282 1.000e+00
#Prevalence           0.0009805    0.3997    0.5992 5.071e-05
#Detection Rate       0.0004733    0.3089    0.4710 1.690e-05
#Detection Prevalence 0.0356014    0.4347    0.5282 1.437e-03
#Balanced Accuracy    0.7237980    0.7815    0.8216 6.660e-01


