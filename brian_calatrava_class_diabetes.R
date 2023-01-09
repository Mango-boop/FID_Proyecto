install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)
install.packages("rattle")
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
install.packages("plotly")
library(plotly)



data <- read.csv("diabetes.csv")
head(data)



#columnas de nuestro dataset
colnames(data)

#dimension de nuestro dataset
dim(data)


sapply(data, class)

mean(data$Age)

# variable de las mujeres con mas de 30
variable_auxiliar<- data %>% count(Age>29)

# Muestra esa variable con un mensaje
print(paste("Mujeres en total: ",variable_auxiliar))


#Contador de mujeres sin diabetes y personas con diabetes
n_outcome <- data %>% 
  group_by(Outcome) %>% 
  count() %>%
  arrange(desc(n))

print(n_outcome, n=100)


#mujeres que no tienen diabetes
people_no_diabetes <- data %>% 
  group_by(Outcome) %>%
  filter(Outcome == 0)

people_no_diabetes
dim(people_no_diabetes)


#mujeres  con diabetes mayores de 35 
people_diabetes <- data %>% 
  group_by(Age,Outcome) %>%
  filter(Outcome == 1 & Age > 35)

people_diabetes
dim(people_diabetes) #137 mujeres


#mujeres con un BMI saludable 
good_bmi <- data %>% 
  group_by(Age,BMI) %>%
  filter(BMI >=18.5 & BMI <= 24.9)

good_bmi
dim(good_bmi) #102 mujeres con un BMI saludable

ggplot(data = good_bmi) +
  stat_count(mapping = aes(x = Age))


#Mujeres con prediabetes
pre_diabetes <- data %>% 
  group_by(Age,Glucose) %>%
  filter(Glucose >=140 & Glucose <= 199)

pre_diabetes
dim(pre_diabetes)

pre_no_diabetes <- pre_diabetes %>% 
  group_by(Outcome) %>%
  filter(Outcome == 0)
pre_no_diabetes
dim(pre_no_diabetes)



#------------------VISUALIZACION---------------------------------------

#visualizacion de la cantidad de personas por edad
ggplot(data = data_zero) +
  stat_count(mapping = aes(x = Age))


#Visualizacion de la cantidad de outcomes que hay
ggplot(data = data) +
  stat_count(mapping = aes(x = Outcome))

ggplot(data, aes(x = Age, y = Glucose, color = Outcome)) +
  geom_line()

ggplot(data, aes(x = Age, y = Glucose, color = Outcome)) +
  geom_density2d()

#el siguiente código hace un gráfico de dispersión interactivo 
#de la edad vs el nivel de glucosa en sangre

plot_ly(data, x = ~Age, y = ~Glucose, type = "scatter", mode = "markers")



#---------------------------------------------------------

#Eliminamos aquellas columnas que son incoherentes para nuestro analisis

#eliminacion de columnas 
data$DiabetesPedigreeFunction <- NULL 
data$Pregnancies <- NULL 
data$SkinThickness <- NULL 
data$BloodPressure <- NULL 
colnames(data)




#Nos toca realizar una tranformacion de los valores que son cero

head(data)


data$SkinThickness <- ifelse(data$SkinThickness == 0, sample(1:40, nrow(data), replace = TRUE), data$SkinThickness)
data$Insulin <- ifelse(data$Insulin == 0, sample(1:100, nrow(data), replace = TRUE), data$Insulin)
data$BMI <- ifelse(data$BMI == 0, sample(1:100, nrow(data), replace = TRUE), data$BMI)

#---------------------------------------------------------
rm(list = ls()) #me permite eliminar la memoria global

#Realizar clasificacion con RPART

split <- round(0.8 * nrow(data))
X_train <- data[1:split, ]
X_test <- data[(split + 1):nrow(data), ]



#validacion cruzada k-fold Cross Validation
library(class)
library(caret)
folds <- createFolds(X_train$Outcome, k = 10)

Knn <- lapply(folds, function(x){
  training_fold <- X_train[-x, ]
  test_fold <- X_train[x, ]
  y_pred <- knn(training_fold, 
                test_fold, 
                cl = training_fold$Outcome, 
                k = 5
                )
  matriz <- table(test_fold$Outcome, y_pred)
  precision <- (matriz[1,1] + matriz[2,2]) / (matriz[1,1] + matriz[2,2] +matriz[1,2] + matriz[2,1])
  return(precision)
})



precision_kNN <- mean(as.numeric(Knn))


#arbol de decision
ArbolDecision <- lapply(folds, function(x){
  training_fold <- X_train[-x, ]
  test_fold <- X_train[x, ]
  clasificador <- rpart(Outcome ~ ., data = training_fold, method="class",control=rpart.control(cp=0.001))
  y_pred <- predict(clasificador,test_fold, type = 'class')
  matriz <- table(test_fold$Outcome, y_pred)
  precision <- (matriz[1,1] + matriz[2,2]) / (matriz[1,1] + matriz[2,2] +matriz[1,2] + matriz[2,1])
  return(precision)
})

precision_DecisionTree <- mean(as.numeric(ArbolDecision))





#Ya hicimos nuestro analisis de cual tendria mayor precision de prediccion

tree_test <- rpart(Outcome ~., X_train, method="class", control=rpart.control(cp=0.001))

fancyRpartPlot(tree_test)
pruned <- prune(tree_test, cp = 0.01)
fancyRpartPlot(pruned)

pred_valid_DT <- predict(pruned, newdata = X_test, type = 'class')

matrizConfusion <- table(X_test$Outcome, pred_valid_DT)
matrizConfusion

accuracy <- mean(pred_valid_DT == X_test$Outcome)

print(accuracy)



#-------------------SIN VALDICACION CRUZADA--------------------------------------

#control=rpart.control(cp=0.00001) esto era lo que se utilizaba al principio
set.seed(1)
tree <- rpart(Outcome ~., X_train, method="class", control=rpart.control(cp=0.00001))

# Poda el árbol
pruned <- prune(tree, cp = 0.01)



tree
pruned

fancyRpartPlot(pruned)



prediccion <- predict(pruned, newdata = X_test, type = 'class')


matrizConfusion <- table(X_test$Outcome, prediccion)
matrizConfusion


prediccion == X_test$Outcome
# Calcular la precisión del modelo
accuracy <- mean(prediccion == X_test$Outcome)

print(accuracy)




#-------------------------------------------------------------------
#Evaluar cual seria el valor de CP mas adecuado para nuestro arbol


tree_complete <- rpart(Outcome ~., data, cp = 0)

rpart.rules(tree_complete, style = "tall")

printcp(tree_complete)


plotcp(tree_complete)

head(tree_complete$cptable, 10)


xerror <- tree_complete$cptable[,"xerror"]
imin.xerror <- which.min(xerror)
# Valor óptimo
tree_complete$cptable[imin.xerror, ]


upper.xerror <- xerror[imin.xerror] + tree_complete$cptable[imin.xerror, "xstd"]
icp <- min(which(xerror <= upper.xerror))
cp <- tree_complete$cptable[icp, "CP"]
cp

podado <- prune(tree_complete, cp = cp)

fancyRpartPlot(podado)


