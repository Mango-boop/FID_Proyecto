
#ESTA YA ES LA EVALUCION CON EL DATASET PREPROCESADO.


install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

install.packages("rattle")
library(rpart)
library(rattle)
library(RColorBrewer)





dataBreaches <- read.csv("data_processed.csv")
head(dataBreaches)

#columnas de nuestro dataset
colnames(dataBreaches)

#dimension de nuestro dataset
dim(dataBreaches)

dataBreaches$Sources <- NULL 
dataBreaches$X <- NULL 
dataBreaches$X.1 <- NULL 


#saber el tipo de clase de todas las columnas
sapply(data_hacked, class)

#-------------------------------------------------------------------------------


#¿Cuáles son las empresas con más brechas de datos en el conjunto de datos? 
n_organization <- dataBreaches %>%
  group_by(Organization.type) %>% 
  count() %>%
  arrange(desc(n))

print(n_organization)



#¿Cuáles son los tipos más comunes de ataques que han causado brechas de datos en el conjunto de datos?
n_method <- dataBreaches %>%
  group_by(Method) %>% 
  count() %>%
  arrange(desc(n))

print(n_method)

#¿Hay alguna tendencia en la cantidad de datos que se han hackeado en brechas de datos a lo largo del tiempo? 

data_hacked <- dataBreaches %>% 
  group_by(Records,Organization.type,Method) %>%
  filter(Method == 'hacked')

data_hacked$Sources <- NULL 
data_hacked$X <- NULL 
data_hacked$X.1 <- NULL 

print(data_hacked)

ggplot(data_hacked, aes(x = Year, y=Records,fill=Method) ) +          
  geom_bar(width = 0.9, stat="identity", position = position_dodge())


#¿Cuáles son las brechas de datos más grandes en el conjunto de datos?

max_breach <- dataBreaches %>%
  group_by(Records,Organization.type,Method) %>%
  filter(Records == max(dataBreaches$Records))

print(max_breach) 

min_breach <- dataBreaches %>%
  group_by(Records,Organization.type,Method) %>%
  filter(Records == min(dataBreaches$Records))

print(min_breach) 



attacks_by_year <- dataBreaches %>%
  group_by(Year, Method) %>%
  summarize(count = n())

# Crea el gráfico de barras apiladas
ggplot(attacks_by_year, aes(x=Year, y=count, fill=Method)) +
  geom_bar(stat="identity", position="stack")

#-------------------------------------------------------------------------------

#Porcentaje de hackeos que han ocurrido por año
porcentaje_breach_hacked <- dataBreaches %>%
  # Añade una columna "breach_hacked": valores logicos
  mutate(breach_hacked = (Method == "hacked")) %>%
  # Agrupamos por year
  group_by(Year) %>%
  # Realiza el cálculo: proporción de TRUE en breach_hacked
  
  summarize(porcentaje = mean(breach_hacked)*100.0)


porcentaje_breach_hacked


porcentaje_breach_hacked %>% 
  ggplot(aes(x=Year,y=porcentaje)) +
  geom_point()

#-------------------------------------------------------------------------------

#Realizar clasificacion con RPART



bool_breach_hacked <- dataBreaches %>%
  # Añade una columna "breach_hacked": valores logicos
  mutate(breach_hacked = (Method == "hacked"))

n_metodo <- bool_breach_hacked %>% 
  group_by(bool_breach_hacked$Method) %>% 
  count()

key_metodo <- as.integer(factor(bool_breach_hacked$Method, levels=n_metodo$`bool_breach_hacked$Method`))

key_metodo

n_organizacion <- bool_breach_hacked %>% 
  group_by(bool_breach_hacked$Organization.type) %>% 
  count()


key_organizacion <- as.integer(factor(bool_breach_hacked$Organization.type, levels=n_organizacion$`bool_breach_hacked$Organization.type`))


data_breaches_aux <- select(bool_breach_hacked,Method,breach_hacked,Organization.type)
data_breaches_aux
data_breaches_aux <- cbind(data_breaches_aux, Metodo = key_metodo)
data_breaches_aux <- cbind(data_breaches_aux, Organizacion = key_organizacion)

#Para saber exactamente el key de la organizacion
num_organizacion <- data_breaches_aux %>%
  group_by(Organization.type,Organizacion) %>% 
  count() %>%
  arrange((Organizacion))

print(num_organizacion)




bool_breach_hacked
colnames(bool_breach_hacked)
dim(bool_breach_hacked)

bool_breach_hacked$Sources <- NULL 
bool_breach_hacked$X.1 <- NULL 
bool_breach_hacked$X <- NULL 
bool_breach_hacked$Entity <- NULL 


split <- round(0.8 * nrow(data_breaches_aux))
X_train <- data_breaches_aux[1:split, ]
X_test <- data_breaches_aux[(split + 1):nrow(data_breaches_aux), ]

X_train
#dataTest <- data_breaches_aux[sample(175:nrow(data_breaches_aux)), ]
#dataTrain <- data_breaches_aux[sample(1:nrow(data_breaches_aux),177), ]




set.seed(1)
tree <- rpart(breach_hacked ~Organizacion, X_train, method="class")


tree
fancyRpartPlot(tree)


pred_valid_DT <- predict(tree, newdata = X_test, type = 'class')


matrizConfusion <- table(X_test$breach_hacked, pred_valid_DT)
matrizConfusion


#-------------------------------------------------------------------------------

set.seed(1)
model <- rpart(Organizacion ~Method + breach_hacked, X_train, method="class")


model
fancyRpartPlot(model)


pred_valid_DT <- predict(tree, newdata = X_test, type = 'class')


matrizConfusion <- table(X_test$breach_hacked, pred_valid_DT)
matrizConfusion