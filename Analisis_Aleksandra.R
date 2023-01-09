# ---
# title: "Analisis de data breaches en los ultimos años. Supervised ML"
# 
# author: "Dubovik Aleksandra"
# ---
#   
library(magrittr)
library(tidyverse)
library(scales)
library(ggplot2)
library(plotly)
library(dplyr)

setwd('/Users/alexandradubovik/FID_Proyecto')
data <- read.csv("data_processed.csv", header = TRUE)


# Este código consiste en dos partes:
#   1. Vamos a visualizar el ratio de numero de datos robados y el año en la siguiente gráfica:
Total_Stolen = data %>%
  group_by(Year) %>%
  summarise_at(vars(Records), list(Total_Stolen = sum))

# Creamos dataframe con columnas Year y Total_Stolen, que indica la suma total de datos robados y usamos este dataframe en plot
ggplot(Total_Stolen, mapping = aes(x = Year, y = Total_Stolen)) + geom_point(size=2) + geom_line(color = "red")

#Check the number of rows for last three years:
print(paste("There are", count(data[data$Year=='2020',]), " rows in year 2020"))
print(paste("There are", count(data[data$Year=='2021',]), " rows in year 2021"))
print(paste("There are", count(data[data$Year=='2022',]), " rows in year 2022"))

# Put it in a table for report
tab <- matrix(c(32, 13, 5), ncol=1, byrow=TRUE)
colnames(tab) <- c('Number of rows with data breaches')
rownames(tab) <- c('2020','2021','2022')
tab <- as.table(tab)
tab

# Median number of rows per year in period 2004-2020
data1 <- data[data$Year != '2021',]
data1 <- data1[data1$Year != '2022',]
str(data1) # 355 - 18 = 337
n_rows_per_year = data1 %>%
  group_by(Year) %>%
  tally()
median_nrows = median(n_rows_per_year$n) # 22

# The rest of exploration

# Count entries (= number of attacks) for each category 
num_rows = data %>% 
  group_by(Organization.type, Year) %>% 
  count() %>%
  arrange(desc(n))

qplot(Year, n, data = num_rows, color = Organization.type, lab="Year", ylab="Total num of attacks per category", geom = "point")
# Count types of attack
data %>% 
  group_by(data$Method) %>% 
  count() %>%
  arrange(desc(n))



#   2. Linear regression:  number of stolen records 

Total_Stolen1 <- Total_Stolen[Total_Stolen$Year != "2021",]
Total_Stolen <- Total_Stolen1[Total_Stolen1$Year != "2022",]

qplot(Year, Total_Stolen, data = Total_Stolen, geom = "point")
qplot(Year, Total_Stolen, data = Total_Stolen, geom = "point") + geom_smooth(method = "lm", se = FALSE)
data_regression <- lm(Total_Stolen ~ Year, data = Total_Stolen) #Records = y-values, Year = x-value
data_prediction = predict(data_regression, data_frame(Year=c(2023, 2030, 2040)))

tab <- matrix(c(2013187767, 2880539291, 4119612896 ), ncol=1, byrow=TRUE)
colnames(tab) <- c('Predicted number of stolen data records')
rownames(tab) <- c('2023','2030','2040')
tab <- as.table(tab)

c_lr = c(Total_Stolen$Year, 2023, 2030, 2040)
stolen = c(Total_Stolen$Total_Stolen, 2013187767, 2880539291, 4119612896)
qplot(c_lr, stolen, geom = c("point", "line"), main = "Predicción numero de datos robados (todos categorías)",
     xlab = "Years",
     ylab = "Numero de datos robados")

# LR for different categories

data <- data[data$Organization.type != "various",]
Total_per_cat  = data %>%
  group_by(Organization.type, Year) %>%
  summarise_at(vars(Records), list(Total_per_cat = sum))

qplot(Year, Total_per_cat, data = Total_per_cat, color = Organization.type, lab="Year", ylab="Total num of  stolen records per category", geom = "point") + geom_smooth(method = "lm", se = FALSE)
tot_academic <- Total_per_cat[Total_per_cat$Organization.type == 'academic',]
tot_financial<- Total_per_cat[Total_per_cat$Organization.type == 'financial',]
tot_governement<- Total_per_cat[Total_per_cat$Organization.type == 'government',]
tot_tech<- Total_per_cat[Total_per_cat$Organization.type == 'tech',]
tot_web<- Total_per_cat[Total_per_cat$Organization.type == 'web',]
tot_social<- Total_per_cat[Total_per_cat$Organization.type == 'social media',]

# Regression for each category
data_reg_academic <- lm(Total_per_cat ~ Year, data = tot_academic) #Records = y-values, Year = x-value
data_pred_academic = predict(data_reg_academic, data_frame(Year=c(2030)))


data_reg_financial <- lm(Total_per_cat ~ Year, data = tot_financial) #Records = y-values, Year = x-value
data_pred_financial = predict(data_reg_financial, data_frame(Year=c(2030)))

data_reg_governement <- lm(Total_per_cat ~ Year, data = tot_governement) #Records = y-values, Year = x-value
data_pred_governement = predict(data_reg_governement, data_frame(Year=c(2030)))

data_reg_tech <- lm(Total_per_cat ~ Year, data = tot_tech) #Records = y-values, Year = x-value
data_pred_tech = predict(data_reg_tech, data_frame(Year=c(2030)))

data_reg_web <- lm(Total_per_cat ~ Year, data = tot_web) #Records = y-values, Year = x-value
data_pred_web = predict(data_reg_web, data_frame(Year=c(2030)))

data_reg_social <- lm(Total_per_cat ~ Year, data = tot_social) #Records = y-values, Year = x-value
data_pred_social = predict(data_reg_social, data_frame(Year=c(2030)))

# Plot regressions
years_w = c(tot_web$Year, 2030)
y_web = c(tot_web$Total_per_cat, data_pred_web)
plot(years_w,y_web, type = 'l', col = "black", main = "Predicción para año 2030 con LR",
     xlab = "Years",
     ylab = "Numero de datos robados")
years_f = c(tot_financial$Year, 2030)
y_financial = c(tot_financial$Total_per_cat, data_pred_financial)
lines(years_f, y_financial, type = "l", col = "red") 
years_a = c(tot_academic$Year, 2030)
y_academic = c(tot_academic$Total_per_cat, data_pred_academic)
lines(years_a,y_academic, type = 'l', col = "green")
years_g = c(tot_governement$Year, 2030)
y_governement = c(tot_governement$Total_per_cat, data_pred_governement)
lines(years_g,y_governement, type = 'l', col = "blue")
years_t = c(tot_tech$Year, 2030)
y_tech = c(tot_tech$Total_per_cat, data_pred_tech)
lines(years_t,y_tech, type = 'l', col = "purple")
years_s = c(tot_social$Year, 2030)
y_social = c(tot_social$Total_per_cat, data_pred_social)
lines(years_s,y_social, type = 'l', col = "orange")
legend("topleft",                                       # Add legend to plot
       legend = c("web", "financial", "academic", "government", "tech", "social"),
       col = c("black", "red", "green", "blue", "purple", "orange"),
       lty = 1)

# Numero de ataques por categoría

Total_Attack = data1 %>%
  group_by(Year, Organization.type) %>%
  tally()
Total_Attack  

 
data_reg_academic <- lm(n ~ Year, data = Total_Attack[Total_Attack$Organization.type == 'academic',]) 
data_pred_academic = predict(data_reg_academic, data_frame(Year=c(2030)))
data_reg_financial <- lm(n ~ Year, data = Total_Attack[Total_Attack$Organization.type == 'financial',]) 
data_pred_financial = predict(data_reg_financial, data_frame(Year=c(2030)))
data_reg_governement <- lm(n ~ Year, data = Total_Attack[Total_Attack$Organization.type == 'government',]) 
data_pred_governement = predict(data_reg_governement, data_frame(Year=c(2030)))
data_reg_tech <- lm(n ~ Year, data = Total_Attack[Total_Attack$Organization.type == 'tech',]) 
data_pred_tech = predict(data_reg_tech, data_frame(Year=c(2030)))
data_reg_web <- lm(n ~ Year, data = Total_Attack[Total_Attack$Organization.type == 'web',]) 
data_pred_web = predict(data_reg_web, data_frame(Year=c(2030)))
data_reg_social <- lm(n ~ Year, data = Total_Attack[Total_Attack$Organization.type == 'social media',]) 
data_pred_social = predict(data_reg_social, data_frame(Year=c(2030)))


# Plot regressions
years_w = c(Total_Attack[Total_Attack$Organization.type == 'web',]$Year, 2030)
y_web = c(Total_Attack[Total_Attack$Organization.type == 'web',]$n, data_pred_web)
plot(years_w, y_web, type = 'l', col = "black", main = "Predicción para año 2030 con LR (número de ataques)",
     xlab = "Years",
     ylab = "Numero de ataques")
years_f = c(Total_Attack[Total_Attack$Organization.type == 'financial',]$Year, 2030)
y_f = c(Total_Attack[Total_Attack$Organization.type == 'financial',]$n, data_pred_financial)
lines(years_f, y_f, type = "l", col = "red") 
years_a = c(Total_Attack[Total_Attack$Organization.type == 'academic',]$Year, 2030)
y_academic = c(Total_Attack[Total_Attack$Organization.type == 'academic',]$n, data_pred_academic)
lines(years_a,y_academic, type = 'l', col = "green")
years_g = c(Total_Attack[Total_Attack$Organization.type == 'governementl',]$Year, 2030)
y_governement = c(Total_Attack[Total_Attack$Organization.type == 'governement',]$n, data_pred_governement)
lines(years_g,y_governement, type = 'l', col = "blue")
years_t = c(Total_Attack[Total_Attack$Organization.type == 'tech',]$Year, 2030)
y_tech = c(Total_Attack[Total_Attack$Organization.type == 'tech',]$n, data_pred_tech)
lines(years_t,y_tech, type = 'l', col = "purple")
years_s = c(Total_Attack[Total_Attack$Organization.type == 'social media',]$Year, 2030)
y_social = c(Total_Attack[Total_Attack$Organization.type == 'social media',]$n, data_pred_social)
lines(years_s,y_social, type = 'l', col = "orange")
legend("topleft",                                       # Add legend to plot
       legend = c("web", "financial", "academic", "government", "tech", "social"),
       col = c("black", "red", "green", "blue", "purple", "orange"),
       lty = 1)
