# ---
# title: "Analisis de data breaches en los ultimos años. Preprocesamiento y Exploración"
# 
# author: "Dubovik Aleksandra"
# ---
#   
# Este código consiste en dos partes:
#   1. En primero parte preproceso los datos 
#   2. En segundo parte trabajo con exploración de datos


# Cargar la librería tidyverse
library('tidyverse')

# Leer el dataset 
setwd('/Users/alexandradubovik/FID_Proyecto')
data <- read.csv("df.csv", header = TRUE)
#head(data)

# What columns/rows are in dataset?
nrow(data) # 352 rows, entries
ncol(data) # 7 fcolumns
colnames(data) #"Entity"            "Year"              "Records"           "Organization.type" "Method"  

---------------------------------------------------------------------------------------------------------
# Preprocess data, correct columns Year, Records, Organization.type and Method which are characters and should be numeric type
# Correct categories, e.g "telecom", "telecoms" and "telecommunications" should be the same category
# For methods "hacked" and "hackibg" should be the same
  

# Start we correcting the Records column
rec_nonum <- which(is.na(as.numeric(data$Records)))    # Get indices of non-numeric values of Records
rec_nonum
# There are length(rec_nonum) = 48 elements to cast manually
# Word values are casted to numerical, unknown numbers are replaced with median value
# Median value is better because we have many outliers in our dataset (the bigger the company is, the more records can be stolen)


# 1. Compute median
data_1$Records <- as.numeric(data$Records) # cast all provided values to numeric, NA's are ignored
x_rec <- na.omit(data_1$Records)           # omit NA's to compute mean
median_rec <- median(x_rec)                # median is 1.800.000 vs mean  43.797.809
median_rec
# 2. With if record[i] from rec_nonum is == "unknown": replace with median
# 3.      if record[i] smth else: print i and print record[i]

for(i in rec_nonum){
  if(is.na(data$Records[i])){
    data$Records[i] <- median_rec
  } else{
    print(i)
    print(data$Records[i])
  }
}
# 4. Replace manually all words written values using output from the for loop
#    Where numbers can not be understood - replace with median

#all calculations are very approximate, every record weight varies from one organisation to other
data$Records[28] = median_rec
data$Records[29] = median_rec*19 # 19 years of stolen data
data$Records[35] = median_rec*63 # 63 shops with different data records
data$Records[41] = median_rec 
data$Records[48] = 59500 
data$Records[49] = 5000000
data$Records[67] = median_rec
data$Records[77] = 5000000
data$Records[83] = 3500 # each email is 10 MB, let assume we need 1 record per email
data$Records[95] = 9002208
data$Records[120] = median_rec
data$Records[143] = 2500000
data$Records[148] = 2500 # Let's assume each loctaion record has several types of data included
data$Records[169] = 5000 # such as coordinates, city, region, country etc.
data$Records[176] = 100
data$Records[200] = median_rec*93
data$Records[222] = median_rec
data$Records[249] = median_rec
data$Records[252] = 10000000 # 100 terabytes, suppose each record is of gigabyte
data$Records[261] = 540
data$Records[266] = median_rec*200
data$Records[287] = 80
data$Records[311] = 510
data$Records[316] = median_rec
data$Records[338] = median_rec
data$Records[341] = median_rec

data$Records <- as.numeric(data$Records)
data$Records

#Now lets fix Year column
unique(data$Year)
# There three non-standard values "2018-2019", "2019-2020" and  "2014 and 2015" 
# Solution: Lets duplicate attack rows with different years

# 1. Get the indexes of original rows
index_1 = data[data$Year=='2019-2020',]$X
index_2 = data[data$Year=='2018-2019',]$X
index_3 = data[data$Year=='2014 and 2015',]$X

# 2. Replace "Year" column value with single value in each row
#    Plus one because X starts at 0
data[index_1+1,]$Year <- "2019"
data[index_2+1,]$Year <- "2018"
data[index_3+1,]$Year <- "2014"

# 3. Duplicate the original row with the year left in "Year" column
row_dupe_1 = data[index_1+1,]
row_dupe_1$Year = "2020"
row_dupe_2 = data[index_2+1,]
row_dupe_2$Year = "2019"
row_dupe_3 = data[index_3+1,]
row_dupe_3$Year = "2015"
data_1 = rbind(data, row_dupe_1)
data_2 = rbind(data_1, row_dupe_2)
data_3 = rbind(data_2, row_dupe_3)
nrow(data_3) # Shows 355 records, we added 3 records successfully

data = data_3
# Check the year column, cast it to the numerical data type
data$Year <- as.numeric(data$Year)


# Work with categories
unique(data$Organization.type) # 70 unique values
table(data$Organization.type)  # How many items in each category, manually go through items

# Correct the category values manually
data[data$Organization.type=='educational services',]$Organization.type <- "academic"
data[data$Organization.type=='banking',]$Organization.type <- "financial"
data[data$Organization.type=='financial service company',]$Organization.type <- "financial"
data[data$Organization.type=='financial, credit reporting',]$Organization.type <- "financial"
data[data$Organization.type=='market analysis',]$Organization.type <- "financial"
data[data$Organization.type=='consulting, accounting',]$Organization.type <- "financial"
data[data$Organization.type=='game',]$Organization.type <- "gaming"
data[data$Organization.type=='government, database',]$Organization.type <- "government"
data[data$Organization.type=='government, healthcare',]$Organization.type <- "government"
data[data$Organization.type=='government, military',]$Organization.type <- "government"
data[data$Organization.type=='personal and demographic data about residents and their properties of US',]$Organization.type <- "government"
data[data$Organization.type=='political',]$Organization.type <- "government"
data[data$Organization.type=='health',]$Organization.type <- "healthcare"
data[data$Organization.type=='Clinical Laboratory',]$Organization.type <- "healthcare"
data[data$Organization.type=='military, healthcare',]$Organization.type <- "military"
data[data$Organization.type=='social network',]$Organization.type <- "social media"
data[data$Organization.type=='social networking',]$Organization.type <- "social media"
data[data$Organization.type=='tech, retail',]$Organization.type <- "tech"
data[data$Organization.type=='tech, web',]$Organization.type <- "tech"
data[data$Organization.type=='telecommunications',]$Organization.type <- "telecom"
data[data$Organization.type=='telecoms',]$Organization.type <- "telecom"
data[data$Organization.type=='Telephone directory',]$Organization.type <- "telecom"
data[data$Organization.type=='ticket distribution',]$Organization.type <- "transport"
data[data$Organization.type=='ticket distribution',]$Organization.type <- "transport"
data[data$Organization.type=='data broker',]$Organization.type <- "various"
data[data$Organization.type=='hosting provider',]$Organization.type <- "tech"
data[data$Organization.type=='local search',]$Organization.type <- "tech"
data[data$Organization.type=='Network Monitoring',]$Organization.type <- "tech"
data[data$Organization.type=='QR code payment',]$Organization.type <- "tech"
data[data$Organization.type=='web, military',]$Organization.type <- "military"
data[data$Organization.type=='advertising',]$Organization.type <- "various"
data[data$Organization.type=='dating',]$Organization.type <- "social media"
data[data$Organization.type=='media',]$Organization.type <- "social media"
data[data$Organization.type=='online marketing',]$Organization.type <- "social media"
data[data$Organization.type=='Question & Answer',]$Organization.type <- "various"
data[data$Organization.type=='software',]$Organization.type <- "tech"
data[data$Organization.type=='fashion',]$Organization.type <- "social media"
data[data$Organization.type=='information technology',]$Organization.type <- "tech"
data[data$Organization.type=='arts group',]$Organization.type <- "social media"
data[data$Organization.type=='messaging app',]$Organization.type <- "social media"
data[data$Organization.type=='publisher (magazine)',]$Organization.type <- "social media"
data[data$Organization.type=='special public corporation',]$Organization.type <- "social media"
data[data$Organization.type=='background check',]$Organization.type <- "various"
data[data$Organization.type=='gambling',]$Organization.type <- "various"
data[data$Organization.type=='restaurant',]$Organization.type <- "various"
data[data$Organization.type=='web service',]$Organization.type <- "web"
data[data$Organization.type=='web, gaming',]$Organization.type <- "web"
data[data$Organization.type=='online shopping',]$Organization.type <- "web"
data[data$Organization.type=='shopping',]$Organization.type <- "web"
data[data$Organization.type=='web, tech',]$Organization.type <- "web"
data[data$Organization.type=='gaming',]$Organization.type <- "web"
data[data$Organization.type=='mobile carrier',]$Organization.type <- "tech"
data[data$Organization.type=='phone accessories',]$Organization.type <- "various"
data[data$Organization.type=='humanitarian',]$Organization.type <- "various"
data[data$Organization.type=='Information Security',]$Organization.type <- "tech"
data[data$Organization.type=='retail',]$Organization.type <- "financial"
data[data$Organization.type=='energy',]$Organization.type <- "financial"
data[data$Organization.type=='genealogy',]$Organization.type <- "various"
data[data$Organization.type=='hotel',]$Organization.type <- "financial"
data[data$Organization.type=='Consumer Goods',]$Organization.type <- "financial"
data[data$Organization.type=='transport',]$Organization.type <- "government"
data[data$Organization.type=='telecom',]$Organization.type <- "tech"
table(data$Organization.type) # Broke down 70 categories into 9 final
unique(data$Organization.type)

# Lastly we correct Method

length(unique(data$Method)) # 26 categories before preprocessing

data[data$Method=='data exposed by misconfiguration',]$Method <- "misconfiguration"
data[data$Method=='hacked/misconfiguration',]$Method <- "misconfiguration"
data[data$Method=='inside job, hacked',]$Method <- "inside job"
data[data$Method=='poor security/inside job',]$Method <- "poor security"
data[data$Method=='poor security / hacked',]$Method <- "poor security"
data[data$Method=='unsecured S3 bucket',]$Method <- "poor security"
data[data$Method=='unprotected api',]$Method <- "poor security"
data[data$Method=='ransomware hacked',]$Method <- "social engineering"
data[data$Method=='accidentally uploaded',]$Method <- "accidentally exposed"
data[data$Method=='accidentally published',]$Method <- "accidentally exposed"
data[data$Method=='improper setting, hacked',]$Method <- "misconfiguration"
data[data$Method=='lost / stolen computer',]$Method <- "lost / stolen media"
data[data$Method=='Poor security',]$Method <- "poor security"
data[data$Method=='rogue contractor',]$Method <- "social engineering"
data[data$Method=='publicly accessible Amazon Web Services (AWS) server',]$Method <- "misconfiguration"
data[data$Method=='intentionally lost',]$Method <- "lost / stolen media"
data[data$Method=='misconfiguration/poor security',]$Method <- "misconfiguration"
data[data$Method=='zero-day vulnerabilities',]$Method <- "hacked"
data[data$Method=='',]$Method <- "hacked"

table(data$Method)
# Preprocessing done, save new dataset in the same folder
#write.csv(data, "/Users/alexandradubovik/FID_Proyecto/data_processed.csv", row.names=TRUE)
---------------------------------------------------------------------------------------------------------

# Here I explore some basics questions for thi dataset - Exploration  
  
# The biggest number of records stolen? 
biggest_loss = max(data$Records)
options(scipen = 999) # disabel scientific notion
print(paste("The biggest number of stolen data records is ", biggest_loss))
#"The biggest number of stolen data records is  885000000"



# Period (in years) of security breach?
first_y <- min(data$Year)
last_y <- max(data$Year)
period_of_records = as.integer(last_y) - as.integer(first_y)
print(paste("We observe records of data breaches of past ", period_of_records, " years."))
#"We observe records of data breaches of past  18  years."
print(paste("Starting  from ", first_y, " and ending in ", last_y))
#"Starting  from  2004  and ending in  2022"


# Most and least attacked category? 
max_attacked_category  = max(table(data$Organization.type)) # 84 
min_attacked_category  = min(table(data$Organization.type)) # 9

print(paste("The most attacked category is financial with", max_attacked_category, "attacks registered."))
print(paste("The least attacked category is military with", min_attacked_category, "attacks registered."))

# Most and least method used?
max_used_method = max(table(data$Method))
min_used_method = min(table(data$Method))

print(paste("The most used method for stealing data is hacking with ", max_used_method, "attacks registered" ))
print(paste("The least used method for stealing data is social engineering with", min_used_method, "attacks registered"))

# Lastly for Entity lets generate a list of companies
# Most of the companies are mentioned 1 
table(data$Entity)
