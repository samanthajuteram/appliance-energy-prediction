#--------------------------------
# REQUIRED LIBRARIES
#--------------------------------

# if not specific lib not installed , uncomment the required lib before running to install

#install.packages('ggplot2')
#install.packages('patchwork')
#install.packages('ggubr')
#install.packages('dplyr')
#install.packages('lubridate')
#install.packages('caret')
#install.packages('car')
#install.packages('rsample')
#install.packages('purrr')
#install.packages('tidyr)
#install.packages('tseries')
#install.packages('tibble')
#install.packages('forecast')
#install.packages('randomForest')
#install.packages('Metrics')

library(ggplot2)
library(patchwork)
library(ggpubr)
library(corrplot)
library(dplyr)
library(lubridate)
library(caret)
library(car)
library(rsample)
library(purrr)
library(tseries)
library(tidyr)
library(tibble)
library(forecast)
library(randomForest)
library(Metrics)

#--------------------------------
# DATA INGESTION + INSPECTION
#--------------------------------

energy_df_w_time<- read.csv('energydata_complete.csv')

# High level overview
head(energy_df_w_time)
cat('Column names:',paste(colnames(energy_df_w_time)))
cat('Data dimensions:',paste(dim(energy_df_w_time)))
print('Feature information:')
str(energy_df_w_time)
cat('Missing values:',paste(sum(is.na(energy_df_w_time))))

# Transforming date data
energy_df_w_time <- energy_df_w_time %>%
  mutate(
    # Convert to datetime
    dt = ymd_hms(date),
    
    # Hour (0–23)
    hour_val = hour(dt),
    
    # Day of week (1–7)
    day_num = wday(dt),
    
    # Cyclic encoding for hour
    hour_sin = sin(2 * pi * hour_val / 24),
    hour_cos = cos(2 * pi * hour_val / 24),
    
    # Cyclic encoding for day of week
    day_sin = sin(2 * pi * day_num / 7),
    day_cos = cos(2 * pi * day_num / 7),
    
    # keep categorical versions for possible inference
    day_name = factor(wday(dt, label = TRUE, abbr = FALSE)),
    
    # Weekend indicator
    is_weekend = if_else(day_num %in% c(1, 7), "Weekend", "Weekday"),
    is_weekend = factor(is_weekend)
  )

# Dropping columns not of interest
cols_to_drop <- c("date", "rv1","rv2")
energy_df <- energy_df_w_time[, !names(energy_df_w_time) %in% cols_to_drop]
str(energy_df)


#--------------------------------
# EXPLORATORY DATA ANALYSIS
#--------------------------------

#--------------------------------
# Univariate Analysis
#--------------------------------

#Bar chart for integer vars

p1<- ggplot(energy_df,aes(x=Appliances))+geom_bar(color="#0D0533",fill="#C4C3FA") +
  labs(title="Distribution of Energy Use by Appliances(Wh)",
       x="Energy use by Appliances", y="Count") + theme_minimal()
p2<- ggplot(energy_df,aes(x=lights))+geom_bar(color="#0D0533",fill="#C4C3FA") +
  labs(title="Distribution of Energy Use by Lights (Wh)",
       x="Energy Use by Lights(Wh)", y="Count") + theme_minimal()
p3<- ggplot(energy_df,aes(x=day_name))+geom_bar(color="#0D0533",fill="#C4C3FA") +
  labs(title="Distribution of Day of Week",
       x="Day of Week", y="Count") + theme_minimal()
p4<- ggplot(energy_df,aes(x=hour_val))+geom_bar(color="#0D0533",fill="#C4C3FA") +
  labs(title="Distribution of Hour of Day",
       x="Hour of Day", y="Count") + theme_minimal()
p5 <- ggplot(energy_df,aes(x=is_weekend))+geom_bar(color="#0D0533",fill="#C4C3FA") +
  labs(title="Distribution of Is Weekend",
       x="Weekend/Weekday", y="Count") + theme_minimal()

p1+p2+p3+p4+p5


# Checking for zero inflation based on plots
mean(energy_df$lights == 0)
mean(energy_df$Appliances == 0)

energy_df %>% 
  filter(lights > 0) %>% 
  count(lights) %>% 
  arrange(desc(n))

# Creating light categories
energy_df <- energy_df %>%
  mutate(
    lights_cat = case_when(
      lights == 0 ~ "Off",
      lights <= 20 ~ "Low",
      lights > 20 ~ "High"
    ),
    lights_cat = factor(lights_cat, levels = c("Off", "Low", "High"))
  )


# Log transforming Appliances to deal w/ skewness
energy_df$lAppliances<-log(energy_df$Appliances)

# Checking new dist for lights cat and lAppliances
log_app<-ggplot(energy_df,aes(x=lAppliances))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Log Energy Use by Appliances(log Wh)",
       x="Log Energy Use by Appliances(log Wh)", y="Frequency")+theme_minimal()
lights_cat <- ggplot(energy_df,aes(x=lights_cat))+geom_bar(color="#0D0533",fill="#C4C3FA") +
  labs(title="Distribution of Light Category",
       x="Light Category", y="Count") + theme_minimal()

log_app+lights_cat

# Hist for continuous data
# Humidity data
h1<-ggplot(energy_df,aes(x=RH_1))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Humidity in \nKitchen Area(%)",
       x="Humidity(%)", y="Frequency")+theme_minimal()
h2<-ggplot(energy_df,aes(x=RH_2))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Humidity in \nLiving Room Area(%)",
       x="Humidity(%)", y="Frequency")+theme_minimal()
h3<-ggplot(energy_df,aes(x=RH_3))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Humidity in \nLaundry Room Area(%)",
       x="Humidity(%)", y="Frequency")+theme_minimal()
h4<-ggplot(energy_df,aes(x=RH_4))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Humidity in \nOffice Room (%)",
       x="Humidity(%)", y="Frequency")+theme_minimal()
h5<-ggplot(energy_df,aes(x=RH_5))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Humidity in \nBathroom (%)",
       x="Humidity(%)", y="Frequency")+theme_minimal()
h6<-ggplot(energy_df,aes(x=RH_6))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Humidity \nOutside Building-North(%)",
       x="Humidity(%)", y="Frequency")+theme_minimal()
h7<-ggplot(energy_df,aes(x=RH_7))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Humidity in \nIroning Room(%)",
       x="Humidity(%)", y="Frequency")+theme_minimal()
h8<-ggplot(energy_df,aes(x=RH_8))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Humidity in \nTeenager Room(%)",
       x="Humidity(%)", y="Frequency")+theme_minimal()
h9<-ggplot(energy_df,aes(x=RH_9))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Humidity in \nParents Room(%)",
       x="Humidity(%)", y="Frequency")+theme_minimal()


h1+h2+h3+h4+h5+h6+h7+h8+h9

# Validity check on RH_6 (Humidity should not be 0)
mean(energy_df$RH_6 == 0)
mean(energy_df$RH_6 <= 1)
mean(energy_df$RH_6 <= 5)


# Temperature data

t1<-ggplot(energy_df,aes(x=T1))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Temperature in \nKitchen Area(Celcius)",
       x="Temperature(Celsius)", y="Frequency")+theme_minimal()
t2<-ggplot(energy_df,aes(x=T2))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Temperature in \nLiving Room Area(Celcius)",
       x="Temperature(Celsius)", y="Frequency")+theme_minimal()
t3<-ggplot(energy_df,aes(x=T3))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Temperature in \nLaundry Room Area(Celcius)",
       x="Temperature(Celsius)", y="Frequency")+theme_minimal()
t4<-ggplot(energy_df,aes(x=T4))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Temperature in \nOffice Room(Celcius)",
       x="Temperature(Celsius)", y="Frequency")+theme_minimal()
t5<-ggplot(energy_df,aes(x=T5))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Temperature in \nBathroom (Celcius)",
       x="Temperature(Celsius)", y="Frequency")+theme_minimal()
t6<-ggplot(energy_df,aes(x=T6))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Temperature \nOutside Building-North(Celcius)",
       x="Temperature(Celsius)", y="Frequency")+theme_minimal()
t7<-ggplot(energy_df,aes(x=T7))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Temperature in \nIroning Room(Celcius)",
       x="Temperature(Celsius)", y="Frequency")+theme_minimal()
t8<-ggplot(energy_df,aes(x=T8))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Temperature in \nTeenager Room(Celcius)",
       x="Temperature(Celsius)", y="Frequency")+theme_minimal()
t9<-ggplot(energy_df,aes(x=T9))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Temperature in \nParents Room(Celcius)",
       x="Temperature(Celsius)", y="Frequency")+theme_minimal()

t1+t2+t3+t4+t5+t6+t7+t8+t9

# Chievers data
c1<-ggplot(energy_df,aes(x=RH_out))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Humidity Outside \n(from Chievres weather station)(%)",
       x="Humidity(%)", y="Frequency")+theme_minimal()
c2<-ggplot(energy_df,aes(x=Press_mm_hg))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Pressure Outside \n(from Chievres weather station)(mm Hg)",
       x="Pressure(mm Hg)", y="Frequency")+theme_minimal()
c3<-ggplot(energy_df,aes(x=T_out))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Temperature Outside \n(from Chievres weather station)(Celcius)",
       x="Temperature(Celsius)", y="Frequency")+theme_minimal()
c4<-ggplot(energy_df,aes(x=Windspeed))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Windspeed Outside \n(from Chievres weather station)(m/s)",
       x="Wind speed(m/s)", y="Frequency")+theme_minimal()
c5<-ggplot(energy_df,aes(x=Visibility))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Visibility Outside \n(from Chievres weather station)(km)",
       x="Visibility(km)", y="Frequency")+theme_minimal()
c6<-ggplot(energy_df,aes(x=Tdewpoint))+geom_histogram(color='#0D0533',fill='#C4C3FA') +
  labs(title="Distribution of Tdewpoint Outside \n(from Chievres weather station)(Celsius)",
       x="Tdewpoint(Celsius)", y="Frequency")+theme_minimal()

c1+c2+c3+c4+c5+c6

#Removing redundant information
cols_to_drop <- c("RH_6", "T6")
energy_df <- energy_df[, !names(energy_df) %in% cols_to_drop]

#--------------------------------
# Correlation Analysis
#--------------------------------

# Compute the correlation matrix

energy_df$is_weekend_num <- as.integer(energy_df$is_weekend == "Weekend")


numeric_df <- energy_df %>% 
  dplyr::select(where(is.numeric))
numeric_df$hour_val <- NULL
numeric_df$day_num <- NULL
cor_matrix <- cor(numeric_df, use = "complete.obs")

#Visualize correlations

corrplot(cor_matrix, 
         method = "color",           
         type = "upper", 
         order='hclust',
         addCoef.col = NULL,      
         diag=FALSE,
         number.cex = 0.7,           
         tl.col = "black",          
         tl.srt = 45,                
         tl.cex = 0.8,               
         col = colorRampPalette(c("#0D0533", "white", "#C4C3FA"))(200),  
         title = "Energy Variables Correlation Matrix",
         mar = c(0,0,2,0))

#Identify highly correlated variables

cor_df <- as.data.frame(as.table(cor_matrix))

strong_corr <- cor_df %>%
  mutate(
    Var1 = as.character(Var1),
    Var2 = as.character(Var2)
  ) %>%
  filter(Var1 != Var2) %>%          #remove same feature pairs
  filter(Var1 < Var2) %>%           #remove duplicates
  filter(!(Var1=='Appliances'&Var2=='lAppliances'))%>%
  filter(abs(Freq) >= 0.7) %>%      #considered strongly correlated if >0.7
  arrange(desc(abs(Freq)))

strong_corr

app_cor_top <- cor_df %>%
  filter (Var1 == 'Appliances') %>%
  filter (Var2 != 'lAppliances') %>%
  filter(Var1 != Var2) %>% 
  filter(Var2 != 'lights') %>% 
  top_n(5,abs(Freq))%>%
  arrange(desc(abs(Freq)))

app_cor_top

lapp_cor_top <- cor_df %>%
  filter (Var1 == 'lAppliances') %>%
  filter (Var2 != 'Appliances') %>%
  filter(Var1 != Var2) %>% 
  filter(Var2 != 'lights') %>% 
  top_n(5,abs(Freq))%>%      
  arrange(desc(abs(Freq)))

lapp_cor_top

#--------------------------------
# Bivariate analysis for strongest numerical predictors
#--------------------------------


cor1<- ggplot(energy_df, aes(x = hour_val, y = lAppliances)) +
  geom_point(color='#C4C3FA') +
  geom_smooth(method = "loess", se = FALSE, color='#0D0533') + 
  labs(title="Log Energy Use by Appliances vs. \nHour of the day",
       x="Hour of the Day", y="Log Energy Use by Appliances(log Wh)")+theme_minimal()
cor2<- ggplot(energy_df, aes(x = RH_out, y = lAppliances)) +
  geom_point(color='#C4C3FA') +
  geom_smooth(method = "loess", se = FALSE, color='#0D0533') + 
  labs(title="Log Energy Use by Appliances vs. \nHumidity Outside",
       x="Humidity Outside (%)", y="Log Energy Use by Appliances(log Wh)")+theme_minimal()
cor3<- ggplot(energy_df, aes(x = T2, y = lAppliances)) +
  geom_point(color='#C4C3FA') +
  geom_smooth(method = "loess", se = FALSE, color='#0D0533') + 
  labs(title="Log Energy Use by Appliances vs. \nTemperature in Living Room Area",
       x="Temperature in Living Room (Celsius)", y="Log Energy Use by Appliances(log Wh)")+theme_minimal()
cor4<- ggplot(energy_df, aes(x = T_out, y = lAppliances)) +
  geom_point(color='#C4C3FA') +
  geom_smooth(method = "loess", se = FALSE, color='#0D0533') + 
  labs(title="Log Energy Use by Appliances vs. \nTemperature Outside",
       x="Temperature Outside Building-North (Celsuis)", y="Log Energy Use by Appliances(log Wh)")+theme_minimal()

cor1+cor2+cor3+cor4

#--------------------------------
# Hierarchical clustering of sensor variables
#--------------------------------

sensor_vars <- energy_df %>%
  dplyr::select(starts_with("T"), starts_with("RH")) %>%
  scale()

cor_mat <- cor(sensor_vars)
dist_mat <- as.dist(1 - abs(cor_mat))
hc <- hclust(dist_mat, method = "complete")

plot(hc, main = "Sensor Correlation Clusters",
     sub = "Method: Complete Linkage",
     xlab = "Distance Metric: 1 - |Correlation|")

#--------------------------------
# PCA to explore potential dim reduction #NOTE : NOT INCLUDED IN FINAL REPORT
#--------------------------------

# pca <- prcomp(sensor_vars, center = TRUE, scale. = TRUE)
# 
# summary(pca)
# 
# # Scree plot
# explained_var <- pca$sdev^2 / sum(pca$sdev^2)
# 
# qplot(y = explained_var, x = seq_along(explained_var),
#       geom = "line") +
#   labs(title = "Scree Plot (PCA on Sensor data) ",
#        x = "Principal Component",
#        y = "Proportion of Variance Explained") +
#   theme_minimal()
# 
# # PC1 vs PC2
# pc_df <- as.data.frame(pca$x)
# 
# ggplot(pc_df, aes(PC1, PC2)) +
#   geom_point(alpha = 0.3) +
#   labs(title = "PCA Projection of Sensor Variables") +
#   theme_minimal()

#--------------------------------
# Correlation w/ categorical vars
#--------------------------------

#Checking assumptions for ANOVA
# Normality 
qqnorm(residuals(aov(lAppliances ~ lights_cat, data = energy_df)), main ='Nomral Q-Q plot: Lights_cat')
qqline(residuals(aov(lAppliances ~ lights_cat, data = energy_df)))

qqnorm(residuals(aov(lAppliances ~ day_name, data = energy_df)), main ='Nomral Q-Q plot: Day_name')
qqline(residuals(aov(lAppliances ~ day_name, data = energy_df)))

# Equal variances
car::leveneTest(lAppliances ~ lights_cat, data = energy_df)
car::leveneTest(lAppliances ~ day_name, data = energy_df)


#Homogeneity violated --> Welch's ANOVA

oneway.test(lAppliances ~ lights_cat, data = energy_df)
oneway.test(lAppliances ~ day_name, data = energy_df)

aggregate(lAppliances ~ lights_cat, data = energy_df, mean)
aggregate(lAppliances ~ day_name, data = energy_df, mean)
boxplot(lAppliances ~ lights_cat, data = energy_df)
boxplot(lAppliances ~ day_name, data = energy_df)


ggplot(energy_df, aes(x = lights_cat, y = lAppliances)) +
  geom_boxplot(box.color = '#0D0533',
               outlier.color = '#0D0533',
               whisker.color = '#0D0533',
               median.color = '#0D0533',
               fill=c('#C4C3FA','#0000FF','#088F8F'))+
  labs(title="Log Energy Use by Appliances vs. Light Usage Energy Category",
       x="Light Usage Energy Category", y="Log Energy Use by Appliances(log Wh)")+theme_minimal()

#--------------------------------
# Time series analysis 
#--------------------------------

#Average energy use over time period
daily<-energy_df%>%
  mutate(dt_only = as.Date(dt))%>%
  group_by(dt_only)%>%
  summarise(daily_avg=mean(Appliances))%>%
  ggplot(aes(x=dt_only,y=daily_avg))+
  geom_line(color='#0000FF',linewidth=1)+
  labs(title='Daily Average Energy Use by Appliances',
         x='Date',
          y='Average Energy Use by Appliances(Wh)')+theme_minimal()

monthly<-energy_df%>%
  mutate(month=floor_date(dt,'month'))%>%
  group_by(month)%>%
  summarise(monthly_avg=mean(Appliances))%>%
  ggplot(aes(x=month,y=monthly_avg))+
  geom_line(color='#0000FF',linewidth = 1)+
  labs(title='Daily Average Energy Use by Appliances',
         x='Month',
       y='Average Energy Use by Appliances(Wh)')+theme_minimal()

daily/monthly

#ACF to look at correlation between observations and their lags
acf(energy_df$lAppliances, lag.max = 60, main = "Autocorrelation Function for lAppliances")


#PACF to determine which lags matter
energy_df <- energy_df %>% arrange(dt)
pacf(energy_df$lAppliances,
     lag.max = 60,
     main = "Partial Autocorrelation Function for lAppliances")

#--------------------------------
# Outlier analysis 
#--------------------------------

#Identify outliers using IQR
Q1<- quantile(energy_df$Appliances,0.25)
Q3<- quantile(energy_df$Appliances,0.75)
IQR <- Q3-Q1

upper_bound<- Q3+1.5*IQR
lower_bound<- Q1-1.5*IQR

outliers<- energy_df%>%
  filter(energy_df$Appliances<lower_bound | energy_df$Appliances>upper_bound)

cat("Outlier range",paste(min(outliers$Appliances),'-',paste(max(outliers$Appliances))))
cat("Normal range",min(energy_df$Appliances[energy_df$Appliances <= upper_bound]), "-", 
    round(upper_bound, 1))

nrow(outliers)
cat('Perecentage outliers:',paste(round(nrow(outliers)/nrow(energy_df)*100,2)))
summary(outliers)

outliers %>%
  count(hour_val) %>%
  arrange(desc(n))

outliers %>%
  mutate(date_only = as.Date(dt)) %>%
  count(date_only) %>%
  arrange(desc(n))

comparison <- energy_df %>%
  mutate(is_outlier = Appliances > upper_bound) %>%
  group_by(is_outlier) %>%
  summarise(
    count = n(),
    mean_appliances = mean(Appliances),
    median_hour = mean(hour_val),
    lights_high = mean(lights_cat == 3) * 100,
    lights_low  = mean(lights_cat == 2) * 100,
    lights_off  = mean(lights_cat == 1) * 100,
    mean_Vis = mean(Visibility),
    mean_Tdewpoint = mean(Tdewpoint),
    mean_Windspeed = mean(Windspeed),
    mean_weekend = mean(is_weekend_num)*100,
    most_common_day = names(sort(table(day_num), decreasing = TRUE))[1],
    mean_T1 = mean(T1),
    mean_T2 = mean(T2),
    mean_T3 = mean(T3),
    mean_T4 = mean(T4),
    mean_T5 = mean(T5),
    mean_T7 = mean(T7),
    mean_T8 = mean(T8),
    mean_T9 = mean(T9),
    mean_T_out = mean(T_out),
    mean_RH_1 = mean(RH_1),
    mean_RH_2 = mean(RH_2),
    mean_RH_3 = mean(RH_3),
    mean_RH_4 = mean(RH_4),
    mean_RH_5 = mean(RH_5),
    mean_RH_7 = mean(RH_7),
    mean_RH_8 = mean(RH_8),
    mean_RH_9 = mean(RH_9),
    mean_RH_out = mean(RH_out)
  )

print(comparison,width=Inf)

#Visualizing outliers over time
energy_df %>%
  mutate(
    date_only = as.Date(dt),
    is_outlier = Appliances > upper_bound
  ) %>%
  ggplot(aes(x = date_only, y = Appliances, color = is_outlier)) +
  geom_point(alpha = 0.3) +
  scale_color_manual(values = c("FALSE" = "#C4C3FA", "TRUE" = "#0D0533")) +
  labs(title = "Outliers Over Time",
       x = "Date", y = "Appliances (Wh)",
       color = "Is Outlier") +
  theme_minimal()

#--------------------------------
# MODELING
#--------------------------------

#--------------------------------
#  Data Preparation
#--------------------------------
# Ensure observations are ordered by time before any time-series modeling.

energy_df <- energy_df %>% arrange(dt)

#--------------------------------
# Stationarity Assessment
#--------------------------------
# Check whether differencing is required before fitting AR/ARMA models.

adf.test(energy_df$lAppliances)

# The ADF test strongly rejected the null hypothesis--> series is stationary -->Differencing is not required

#--------------------------------
# Feature Engineering: Lagged Variables
#--------------------------------
# Create lag1–lag10 for autoregressive modeling.

for(i in 1:10){
  energy_df[[paste0("lag",i)]] <- dplyr::lag(energy_df$lAppliances, i)
}

# Remove rows with NA values created by lagging
energy_df <- energy_df %>% drop_na(starts_with("lag"))

#--------------------------------
# Rolling-Origin Cross Validation
#--------------------------------
# Time-series models cannot use random CV due to temporal ordering (would cause data leakage)
# Rolling-origin CV preserves structure.

set.seed(123)

n <- nrow(energy_df)
folds <- 11

assess  <- floor(n/(folds+1)) #size of each window
initial <- n - folds*assess   #initial window size
skip    <- assess  #how much to move the window forward

cv_splits <- rolling_origin(
  energy_df,
  initial = initial,
  assess  = assess,
  skip    = skip,
  cumulative = TRUE #true so that training set grows w/ wach fold
)

length(cv_splits$splits) #verifying number of folds 

#--------------------------------
# RMSE function
#--------------------------------
# RMSE will serve as key comparsion tool for predictive accuracy

rmse <- function(y, yhat){
  sqrt(mean((y-yhat)^2, na.rm=TRUE))
}

#--------------------------------
# Baseline Model: Autoregression
#--------------------------------
# Evaluate AR(p) models for p = 1…10 using rolling CV. (10 chosen based on PACF plot)

cv_rmse_ar_p <- function(p, splits){
  
  # dynamically add p lag terms to test for all AR(p) models 
  lag_terms <- paste0("lag",1:p,collapse=" + ")
  fml <- as.formula(paste("lAppliances ~",lag_terms))
  
  map_dbl(splits$splits,function(s){
    
    train <- analysis(s)
    test  <- assessment(s)
    
    mod <- lm(fml,data=train)
    pred <- predict(mod,newdata=test)
    
    rmse(test$lAppliances,pred)
    
  })
}

ar_results <- map_dfr(1:10,function(p){
  
  rmse_vals <- cv_rmse_ar_p(p,cv_splits)
  
  tibble(
    p=p,
    mean_rmse=mean(rmse_vals),
    sd_rmse=sd(rmse_vals)
  )
  
})

ar_results

# Plot RMSE vs AR order

ggplot(ar_results,aes(p,mean_rmse))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(
    ymin=mean_rmse-sd_rmse,
    ymax=mean_rmse+sd_rmse
  ),width=.2)+
  labs(
    title="Rolling-Origin CV RMSE for AR(p)",
    x="AR Order",
    y="Mean RMSE (±1 SD)"
  )+
  theme_minimal()

#--------------------------------
# Selected Baseline: AR(1)
#--------------------------------

fml_ar1 <- lAppliances ~ lag1

rmse_ar1 <- map_dbl(cv_splits$splits,function(s){
  
  train <- analysis(s)
  test  <- assessment(s)
  
  mod <- lm(fml_ar1,data=train)
  pred <- predict(mod,newdata=test)
  
  rmse(test$lAppliances,pred)
  
})

mean(rmse_ar1); sd(rmse_ar1)

# Diagnostics on full sample

fit_ar1 <- lm(fml_ar1,data=energy_df)

acf(residuals(fit_ar1),lag.max=50)
Box.test(residuals(fit_ar1),lag=20,type="Ljung-Box")

#--------------------------------
# Time Series Regression (TSRF)
#--------------------------------
# Incorporate environmental predictors identified during EDA.

fml_arx_sat <- lAppliances ~ lag1 + hour_sin + hour_cos + RH_out + T2 + T_out + lights_cat

rmse_arx_sat <- map_dbl(cv_splits$splits,function(s){
  
  train <- analysis(s)
  test  <- assessment(s)
  
  mod <- lm(fml_arx_sat,data=train)
  pred <- predict(mod,newdata=test)
  
  rmse(test$lAppliances,pred)
  
})

mean(rmse_arx_sat); sd(rmse_arx_sat)

# Fit full model on entire dataset

fit_arx_sat <- lm(fml_arx_sat,data=energy_df)

summary(fit_arx_sat)
vif(fit_arx_sat)

#--------------------------------
# AIC Model Selection
#--------------------------------

model_arx_aic <- step(fit_arx_sat,direction="both",trace=0)

fml_arx_aic <- formula(model_arx_aic)

rmse_arx_aic <- map_dbl(cv_splits$splits,function(s){
  
  train <- analysis(s)
  test  <- assessment(s)
  
  mod <- lm(fml_arx_aic,data=train)
  pred <- predict(mod,newdata=test)
  
  rmse(test$lAppliances,pred)
  
})

mean(rmse_arx_aic); sd(rmse_arx_aic)

# Fit full model on entire dataset

fit_arx_red <-lm(fml_arx_aic,data=energy_df)

summary(fit_arx_red)
vif(fit_arx_red)

#--------------------------------
# ARMA Models
#--------------------------------
# Because ARX residuals showed remaining autocorrelation, ARMA error structures were explored.

energy_df_arma <- energy_df %>%
  dplyr::select(-starts_with("lag"))  #removing lag terms

# One-step-ahead ARMAX CV

cv_rmse_armax_1step <- function(order_vec, splits_obj, fold_ids = NULL) {
  
  splits_to_use <- splits_obj$splits
  
  #optionally restrict to specific folds -- implemented because 10-fold training takes hours 
  if (!is.null(fold_ids)) {
    splits_to_use <- splits_to_use[fold_ids]
  }
  
  map_dbl(splits_to_use, function(s) {
    
    train <- analysis(s)
    test  <- assessment(s)
    
    preds <- numeric(nrow(test))
    history <- train
    
    #walk through the test set one observation at a time 
    for (i in seq_len(nrow(test))) {
      
      #predictors for training 
      xreg_hist <- model.matrix( #arma requires numeric matrix
        ~ hour_sin + hour_cos + RH_out + T2 + lights_cat,
        data = history
      )[, -1, drop = FALSE] #remove intercept column
      
      fit <- arima(
        history$lAppliances,
        order = order_vec,
        xreg = xreg_hist
      )
      
      #predictors for next tsep
      newxreg <- model.matrix(
        ~ hour_sin + hour_cos + RH_out + T2 + lights_cat,
        data = test[i, , drop = FALSE]
      )[, -1, drop = FALSE]
      
      #one-step forecast
      preds[i] <- predict(fit, n.ahead = 1, newxreg = newxreg)$pred
      
      #update history
      history <- dplyr::bind_rows(history, test[i, , drop = FALSE])
    }
    
    rmse(test$lAppliances, preds)
  })
}

###NOTE: Running the following will take a couple hours
rmse_armax_101_1step <- cv_rmse_armax_1step(
  order_vec = c(1, 0, 1),
  splits_obj = cv_splits,
  fold_ids = 1:10
)

rmse_armax_201_1step <- cv_rmse_armax_1step(
  order_vec = c(2, 0, 1),
  splits_obj = cv_splits,
  fold_ids = 1:10
)
rmse_armax_102_1step <- cv_rmse_armax_1step(
  order_vec = c(1, 0, 2),
  splits_obj = cv_splits,
  fold_ids = 1:10
)

# Summarize ARMA CV performance
arma_results <- tibble(
  model = c("ARMAX(1,1)", "ARMAX(2,1)", "ARMAX(1,2)"),
  mean_rmse = c(
    mean(rmse_armax_101_1step),
    mean(rmse_armax_201_1step),
    mean(rmse_armax_102_1step)
  ),
  sd_rmse = c(
    sd(rmse_armax_101_1step),
    sd(rmse_armax_201_1step),
    sd(rmse_armax_102_1step)
  )
)

arma_results

# Fit ARMA models on full dataset for diagnostics
xreg_mat <- model.matrix(
  ~ hour_sin + hour_cos + RH_out + T2 + lights_cat,
  data = energy_df_arma
)[,-1]

fit_arma11 <- arima(energy_df_arma$lAppliances,order=c(1,0,1),xreg=xreg_mat)
fit_arma21 <- arima(energy_df_arma$lAppliances,order=c(2,0,1),xreg=xreg_mat)
fit_arma12 <- arima(energy_df_arma$lAppliances,order=c(1,0,2),xreg=xreg_mat)

summary(fit_arma11)
summary(fit_arma21)
summary(fit_arma12)

# Residual diagnostics
par(mfrow = c(2, 2))
acf(residuals(fit_arma11),lag.max=50,main = "Autocorrelation Function for lAppliances",sub='ARMA(1,1)')
acf(residuals(fit_arma21),lag.max=50,main = "Autocorrelation Function for lAppliances",sub='ARMA(1,2)')
acf(residuals(fit_arma21),lag.max=50,main = "Autocorrelation Function for lAppliances",sub='ARMA(2,1)')
Box.test(residuals(fit_arma11),lag=20,type="Ljung-Box")
Box.test(residuals(fit_arma12),lag=20,type="Ljung-Box")
Box.test(residuals(fit_arma21),lag=20,type="Ljung-Box")

#--------------------------------
# TSRF
#--------------------------------
# Evaluate nonlinear relationships (non-lineraity suggested in EDA) using a time-series random forest.

# Find optimal hyperparams

grid <- expand.grid(
  mtry=c(2,3,4),
  nodesize=c(1,10)
)

ntree_tune <- 100
set.seed(123)

grid$cv_rmse <- pmap_dbl(grid,function(mtry,nodesize){
  
  mean(map_dbl(cv_splits$splits,function(s){
    
    train <- analysis(s)
    test  <- assessment(s)
    
    mod <- randomForest(
      fml_arx_aic,
      data=train,
      ntree=ntree_tune,
      mtry=mtry,
      nodesize=nodesize
    )
    
    pred <- predict(mod,newdata=test)
    
    rmse(test$lAppliances,pred)
    
  }))
  
})

grid[order(grid$cv_rmse),]

# Final RF model

rmse_arx_rf <- map_dbl(cv_splits$splits,function(s){
  
  train <- analysis(s)
  test  <- assessment(s)
  
  mod <- randomForest(
    fml_arx_aic,
    data=train,
    ntree=500,
    mtry=3,
    nodesize=10
  )
  
  pred <- predict(mod,newdata=test)
  
  rmse(test$lAppliances,pred)
  
})

mean(rmse_arx_rf); sd(rmse_arx_rf)

# Fit on full dataset for diagnostics
fit_rf <-randomForest(
  fml_arx_aic,
  data=energy_df,
  ntree=500,
  mtry=3,
  nodesize=10
)

summary(fit_rf)

#--------------------------------
# Naive Benchmark
#--------------------------------
# ̂y_t = y_(t−1)

rmse_naive <- map_dbl(cv_splits$splits,function(s){
  
  train <- analysis(s)
  test  <- assessment(s)
  
  full_y <- c(tail(train$lAppliances,1),test$lAppliances)
  pred <- full_y[-length(full_y)]
  
  rmse(test$lAppliances,pred)
  
})

mean_naive_rmse <- mean(rmse_naive)
sd_naive_rmse   <- sd(rmse_naive)

# 'Fit' on entire dataset
energy_df$naive_pred <- dplyr::lag(energy_df$lAppliances, 1)
energy_df$naive_resid <- energy_df$lAppliances - energy_df$naive_pred

#--------------------------------
# Model Comparison
#--------------------------------

results <- tibble(
  model=c("Naive","AR(1)","ARX Saturated","ARX AIC","TSRF"),
  mean_rmse=c(
    mean_naive_rmse,
    mean(rmse_ar1),
    mean(rmse_arx_sat),
    mean(rmse_arx_aic),
    mean(rmse_arx_rf)
  ),
  sd_rmse=c(
    sd_naive_rmse,
    sd(rmse_ar1),
    sd(rmse_arx_sat),
    sd(rmse_arx_aic),
    sd(rmse_arx_rf)
  )
)

results

# aggregating ACF plots for comparability
par(mfrow = c(2, 4))
acf(na.omit(energy_df$naive_resid), main = "ACF of Naive Model Residuals")
acf(residuals(fit_ar1),lag.max=50,main = "ACF of AR(1) Residuals")
acf(residuals(fit_arx_sat),lag.max=50,main = "ACF of ARX (saturated) Residuals")
acf(residuals(fit_arx_red),lag.max=50,main = "ACF of ARX (reduced) Residuals")
acf(residuals(fit_arma11),lag.max=50,main = "ACF of ARMA(1,1) Residuals")
acf(residuals(fit_arma21),lag.max=50,main = "ACF of ARMA(1,2) Residuals")
acf(residuals(fit_arma21),lag.max=50,main = "ACF of ARMA(2,1) Residuals")
acf(residuals(fit_rf),lag.max=50,main = "ACF of TSRF Residuals")


#--------------------------------
# Final Model Checks/ Eval (ARX-AIC reduced)
#--------------------------------

fit_arx_final <-lm(fml_arx_aic,data=energy_df)
energy_df$fitted_arx <- fitted(fit_arx_final)
energy_df$residuals_arx <- residuals(fit_arx_final)

#small subset so lines are visible
ggplot(energy_df[8000:8200, ], aes(x = dt)) +
  geom_line(aes(y = lAppliances, color = "Actual")) +
  geom_line(aes(y = fitted_arx, color = "Model Fit")) +
  labs(
    title = "ARX (reduced) Model Fit",
    x = "Date",
    y = "Log Appliance Energy Consumption (log Wh)",
    color = ""
  ) +
  scale_color_manual(values = c("Actual" = "red", "Model Fit" = "blue")) +
  theme_minimal()

#residuals vs fitted 
ggplot(energy_df, aes(x = fitted_arx,y=residuals_arx)) +
  geom_point(color='#C4C3FA') +
  geom_smooth(method='loess',se=FALSE, color='#0D0533') +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(
    title = "Residuals vs Fitted (ARX Reduced Model)",
    x = "Fitted Values",
    y = "Residuals",
  )+ theme_minimal()
