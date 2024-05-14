library(tidyverse)
library(haven)
library(corrplot)
library(e1071)
library(gridExtra)
library(ggcorrplot)
library(gplots)
require(foreign)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stats)
library(lmtest)
library(sandwich)
library(stargazer)
library(broom)
library(knitr)
library(plm)
library(fixest)
library(car)

# Load data
guns = read_dta("C:/Users/rahul/Documents/UT Dallas/Fall 2023/Econometrics/Project/guns.dta")

# Check structure
str(guns)

# Check for missing values
sum(is.na(guns))

# Data exploration and descriptive statistics
summary(guns)

# Correlation analysis
correlation_matrix <- cor(guns[c('vio', 'rob', 'mur', 'shall', 'incarc_rate', 'density', 'avginc', 'pop', 'pm1029', 'pw1064', 'pb1064')])
print(correlation_matrix)

# Explore correlations with visualization
corrplot(correlation_matrix, 
         method = "number",   # Display correlation coefficients as numbers
         type = "upper",      # Display upper triangle
         tl.col = "black",    # Text label color
         tl.cex = 0.8,        # Text label size
         number.cex = 0.8,    # Size of the number in each cell
         col = colorRampPalette(c("red", "white", "blue"))(50),  # Blue-to-red color scale
         cl.ratio = 0.2,      # Color ratio between positive and negative correlations
         addCoef.col = "black",  # Coefficient color
         addCoef.cex = 0.7   # Coefficient size
)

# Check skewness for each variable
skewness_values <- sapply(guns, skewness)

# Display skewness values
print(skewness_values)

#Historgrams to understand skewness
histograms <- map2(guns, names(guns), ~ ggplot(guns, aes(x = .)) +
                     geom_histogram(bins = 30, fill = "blue", color = "black") +
                     ggtitle(paste("Histogram of", .y, ", Skewness =", round(skewness(.x), 2))))
# Display histograms
print(histograms)
grid.arrange(grobs = histograms)

# Calculate the total crime rate for each year and shall-issue law status
total_crime <- guns %>%
  group_by(year, shall) %>%
  summarise(total_crime = sum(vio))

# Time trends in crime rates
ggplot(total_crime, aes(x = year, y = total_crime, color = factor(shall))) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Total Violent Crime Rate (per 100,000)",
       title = "Total Violent Crime Rate Over Time by Shall-Carry Laws") +
  theme_minimal() +
  scale_color_manual(name = "Shall-Carry Law", values = c("blue", "red")) +
  facet_wrap(vars(shall))


# Comparative analysis of crime rates
ggplot(guns, aes(x = vio, fill = factor(shall))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Violent Crime Rates by Shall-Carry Law",
       x = "Violent Crime Rate",
       y = "Density",
       fill = "Shall-Carry Law") +
  theme_minimal() +
  theme(legend.position = "top")  

# Box plot
ggplot(guns, aes(x = factor(shall), y = vio, fill = factor(shall))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Box Plot of Violent Crime Rates by Shall-Carry Law",
       x = "Shall-Carry Law",
       y = "Violent Crime Rate") +
  theme_minimal() +
  theme(legend.position = "top")  

# Violent Crime Rates Across States
plotmeans(vio ~ stateid, data = guns, xlab = "State ID", ylab = "Mean Violent Crime Rate",
          main = "Heterogeneity of Violent Crime Rates Across States")

# Violent Crime Rates Across Years
plotmeans(vio ~ year, data = guns, xlab = "Year", ylab = "Mean Violent Crime Rate",
          main = "Heterogeneity of Violent Crime Rates Across Years")

# Factoring the shall column
guns$factor_shall <- as.factor(guns$shall)

# Taking natural logarithm of the following columns to correct skew
guns$log_vio <- log(guns$vio)
guns$log_incarc <- log(guns$incarc_rate)
guns$log_density <- log(guns$density)
guns$log_pb1064 <- log(guns$pb1064)
guns$log_pop <- log(guns$pop)



# Layout for histograms after handling skewness
par(mar = c(4, 4, 2, 2))  # Adjust the values as needed
par(mfrow = c(5, 2))
# Original and log-transformed histograms for the violent crime rate
hist(guns$vio,
     xlab = "Violent Crime Rate",
     main = "Original Violent Crime Rate",
     col = "yellow")
hist(log(guns$vio),
     xlab = "log(Violent Crime Rate)",
     main = "Log-Transformed Violent Crime Rate",
     col = "lightgreen")

# Original and log-transformed histograms for the incarceration rate
hist(guns$incarc_rate,
     xlab = "Incarceration Rate",
     main = "Original Incarceration Rate",
     col = "lightblue")
hist(log(guns$incarc_rate),
     xlab = "log(Incarceration Rate)",
     main = "Log-Transformed Incarceration Rate",
     col = "lightblue")

# Original and log-transformed histograms for population density
hist(guns$density,
     xlab = "Population Density",
     main = "Original Population Density",
     col = "lightcoral")
hist(log(guns$density),
     xlab = "log(Population Density)",
     main = "Log-Transformed Population Density",
     col = "lightcoral")

# Original and log-transformed histograms for percent of the population that is black
hist(guns$pb1064,
     xlab = "Percent Black (Ages 10-64)",
     main = "Original Percent Black (Ages 10-64)",
     col = "lightgreen")
hist(log(guns$pb1064),
     xlab = "log(Percent Black)",
     main = "Log-Transformed Percent Black (Ages 10-64)",
     col = "lightgreen")

# Original and log-transformed histograms for population
hist(guns$pop,
     xlab = "Population",
     main = "Original Population",
     col = "lightyellow")
hist(log(guns$pop),
     xlab = "log(Population)",
     main = "Log-Transformed Population",
     col = "lightyellow")

# Creating indicator variables for time
guns$y78<- ifelse(guns$year==78,1,0)
guns$y79<- ifelse(guns$year==79,1,0)
guns$y80<- ifelse(guns$year==80,1,0)
guns$y81<- ifelse(guns$year==81,1,0)
guns$y82<- ifelse(guns$year==82,1,0)
guns$y83<- ifelse(guns$year==83,1,0)
guns$y84<- ifelse(guns$year==84,1,0)
guns$y85<- ifelse(guns$year==85,1,0)
guns$y86<- ifelse(guns$year==86,1,0)
guns$y87<- ifelse(guns$year==87,1,0)
guns$y88<- ifelse(guns$year==88,1,0)
guns$y89<- ifelse(guns$year==89,1,0)
guns$y90<- ifelse(guns$year==90,1,0)
guns$y91<- ifelse(guns$year==91,1,0)
guns$y92<- ifelse(guns$year==92,1,0)
guns$y93<- ifelse(guns$year==93,1,0)
guns$y94<- ifelse(guns$year==94,1,0)
guns$y95<- ifelse(guns$year==95,1,0)
guns$y96<- ifelse(guns$year==96,1,0)
guns$y97<- ifelse(guns$year==97,1,0)
guns$y98<- ifelse(guns$year==98,1,0)
guns$y99<- ifelse(guns$year==99,1,0)

guns_p <- pdata.frame(guns,index=c("stateid","year"))

# POOLED OLS MODEL
ols_model<-plm(log_vio~factor_shall+log_incarc+log_density+avginc+log_pop+log_pb1064
               +pm1029, model='pooling',data = guns_p)

summary(ols_model)
kable(tidy(ols_model))


# Checking for heteroskedasticity
a<-resid(ols_model)
b<-fitted(ols_model)
# Informal check
ggplot(mapping=aes(x=b,y=a))+geom_point(color="red")+geom_abline(slope = 0,intercept = 0,color="blue",size=1)+xlab("Fitted Values of ln_vio (y-hat)")+ylab("Residuals")+ggtitle("Informal Residual Plot To Check Heteroskedasticity")

# Formal check using White Test
bptest(ols_model,~factor_shall+log_incarc+log_density+avginc+log_pop+log_pb1064+pw1064+pm1029,data = guns_p)

# Using Cluster Robust Standard Errors to correct the standard errors
ols_cluster_robust<-coeftest(ols_model, vcov=vcovHC(ols_model, cluster="group"))

kable(tidy(ols_cluster_robust))

# Entity Fixed Model
entity_fixed_model<-plm(log_vio~factor_shall+log_incarc+log_density+avginc+log_pop+log_pb1064+pm1029,data = guns_p,model = "within")

summary(entity_fixed_model)

kable(tidy(entity_fixed_model))

# Informal test for heteroskedasticity
entity_fixed_resid<- resid(entity_fixed_model)
entity_fixed_fitted<- fitted(entity_fixed_model)

ggplot(mapping=aes(x=entity_fixed_fitted,y=entity_fixed_resid))+geom_point(color="red")+geom_abline(slope = 0,intercept = 0,color="blue",size=1)+xlab("Fitted Values of ln_allcrime (y-hat)")+ylab("Residuals")+ggtitle("Informal Residual Plot To Check Heteroskedasticity")

# Time and Entity Fixed Model
time_entity_fixed_model<-plm(log_vio ~ factor_shall+log_incarc+log_density+avginc
                             +log_pop+log_pb1064+pm1029+y78+y79+y80+y81+y82+y83
                             +y84+y85+y86+y87+y88+y89+y90+y91+y92+y93+y94+y95+y96
                             +y97+y98+y99, model='within', data=guns_p)

summary(time_entity_fixed_model)
kable(tidy(time_entity_fixed_model))

# Informal test for heteroskedasticity
time_entity_fixed_resid<- resid(time_entity_fixed_model)
time_entity_fixed_fitted<- fitted(time_entity_fixed_model)

ggplot(mapping=aes(x=time_entity_fixed_fitted,y=time_entity_fixed_resid))+geom_point(color="red")+geom_abline(slope = 0,intercept = 0,color="blue",size=1)+xlab("Fitted Values of ln_allcrime (y-hat)")+ylab("Residuals")+ggtitle("Informal Residual Plot To Check Heteroskedasticity")


# Comparing all 3 models
stargazer(ols_model,entity_fixed_model,time_entity_fixed_model, column.labels=c("POOLED OLS","ENTITY FIXED","TIME AND ENTITY FIXED EFFECTS"),
          type="text",align = TRUE,title="Comparing Models", digits = 1, out="Comparing.docx")

# Comparing fixed time and entity models and entity fixed models
n<-c("y78=0","y79=0","y80=0","y81=0","y82=0","y83=0","y84=0","y85=0","y86=0","y87=0","y88=0","y89=0","y90=0","y91=0","y92=0","y93=0","y94=0","y95=0","y96=0","y97=0","y98=0","y99=0")

linearHypothesis(time_entity_fixed_model,n)
kable(linearHypothesis(time_entity_fixed_model,n))
pFtest(time_entity_fixed_model,entity_fixed_model) # Time and Entity Fixed Model is better

# Comparing fixed time and entity models and OLS Pooling model
pFtest(time_entity_fixed_model,ols_model) # Time and Entity Fixed Model is better

