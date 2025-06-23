
###########################################################################
###########################################################################
###                                                                     ###
###                    HEART FAILURE CLINICAL                           ###
###                   PROJECT LINEAR REGRESSION                         ###
###                                                                     ###
###########################################################################
###########################################################################
###*                        Loading Packages
###*                        ----------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(Metrics))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(bannerCommenter))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(WVPlots))
suppressPackageStartupMessages(library(PerformanceAnalytics))
suppressPackageStartupMessages(library(car))

##===============================================================
##                     Reading in the data                     ==
##===============================================================

download.file("https://raw.githubusercontent.com/ABUALHUSSEIN/test/main/data/movie.csv",destfile = "movie.csv")
##source(destfile)
##ls()
#======================================
## ** We replaced the missing values with the Mean**
setwd("C:/Users/WAFA/Desktop")
movie <- read.csv("movie.csv",header=TRUE)
View(movie)
is.na(movie)
sum(is.na(movie))

#  Gives the name of columns that do not have data ------------------------
list_na <- colnames(movie)[ apply(movie, 2, anyNA) ]
list_na

#Now we need to compute of the mean with the argument na.rm = TRUE. 
#This argument is compulsory because the columns have missing data, 
# and this tells R to ignore them.


# Create mean -------------------------------------------------------------

average_missing <- apply(movie[,colnames(movie) %in% list_na],
                         2,
                         mean,
                         na.rm =  TRUE)
average_missing


# Replace the NA Values ---------------------------------------------------

#The verb mutate from the dplyr library is useful in creating a new variable.
#We don't necessarily want to change the original column so we can create a new variable without the NA.
#mutate is easy to use, 
# we just choose a variable name and define how to create this variable
movie_replace <- movie %>%
  mutate(replace_mean_Year  = ifelse(is.na(Year), average_missing[1], Year),
         replace_mean_Sequel = ifelse(is.na(Sequel), average_missing[2], Sequel),
         replace_mean_Sentiment = ifelse(is.na(Sentiment), average_missing[3], Sentiment),
         replace_mean_Genre = ifelse(is.na(Genre), average_missing[4], Genre),
         replace_mean_Ratings  = ifelse(is.na(Ratings), average_missing[5],Ratings),
         replace_mean_Gross  = ifelse(is.na(Gross), average_missing[6], Gross),
         replace_mean_Budget  = ifelse(is.na(Budget), average_missing[7], Budget),
         replace_mean_Screens  = ifelse(is.na(Screens), average_missing[8], Screens),
         replace_mean_Views  = ifelse(is.na(Views), average_missing[9], Views),
         replace_mean_Likes  = ifelse(is.na(Likes), average_missing[10],Likes),
         replace_mean_Dislikes = ifelse(is.na(Dislikes), average_missing[11],Dislikes),
         replace_mean_Comments = ifelse(is.na(Comments), average_missing[12], Comments),
         replace_mean_Aggregate.Followers  = ifelse(is.na(Aggregate.Followers), average_missing[13], Aggregate.Followers))

sum(is.na(movie_replace$Year))
sum(is.na(movie_replace$Sequel))
sum(is.na(movie_replace$Sentiment))
sum(is.na(movie_replace$Genre))
sum(is.na(movie_replace$Ratings))
sum(is.na(movie_replace$Gross))
sum(is.na(movie_replace$ Budget))
sum(is.na(movie_replace$ Screens))
sum(is.na(movie_replace$ Views))
sum(is.na(movie_replace$Likes))
sum(is.na(movie_replace$Dislikes))
sum(is.na(movie_replace$Comments))
sum(is.na(movie_replace$Aggregate.Followers))


#  Perform the replacement ------------------------------------------------

sum(is.na(movie_replace$replace_mean_Aggregate.Followers))
sum(is.na(movie_replace))


#  Other Ways of replace the NA Values ------------------------------------

movie_replace<- data.frame(
  sapply(
    movie,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))
movie_replace
sum(is.na(movie_replace))



Hmisc::contents(movie_replace)

glimpse(movie_replace)

head(movie_replace, 10)

tail(movie_replace)

car::brief(movie_replace)
str(movie_replace)








  list_na <- colnames(movie)[ apply(movie, 2, anyNA) ]
list_na

txt <- "Explore the data set"
banner(txt, centre = TRUE, bandChar = "-")
##----------------------------------------------------------------
##                     Explore the data set                     --
##----------------------------------------------------------------
txt <- "Data contents"
banner(txt, centre = TRUE, bandChar = "=")
##===============================================================
##                        Data contents                        ==
##===============================================================
Hmisc::contents(heart)

glimpse(heart)

head(heart, 10)

tail(heart)

car::brief(heart)
str(heart)

##===============================================================
##                       Data  Description                    ==
##===============================================================

## **age**: age of the patient (years)
## **anaemia**: decrease of red blood cells or hemoglobin (boolean)
## **high blood pressure**: if the patient has hypertension (boolean) creatinine phosphokinase (CPK): level of the CPK enzyme in the blood (mcg/L)
## **diabetes**: if the patient has diabetes (boolean)
## **ejection fraction**: percentage of blood leaving the heart at each contraction (percentage)
## **platelets**: platelets in the blood (kiloplatelets/mL)
## ** sex**: woman or man (binary)
## **serum creatinine**: level of serum creatinine in the blood (mg/dL)
## **serum sodium**: level of serum sodium in the blood (mEq/L)
## ** smoking**: if the patient smokes or not (boolean)
## **time**: follow-up period (days)
## **death event**: if the patient deceased during the follow-up period (boolean)


##================================================================
##                EDA: Exploratory Data Analysis                ==
##================================================================

heart_data <- heart[, 7:ncol(heart)]

# Check the column names again, 
colnames(heart_data)            # The names contain dots, I will substitute 
txt <- "Get the summary statistics of the variables"
banner(txt, centre = TRUE, bandChar = "*")
##***************************************************************
##         Get the summary statistics of the variables         **
##***************************************************************
summary(heart_data)
Hmisc::describe(heart_data)
psych::describe(heart_data, skew = TRUE, 
                IQR = TRUE)

txt <- "The Dependent Variables in this study is ejection_fraction"
banner(txt, centre = TRUE, bandChar = ":")

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  The Dependent Variables in this study is ejection_fraction  ::
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# We are going to explore the distribution of this variable

ggplot(heart_data, aes(x = ejection_fraction)) + 
  geom_histogram(aes(y = ..density..), 
                 fill = "steelblue") +
  geom_density(color = "red", lwd = 1.2) + 
  stat_function(fun = dnorm, args = list(mean = mean(heart_data$ejection_fraction), 
                                         sd = sd(heart_data$ejection_fraction)),
                color = "orange", lwd = 1.2) +
  theme_linedraw()

#################################################################
##                        Joint Graphs                         ##
#################################################################

library(WVPlots)

#ejection_fraction VS age

ScatterHist(heart_data, title = "ejection_fraction VS age", 
            xvar = "age", yvar = "ejection_fraction", 
            smoothmethod = "lm")

## There is a very weak linear relationshipbetween the variables(ejection_fraction VS age)

#age VS serum_creatinine
ScatterHist(clean_heart, title = "age serum_creatinine", 
            xvar = "serum_creatinine", yvar = "age", 
            smoothmethod = "lm")

# There is  a positive linear relationship between the variables(age & serum_creatinine)

# ejection_fraction VS serum_sodium 
ScatterHist(heart_data, title = "ejection_fraction VS serum_sodium", 
            xvar = "serum_sodium", yvar = "ejection_fraction", 
            smoothmethod = "lm", hist_color = "#00ACBB", 
            point_alpha = 0.3, 
            point_color = "#FF00CC")
# There is  a positive linear relationship between the variables (ejection_fraction & serum_sodium)


##***************************************************************
##                    Study the correlation                    **
##***************************************************************
psych::lowerCor(x = heart_data)
psych::corr.test(heart_data)$p

# Plotting the ScatterPlotMatrix
# First look at the help, and the arguments 
# I am going to tweak the knobs a little
pairs(heart_data)  

#change the color and get halp matrix
pairs(heart_data, lower.panel = NULL, col= "blue")

# Or if you want only the lower part matrix
pairs(heart_data, upper.panel = NULL, col= "blue")

# Check the documentation for more information
?pairs

# Scatter Matrix with psych package
library(psych)

pairs.panels(heart_data, 
             method = "pearson", # Correlation method
             hist.col = "#11AACC",
             density = TRUE, 
             cex.cor = 1.3, 
             col = "red", 
             lm = TRUE, 
             pch = 25,    # point character
             bg = "cyan") # background

# Scatter Matrix with car package
car::scatterplotMatrix(heart_data, 
                       col = "steelblue", 
                       pch = 21, 
                       upper.panel = NULL)
# Lastly
library(PerformanceAnalytics)
chart.Correlation(heart_data, 
                  histogram=TRUE,
                  pch=19, 
                  col = "blue")


##***************************************************************
##                  correlation plot matrices                  **
##***************************************************************
correl <- cor(heart_data)

psych::cor.plot(correl)

corrplot::corrplot(correl)

# Tweak the knobs
corrplot(correl, type = "upper", 
         order = "hclust", 
         tl.col = "black",
         tl.srt = 45)

# check 
?corrplot

# Heatmap 

heatmap(correl, symm = TRUE, 
        cexRow = 0.7, 
        cexCol = 0.7)

# ggcorrplot
p <- ggcorrplot::ggcorrplot(correl, 
                            method = "circle", 
                            type = "lower", 
                            ggtheme = ggplot2::theme_linedraw, 
                            lab_col = "blue", 
                            lab_size = 3,
                            tl.cex = 10, 
                            lab = TRUE, 
                            pch.cex = 10, 
                            colors = c("#6D9EC1", "white", "#E46726")) 
p
p + guides(scale = "none")


##----------------------------------------------------------------
##               Fitting Multiple Regression               --
##----------------------------------------------------------------

model <- lm(ejection_fraction ~ serum_sodium + age + platelets + serum_creatinine + time + creatinine_phosphokinase , data = heart_data)
model


##---------------------------------------------------------------
##                     Checking lm objects                     --
##---------------------------------------------------------------
# Show the components of lm object
str(model)
class(model)
typeof(model)
length(model)
names(model)

# Summary Function
summary(model)


# Coefficients Function

model$coefficients

# Fitted function

model$fitted


## We will improve our result by removing the outlier
##create a boxplot to identify   outliers 
boxplot(heart_data$ejection_fraction)
boxplot(heart_data$serum_sodium)
boxplot(heart_data$age)
boxplot(heart_data$platelets)
boxplot(heart_data$serum_creatinine)
boxplot(heart_data$time)
boxplot(heart_data$creatinine_phosphokinase)
boxplot(heart_data)
boxplot(heart_data)$out
###How to Remove Outliers from Multiple Columns in R

outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(heart_data, cols = names(heart_data)) {
  for (col in cols) {
    heart_data <- heart_data[!outliers(heart_data[[col]]),]
  }
  heart_data
}
clean_heart <- remove_outliers(heart_data)
boxplot(clean_heart)
mode2 <- lm(ejection_fraction ~ serum_sodium + age + platelets + serum_creatinine + time + creatinine_phosphokinase , data = clean_heart)
mode2
summary(mode2)

# Coefficients Function

model$coefficients

# Fitted function

model$fitted

#### We can see here,that the coefficient is different between our new model and our previous model 
## Also, after removing the outlier from our dataset the residual standard error is decreasing, while the R_ squared is increasing
## It means that our new model fits better to our data compared to the previous model

## Other Ways of Removing Outliers

## To detect the outlier,we can use the influencePlot
##influencePlot(model)

##mode2 <- lm(ejection_fraction ~ serum_sodium + age + platelets + serum_creatinine + time + creatinine_phosphokinase , data = heart_data[!(row.names(heart_data)=="218"&"65")])

##mode2



###*************************************************************************
###*************************************************************************
###                                                                      ***
###                          SPLITTING THE DATA                          ***
###                        TRAINING AND TEST SETS                        ***
###                                                                      ***
###*************************************************************************
###*************************************************************************
###*
set.seed(299)
ind <- createDataPartition(heart_data$ ejection_fraction, 
                           p = 0.8, times = 1, list = FALSE)
length(ind)
train_set <- heart_data[ind, ]
test_set <- heart_data[-ind, ]
nrow(train_set); nrow(test_set)


# Training the model ------------------------------------------------------

lm_fit <- lm(ejection_fraction ~ . , data = train_set)

broom::tidy(lm_fit)
broom::glance(lm_fit)

###*  *** Prediction ***
#     --------------------

pred <- predict(object = lm_fit, newdata = test_set, type = "response") 

head(pred)

###       **** Model Evaluation ***
#         -------------------------
actual <- test_set$ejection_fraction
mae <- Metrics::mae(actual = actual, predicted = pred)
mse <- Metrics::mse(actual = actual, predicted = pred)
rmse <- Metrics::rmse(actual = actual, predicted = pred)

# Table of results

knitr::kable(cbind(mae, mse, rmse))

