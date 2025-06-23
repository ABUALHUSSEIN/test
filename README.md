# test
🎬 Movie Dataset Analysis using R
This project focuses on analyzing and cleaning a movie dataset using the R programming language. The workflow includes loading data, handling missing values, renaming variables, and generating a clean dataset suitable for further modeling or visualization.

📁 Project Summary
Title: Movie Dataset Cleaning & Preparation

Author: Anwar Abu Alhussein

Language: R

Goal: Prepare a movie dataset for further statistical or machine learning analysis by cleaning and imputing missing values using summary statistics.

🧰 Tools and Libraries Used
tidyverse

dplyr

ggplot2

caret

Metrics

corrplot

PerformanceAnalytics

WVPlots

outliers

broom

equatiomatic

psych

📦 Dataset Description
Source: Downloaded from GitHub:

https://raw.githubusercontent.com/ABUALHUSSEIN/test/main/data/movie.csv

Columns include:

Movie metadata: Movie, Year, Genre, Sequel

Audience sentiment: Sentiment, Ratings

Popularity metrics: Gross, Budget, Views, Likes, Comments, Aggregate Followers

Engagement: Dislikes, Screens

🔍 Key Processing Steps
Handling Missing Data:

Checked for missing values using is.na()

Created a list of columns with missing data

Calculated column means (na.rm = TRUE)

Used mutate() from dplyr to replace NAs with column means

Column Renaming:

Dots in column names were replaced with underscores

Final consistent format: Aggregate.Followers → Aggregate_Followers

Data Frame Creation:

Constructed new_movie data frame with imputed values

Verified: sum(is.na(new_movie)) == 0

Alternative Imputation:

Used sapply() method to apply mean replacement across all numeric columns

📊 Result
A fully cleaned movie dataset (new_movie) is now available for visualization, regression modeling, or further exploratory analysis.

All NA values have been imputed.

Dataset includes 200+ movies with comprehensive metadata and viewer interaction metrics.





