# Microsoft Data Science Professional Repo

This repo contains the submission files, R code and data given for the Microsoft Data Science Professional hosted on Driven Data.

# Analysis Results

This anlaysis presents an overview of features and relevant findings pertaining to the prediction of poverty around the world, carried out for the DAT102x June 2019 Capstone machine learning competition. 
All the analysis was based on obfuscated data derived from PPI surveys and household surveys conducted by InterMedia. The aim of the competition was to predict the probability of an individual living below the poverty threshold of $2.50 per day across 7 different countries. To this effect, 58 features capturing different socio-economic dimensions were provided to help construct a regression model, which would be evaluated using the R-Squared.

The analysis was conducted following the established CRISP-DMframework for data mining using R version 3.6.1. After conducting an exploratory data analysis and feature engineering; different
iterations of Supervised Random Forest models, trained on different samples of the data, were used to predict the required probability. The best performing model scored a R-squared 0.3986 on the
undisclosed holdout set.
