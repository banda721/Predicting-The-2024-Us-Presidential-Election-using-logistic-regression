# Predicting-The-2024-Us-Presidential-Election-using-logistic-regression
This project aims to utilize a statistical and predictive model that integrates many key economic and political variables to predict the results of upcoming election.

Project Overview
This R script is designed to analyze and visualize polling and election data. It integrates polling data with fundamental election data to forecast and analyze potential outcomes in U.S. elections.

Dependencies
To run this script, you will need the following R packages installed:

dplyr: For data manipulation and aggregation.
ggplot2: For creating visualizations.
tidyr: For data tidying.
corrplot: For generating correlation matrices.
maps: For plotting geographic data.
Data Files
The script requires the following CSV files:

fundamental_data_election.csv: Contains fundamental data related to elections.
Polls.csv: Contains polling data for various candidates and states.
Ensure these files are correctly pathed in your project directory or update the file paths in the script accordingly.

Script Features
Data Preprocessing: Converts column types and cleans up the data.
Aggregation: Calculates the mean polling estimates grouped by cycle, state, and candidate.
Data Transformation: Transforms data into a wide format for easier analysis and merges it with fundamental election data.
Visualization: Generates various plots including:
Geographic maps of predicted election outcomes.
Correlation matrices of predictors.
Boxplots and line plots to examine trends and distributions.
Predictive Analysis: Integrates predictive models to estimate future election outcomes based on current data.
Running the Script
To run this script, open your R environment and set the working directory to the location of this script and associated data files. Execute the script commands sequentially to reproduce the analysis.
