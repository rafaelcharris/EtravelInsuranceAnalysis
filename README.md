# Insurance Analysis

This repository is an example of data analysis for a project I worked on with the European Comission.
I cannot share the original data, so the data I uploaded is fake, but similar to the original in case someone wants to run the code.
The purpose of this analysis is to understand wheather covid priming influences how people make insurance decisions in a online web experiment conducted in 
different european countries. We this data plus mortality rate by european region to analyze the data.

## Contents

* `DataCleaning.R`: This file conducts all the cleaning and construction of new varaibles for analysis. It creates a new cleaned data set `df_clean` that is stored in the `data` folder.
* `MortalityDatawrangling.R`: This file loads mortality data from five countries by region, creates variables for analysis, and outputs a new data set with the merged data
* `InsuranceAnalysis.R`: This file conducts all the OLS regression with robust standard errors, descriptive statistics tables, and sentiment analysis. The output are regression and descriptives tables.
