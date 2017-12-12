BIOS 6640 Final Project Read Me:

This file contains descriptions of all files in the RProject Folder. The repository can be found on my github. 

DataClean.R - loads the data, scrapes the weather data from the website and creates an output weather dataset

DataMerge.R - merges the weather data with incident and intervention data, does some data cleaning like creating lag variables, creates an output merged dataset

Decay.R - creates the decay effect for the variables for ITN and IRS interventions, creates an output final dataset

Exploratory.R - data exploration, creates graphics, does descriptive stats, etc.

FinalReport.Rmd - final written report for the project

FinalReport.pdf - pdf version of final report created by the .Rmd file

ITNdecayExample2.R - code provided by Katie for example of decay, I added comments to understand what was happening in the code for when I recreated it in my file

Models.R - contains models run for analyzing data set with poisson regression and cases as outcome 

Plots folder - contains images of all plots created to include in the report and supplement