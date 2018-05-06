# MA415-615-Final-Project

## Summary

This is the final project for MA415/615. It focuses on "supply and demand" in the US film industry by exploring the relationship between movies released in theaters and viewers' preferences. This study intends to help US movie makers stay competitive in the film industry, making movies that have high quality and generate profits.

## Suggestions for Viewing Files in This Repository

It's recommended to run files in the following order to install required packages and produce expected results:

1_Data_Download_Basic.R

2_Data_Cleaning.R

3_Tidy_Data.R

4_Analysis.R

Additionally, the file "movies_metadata.csv" inside folder "the-movies-dataset" contains TMDb information for movies released on or before July 2017, and it was downloaded from ["The Movies Dataset"](https://www.kaggle.com/rounakbanik/the-movies-dataset) at Kaggle. 

## Shiny App

Link for Shiny app: 
https://xlyu.shinyapps.io/MA615_final_project/

### A few notes for using Shiny app:

1. When using "Create Your Own" tab to generate box office report, please select a relatively short time range (preferably <30 days) so that the loading time is bearable. Please keep in mind that when specifying the time range, the application will be downloading real-time information from the database.

2. Usually box office data would be released at 8 pm (PT) one day after the actual show date but might not be the case some times. To avoid errors in downloading data, users are expected to set the end date of the report to be two days earlier than the system date.

## THANK YOU for reading my report and trying out the app!!!
