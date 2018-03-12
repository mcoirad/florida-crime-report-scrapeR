# Crime and Voter Turnout in Florida

Web scraping and geocoding methods for public voter and crime data in R

## Getting Started

This project requires the set-up of a PostgreSQL database with PostGIS installed.

[A quick tutorial setting this up is located here](https://docs.google.com/document/d/1hhwa-ivnAsKP2QJlwx_KGMGpTV4GsyMVnBgJx8dnbbk/edit?usp=sharing)

## Running the Geocoder

The geocoder is set to run on data obtained through CDs throught the Florida Department of Elections. 

Edit the 'database_conx.R' file for your system to point to the folder containing the voter data, and modify the database connection user and password if it is different from what is listed there.

Script should output a modified file for each county file in the dataset.

## Running the Crime Report Web Scraper

These scripts will collect data from online databases for zipcodes specified in files in the respective 'zipcodes' folders. 

TODO: instructions
