# dynasim-shiny5

Interactive visualizations of DYNASIM projections of retirement account assets, financial assets, home equity, and total assets across different ages under a baseline and a range of alternative simulations. The interactive includes HRS, PSID, SCF, and SIPP data for validation. 

## data

Data source: Urban Institute's Dynamic Simulation of Income Model (DYNASIM), 2017

## Scripts

### get-ntiles.R

Pulls and cleans ntiles data for the baseline, HRS, PSID, SCF, and SIPP. 

### get-options.R

Pulls and cleans ntiles data for counterfactual simulations.

### clean-and-merge.R

Combines and cleans the data from `get-ntiles.R` and `get-options.R`.

### /www

The /www subdirectory contains `shiny.css`. Shiny applications automatically look for material in the www subdirectory. 

### themes

The R Shiny graphic is built using the [Urban Institute R theme](https://github.com/UrbanInstitute/urban_R_theme). The theme works better using Mac OSX than Windows so `urban_theme_mac.R` is used when publishing the Shiny graphic and `urban_theme_windows.R` is used for developing edits and new features. 

**Note:** Lines at the top of `income_distribution_shiny.R` need to be commented out when switching between operating systems. 

## Built With
* R
* [Shiny](https://shiny.rstudio.com/)

## Authors
* Aaron Williams
* Karen Smith