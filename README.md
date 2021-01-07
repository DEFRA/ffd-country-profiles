# ffd-country-profiles
## Project overview
This is a shiny app project build to automate the creation of country profile reports to different stakeholders. <br/>
Through the sniny app UI you can create a country profile report ready to download in word document format by intoducing only the country name and the years you are interested to build the analysis from. 
## Country profile report content
Country profile reports provides Food Feed and Drink (FFD+) trading information "imports/exports" between UK and the country selected.
The report is build on average data of specific years also selected by the user. 
The report has 3 main parts: 
- Executive summary
- Key facts
- Country selected perspective
- Uk perspective 
- Annexes 
A shiny app is used as a user interface to select the country name, and years interested. 
The app is served and can be used -> http://pc-209.persephone.gov.uk/shiny/Country_Profiles/  
## Project R environment / R path
This project uses renv to set up project-local R dependencies.  <br/>
renv helps isolate this project's R dependencies with others in your R ecosystem.  <br/>
In order to run this R project in your environment do:
1. install renv in your system -> install.package("renv")
2. clone this project
3. Open this project with R studio "Country_profile.Rproj"
4. Run comands in RStudio console:
4.1 library(renv)
4.2 renv::restore()
4.3 click run app
for more information refer to: https://rstudio.github.io/renv/articles/renv.html 

## Developer
This product is created by Elia Borras  <br/>
For more information Elia.Borras@defra.gov.uk
