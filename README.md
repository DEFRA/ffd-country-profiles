# ffd-country-profiles
## Project R environment / R path
This project uses renv to set up project-local R dependencies. 
renv helps isolate this project's R dependencies with others in your R ecosystem. 
In order to clone this R project environment do:
1. install renv in your system -> install.package("renv")
2. clone this project
3. Open this project with R studio
4. Run comands in RStudio console:
4.1 library(renv)
4.2 renv::restore()
for more information refer to: https://rstudio.github.io/renv/articles/renv.html 
