# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "golem")
usethis::use_pipe(export = TRUE)
usethis::use_data(litigii_sep,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(litigii_update,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(bi_litigii_contracte_platite,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(sinteza_bi_litigii,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(dosare_excluse,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(bi_contracte,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)
usethis::use_data(litigiu_nou_from_updates,internal = FALSE,overwrite = TRUE,compress = "gzip",version = 3)

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "sidebar" ) # Name of the module
golem::add_module( name = "litigii" ) # Name of the module
golem::add_module( name = "actualizare_automata")
golem::add_module( name = "actualizare_manuala")
golem::add_module( name = "litigii_current")
golem::add_module( name = "litigii_inghetate")
golem::add_module( name = "litigii_noi")
golem::add_module( name = "home")
golem::add_module( name = "litigii_platite")

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct("helpers",module = "litigii_noi") 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("LitigiiRisc")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

