# 

# DO NOT EDIT THIS FILE - THIS DOCUMENTATION WAS CREATED BY A SCRIPT - see EJAM/data-raw/datacreate_modelEjamit.R or similar files

#' @name modelEjamit
#' @docType data
#' @title Regression model to predict runtime for ejamit
#' @description Modeled runtime for doaggregate and ejamitbased off 100 runs with random parameters. Use these models to make predictions in app_server
#' @details Ejamit's runtime is modeled off radius and number of rows of input dataset, doaggregate runtime is modeled off rows of getblocksnearby output
'modelEjamit'