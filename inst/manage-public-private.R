
# This "manage-public-private.R" file defines many 
# shiny app defaults, options, and variables needed in the global environment,
# (mostly for the shiny app to work rather than for the RStudio-focused functions like ejamit() etc.)
#
# The complementary "global.R" file actually defines most default settings,
# while this file was intended to define 
# just those settings that differ depending on whether the version is "Public" or "Private" 
# "Public" here meant a more basic version intended for general public use, 
# with fewer complicated or narrow-purpose/uncommonly-used tools and options, 
# rather than more advanced/specialized/niche/expert/analyst use.
# "Private" here used to refer to a version hosted only internally for staff analysts, e.g., 
# but also referred to any version of the app run (or package used) locally by analysts.
#
# if (run_app(isPublic = TRUE))  it is "Public"
#
# Note that manage-public-private.R is sourced, prior to global.R being sourced, by run_app()
# it's also sourced by .onAttach, in case user is using only the package
# this next line is necessary because while most toggled items are UI/specific to the application,
# a few are variables used also by the package, like the report titles
# so we need to default the isPublic parameter

if (!exists("golem_opts")) golem_opts <- list(isPublic = TRUE)

# Most items can be toggled in app_server.R, unless otherwise specified
################################################################### #

# CHECK PACKAGE VERSION ####

# note that manage-public-private.R is sourced prior to global.R being source, by run_app()
# but global.R and manage-public-private.R both need to know version info so this is done in both to get the version info:

## one option:
# pkg_info <- package_description()
# ejam_app_version  <-  ifelse(is.null(pkg_info), "", pkg_info$get("Version"))
## or just:
ejam_app_version <- EJAM:::description_file$get("Version") # object created in EJAM namespace by metadata_mapping.R when loaded/attached
ejam_app_version <- substr(ejam_app_version, start = 1, stop = gregexpr('\\.',ejam_app_version)[[1]][2] - 1) ## trim version number to Major.Minor
################################################################### #
################################################################### #
# ~ ####

######################################################################################################## #

# GENERAL OPTIONS & Testing ####

## ------------------------ Title of App ####

.app_title <-  ifelse(isTRUE(golem_opts$isPublic), 
                      "Environmental and Residential Population Analysis Multisite tool", 
                      "Environmental and Residential Population Analysis Multisite tool"
)

## ------------------------ logo ####

## logo for app header: 
## (and also see .community_report_logo_path etc. below)
default_titleLogo_html = ' <img id="titleLogo" src="www/favicon.png" alt="logo" title="logo" 
              style="margin: 0px; padding-bottom: 4px; padding-top: 4px;   padding-left: 38px; padding-right: 15px; height: 20%">'
 ## old one:
# default_titleLogo_html = '<img id="titleLogo" src="www/epa_logo_horizBlue.png" alt="EPA" title="EPA" 
#             style="margin: 0px; padding-bottom: 4px; padding-top: 4px;   padding-left: 4px; padding-right: 4px">'          

## ------------------------ default_show_full_header_footer (EPA header) #### 

## constant to show/hide EPA HTML header and footer in app UI
## for public branch, want to hide so it can be legible when embedded as an iframe
default_show_full_header_footer <- FALSE

## ------------------------ Tabs shown ####

# About tab
default_hide_about_tab <- isTRUE(golem_opts$isPublic)

# Histograms tab
default_hide_plot_histo_tab <- isTRUE(golem_opts$isPublic)

# Advanced settings
default_hide_advanced_settings <- TRUE # isTRUE(golem_opts$isPublic)

# Written Report
default_hide_written_report <- TRUE

# Barplots - Plot Average Scores
default_hide_plot_barplots_tab <- FALSE

default_hide_ejscreenapi_tab <- isTRUE(golem_opts$isPublic)  # not yet used

######################################################################################################## #

# ~ ####

# SITE SELECTION: CAPS ON UPLOADS, PTS, RADIUS, etc.   ####


## ------------------------ Limits on # of points etc. ####

# see global.R

## ------------------------ EPA Programs options  #####

# see global.R

## ------------------------ default_choices_for_type_of_site_category #####

default_choices_for_type_of_site_category = if (isTRUE(golem_opts$isPublic)) {
  c('by Industry (NAICS) Code' = 'NAICS')
} else {
  c(
    'by Industry (NAICS) Code' = 'NAICS',
    'by Industry (SIC) Code'   = 'SIC',
    'by EPA Program'           = 'EPA_PROGRAM',
    'by MACT subpart'          = 'MACT'
  )
}

## ------------------------ default_choices_for_type_of_site_upload  #####

default_choices_for_type_of_site_upload <- if (isTRUE(golem_opts$isPublic)) {
  c(
    'Latitude/Longitude file upload'                = 'latlon',
    'EPA Facility IDs (FRS Identifiers)'            = 'FRS',
    'Shapefile of polygons'                         = 'SHP'
  )
} else {
  c(
    'Latitude/Longitude file upload'               = 'latlon',
    'EPA Facility ID (FRS Identifiers)'            = 'FRS',
    'EPA Program IDs'                              = 'EPA_PROGRAM',
    'FIPS Codes'                                   = 'FIPS',
    'Shapefile of polygons'                        = 'SHP'
  )
}
######################################################################################################## #
# ~ ####

# CALCULATIONS & what stats to return ####



######################################################################################################## #
# ~ ####

# RESULTS VIEWS ####

## ------------------------ Short report options ####

# top level header
.community_report_title <- ifelse(isTRUE(golem_opts$isPublic), 
                                  "EJAM Multisite Report",
                                  "EJAM Multisite Report"  # "Summary Report" might be appropriate for single site barplots version via build_barplot_report.R
)

default_show_ratios_in_report <- ifelse(isTRUE(golem_opts$isPublic), 
                                                   FALSE, 
                                                   TRUE
)
default_extratable_show_ratios_in_report <- ifelse(isTRUE(golem_opts$isPublic), 
                                                   FALSE, 
                                                   TRUE
)
# if passed as parameter to run_app(), override the above settings
default_show_ratios_in_report = EJAM:::global_or_param('default_show_ratios_in_report')
default_extratable_show_ratios_in_report = EJAM:::global_or_param('default_extratable_show_ratios_in_report')

######################################################################################################## #
