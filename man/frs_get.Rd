% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frs_get.R
\name{frs_get}
\alias{frs_get}
\title{Download, unzip, read, clean the Facility Registry Service dataset}
\usage{
frs_get(
  only_essential_cols = TRUE,
  folder = NULL,
  downloaded_and_unzipped_already = FALSE,
  zfile = "national_single.zip",
  zipbaseurl = "https://ordsext.epa.gov/FLA/www3/state_files/",
  csvname = "NATIONAL_SINGLE.CSV",
  date = Sys.Date()
)
}
\arguments{
\item{only_essential_cols}{TRUE by default. used in frs_read()}

\item{folder}{NULL by default which means it downloads to and unzips in a temporary folder}

\item{downloaded_and_unzipped_already}{If set to TRUE, looks in folder for csv file
instead of trying to download/unzip. Looks in working directory if folder not specified.}

\item{zfile}{filename, just use default unless EPA changes it}

\item{zipbaseurl}{url, just use default unless EPA changes it}

\item{csvname}{name of csv file. just use default unless EPA changes it}

\item{date}{default is Sys.Date() which is today, but this is used as
an attribute assigned to the results,
representing the vintage, such as the date the frs was downloaded, obtained.}
}
\description{
Download, unzip, read, clean the Facility Registry Service dataset
}
\details{
Used by \code{\link[=frs_update_datasets]{frs_update_datasets()}}

Uses \code{\link[=frs_download]{frs_download()}}, \code{\link[=frs_unzip]{frs_unzip()}}, \code{\link[=frs_read]{frs_read()}}, \code{\link[=frs_clean]{frs_clean()}}

\strong{See examples for how package maintainer might use this.}

See source code of this function for more notes.
For a developer updating the frs datasets in this package,
see \code{\link[=frs_update_datasets]{frs_update_datasets()}}

frs_get() invisibly returns the table of data, as a data.table.
It will download, unzip, read, clean, and set metadata for the data.

This function gets the whole thing in one file from

NATIONAL_SINGLE.CSV from
\url{https://ordsext.epa.gov/FLA/www3/state_files/national_single.zip}

Other files and related information:
\itemize{
\item \url{https://www.epa.gov/frs/frs-data-resources}
\item \url{https://www.epa.gov/frs/geospatial-data-download-service}
\item \url{https://www.epa.gov/frs/epa-frs-facilities-state-single-file-csv-download}
\item Also could download individual files from ECHO for parts of the info:
\url{https://echo.epa.gov/tools/data-downloads/frs-download-summary}
for a description of other related files available from EPA's ECHO.
}

This function creates the following:

\preformatted{

 > head(frs_by_programid)
           lat        lon  REGISTRY_ID   program   pgm_sys_id
   1: 44.13415 -104.12563 110012799846     STATE        #5005
   2: 41.16163  -80.07847 110057783590 PA-EFACTS         ++++
   3: 41.21463 -111.96224 110020117862       CIM            0
   4: 29.62889  -83.10833 110040716473 LUST-ARRA            0
   5: 40.71490  -74.00316 110019246163       FIS 0-0000-01097
   6: 40.76395  -73.97037 110019163359       FIS 0-0000-01103
   
   > frs_by_naics[1:2, ]
           lat        lon  REGISTRY_ID NAICS
   1: 30.33805  -87.15616 110002524055     0
   2: 48.77306 -104.56154 110007654038     0
   
   > names(frs)
   "lat"    "lon"   "REGISTRY_ID" "PRIMARY_NAME" "NAICS" "PGM_SYS_ACRNMS"
   
    > head(frs[,1:4]) # looks something like this:
           lat       lon  REGISTRY_ID                    PRIMARY_NAME
   1: 18.37269 -66.14207 110000307695      xyz CHEMICALS INCORPORATED
   x: 17.98615 -66.61845 110000307784                         ABC INC
   x: 17.94930 -66.23170 110000307800                   COMPANY QRSTU
   
   
  **WHICH SITES ARE ACTIVE VS INACTIVE SITES**
 
 See frs_active_ids() or frs_inactive_ids()
 
 Approx 4.6 million rows total 10/2022.
 
 table(is.na(frs$lat))
 table(is.na(frs$NAICS))
 
 It is not entirely clear how to simply identify 
 which ones are active vs inactive sites. 
 See inst folder for notes on that. 
 This as of 2/10/23 is not exactly how ECHO/OECA defines "active" 
 
  **WHICH SITES HAVE LAT LON INFO**
  
 As of 2022-01-31:  Among all including inactive sites, 
 
  1/3 have no latitude or longitude.
  Even those with lat lon have some problems:
    Some are are not in the USA.
    Some have errors in country code.
    Some use alternate ways of specifying USA.
  
  **WHICH SITES HAVE NAICS OR SIC INDUSTRY CODES**
  
 Only 1/4 have both location and some industry code (27%)
 
 2/3 lack industry code (have no NAICS and no SIC).  
    NAICS vs SIC codes:
 11 percent have both NAICS and SIC, 
 9.5 percent have just NAICS = 
    (21 percent have NAICS). 
 12.5 percent have just SIC. 
 2/3 have neither NAICS nor SIC.
 
 
  **WHICH COLUMNS TO IMPORT AND KEEP**
 
 approx 39 columns if all are imported, but most useful 10 is default.
 
 [1] "REGISTRY_ID"             "PRIMARY_NAME"        "PGM_SYS_ACRNMS"         
 [4] "INTEREST_TYPES"    "NAICS_CODES"       "NAICS_CODE_DESCRIPTIONS"
 [7] "SIC_CODES"       "SIC_CODE_DESCRIPTIONS"  "LATITUDE83"             
 [10] "LONGITUDE83" 
 
      
  Some fields are csv lists actually, to be split into separate rows
   to enable queries on NAICS code or program system id:
      
  PGM_SYS_ACRNMS = 'c', # csv format like AIR:AK999, AIRS/AFS:123,
     NPDES:AK0020630, RCRAINFO:AK6690360312, RCRAINFO:AKR000206516"
  INTEREST_TYPES = 'c', # eg "AIR SYNTHETIC MINOR, ICIS-NPDES NON-MAJOR"
      NAICS_CODES = 'c',  # csv of NAICS
}
}
\examples{
\donttest{
  # These steps in the examples are all done by frs_update_datasets() 
  #  (a function not exported by the package)
  # Note these take a long time to run, for downloads and processing.
  frs <- frs_get() 
  # keep only if an active site, or unclear whether active. Remove clearly inactive ones.
  closedidlist <- frs_inactive_ids()
  frs <- frs_drop_inactive(frs, closedid = closedidlist) 
  frs_by_programid <- frs_make_programid_lookup(x = frs) # another super slow step 
  frs_by_naics     <- frs_make_naics_lookup(    x = frs) #  NAs introduced by coercion
  usethis::use_data(frs,              overwrite = TRUE)
  usethis::use_data(frs_by_programid, overwrite = TRUE)
  usethis::use_data(frs_by_naics,     overwrite = TRUE)
}
}
\seealso{
\code{\link[=frs_update_datasets]{frs_update_datasets()}} \code{\link[=frs_read]{frs_read()}} \code{\link[=frs_clean]{frs_clean()}} frs_by_naics \code{\link[=frs_active_ids]{frs_active_ids()}}
\code{\link[=frs_drop_inactive]{frs_drop_inactive()}} \code{\link[=frs_make_programid_lookup]{frs_make_programid_lookup()}} \code{\link[=frs_make_naics_lookup]{frs_make_naics_lookup()}}
}
