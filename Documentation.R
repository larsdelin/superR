#’@title Import a dataset
#'
#’@description
#’The import function can import data from delimited text files, excel spreadsheets, and statistical packages such as SAS, SPSS, and STATA
#’@param file datafile to import. If missing, the user is prompted to select a file interactively.
#'@param ... parameters passed to the import function. See details.
#'
#'@return a data frame
#'
#'@details put details in here!
#'
#'@export
#'
#'@importFrom haven read_sas read_stata read_spss
#'@importFrom readxl read_excel
#'@import vroom
#'@importFrom tools file_ext
#'
#'@examples
#'\dontrun{
#' # import a comma delimited file
#' my dataframe <- import("mydata.csv")
#'
#' # import a SAS binary data file
#' mydataframe <- import("mydata.sas7bdat")
#'
#'  # import the second worksheet of an Excel workbook
#'  mydataframe <- import
#'
#'  # prompt for a file to import
#'  mydataframe <- import()
#'  }
