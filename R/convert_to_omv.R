#' Convert data files (CSV, R, other statistics packages) into .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org)
#'
#' @param fleInp name (including the path, if required) of the data file to be read ("FILENAME.ext"; default: ""); supports CSV and R-files natively, other file types if 'haven' or 'foreign are installed
#' @param fleOut name (including the path, if required) of the data file to be written ("FILENAME.omv"; default: ""); if empty, the extension of fleInp is replaced with '.omv'
#' @param ...    
#'
#' @details
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#' }
#'
#' @export convert_to_omv
#'
convert_to_omv <- function(fleInp = "", fleOut = "", ...) {

    # read file

    # adjust settings / attributes

    # write file

}

# =================================================================================================
# functions for reading data files from various formats

read_all <- function(fleInp = "", ...) {

    # OMV
    if        (any(tolower(tools::file_ext(fleInp)) == c("omv"))) {
        print("jamovi");
    
    # CSV   
    } else if (any(tolower(tools::file_ext(fleInp)) == c("tsv"))) {   
        print("CSV");

    # TSV
    } else if (any(tolower(tools::file_ext(fleInp)) == c("tsv"))) {
        print("TSV");

    # Rdata
    } else if (any(tolower(tools::file_ext(fleInp)) == c("rdata", "rda"))) {
        print("Rdata");

    # RDS
    } else if (any(tolower(tools::file_ext(fleInp)) == c("rds"))) {
        print("RDS");

    # SPSS (haven / foreign)
    } else if (any(tolower(tools::file_ext(fleInp)) == c("sav"))) {
        print("SPSS");
        if        (nzchar(system.file(package = "haven"))) {
            print("haven");
        } else if (nzchar(system.file(package = "foreign"))) {
            print("foreign");
        } else {
            stop(sprintf("In order to read the SPSS-file \"%s\" either of the R-packages \"haven\" or \"foreign\" needs to be installed."), fleInp);
        }
    # Stata (haven / foreign)
    } else if (any(tolower(tools::file_ext(fleInp)) == c("dta"))) {
        print("Stata");
        if        (nzchar(system.file(package = "haven"))) {
            print("haven");
        } else if (nzchar(system.file(package = "foreign"))) {
            print("foreign");
        } else {
            stop(sprintf("In order to read the Stata-file \"%s\" either of the R-packages \"haven\" or \"foreign\" needs to be installed."), fleInp);
        }
    # SAS data (haven)
    } else if (any(tolower(tools::file_ext(fleInp)) == c("sas7bdat", "sd2", "sd7", "sas7bcat", "sc2", "sc7"))) {
        print("SAS data");
        if        (nzchar(system.file(package = "haven"))) {
            print("haven");
        } else {
            stop(sprintf("In order to read the SAS-file \"%s\" the R-package \"haven\" needs to be installed."), fleInp);
        }
    } else if (any(tolower(tools::file_ext(fleInp)) == c("xpt", "stx", "stc"))) {
        print("SAS transport");
        if        (nzchar(system.file(package = "haven"))) {
            print("haven");
        } else if (nzchar(system.file(package = "foreign"))) {
            print("foreign");
        } else {
            stop(sprintf("In order to read the SAS-transport-file \"%s\" either of the R-packages \"haven\" or \"foreign\" needs to be installed."), fleInp);
        }
    }
}
