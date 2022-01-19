#' Merges two .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org) by adding the content of the second file (fleIn2) as columns to the first file (fleIn1)
#'
#' @param fleIn1 name (including the path, if required) of the data file to be read ("FILENAME.omv"; default: "")  
#' @param fleIn2 name (including the path, if required) of the data file to be read ("FILENAME.omv"; default: "")  
#' @param fleOut name (including the path, if required) of the data file to be written ("FILENAME.omv"; default: ""); if empty, FILENAME from fleIn1 is extended with "_addCols"
#' @param typMrg type of merging operation ("inner" [default], "outer", "left", "right", "cross")
#'
#' @details
#' The different types of merging operations: 
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#' }
#'
#' @export merge_cols_omv
#'
merge_cols_omv <- function(fleIn1 = "", fleIn2 = "", fleOut = "", typMrg = c("inner", "outer", "left", "right", "cross")) {

    # read files

    # merge files

    # write files

}
