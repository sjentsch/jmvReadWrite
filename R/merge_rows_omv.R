#' Merges two .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org) by adding the content of the second file (fleIn2) as rows to the first file (fleIn1)
#'
#' @param fleInp vector with file names (including the path, if required) of the data files to be read (c("FILE1.omv", "FILE2.omv"); default: c())
#' @param fleOut name (including the path, if required) of the data file to be written ("FILENAME.omv"; default: ""); if empty, FILE1 (i.e., the first file in the vector) is extended with "_addRows"
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
#' @export merge_rows_omv
#'
merge_rows_omv <- function(fleInp = c(), fleOut = "", typMrg = c("inner", "outer", "left", "right"), ...) {

    # get variable arguments
    # formalArgs(f)

    data <- vector(mode = "list", length = length(fleInp));

    # read files

    # merge files
    allVar <- unique(sort(unlist(sapply(data, names))));
    srtVar <- sapply(sapply(data, names), sort);
    if (! all(sapply(srtVar, identical, allVar))) {
    }

    # cbind()

    # write files

}

# merge(list(name = names(DA)), list(name = names(DB)), by = "name", all = TRUE)
