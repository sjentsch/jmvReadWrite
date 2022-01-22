#' Converts .omv-files for the statistical spreadsheet 'jamovi' (www.jamovi.org) from wide to long format
#'
#' @param fleInp name (including the path, if required) of the data file to be read ("FILENAME.omv"; default: "")
#' @param fleOut name (including the path, if required) of the data file to be written ("FILENAME.omv"; default: ""); if empty, FILENAME from fleInp is extended with "_long"
#'
#' @examples
#' \dontrun{
#' library(jmvReadWrite);
#' }
#'
#' @export long2wide_omv
#'
wide2long_omv <- function(fleInp = "", fleOut = "") {
    # check input and output parameters
    # input file in existence?
    # directory for output file in existence?

    # read file

    # transform data set

#reshape(data, varying = NULL, v.names = NULL, timevar = "time",
#             idvar = "id", ids = 1:NROW(data),
#             times = seq_along(varying[[1]]),
#             drop = NULL, direction, new.row.names = NULL,
#             sep = ".",
#             split = if (sep == "") {
#                 list(regexp = "[A-Za-z][0-9]", include = TRUE)
#             } else {
#                 list(regexp = sep, include = FALSE, fixed = TRUE)
#             }

    # write file

}
