#' Reads and cleans SPSS-SAV-files
#'
#' @param string containing the location of a SPSS-data-file [.sav; incl. path]
#' @param string a filter condition to be applied to the dataset (default = "" → no filter; e.g., "VARNAME == 1")
#' 
#' @return data frame containing the data read from fleSAV
#'
#' @export read_sav
#'
read_sav <- function(fleSAV = "") {

    if (! file.exists(fleSAV) && file.exists(file.path(getwd(), basename(fleSAV)))) { fleSAV = file.path(getwd(), basename(fleSAV)); }

    if (file.exists(fleSAV)) {
        data = suppressMessages(suppressWarnings(foreign::read.spss(file = fleSAV, use.value.labels = FALSE, to.data.frame = TRUE)));
    } else {
        stop(sprintf("File \"%\" was not found at the location given:\n%s\n\n", basename(fleSAV), dirname(fleSAV)));
    }

    # do some cleaning: first for the dataset attributes
    attributes(data) = rplAtt(attributes(data), basename(fleSAV));
    # afterwards, clean each column (value labels, etc.)
    for (colNme in names(data)) {
        if (is.null(attributes(data[[colNme]]))) next
        attributes(data[[colNme]]) = rplAtt(attributes(data[[colNme]]), basename(fleSAV), colNme);
        if (is.factor(data[[colNme]])) data[[colNme]] = droplevels(data[[colNme]]);
    }
        
    data
}

rplAtt <- function(lstAtt = list(), fleNme = "", colNme = "") {
    # lstRpl is defined in globals.R
    
    for (nmeAtt in names(lstAtt)) {
        if (!is.character(lstAtt[[nmeAtt]])) next
        if (!all(validEnc(lstAtt[[nmeAtt]]))) {
            for (j in seq_along(lstRpl)) lstAtt[[nmeAtt]] = gsub(names(lstRpl[j]), lstRpl[[j]], lstAtt[[nmeAtt]]);
            if (!all(validEnc(lstAtt[[nmeAtt]])) && colNme == "") {
                stop(sprintf("Data set \"%s\" contains an attribute \"%s\" where an invalid encoding could not be corrected.", fleNme, nmeAtt));
            } else if (!all(validEnc(lstAtt[[nmeAtt]])) && colNme != "") {
                stop(sprintf("Data set \"%s\" contains a column \"%s\" with an attribute \"%s\" where an invalid encoding could not be corrected.", fleNme, colNme, nmeAtt));
            }
        }
    }
    
    lstAtt
}
