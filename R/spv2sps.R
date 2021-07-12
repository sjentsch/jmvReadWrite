#' Reads SPSS output files (.spv) and extracts the syntax from them
#'
#' @param fleNme name (incl. path) of the SPSS-output-file to be read ("FILENAME.spv"; default = "")
#' @return list with SPSS commands (like what you store in SPSS syntax files - .sps)
#'
#' @export spv2sps

spv2sps <- function(fleNme = "") {

    # check whether the file / archive exists, get list of files contained in the archive and check whether it has the correct format
    if (! file.exists(fleNme))                                            { stop(paste0('File "', fleNme, '" not found.')) }
    hdrStr <- readBin(tmpHdl <- file(fleNme, 'rb'), 'character'); close(tmpHdl); rm('tmpHdl');
    if (! hdrStr == "PK\003\004\024")                                     { stop(paste0('File "', fleNme, '" has not the correct file format (is not a ZIP archive).')) }
    fleLst = unzip(fleNme, list=TRUE)$Name;   

}
