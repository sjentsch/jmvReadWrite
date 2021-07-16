#' Reads SPSS output files (.spv) and extracts the syntax and the data file used in the analyses from them
#'
#' @param fleSPV name (incl. path) of the SPSS-output-file to be read ("FILENAME.spv"; default = "")
#'
#' @return list with SPSS commands (like what is typically stored in SPSS syntax files - .sps); the list contains one command per entry (the command being a character vector) and the position of the SPSS-data-file [incl. path] that was used in the analyses is stored as a character vector in the attribute 'datafile' (the attribute will be empty if either the SPSS-datafile used in the analyses was not stored or if there was more than one data file used in the analyses stored in the original .spv-file 
#'
#' @export spv2sps
#'
spv2sps <- function(fleSPV = "") {

    # check whether the file / archive exists, get list of files contained in the archive and check whether it has the correct format
    if (! file.exists(fleSPV))                                            { stop(paste0('File "', fleSPV, '" not found.')) }
    fleSPV <- normalizePath(fleSPV);
    hdrStr <- readBin(tmpHdl <- file(fleSPV, 'rb'), 'character'); close(tmpHdl); rm('tmpHdl');
    if (! hdrStr == "PK\003\004\024")                                     { stop(paste0('File "', fleSPV, '" has not the correct file format (is not a ZIP archive).')) }
    zipLst = unzip(fleSPV, list=TRUE)$Name;

    # extract the syntax from the XML-files where the different elements of the
    # output are stored (those files start with 'outputViewer' followed by
    # numbers and the .xml-file-extension); the text with the syntax is just
    # HTML-code (the formatting nd the HTML-tags are removed)
    txtSPS = c();
    logLst = zipLst[grepl('outputViewer[[:digit:]]+\\.xml', zipLst)];
    for (logNme in logLst) {
        logTxt <- readLines(logHdl <- file(logFle <- unzip(fleSPV, logNme, junkpaths = TRUE), 'r'),  warn = FALSE); close(logHdl); unlink(logFle); rm('logHdl', 'logFle');
        txtSPS <- c(txtSPS, unlist(strsplit(gsub("\\s+", " ", gsub('&#160;', ' ', trimws(gsub("<.*?>", " ", logTxt[grepl('</font>$', logTxt)])))), '\\. ')));
    }
    txtSPS = as.list(gsub('\\.\\.', '\\.', paste0(txtSPS, '.')));
    
    # the data file (.sav) that was used for a particular analysis is stored in
    # files whose file names begin with numbers and end with lightNotesData.xml
    # (those contain the element called Notes (usually hidden) at the begin of
    # each analysis; inside the files are binary data within which we look for
    # the string '.sav' followed by character(0) which is the end of the file
    # name; the begin is another character(0)
    fleSAV = c();
    nteLst = zipLst[grepl('[[:digit:]]+_lightNotesData\\.bin', zipLst)];
    for (nteNme in nteLst) {
        nteBin <- readBin(nteHdl <- file(nteFle <- unzip(fleSPV, nteNme, junkpaths = TRUE), 'rb'), 'raw', file.info(nteFle)$size); close(nteHdl); unlink(nteFle); rm('nteHdl', 'nteFle');
        savPos = min(which(nteBin[seq(1, length(nteBin) - 4)] == charToRaw('.') & nteBin[seq(2, length(nteBin) - 3)] == charToRaw('s') & nteBin[seq(3, length(nteBin) - 2)] == charToRaw('a') & nteBin[seq(4, length(nteBin) - 1)] == charToRaw('v') & nteBin[seq(5, length(nteBin))] == '00'));
        fleSAV = c(fleSAV, rawToChar(nteBin[seq(max(which(nteBin[seq(1, savPos)] == '00')) + 1, savPos + 3)]));
    }
    # check whether exactly one data file was used for all analyses in the
    # .spv-file; if so, return the file name as atrribute 'datafile' of txtSPS;
    # if not throw a warning
    fleSAV = unique(fleSAV);
    if (length(fleSAV) == 1) {
        # check for existence of the SPSS-data-file
        if (file.exists(fleSAV)) { 
            attr(txtSPS, 'datafile') <- fleSAV;
        } else if (file.exists(file.path(getwd(), basename(fleSAV)))) {
            attr(txtSPS, 'datafile') <- file.path(getwd(), basename(fleSAV));
        } else if (file.exists(file.path(dirname(fleSPV), basename(fleSAV)))) {
            attr(txtSPS, 'datafile') <- file.path(dirname(fleSPV), basename(fleSAV));
        } else {
            stop('SPSS data file not found: It is expected to be found either at the position that is stored in the .spv-file, the current working directory, or the directory where the .spv-file was located. Please correct it (copy the data file to one of these places) and run the function again.');
        }
    } else {
        warning('File name of the data file insufficient: Either there was more than one data file used in the analyses that are stored in the .spv-file or the file was not stored (e.g., because you worked with a dataset with values that were typed in but not stored before conducting the analyses).');
    }

    # return the syntax
    txtSPS
}
