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

    # extract the syntax from the XML-files where the different elements of the
    # output are stored (those files start with 'outputViewer' followed by
    # numbers and the .xml-file-extension); the text with the syntax is just
    # HTML-code (the formatting nd the HTML-tags are removed)
    synTxt = c();
    logLst = fleLst[grepl('outputViewer[[:digit:]]+\\.xml', fleLst)];
    for (logNme in logLst) {
        logTxt <- readLines(logHdl <- file(logFle <- unzip(fleNme, logNme, junkpaths = TRUE), 'r'),  warn = FALSE); close(logHdl); unlink(logFle); rm('logHdl', 'logFle');
        synTxt <- c(synTxt, unlist(strsplit(gsub("\\s+", " ", gsub('&#160;', ' ', trimws(gsub("<.*?>", " ", logTxt[grepl('</font>$', logTxt)])))), '\\. ')));
    }
    synTxt = as.list(gsub('\\.\\.', '\\.', paste0(synTxt, '.')));
    
    # the data file (.sav) that was used for a particular analysis is stored in
    # files whose file names begin with numbers and end with lightNotesData.xml
    # (those contain the element called Notes (usually hidden) at the begin of
    # each analysis; inside the files are binary data within which we look for
    # the string '.sav' followed by character(0) which is the end of the file
    # name; the begin is another character(0)
    savFNm = c();
    nteLst = fleLst[grepl('[[:digit:]]+_lightNotesData\\.bin', fleLst)];
    for (nteNme in nteLst) {
        nteBin <- readBin(nteHdl <- file(nteFle <- unzip(fleNme, nteNme, junkpaths = TRUE), 'rb'), 'raw', file.info(nteFle)$size); close(nteHdl); unlink(nteFle); rm('nteHdl', 'nteFle');
        savPos = min(which(nteBin[seq(1, length(nteBin) - 4)] == charToRaw('.') & nteBin[seq(2, length(nteBin) - 3)] == charToRaw('s') & nteBin[seq(3, length(nteBin) - 2)] == charToRaw('a') & nteBin[seq(4, length(nteBin) - 1)] == charToRaw('v') & nteBin[seq(5, length(nteBin))] == '00'));
        savFNm = c(savFNm, rawToChar(nteBin[seq(max(which(nteBin[seq(1, savPos)] == '00')) + 1, savPos + 3)]));
    }
    # check whether exactly one data file was used for all analyses in the
    # .spv-file; if so, return the file name as atrribute 'datafile' of synTxt;
    # if not throw a warning
    savFNm = unique(savFNm);
    if (length(savFNm) == 1) {
        attr(synTxt, 'datafile') <- savFNm;
    } else {
        warning();
    }
    
    # return the syntax
    synTxt
}
