% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_omv.R
\name{read_omv}
\alias{read_omv}
\title{Read files created of the statistical spreadsheet 'jamovi' (\url{https://www.jamovi.org})}
\usage{
read_omv(
  fleInp = "",
  useFlt = FALSE,
  rmMsVl = FALSE,
  sveAtt = TRUE,
  getSyn = FALSE,
  getHTM = FALSE
)
}
\arguments{
\item{fleInp}{Name (including the path, if required) of the 'jamovi'-file to be read ("FILENAME.omv"; default: "")}

\item{useFlt}{Apply filters (remove the lines where the filter is set to 0; default: FALSE)?}

\item{rmMsVl}{Remove values defined as missing values (replace them with NA; default: FALSE)?}

\item{sveAtt}{Store attributes that are not required in the data set (if you want to write the same data set using write_omv; default: FALSE)?}

\item{getSyn}{Extract syntax from the analyses in the 'jamovi'-file and store it in the attribute "syntax" (default: FALSE)?}

\item{getHTM}{Store index.html in the attribute "HTML" (default: FALSE)?}
}
\value{
data frame (can be directly used with functions included in the R-package \code{jmv} and syntax from 'jamovi'; also compatible with the format of the
R-package \code{foreign})
}
\description{
Read files created of the statistical spreadsheet 'jamovi' (\url{https://www.jamovi.org})
}
\examples{
\dontrun{
nmeInp <- system.file("extdata", "ToothGrowth.omv", package = "jmvReadWrite")
data <- jmvReadWrite::read_omv(fleInp = nmeInp, getSyn = TRUE)
# if the syntax couldn't be extracted, an empty list - length = 0 - is returned,
# otherwise, the commands are shown and the first analysis is run, with the output
# from the second analysis being assigned to the variable result
if (length(attr(data, "syntax")) >= 1) {
    print(attr(data, "syntax"))
    if (nzchar(system.file(package = "jmv"))) {
        # the print-function is only used to force devtools::run_examples() to show output
        eval(parse(text = paste0("result = ", attr(data, "syntax")[1])))
        # without assigning the output to a variable, the command would be:
        # eval(parse(text = attr(data, "syntax")[1]))
        print(names(result))
        print(result$main)
        # -> "main"      "assump"    "contrasts" "postHoc"   "emm"       "residsOV"
        # (the names of the six output tables)
    }
}
}

}
