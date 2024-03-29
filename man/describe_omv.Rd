% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/describe_omv.R
\name{describe_omv}
\alias{describe_omv}
\title{Adds a title and a description for a data set stored as .omv-file for the statistical spreadsheet 'jamovi' (\url{https://www.jamovi.org})}
\usage{
describe_omv(
  dtaInp = NULL,
  fleOut = "",
  dtaTtl = c(),
  dtaDsc = c(),
  usePkg = c("foreign", "haven"),
  selSet = "",
  ...
)
}
\arguments{
\item{dtaInp}{Either a data frame or the name of a data file to be read (including the path, if required; "FILENAME.ext"; default: NULL); files can be of
any supported file type, see Details below}

\item{fleOut}{Name of the data file to be written (including the path, if required; "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
returned instead}

\item{dtaTtl}{Character vector with a title to be added to the data set (see Details; default: "")}

\item{dtaDsc}{Description of the data set, either as character vector (HTML-formatted) or as named list with the entries "description", "variables",
"references", and "license" (see Details; default: "")}

\item{usePkg}{Name of the package: "foreign" or "haven" that shall be used to read SPSS, Stata and SAS files; "foreign" is the default (it comes with
base R), but "haven" is newer and more comprehensive}

\item{selSet}{Name of the data set that is to be selected from the workspace (only applies when reading .RData-files)}

\item{...}{Additional arguments passed on to methods; see Details below}
}
\value{
a data frame (only returned if \code{fleOut} is empty) where the order of variables / columns of the input data set is re-arranged
}
\description{
Adds a title and a description for a data set stored as .omv-file for the statistical spreadsheet 'jamovi' (\url{https://www.jamovi.org})
}
\details{
\itemize{
\item The aim of this function is to add a title and a data set description to jamovi data files. Two typical use cases would be (1) to help creating data sets
to be used in teaching (i.e., either creating or using data sets in R, and afterwards adding a description to those), and (2) to provide ”properly
described“ data when publishing in a repository such as the OSF).
\item NB: The data set should not contain any existing analyses. These will be overwritten (a  warning is issued informing you about that).
\item \code{dtaTtl} is a title for the dataset (at the top of the results output, i.e., that title which initially is “Results” when you create a new data set in
jamovi).
\item \code{dtaDsc} can either be a character vector (with length = 1) containing HTML-formatted text that describes the data set (see \code{chrDsc} in the examples for
HTML tags that are currently implemented; putting “unformatted” text is not a problem, but then the result is just plain text without formatting).
Alternatively, \code{dtaDcs} can be a named list with the entries \code{description}, \code{variables}, \code{references}, \code{license}. All entries except from \code{variables}
contain character vectors (length = 1); \code{variables} shall be a named list with the variable name as name and a description what the variable contains as
entry. \code{description} and \code{variables} must be given, \code{references} and \code{license} can be left blank (""; but the names must be present in the list). An
example for both a named list with a description (\code{lstDsc}), as well as a character vector with all HTML tags that are implemented (\code{chrDsc}) can be found
in the examples below.
\item The ellipsis-parameter (\code{...}) can be used to submit arguments / parameters to the functions that are used for reading and writing the data. By clicking
on the respective function under “See also”, you can get a more detailed overview over which parameters each of those functions take. The functions are:
\code{read_omv} and \code{write_omv} (for jamovi-files), \code{read.table} (for CSV / TSV files; using similar defaults as \code{read.csv} for CSV and \code{read.delim} for TSV
which both are based upon \code{read.table}), \code{load} (for .RData-files), \code{readRDS} (for .rds-files), \code{read_sav} (needs the R-package \code{haven}) or \code{read.spss}
(needs the R-package \code{foreign}) for SPSS-files, \code{read_dta} (\code{haven}) / \code{read.dta} (\code{foreign}) for Stata-files, \code{read_sas} (\code{haven}) for SAS-data-files,
and \code{read_xpt} (\code{haven}) / \code{read.xport} (\code{foreign}) for SAS-transport-files. If you would like to use \code{haven}, you may need to install it using
\code{install.packages("haven", dep = TRUE)}.
}
}
\examples{
\dontrun{
dtaFrm <- jmvReadWrite::ToothGrowth[, c("len", "supp", "dose")]
nmeOut <- tempfile(fileext = ".omv")

# the paste's underneath are only for readability (without them, the vignettes are misformatted)
lstDsc <- list(description = paste("The response is the length of odontoblasts (cells responsible",
                                   "for tooth growth) in 60 guinea pigs. Each animal received one",
                                   "of three dose levels of vitamin C (0.5, 1, and 2 mg / day) by",
                                   "one of two delivery methods, orange juice or ascorbic acid (a",
                                   "form of vitamin C and coded as VC)."),
               variables = list(len  = "Tooth length",
                                supp = "Supplement type (VC or OJ)",
                                dose = "Dose (in milligrams / day"),
               references = paste("Crampton, E. W. (1947). The growth of the odontoblast of the",
                                  "incisor teeth as a criterion of vitamin C intake of the guinea",
                                  "pig. <em>The Journal of Nutrition, 33</em>(5), 491-504.",
                                  "https://doi.org/10.1093/jn/33.5.491"),
               license = "")
jmvReadWrite::describe_omv(dtaInp = dtaFrm, fleOut = nmeOut, dtaTtl = "ToothGrowth",
  dtaDsc = lstDsc)
# don't include the unlink, if you copy the code and want to look at the resulting output file
unlink(nmeOut)

# the code underneath should cover all formatting options jamovi is able to use (paste0 is only
# for readability)
chrDsc <- paste0("<p><strong>Trial – all formattings:</strong><br/><strong>bold</strong><br/>",
                 "<strong><em>bold, italics</em></strong><br/><em>italics</em><br/><u>underlined",
                 "</u><br/>link:<a href=\"https://jamovi.org﻿﻿﻿\" target=\"_blank\">https://",
                 "jamovi.org﻿﻿﻿</a><br/><s>strikethrough</s><br/>C<sub>2</sub>H<sub>5</sub>",
                 "OH<br/>R<sup>2</sup><br/><span style=\"background-color:#e60000\">background ",
                 "colour: red</span><br/><span style=\"color:#e60000\">foreground color: red",
                 "</span></p><p class=\"ql-align-center\">centered</p><p class=\"ql-align-right\">",
                 "right</p><p class=\"ql-align-justify\">justify justify justify justify justify ",
                 "justify justify justify justify justify justify justify justify justify justify ",
                 "justify justify justify justify justify justify justify justify justify justify",
                 "</p><p><br/></p><ol><li>numbered list</li><li>numbered list</li></ol><p><br/>",
                 "</p><ul><li>bullet point</li><li>bullet point</li></ul><p class=\"ql-indent-1\">",
                 "indented once</p><p class=\"ql-indent-2\">indented twice</p><p ",
                 "class=\"ql-indent-1\">indented once</p><p>Formula: <span class=\"ql-formula\">",
                 "e=mc^2</span></p><pre>Preformatted</pre><p>normal again</p><h2>Heading</h2>")
jmvReadWrite::describe_omv(dtaInp = dtaFrm, fleOut = nmeOut, dtaTtl = "ToothGrowth",
  dtaDsc = chrDsc)
unlink(nmeOut)
}

}
\seealso{
\code{describe_omv} internally uses the following functions for reading and writing data files in different formats: \code{\link[=read_omv]{read_omv()}} and
\code{\link[=write_omv]{write_omv()}} for jamovi-files, \code{\link[utils:read.table]{utils::read.table()}} for CSV / TSV files, \code{\link[=load]{load()}} for reading .RData-files, \code{\link[=readRDS]{readRDS()}} for .rds-files,
\code{\link[haven:read_spss]{haven::read_sav()}} or \code{\link[foreign:read.spss]{foreign::read.spss()}} for SPSS-files, \code{\link[haven:read_dta]{haven::read_dta()}} or \code{\link[foreign:read.dta]{foreign::read.dta()}} for Stata-files, \code{\link[haven:read_sas]{haven::read_sas()}} for
SAS-data-files, and \code{\link[haven:read_xpt]{haven::read_xpt()}} or \code{\link[foreign:read.xport]{foreign::read.xport()}} for SAS-transport-files.
}
