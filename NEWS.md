# jmvReadWrite 0.4.4

## Enhancements:
* added `var_labels_omv` (assign labels to variables in a data set; including unit tests, and files for
  the tests and the examples)

---

# jmvReadWrite 0.4.3

## Bug fixes and enhancements:
* replaced `sapply` with `vapply` or `lapply` (in accordance with `goodpractice`)
* reducing cyclomatic complexity to 30
* fixed bugs and created test cases for `rplAtt` (replace non-UTF with UTF characters, part of `read_omv`)
* added `invisible(NULL)`where NULL or nothing was returned 
* adjust `strsplit` in `long2wide_omv` and `wide2long_omv` in order to permit having “.” as varSep
* added `codemeta.json`, badge about package status, and how to contribute to the package (for `pkgcheck`)
* added documentation of return value to `convert_to_omv`

---

# jmvReadWrite 0.4.2

## Bug fixes and enhancements:
* check for empty rows and missing values within `varID` in `merge_cols_omv`
* corrected replacement of numerical values in `replace_omv`
* replaced all occurrences of `library(jmvReadWrite)`
* smaller corrections to the documentation (added parameter descriptions)
* added unit tests for `replace_omv` (replacement in character columns)
* adjusted `wide2long_omv` in order to accept `NULL` for `varID`

---

# jmvReadWrite 0.4.1

## Bug fix:
* ensure that the examples, tests and creating the vignette also works when
  `jmv` is not present

---

# jmvReadWrite 0.4.0

## Enhancements and bug fixes:
* added `describe_omv` (put a title and a description at the top of a data
  set), `search_omv` (finding values in a data set) and `replace_omv`
  (replacing values in a data set with other values)
* improved handling in jamovi (if jmvReadWrite is run in jamovi modules, e.g.,
  `Rj`, a new jamovi session is opened with the data set that was modified by
  one of the helper functions)
* improved handling of the jamovi protocol buffers (currently used for
  extracting `jmv`-syntax in the data sets, but with the perspective of
  creating analyses from `jmv`-syntax)
* update the content of `index.html` (generated when writing data set with
  `write_omv` to be in line with more recent versions of jamovi)
* added attaching `dataType` and `measureType` attributes to a data frame when
  `write_omv` is called from within jamovi (while ensuring not to overwrite
  existing `measureType` / `dataType` attributes)
* enforce `jmv-id`-status for ID-variable if the variable is unique in
  `long2wide_omv`
* removed `fleOut` from `inp2DF` (making it impossible to attach `fleOut` as
  attribute to a data frame) - earlier, the output file name could be
  “assembled” by adding a suffix to the input file name, with leaving `fleOut`
  empty to return the resulting data frame that became obsolete
* added `drop = FALSE` in sort_omv to prevent reduction to a vector
* added further unit-tests for `globals`, `read_omv`, and `write_omv` 
* improved the documentation (adding missing information, better formatting,
  etc.)

---

# jmvReadWrite 0.3.8

## Enhancements and bug fixes:
* adjustments to make `jmvReadWrite` more suited for using it together with the
  jamovi-module `jTransform` (https://github.com/sjentsch/jTransform)
* implement reading both data frames or file names for `merge_cols_omv` and
  `merge_rows_omv` (incl. phasing out `fleInp` as parameter for the helper
  functions: it now throws an error to prevent using it)
* initial handling of weights in `read_omv` and `write_omv`
* improvements and corrections for `long2wide_omv` (added aggregation, mean or
  take first, and fixed a bug that led to incorrect naming of variables when
  transforming complex data sets)
* improvements and corrections for `wide2long_omv` (added parameter `excLvl`,
  to prevent that measurements (if there are more than one) are also
  transformed to long; fixed a bug that led to incorrect an order of values
  within variables when transforming complex data sets)
* improved unit tests (implementation of regular expressions for `expect_error`
  and `expect_warning`, bug-fixes and additional coverage)
* improvements to setting attributes (`setAtt`, e.g., from the metadata to the
  data frame and vice versa)
* improved handling of factors with numerical values (measureType Nominal or
  Ordinal and dataType Integer)
* added function `jmvAtt` to attach the attributes `measureType` and `dataType`
  to data frames coming from inside jamovi (i.e., when `jmvReadWrite` is used
  in modules, e.g., `Rj`)
  OBS: rather for internal use and thus not exported, use
  `jmvReadWrite:::jmvAtt()` to call it
* smaller bug fixes (typos, to ensure consistency in naming, etc.)

---

# jmvReadWrite 0.3.7

## Enhancements and bug fixes:
* added `transpose_omv` (transpose data frame and write the resulting
  jamovi-file)
* enable to have either data frames or strings with a file name as input to the
  helper functions `arrange_cols_omv`, `long2wide_omv`, `wide2long_omv`,
  `sort_omv`, and `transpose_omv`
* preserve attributes when merging columns (`merge_cols_omv`)
* changed R-package that handles JSON files from `RJSON` to `jsonlite`
* bug fixes: added a check in `fmtFlO` to ensure that the output files are in
  jamovi-format (.omv); removed typos, etc.

---

# jmvReadWrite 0.3.6

## Enhancements and bug fixes:
* added `arrange_cols_omv` (to change the order of variables / columns in
  jamovi-files)
* added the parameter `psvAnl` to preserve the analyses in data files
  (only for `arrange_cols_omv`, `merge_cols_omv`, `sort_omv` where it makes
  sense – there the number of rows / participants / units doesn't change)
* bug fixes: corrected an error in `chkFle`, removed duplicate functions from
  `globals.R`

---

# jmvReadWrite 0.3.5

## Bug fixes and enhancements:
* fixed an error that led to a warning when adding columns (to make different
  data frames have the same set of variables) in `merge_cols_omv`
* added logo and citation

---

# jmvReadWrite 0.3.4

## Bug fixes and enhancements:
* fixed an error that led to a crash when an ID variable contained empty
  values ("")
* general bug fixes and improvements (removing ;s as suggested by lintr,
  etc.)

---

# jmvReadWrite 0.3.3

## Enhancements and bug fixes:
* made replacements using regular expressions compatible with R >= 4.3
  (changing occurrences of replacement pattern that included `\xNN`)
* improvements to `wide2long_omv` and `long2wide_omv` (those can now handle
  several factors, not only one)
* improvement to how `write_omv` handles / stores factors (those are now
  zero-based to be in accordance with most programming languages including
  Python and C++ used in the jamovi engine)
* simplify handling variables with the measurement type "ID" (converted to /
  treated as character)
* simplify handling variable labels (now, the attribute `label`, e.g., from
  importing files using haven, is converted to the jamovi-conforming
  `jmv-desc`)
* adding further the unit tests
* changed web-addresses into their canonical format (i.e., including protocol
  and angle brackets; for help, documentation, etc.)
* general bug fixes and improvements (simplifying the code)

---

# jmvReadWrite 0.3.2

## Bug fixes and enhancements:
* added a warning about packages that are required when the extraction of
  syntax can't be carried out due to these missing dependencies

---

# jmvReadWrite 0.3.1

## Bug fixes and enhancements:
* fixed a "bug" in a testthat-condition that resulted in an error on several
  systems

---

# jmvReadWrite 0.3.0

## Enhancements and bug fixes:
* added a couple of helper functions:
  `convert_to_omv` (converts data sets from other formats - CSV, R, SPSS, SAS,
  Stata - to `.omv`)
  `merge_cols_omv` (merges two or more data sets by concatenating columns)
  `merge_rows_omv` (merges two or more data sets by concatenating rows)
  `long2wide_omv` (converts data sets from long to wide, e.g., for running
  mixed-model-analyses in jamovi)
  `wide2long_omv` (converts data sets from wide to long, e.g., for running
  mixed-model-analyses in jamovi)
  `sort_omv` (sorts the dataset according to one or more variable)
* implemented unit tests and code coverage for `read_omv`, `write_omv`,
  `convert_to_omv`, `merge_cols_omv`, `merge_rows_omv`, `long2wide_omv`,
  `wide2long_omv`, `sort_omv`
* implemented treating variables in `read_omv` and `write_omv` as ordered
  factor if `measureType` has the value `Ordinal`
* default for `sveAtt` in `read_omv` (now `TRUE`; it makes more sense to store
  this attributes be default since `write_omv` will give you an exact copy of
  the original data set if they are stored and the helper functions above also
  respect and adjust them)

---

# jmvReadWrite 0.2.4

## Bug fixes and enhancements:
* fixed an error when assembling the file name in `write_omv`, added assembling
  the file name with `normalizePath` to `read_omv`
* fixed missing retDbg-parameter in one of the examples

---

# jmvReadWrite 0.2.3

## Bug fixes and enhancements:
* use `zip` R-package instead of `utils` to prevent that no ZIP-executable-file
  is found on Windows (`utils::zip` only works in cases where a `zip.exe` is
  found on the path)
* use the (session-specific) temporary directory for creating files to be
  zipped and those files extracted
* improved handling of the different variable types, implemented logical /
  boolean variables / data columns
* improved handling of column attributes
* added parameter `retDbg` (default: `FALSE`) to write_omv so that output for
  debugging is only produced upon setting it 

---

# jmvReadWrite 0.2.2

## Bug fixes and enhancements:
* bug fix in read_omv (some libraries required for syntax extraction are not
  available on certain platforms (Solaris, Windows with MinGW)
* added documentation (pkgdown) in docs/
* set up examples and lintr

---

# jmvReadWrite 0.2.1

## Bug fixes and enhancements:
* bug in write_omv fixed (@MAgojam, #2); jamovi could not read the manifest
  (meta) when the file was created with `write_omv` on Windows (LF + CR, but
  only CR expected / decoded)
* updated the Created-by in the manifest string to use the jmvReadWrite version
* improved the README (switched it to be generated from Rmd so that the
  vignette could be included

---

# jmvReadWrite 0.2.0

## Enhancements:

* renamed `jmvRead` to `read_omv`, and `jmvWrite` into `write_omv`
* extracts syntax from analyses contained in the `.omv`-file (set the parameter
  `getSyn = TRUE`; default is `FALSE`)
* imports the output from the `.omv`-file (set the parameter `getHTM = TRUE`;
  default is `FALSE`)

---

# jmvReadWrite 0.1.0

* first implementation, reads (`jmvRead`) and writes (`jmvWrite`) files using
  a file format similar to jamovi 1.2 (can be read with more recent versions)
