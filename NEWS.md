# jmvReadWrite 0.3.0

## Bug fixes and enhancements:
* added a couple of helper functions:
  `convert_to_omv` (converts data sets from other formats - CSV, R, SPSS, SAS, Stata) to `.omv` 
  `merge_cols_omv` (merges two or more data sets by concatenating columns)
  `merge_rows_omv` (merges two or more data sets by concatenating rows)
  `long2wide_omv` (converts data sets from long to wide, e.g., for running mixed-model-analyses in jamovi)
  `wide2long_omv` (converts data sets from wide to long, e.g., for running mixed-model-analyses in jamovi)
  `sort_omv` (sorts the dataset according to one or more variable)
* implemented unit tests and code coverage for `read_omv`, `write_omv`, `convert_to_omv`, `merge_cols_omv`, `merge_rows_omv`, `long2wide_omv`, `wide2long_omv`, `sort_omv`
* implemented treating variables in `read_omv` and `write_omv` as ordered factor if `measureType` has the value `Ordinal`
* default for `sveAtt` in `read_omv` (now `TRUE`; it makes more sense to store this attributes be default since `write_omv` will give you an exact copy of the original data set if they are stored and the 
  helper functions above also respect and adjust them)


# jmvReadWrite 0.2.4

## Bug fixes and enhancements:
* fixed an error when assembling the file name in `write_omv`, added assembling the file name with `normalizePath` to `read_omv`
* fixed missing retDbg-parameter in one of the examples


# jmvReadWrite 0.2.3

## Bug fixes and enhancements:
* use `zip` R-package instead of `utils` to prevent that no ZIP-executable-file is found on Windows (`utils::zip` only works in cases where a `zip.exe` is found on the path)
* use the (session-specific) temporary directory for creating files to be zipped and those files extracted
* improved handling of the different variable types, implemented logical / boolean variables / data columns
* improved handling of column attributes
* added parameter `retDbg` (default: `FALSE`) to write_omv so that output for debugging is only produced upon setting it 


# jmvReadWrite 0.2.2

## Bug fixes and enhancements:
* bug fix in read_omv (some libraries required for syntax extraction are not available on certain platforms (Solaris, Windows with MinGW)
* added documentation (pkgdown) in docs/
* set up examples and lintr


# jmvReadWrite 0.2.1

## Bug fixes and enhancements:
* bug in write_omv fixed (@MAgojam, #2); jamovi could not read the manifest (meta) when the file was created with `write_omv` on Windows (LF + CR, but only CR expected / decoded)
* updated the Created-by in the manifest string to use the jmvReadWrite version
* improved the README (switched it to be generated from Rmd so that the vignette could be included


# jmvReadWrite 0.2.0

## Enhancements:

* renamed `jmvRead` to `read_omv`, and `jmvWrite` into `write_omv`
* extracts syntax from analyses contained in the `.omv`-file (set the parameter `getSyn = TRUE`; default is `FALSE`)
* imports the output from the `.omv`-file (set the parameter `getHTM = TRUE`; default is `FALSE`)

---

# jmvReadWrite 0.1.0

* first implementation, reads (`jmvRead`) and writes (`jmvWrite`) files using a file format similar to jamovi 1.2 (can be read with more recent versions)
