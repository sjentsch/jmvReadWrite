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
