# styler 1.5.1.9000 (Development version)

* Files with `.Rmarkdown` extension are now recognized as an R markdown files in
  `style_file()` and friends (#824).

* Don't break line before comments in pipes (#822).

* Ordinary comments (starting with `#`) that break a roxygen code example block 
  (starting with `#'`) are now recognized and preserved (#830).

* `@examplesIf` conditions longer than one line after styling throw an error for
  compatibility with {roxygen2} (#833).
  
* R Markdown chunk headers are no longer required to be parsable R code (#832).

* Break the line between `%>%` and `{` inside and outside function calls (#825).

# styler 1.5.1

## Alignment detection

* Code with left alignment after `=` in function calls is now recognized as 
  aligned and won't be reformatted (#774, #777).
  ```
  # already detected previously
  call(
    x  = 12345,
    y2 =    17
  )
  
  # newly detected
  call(
    x  = 12345,
    y2 = 17
  )
  ```

* Similarly, left aligned after comma is now detected (#785, #786).
  ```
  # previously detected
  call(
    x  = 12345, "It's old",
    y2 = 17,      "before"
  )
  
  tribble(
    ~x,             ~y,
    "another",     1:3,
    "b",       1211234
  )
  
  # newly detected
  call(
    x = 2,           p = "another",
    y = "hhjkjkbew", x = 3
  )

  
  tribble(
    ~x,        ~y,
    "another", 1:3,
    "b",       1211234
  )
  ```
  Also see `vignette("detect-alignment")`.

# v0.2.2 (10/09/2021)

## Bug fixes and enhancements:
* bug fix in read_omv (some libraries required for syntax extraction are not available on certain platforms (Solaris, Windows with MinGW)
* added documentation (pkgdown) in docs/


# v0.2.1 (23/08/2021)

## Bug fixes and enhancements:
* bug in write_omv fixed (thanks @MAgojam, #2); jamovi could not read the manifest (meta) when the file was created with `write_omv` on Windows (LF + CR, but only CR expected / decoded)
* updated the Created-by in the manifest string to use the jmvReadWrite version
* improved the README (switched it to be generated from Rmd so that the vignette could be included


# v0.2.0 (12/07/2021)

## Enhancements:

* renamed `jmvRead` to `read_omv`, and `jmvWrite` into `write_omv`
* extracts syntax from analyses contained in the `.omv`-file (set the parameter `getSyn = TRUE`; default is `FALSE`)
* imports the output from the `.omv`-file (set the parameter `getHTM = TRUE`; default is `FALSE`)(@wch, #2355)

---

# v0.1.0 (20/09/2020)

* first implementation, reads (`jmvRead`) and writes (`jmvWrite`) files using a file format similar to jamovi 1.2 (can be read with more recent versions)
