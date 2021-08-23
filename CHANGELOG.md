# Changelog

## v0.2.1 (23/08/2021)

#### Bug fixes and enhancements:
- bug in write_omv fixed (thanks to MAgojam); jamovi could not read the manifest (meta) when the file was created with `write_omv` on Windows (LF + CR, but only CR expected / decoded)
- updated the Created-by in the manifest string to use the jmvReadWrite version
- improved the README (switched it to be generated from Rmd so that the vignette could be included


## v0.2.0 (12/07/2021)

#### Enhancements:

- renamed `jmvRead` to `read_omv`, and `jmvWrite` into `write_omv`
- extracts syntax from analyses contained in the `.omv`-file (set the parameter `getSyn = TRUE`; default is `FALSE`)
- imports the output from the `.omv`-file (set the parameter `getHTM = TRUE`; default is `FALSE`)

---

## v0.1.0 (20/09/2020)

- first implementation, reads (`jmvRead`) and writes (`jmvWrite`) files using a file format similar to jamovi 1.2 (can be read with more recent versions)
