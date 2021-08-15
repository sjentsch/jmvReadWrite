# Changelog

## v0.2.1 (15/08/2021)

- bug in write_jmv fixed (thanks to MAgojam); jamovi 2.0 did not open files that had jamovi-Archive-version 8.0 in the manifest
- updated the Created-by in the manifest string to use the jmvReadWrite version

## v0.2.0 (12/07/2021)

#### Enhancements:

- renamed `jmvRead` to `read_jmv`, and `jmvWrite` into `write_jmv`
- extracts syntax from analyses contained in the `.omv`-file (set the parameter `getSyn = TRUE`; default is `FALSE`)
- imports the output from the `.omv`-file (set the parameter `getHTM = TRUE`; default is `FALSE`)

```R
library(jmvReadWrite)

data = read_jmv(fleNme = system.file("extdata", "ToothGrowth.omv", package = "jmvReadWrite"), getSyn = TRUE)
# shows the syntax of the analyses from the .omv-file
attr(data, 'syntax')
# runs the command of the first analysis
eval(parse(text=attr(data, 'syntax')[[1]]))
# runs the command of the second analysis and assigns the output from that analysis to the variable result2
eval(parse(text=paste0('result2 = ', attr(data, 'syntax')[[2]])))
names(result2)
# → "main"      "assump"    "contrasts" "postHoc"   "emm" (the names of the five output tables)
```

---

## v0.1.0 (20/09/2020)

