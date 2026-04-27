# Write files to be used with the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Write files to be used with the statistical spreadsheet 'jamovi'
(<https://www.jamovi.org>)

## Usage

``` r
write_omv(
  dtaFrm = NULL,
  fleOut = "",
  wrtPtB = FALSE,
  frcWrt = FALSE,
  retDbg = FALSE,
  vldExt = TRUE
)
```

## Arguments

- dtaFrm:

  Data frame to be exported (default: NULL)

- fleOut:

  Name / position of the output file to be generated ("FILENAME.omv";
  default: "")

- wrtPtB:

  Whether to write protocol buffers (see Details; default: FALSE)

- frcWrt:

  Whether to overwrite existing files with the same name (see Details;
  default: FALSE)

- retDbg:

  Whether to return a list with debugging information (see Value;
  default: FALSE)

- vldExt:

  Whether to validate that the file name has a correct extension
  (default: TRUE)

## Value

a list (if retDbg == TRUE), containing the meta data (mtaDta,
metadata.json in the OMV-file), the extended data (xtdDta, xdata.json in
the OMV-file) and the original data frame (dtaFrm)

## Details

- jamovi has a specific measurement level / type "ID" (in addition to
  the "standard" ones "Nominal", "Ordinal", and "Continuous"). "ID" is
  used for columns that contain some form of ID (e.g., a participant
  code). In order to set a variable of your data frame to "ID", you have
  to set the attribute `jmv-id` (e.g.,
  `attr(dtaFrm$column, "jmv-id") = TRUE`).

- CAUTION: Setting wrtPtB to TRUE currently overwrites analyses that
  already exist in a data file. It is meant to be used for
  `describe_omv` only. If you set wrtPtB to TRUE, ensure to use an
  output file name that isn't would not overwrite any existing file.
  Protocol buffers are used to exchange data between the different parts
  of jamovi (the server and the client) and also the format in which
  analyses are stored in the jamovi data files.

- `write_omv` checks whether the output file already exists and throws
  an error if this is the case. frcWrt permits you to overwrite the
  existing file.

## Examples

``` r
# use the data set "ToothGrowth" and, if it exists, write it as
# jamovi-file using write_omv()
jmvReadWrite::ToothGrowth
#>    ID supp supp2 dose dose2  len    logLen
#> 1   1   VC     1  0.5   0.5  4.2 0.6232493
#> 2   2   VC     1  0.5   0.5 11.5 1.0606978
#> 3   3   VC     1  0.5   0.5  7.3 0.8633229
#> 4   4   VC     1  0.5   0.5  5.8 0.7634280
#> 5   5   VC     1  0.5   0.5  6.4 0.8061800
#> 6   6   VC     1  0.5   0.5 10.0 1.0000000
#> 7   7   VC     1  0.5   0.5 11.2 1.0492180
#> 8   8   VC     1  0.5   0.5 11.2 1.0492180
#> 9   9   VC     1  0.5   0.5  5.2 0.7160033
#> 10 10   VC     1  0.5   0.5  7.0 0.8450980
#> 11 11   VC     1  1.0   1.0 16.5 1.2174839
#> 12 12   VC     1  1.0   1.0 16.5 1.2174839
#> 13 13   VC     1  1.0   1.0 15.2 1.1818436
#> 14 14   VC     1  1.0   1.0 17.3 1.2380461
#> 15 15   VC     1  1.0   1.0 22.5 1.3521825
#> 16 16   VC     1  1.0   1.0 17.3 1.2380461
#> 17 17   VC     1  1.0   1.0 13.6 1.1335389
#> 18 18   VC     1  1.0   1.0 14.5 1.1613680
#> 19 19   VC     1  1.0   1.0 18.8 1.2741578
#> 20 20   VC     1  1.0   1.0 15.5 1.1903317
#> 21 21   VC     1  2.0   2.0 23.6 1.3729120
#> 22 22   VC     1  2.0   2.0 18.5 1.2671717
#> 23 23   VC     1  2.0   2.0 33.9 1.5301997
#> 24 24   VC     1  2.0   2.0 25.5 1.4065402
#> 25 25   VC     1  2.0   2.0 26.4 1.4216039
#> 26 26   VC     1  2.0   2.0 32.5 1.5118834
#> 27 27   VC     1  2.0   2.0 26.7 1.4265113
#> 28 28   VC     1  2.0   2.0 21.5 1.3324385
#> 29 29   VC     1  2.0   2.0 23.3 1.3673559
#> 30 30   VC     1  2.0   2.0 29.5 1.4698220
#> 31 31   OJ     2  0.5   0.5 15.2 1.1818436
#> 32 32   OJ     2  0.5   0.5 21.5 1.3324385
#> 33 33   OJ     2  0.5   0.5 17.6 1.2455127
#> 34 34   OJ     2  0.5   0.5  9.7 0.9867717
#> 35 35   OJ     2  0.5   0.5 14.5 1.1613680
#> 36 36   OJ     2  0.5   0.5 10.0 1.0000000
#> 37 37   OJ     2  0.5   0.5  8.2 0.9138139
#> 38 38   OJ     2  0.5   0.5  9.4 0.9731279
#> 39 39   OJ     2  0.5   0.5 16.5 1.2174839
#> 40 40   OJ     2  0.5   0.5  9.7 0.9867717
#> 41 41   OJ     2  1.0   1.0 19.7 1.2944662
#> 42 42   OJ     2  1.0   1.0 23.3 1.3673559
#> 43 43   OJ     2  1.0   1.0 23.6 1.3729120
#> 44 44   OJ     2  1.0   1.0 26.4 1.4216039
#> 45 45   OJ     2  1.0   1.0 20.0 1.3010300
#> 46 46   OJ     2  1.0   1.0 25.2 1.4014005
#> 47 47   OJ     2  1.0   1.0 25.8 1.4116197
#> 48 48   OJ     2  1.0   1.0 21.2 1.3263359
#> 49 49   OJ     2  1.0   1.0 14.5 1.1613680
#> 50 50   OJ     2  1.0   1.0 27.3 1.4361626
#> 51 51   OJ     2  2.0   2.0 25.5 1.4065402
#> 52 52   OJ     2  2.0   2.0 26.4 1.4216039
#> 53 53   OJ     2  2.0   2.0 22.4 1.3502480
#> 54 54   OJ     2  2.0   2.0 24.5 1.3891661
#> 55 55   OJ     2  2.0   2.0 24.8 1.3944517
#> 56 56   OJ     2  2.0   2.0 30.9 1.4899585
#> 57 57   OJ     2  2.0   2.0 26.4 1.4216039
#> 58 58   OJ     2  2.0   2.0 27.3 1.4361626
#> 59 59   OJ     2  2.0   2.0 29.4 1.4683473
#> 60 60   OJ     2  2.0   2.0 23.0 1.3617278
nmeOut <- tempfile(fileext = ".omv")
# typically, one would use a "real" file name instead of tempfile(),
# e.g., "Data1.omv"
dtaDbg = jmvReadWrite::write_omv(dtaFrm = ToothGrowth, fleOut = nmeOut, retDbg = TRUE)
print(names(dtaDbg))
#> [1] "mtaDta" "xtdDta" "dtaFrm"
# the print-function is only used to force devtools::run_examples()
# to show output
# -> "mtaDta" "xtdDta" "dtaFrm"
# returns a list with the metadata (mtaDta, e.g., column and data type),
# the extended data (xtdDta, e.g., variable lables), and the data frame
# (dtaFrm) the purpose of these variables is merely for checking (under-
# standing the file format) and debugging

# check whether the file was written to the disk, get the file informa-
# tion (size, etc.) and delete the file afterwards
print(list.files(dirname(nmeOut), basename(nmeOut)))
#> [1] "file29042c2982a5.omv"
# -> "file[...].omv" ([...] is a combination of random numbers / characters
print(file.info(nmeOut)$size)
#> [1] 2618
# -> approx. 2600 (size may differ on different OSes)
unlink(nmeOut)

```
