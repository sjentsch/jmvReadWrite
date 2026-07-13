# Search values in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Search values in .omv-files for the statistical spreadsheet 'jamovi'
(<https://www.jamovi.org>)

## Usage

``` r
search_omv(
  dtaInp = NULL,
  srcTrm = c(),
  whlTrm = FALSE,
  ignCse = FALSE,
  incNum = TRUE,
  incOrd = TRUE,
  incNom = TRUE,
  incID = TRUE,
  incCmp = TRUE,
  incRcd = TRUE,
  ...
)
```

## Arguments

- dtaInp:

  Either a data frame or the name of a jamovi data file to be read
  (including the path, if required; "FILENAME.omv"; default: NULL)

- srcTrm:

  (Character or numeric) Vector (with length = 1) with a search term to
  be found in the data frame (default: c())

- whlTrm:

  Whether the exact search term shall be found (TRUE) or whether a
  partial match is sufficient (FALSE; default: FALSE)

- ignCse:

  Whether to ignore the case of the search term (default: FALSE)

- incNum:

  Whether to include continuous variables in the search (default: TRUE)

- incOrd:

  Whether to include ordinal variables in the search (default: TRUE)

- incNom:

  Whether to include nominal variables in the search (default: TRUE)

- incID:

  Whether to include ID variables in the search (default: TRUE)

- incCmp:

  Whether to include Computed variables in the search (default: TRUE)

- incRcd:

  Whether to include Recoded variables in the search (default: TRUE)

- ...:

  Additional arguments passed on to methods; see Details below

## Value

a named list with the places where the search term was found: names in
the list are the variables / columns, the entries the respective row
names within that variable / column (row names are used for being
tolerant to filtered-out cases in jamovi, if a filter is used, row
numbers would be incorrect)

## Details

- The ellipsis-parameter (`...`) can be used to submit arguments /
  parameters to the function that is used for reading and writing the
  data. Clicking on the respective function under “See also”, you can
  get a more detailed overview over which parameters each of those
  functions take. The functions are: `read_omv` and `write_omv` (for
  jamovi-files).

## See also

`replace_omv` uses
[`read_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/read_omv.md)
and
[`write_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/write_omv.md)
for reading and writing jamovi-files.

## Examples

``` r
# the exact value 24 appears 13 times in age
bfi_sample <- jmvReadWrite::bfi_sample
jmvReadWrite::search_omv(bfi_sample, 24, whlTrm = TRUE)
#> $age
#>  [1] "2"   "6"   "11"  "12"  "61"  "75"  "77"  "99"  "109" "169" "197" "200"
#> 
# taking the fifth entry from the search results
bfi_sample["61", "age"]
#> [1] 24
# with the following search, both Males and Females are found
# (the M of Males, wouldn't be matched if ignCse were FALSE and males is
#  only a partial match within Females, thus whlTrm must be set to FALSE)
jmvReadWrite::search_omv(bfi_sample, "males", whlTrm = FALSE, ignCse = TRUE)
#> $gender
#>   [1] "1"   "2"   "3"   "4"   "5"   "6"   "7"   "8"   "9"   "10"  "11"  "12" 
#>  [13] "13"  "14"  "15"  "16"  "17"  "18"  "19"  "20"  "21"  "22"  "23"  "24" 
#>  [25] "25"  "26"  "27"  "28"  "29"  "30"  "31"  "32"  "33"  "34"  "35"  "36" 
#>  [37] "37"  "38"  "39"  "40"  "41"  "42"  "43"  "44"  "45"  "46"  "47"  "48" 
#>  [49] "49"  "50"  "51"  "52"  "53"  "54"  "55"  "56"  "57"  "58"  "59"  "60" 
#>  [61] "61"  "62"  "63"  "64"  "65"  "66"  "67"  "68"  "69"  "70"  "71"  "72" 
#>  [73] "73"  "74"  "75"  "76"  "77"  "78"  "79"  "80"  "81"  "82"  "83"  "84" 
#>  [85] "85"  "86"  "87"  "88"  "89"  "90"  "91"  "92"  "93"  "94"  "95"  "96" 
#>  [97] "97"  "98"  "99"  "100" "101" "102" "103" "104" "105" "106" "107" "108"
#> [109] "109" "110" "111" "112" "113" "114" "115" "116" "117" "118" "119" "120"
#> [121] "121" "122" "123" "124" "125" "126" "127" "128" "129" "130" "131" "132"
#> [133] "133" "134" "135" "136" "137" "138" "139" "140" "141" "142" "143" "144"
#> [145] "145" "146" "147" "148" "149" "150" "151" "152" "153" "154" "155" "156"
#> [157] "157" "158" "159" "160" "161" "162" "163" "164" "165" "166" "167" "168"
#> [169] "169" "170" "171" "172" "173" "174" "175" "176" "177" "178" "179" "180"
#> [181] "181" "182" "183" "184" "185" "186" "187" "188" "189" "190" "191" "192"
#> [193] "193" "194" "195" "196" "197" "198" "199" "200" "201" "202" "203" "204"
#> [205] "205" "206" "207" "208" "209" "210" "211" "212" "213" "214" "215" "216"
#> [217] "217" "218" "219" "220" "221" "222" "223" "224" "225" "226" "227" "228"
#> [229] "229" "230" "231" "232" "233" "234" "235" "236" "237" "238" "239" "240"
#> [241] "241" "242" "243" "244" "245" "246" "247" "248" "249" "250" "251" "252"
#> [253] "253" "254"
#> 
# the first entry is a female, the first entry is a male
bfi_sample["1", "gender"] # Females
#> [1] Females
#> Levels: Females Males
bfi_sample["6", "gender"] # Males
#> [1] Males
#> Levels: Females Males
# using the search results assigned to a variable
srcRes <- jmvReadWrite::search_omv(bfi_sample, "males", whlTrm = FALSE, ignCse = TRUE)
bfi_sample[srcRes[[1]][1], names(srcRes[1])] # Females
#> [1] Females
#> Levels: Females Males
bfi_sample[srcRes[[1]][6], names(srcRes[1])] # Males
#> [1] Males
#> Levels: Females Males

```
