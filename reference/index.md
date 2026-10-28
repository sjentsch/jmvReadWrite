# Package index

## Read, Write and Convert

Functions to read, write, and batch-convert jamovi (.omv) data files

- [`read_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/read_omv.md)
  :

  Read files created of the statistical spreadsheet 'jamovi'
  (<https://www.jamovi.org>)

- [`write_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/write_omv.md)
  :

  Write files to be used with the statistical spreadsheet 'jamovi'
  (<https://www.jamovi.org>)

- [`convert_to_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/convert_to_omv.md)
  :

  Convert data files (CSV, R, other statistics packages) into .omv-files
  for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

## Search and Replace

Search for or replace values in jamovi datasets

- [`replace_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/replace_omv.md)
  :

  Search values in .omv-files for the statistical spreadsheet 'jamovi'
  (<https://www.jamovi.org>)

- [`search_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/search_omv.md)
  :

  Search values in .omv-files for the statistical spreadsheet 'jamovi'
  (<https://www.jamovi.org>)

## Data Reshaping and Transformation

Functions for altering the structure, shape, and layout of your datasets

- [`long2wide_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/long2wide_omv.md)
  :

  Converts .omv-files for the statistical spreadsheet 'jamovi'
  (<https://www.jamovi.org>) from long to wide format

- [`wide2long_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/wide2long_omv.md)
  :

  Converts .omv-files for the statistical spreadsheet 'jamovi'
  (<https://www.jamovi.org>) from wide to long format

- [`transpose_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/transpose_omv.md)
  :

  Transpose .omv-files for the statistical spreadsheet 'jamovi'
  (<https://www.jamovi.org>)

## Dataset Combination and Arrangement

Functions to combine datasets, adjust column positioning, merge two
columns into one or sort data

- [`arrange_cols_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/arrange_cols_omv.md)
  :

  Re-arrange columns / variables in .omv-files for the statistical
  spreadsheet 'jamovi' (<https://www.jamovi.org>)

- [`combine_cols_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/combine_cols_omv.md)
  :

  Combines pairs of columns from a raw data matrix in .omv-files for the
  statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

- [`merge_cols_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/merge_cols_omv.md)
  :

  Merges two or more data files by adding the content of other input
  files as columns to the first input file and outputs them as files for
  the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

- [`merge_rows_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/merge_rows_omv.md)
  :

  Merges two .omv-files for the statistical spreadsheet 'jamovi'
  (<https://www.jamovi.org>) by adding the content of the second, etc.
  file(s) as rows to the first file

- [`sort_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/sort_omv.md)
  :

  Sort data (using one or more variables) in .omv-files for the
  statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

## Adding Metadata

Functions to add descriptions at the top of jamovi files or labels to
the variables

- [`describe_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/describe_omv.md)
  :

  Adds a title and a description for a data set stored as .omv-file for
  the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

- [`label_vars_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/label_vars_omv.md)
  :

  Label columns / variables in .omv-files for the statistical
  spreadsheet 'jamovi' (<https://www.jamovi.org>)

## Transform or aggregate variables

Transform (skewed) variables (to achieve a normal distribution),
aggregating data (e.g. per group) or calculate distances

- [`aggregate_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/aggregate_omv.md)
  :

  Aggregates data from a data set / frame (in long format) and returns
  them as a .omv-file for the statistical spreadsheet 'jamovi'
  (<https://www.jamovi.org>)

- [`distances_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/distances_omv.md)
  :

  Calculates distances (returning a symmetric matrix) from a raw data
  matrix in .omv-files for the statistical spreadsheet 'jamovi'
  (<https://www.jamovi.org>)

- [`transform_vars_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/transform_vars_omv.md)
  :

  Transform skewed variables (aiming at they conform to a normal
  distribution) in .omv-files for the statistical spreadsheet 'jamovi'
  (<https://www.jamovi.org>)

## Example datasets

Built-in datasets included for examples and testing.

- [`AlbumSales`](https://sjentsch.github.io/jmvReadWrite/reference/AlbumSales.md)
  : Imagine that you worked for a record company and that your boss was
  interested in predicting album sales from advertising.
- [`ToothGrowth`](https://sjentsch.github.io/jmvReadWrite/reference/ToothGrowth.md)
  : The Effect of Vitamin C on Tooth Growth in Guinea Pigs
- [`bfi_sample`](https://sjentsch.github.io/jmvReadWrite/reference/bfi_sample.md)
  : Twenty-five personality self-report items taken from the
  International Personality Item Pool
- [`bfi_sample2`](https://sjentsch.github.io/jmvReadWrite/reference/bfi_sample2.md)
  : Twenty-five personality self-report items taken from the
  International Personality Item Pool (includes jamovi-attributes;
  should result in a file identical to bfi_sample2.omv under "extdata"
  when written with write_omv)
- [`bfi_sample3`](https://sjentsch.github.io/jmvReadWrite/reference/bfi_sample3.md)
  : Twenty-five personality self-report items taken from the
  International Personality Item Pool (testing file for ordered factors
  / "Ordinal"-variables in jamovi)
