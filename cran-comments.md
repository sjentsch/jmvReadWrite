## CRAN Notes - jmvReadWrite

## Current version
0.2.0

## Test environments
* ``devtools::check()`` - local: Ubuntu 20.04, R 4.1.0 (x86_64-pc-linux-gnu)
* ``rhub::check_for_cran()``
* ``rhbPlt = rhub::platforms()$name; rhub::check(platform = rhbPlt[grepl('windows-.*-devel', rhbPlt)]); rhub::check(rhbPlt[grepl('macos-.*-release-cran', rhbPlt)])``

## R CMD check results
0 errors | 0 warnings | 1 / 2 notes

* read_jmv: no visible binding for global variable ‘jamovi.coms.AnalysisResponse’
  The file defines the format of the analysis results in RProtoBuf-format.
  It is required to read / decode those analyses from the jamovi-OMV-files.
  
* Namespace in Imports field not imported from: ‘jmvcore’ (on some platforms)
  This (see above) is also the reason why jmvcore is defined as import.
