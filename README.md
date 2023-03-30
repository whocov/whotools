R Tools for WHO Analytics
================

## Installing the package

Install the up-to-date version of this package using `remotes::install_github("whocov/whotools")`.

## Coding practices

- We use roxygen2 for documentation and dependency handling:
  - For imports used in a single function, include `@importFrom pkg func` in
    the documentation section above that function. 
  - For imports used more generally, use `usethis::use_import_from("pkg",
    "func")`, which will update the general documentation file
    `whotools-package.R`.

