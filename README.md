# TBmiscr

This is a repository for R utility functions.

## Usage

```
devtools::install_github("twesleyb/TBmiscr")
```

## Creating an R package.

#### Resources:
* roxygen format: https://kbroman.org/pkg_primer/pages/docs.html
* simple tutorial: https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
* More on the roxygen format: http://r-pkgs.had.co.nz/man.html

## Write r function with roxygen format. 
* Example: https://github.com/kbroman/pkg_primer/blob/gh-pages/example/stage3/R/plot_crayons.R

```r
library(devtools)
library(roxygen2)

devtools::create("package")

# Insure you are in the new packages directory.
devtools::document()

# Install the package.
install("cats")

# The package will now be in your library directory.
# To check where your pacakges are stored:
# Check where your packages are installed.
.libPaths()

# You can source the package like normal.
library("package")

# Once the package is on github, you can install it with devtools::install_github()
install_github("kbroman/broman") #username/package

```
