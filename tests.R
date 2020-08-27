#!/usr/bin/env Rscript

# Load renv.
renv::load()

# Load local TBmiscr package.
devtools::load_all()

# Convert a color's character string to its hex code.
my_color <- col2hex("purple")

# Get a random palette from colormind.
my_palette <- colormind()

# Get a function's documentation as an R object.
getRdoc("install.packages")
