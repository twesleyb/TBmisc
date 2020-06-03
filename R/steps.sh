#!/usr/bin/env bash

# Steps to create a reproducible research environment.
intit-env TBmiscR # Name of new environment.

# Activate the environment.
ca TBmiscR

# Install r-renv with conda.
conda install -c conda-forge r-renv 
# respond yes to the prompt.

# Launch R.
R

# renv should already be installed!
renv::init()

# Export the new conda/R environment.
export-env # assumes you want to export currently active env.

# Launch R, install devtools.


