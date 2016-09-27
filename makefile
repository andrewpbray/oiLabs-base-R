#!/usr/bin/env Rscript

all: make.R 

make.R: 01_intro_to_r/intro_to_r.Rmd 02_intro_to_data/intro_to_data.Rmd
	Rscript make.R
	cp ~/Dropbox/lib/oiLabs-mosaic/*/*.html docs/
