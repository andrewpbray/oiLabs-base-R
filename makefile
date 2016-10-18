#!/usr/bin/env Rscript

all: make.R 

make.R: index.Rmd 01_intro_to_r/intro_to_r.Rmd 02_intro_to_data/intro_to_data.Rmd
	Rscript make.R
	cp -fu ~/Dropbox/lib/oiLabs-mosaic/*/*.html docs/
	cp -fu ~/Dropbox/lib/oiLabs-mosaic/*/*.Rmd docs/
	cp -fu ~/Dropbox/lib/oiLabs-mosaic/*/*.css docs/
	mv ~/Dropbox/lib/oiLabs-mosaic/*.html docs/

clean:
	rm docs/*