
current_dir = $(shell pwd)

all: ./chapter-02/README.md

./chapter-02/README.md ./chapter-02/README.R: ./chapter-02/README.Rmd
	@echo "Building Chapter  2"
	@cd chapter-02;Rscript -e "library(knitr); knit('README.Rmd', quiet = TRUE); purl('README.Rmd', quiet = TRUE)" 
	@cd ..
