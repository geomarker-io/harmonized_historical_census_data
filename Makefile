site: *.Rmd
		R -e "rmarkdown::render(input = 'index.Rmd', output_file = 'docs/index.html', encoding = 'UTF-8')"
		open docs/index.html
