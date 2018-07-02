countries.txt:
	Rscript -e "writeLines(trimws(sort(rworldmap::countryExData[ , 2])), '$@')"
