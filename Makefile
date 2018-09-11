all: bootstrap rmnist_data

countries.txt:
	Rscript -e "writeLines(trimws(sort(rworldmap::countryExData[ , 2])), '$@')"

iframe.png:
	Rscript "iframe.R"

build_cache:
	Rscript build_cache

emails.txt: emails.db/data.mdb
	Rscript -e 'writeLines(sort(thor::mdb_env("emails.db")[["list"]]()), "$@")'

.PHONY: build_cache iframe.png
