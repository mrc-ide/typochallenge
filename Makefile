all: bootstrap rmnist_data

rmnist_data:
	Rscript -e 'rmnist::download_mnist()'

bootstrap:
	Rscript -e "provisionr::provision_dependencies_bootstrap(src = provisionr::package_sources(github = 'richfitz/rmnist'))"

countries.txt:
	Rscript -e "writeLines(trimws(sort(rworldmap::countryExData[ , 2])), '$@')"

iframe.png:
	Rscript "iframe.R"

build_cache:
	Rscript build_cache

.PHONY: bootstrap build_cache iframe.png
