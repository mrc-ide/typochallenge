all:

include/countries.txt:
	Rscript -e "writeLines(trimws(sort(rworldmap::countryExData[ , 2])), '$@')"

update_googlesheet:
	Rscript update_googlesheet

emails.txt: emails.db/data.mdb
	Rscript -e 'writeLines(sort(thor::mdb_env("emails.db")[["list"]]()), "$@")'

.PHONY: update_googlesheet
