## typochallenge

### Requirements:

* `shiny`
* `rworldmap`
* `rmnist`

Install with

```
install.packages(c("shiny", "shinyjs", "remotes", "thor", "uuid"))
remotes::install_github(c("mrc-ide/rmnist"), upgrade = FALSE)
```

Prepare `rmnist` with

```r
rmnist::download_mnist(cache_dir = ".rmnist", verbose = TRUE)
```

which will download all the images.

### Run the app

```
shiny::runApp()
```

### Analyse

Contributions are in the `contributions` directory, one `rds` file per session.
