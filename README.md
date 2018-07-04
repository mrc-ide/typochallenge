## typochallenge

### Requirements:

* `shiny`
* `rworldmap`
* `rmnist`

Install with

```
install.packages(c("shiny", "shinyjs", "remotes", "uuid"))
remotes::install_github(c("richfitz/rmnist", "richfitz/thor"), upgrade = FALSE)
```

Prepare `rmnist` with

```r
rmnist::download_mnist()
```

which will download all the images.

### Run the app

```
shiny::runApp()
```

### Analyse

Contributions are in the `contributions` directory, one `rds` file per session.
