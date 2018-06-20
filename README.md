## TypoChallenge

### Requirements:

* `shiny`
* `rworldmap`
* `rmnist`

Install with

```
install.packages(c("shiny", "rworldmap", "remotes"))
remotes::install_github("richfitz/rmnist")
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
