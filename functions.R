read_contributions <- function(path_output = "contributions", cache = NULL) {
  path_cache <- file.path(path_output, ".cache.rds")
  if (file.exists(path_cache)) {
    d <- readRDS(path_cache)
  } else {
    d <- NULL
  }

  files <- dir(path_output, pattern = "\\.rds$")
  files_new <- setdiff(files, names(d))
  d <- c(
    unname(d),
    lapply(files_new, read_contribution, path_output, cache))

  i <- !vapply(d, is.null, logical(1))
  if (any(i)) {
    d <- d[i]
    s <- as.data.frame(do.call("rbind", d))
    ### need to convert numeric columns to numeric ###
    s[, "total"] <- as.numeric(as.character(s[, "total"]))
    s[, "correct"] <- as.numeric(as.character(s[, "correct"]))
    s[, "best"] <- as.numeric(as.character(s[, "best"]))
    s[, "mean"] <- as.numeric(as.character(s[, "mean"]))
    
    if (all(is.na(s[, "best"]))) {
      ## Still just testing here.
      return(NULL)
    }
    list(total_sum = as.integer(sum(s[, "total"])),
         total_mean = mean(s[, "total"]),
         correct_sum = as.integer(sum(s[, "correct"])),
         correct_mean = mean(s[, "correct"]),
         best_total = as.integer(max(s[, "total"])),
         best_correct = as.integer(max(s[, "correct"])),
         best_best = min(s[, "best"], na.rm = TRUE),
         best_mean = mean(s[, "best"], na.rm = TRUE),
         best_mean = min(s[, "mean"], na.rm = TRUE),
         mean_mean = mean(s[, "mean"], na.rm = TRUE),
         n_contributions = length(unique(s[, "id_parent"])))
  } else {
    NULL
  }
}


read_contribution <- function(p, path_output = "contributions", cache = NULL) {
  if (p %in% names(cache)) {
    return(cache[[p]])
  }
  d <- readRDS(file.path(path_output, p))
  i <- d$data$correct
  if (length(i) > 0L) {
    t <- d$data$elapsed[i]
    ret <- c(id = d$id, 
             id_parent = ifelse(is.null(d$id_parent), d$id, d$id_parent),
             total = length(i),
             correct = sum(i),
             best = if (any(i)) min(t) else NA_real_,
             mean = if (any(i)) mean(t) else NA_real_)
  } else {
    ret <- NULL
  }
  if (!is.null(cache)) {
    cache[[p]] <- ret
  }
  ret
}


build_cache <- function(path_output = "contributions") {
  path_cache <- file.path(path_output, ".cache.rds")
  unlink(path_cache)
  files <- dir(path_output, pattern = "\\.rds$")
  if (length(files) > 0L) {
    d <- lapply(files, read_contribution, path_output, NULL)
    names(d) <- files
    saveRDS(d, path_cache)
  }
}
