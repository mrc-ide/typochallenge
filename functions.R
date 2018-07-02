read_contributions <- function(PATH_OUTPUT = "contributions") {
  d <- lapply(dir(PATH_OUTPUT, pattern = "\\.rds$"), read_contribution, PATH_OUTPUT)
  i <- !vapply(d, is.null, logical(1))
  if (any(i)) {
    d <- d[i]
    s <- do.call("rbind", d)
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
         mean_mean = mean(s[, "mean"], na.rm = TRUE))
  } else {
    NULL
  }
}


read_contribution <- function(p, PATH_OUTPUT = "contributions", cache = new.env(parent = emptyenv())) {
  if (p %in% names(cache)) {
    return(cache[[p]])
  }
  d <- readRDS(file.path(PATH_OUTPUT, p))
  i <- d$data$correct
  if (length(i) > 0L) {
    t <- d$data$elapsed[i]
    ret <- c(total = length(i),
             correct = sum(i),
             best = if (any(i)) min(t) else NA_real_,
             mean = if (any(i)) mean(t) else NA_real_)
  } else {
    ret <- NULL
  }
  cache[[p]] <- ret
  ret
}