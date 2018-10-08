KEY_PREFIX <- "shiny_dide:typochallenge:"
KEY_CONTRIBUTIONS <- paste0(KEY_PREFIX, "contributions")
KEY_CACHE <- paste0(KEY_PREFIX, "cache")
KEY_STATS <- paste0(KEY_PREFIX, "stats")
KEY_EMAILS <- paste0(KEY_PREFIX, "emails")

read_stats <- function(redis) {
  if (redis) {
    read_stats_redis()
  } else {
    read_stats_disk()
  }
}


read_stats_disk <- function(path_output = "contributions") {
  files <- dir(path_output, pattern = "\\.rds$")
  if (length(files) > 0L) {
    d <- lapply(files, read_contribution_disk, path_output)
    dat <- combine_contributions(d)
    build_overall_statistics(dat)
  } else {
    NULL
  }
}


read_contribution_disk <- function(p, path_output = "contributions") {
  d <- readRDS(file.path(path_output, p))
  summarise_contribution(d)
}


read_stats_redis <- function(readonly = FALSE) {
  con <- redux::hiredis()

  ## This could be done faster but with more complexity by keeping a
  ## set of seen and unseen keys and using 'SDIFF'
  d <- con$GET(KEY_CACHE)
  if (!is.null(d)) {
    d <- redux::bin_to_object(d)
  }

  ids <- list_to_character(con$HKEYS(KEY_CONTRIBUTIONS))
  ids_new <- setdiff(ids, d$id)

  if (length(ids_new) == 0L) {
    stats <- con$GET(KEY_STATS)
    if (!is.null(stats)) {
      stats <- redux::bin_to_object(stats)
    }
  } else {
    d_new <- lapply(ids_new, function(id)
      summarise_contribution(
        redux::bin_to_object(con$HGET(KEY_CONTRIBUTIONS, id))))
    d_all <- rbind(d, combine_contributions(d_new))
    stats <- build_overall_statistics(d_all)
    if (!readonly) {
      con$SET(KEY_CACHE, redux::object_to_bin(d_all))
      con$SET(KEY_STATS, redux::object_to_bin(stats))
    }
  }

  stats
}


build_overall_statistics <- function(d) {
  if (is.null(d)) {
    return(NULL)
  }
  d <- d[d$total > 0, ]
  if (nrow(d) == 0) {
    return(NULL)
  }
  list(total_sum = as.integer(sum(d$total)),
       total_mean = mean(d$total),
       total_max = max(d$total),
       correct_sum = as.integer(sum(d$correct)),
       correct_mean = mean(d$correct),
       correct_max = as.integer(max(d$correct)),
       best_total = as.integer(max(d$total)),
       best_correct = as.integer(max(d$correct)),
       best_best = min(d$best, na.rm = TRUE),
       best_mean = mean(d$best, na.rm = TRUE),
       best_mean = min(d$mean, na.rm = TRUE),
       mean_mean = mean(d$mean, na.rm = TRUE),
       correct_less_5s_sum = as.integer(sum(d$correct_less_5s)),
       correct_less_5s_mean = mean(d$correct_less_5s),
       correct_less_5s_max = max(d$correct_less_5s),
       n_contributions = length(unique(d$id_parent)))
}


summarise_contribution <- function(d) {
  i <- d$data$correct
  t <- d$data$elapsed[i]
  list(
    id = d$id,
    id_parent = if (is.null(d$id_parent)) d$id else d$id_parent,
    total = length(i),
    correct = sum(i),
    best = if (any(i)) min(t) else NA_real_,
    mean = if (any(i)) mean(t) else NA_real_,
    correct_less_5s = if (any(i)) sum(t < 5) else 0L)
}


read_string <- function(filename) {
  readChar(filename, file.size(filename))
}


update_googlesheets <- function() {
  d <- read_stats_redis(readonly = FALSE)
  googlesheets::gs_auth(token = ".googlesheets-oauth")

  if (is.null(d)) {
    n_total <- 0L
    n_contributions <- 0L
  } else {
    n_total <- d$total_sum
    n_contributions <- d$n_contributions
  }

  key <- "1EFEDot3AY4DGP0X7ZEJbOEMfBj4QpWjBmhPDeEyhrTE"
  sheet <- googlesheets::gs_key(key)
  googlesheets::gs_edit_cells(sheet, 1L, n_total, "B4")
  googlesheets::gs_edit_cells(sheet, 3L, n_contributions, "A1")
}


check_redis <- function(path = ".redis") {
  if (!file.exists(path)) {
    return(FALSE)
  }
  if (!requireNamespace("redux", quietly = TRUE)) {
    return(FALSE)
  }
  redis_url <- readLines(path)
  con <- tryCatch(redux::hiredis(url = redis_url), error = identity)
  if (inherits(con, "error")) {
    return(FALSE)
  }
  ok <- tryCatch(con$PING(), error = identity)
  if (inherits(ok, "error")) {
    return(FALSE)
  }
  Sys.setenv(REDIS_URL = redis_url)
  TRUE
}


combine_contributions <- function(d) {
  d <- d[!vapply(d, function(x) is.null(x$id), TRUE)]
  if (length(d) == 0L) {
    return(NULL)
  }
  i <- lengths(lapply(d, "[[", "id"))
  ex <- d[[1L]]
  res <- lapply(seq_along(ex), function(i) vapply(d, "[[", ex[[i]], i))
  names(res) <- names(ex)
  data.frame(res, check.names = FALSE, stringsAsFactors = FALSE)
}


list_to_character <- function(x) {
  vapply(x, identity, "")
}
