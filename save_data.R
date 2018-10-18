source("R/functions.R")
if (!check_redis()) {
  stop("Can't contact redis server")
}
con <- redux::hiredis()
d <- con$HGETALL(KEY_CONTRIBUTIONS)
d <- matrix(d, 2)

dat <- lapply(d[2, ], redux::bin_to_object)
names(dat) <- vapply(d[1, ], identity, "")

saveRDS(dat, "contributions.rds")