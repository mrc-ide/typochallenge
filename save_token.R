## This needs running by Rich to create an oauth token and save it
## into the vault.  It practically needs to be run interactively to do
## the auth.
vault <- vaultr::vault_client(
  "github",
  addr = "https://vault.dide.ic.ac.uk:8200")
token <- googlesheets::gs_auth(cache = FALSE)

tmp <- tempfile()
saveRDS(token, tmp)
dat <- jsonlite::base64_enc(readBin(tmp, raw(), file.size(tmp)))
unlink(tmp)

vault$write("/secret/shiny.dide/misc/googlesheets-oauth", list(token = dat))
