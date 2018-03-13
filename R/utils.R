sealr_timestamp <- function() {
  utils::timestamp(
    stamp = Sys.Date(),
    prefix = paste0(clisymbols::symbol$info,
                    ": Created on "),
    suffix = paste0(" by the sealr package (v",
                    as.character(
                      utils::packageVersion("sealr")),
                    ")"))
}

