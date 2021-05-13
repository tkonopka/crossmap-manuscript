# conversion of genes


#' get a table mapping approved and other gene symbols
#'
#' @param hgnc.symbols.file character, file path to an hgnc download
#'
#' @return data table with approved and alt symbols
get.hgnc.mapping <- function(hgnc.symbols.file) {
  hgnc.mapping <- fread(hgnc.symbols.file)
  setnames(hgnc.mapping, c("Approved symbol", "Previous symbols", "Alias symbols"),
           c("approved_symbol", "prev_symbols", "alias_symbols"))
  hgnc.approved <- unique(hgnc.mapping$approved_symbol)
  hgnc.mapping <- hgnc.mapping[, list(alt=c(unlist(strsplit(prev_symbols, ",")),
                                           unlist(strsplit(alias_symbols, ",")))),
                              by="approved_symbol"]
  hgnc.mapping$alt <- trimws(hgnc.mapping$alt)
  hgnc.mapping <- unique(hgnc.mapping[!(alt %in% hgnc.approved)])
  result <- rbind(data.table(approved_symbol=hgnc.approved, alt=hgnc.approved),
                 hgnc.mapping)
  result
}


#' convert a geneset to hgnc approved sets
#'
#' @param x character vector with gene symbols
#' @return character vector with hgnc approved vectors
geneset.to.hgnc <- function(x, hgnc.mapping) {
  result <- hgnc.mapping[alt %in% unlist(x)]$approved_symbol
  unique(result)
}

