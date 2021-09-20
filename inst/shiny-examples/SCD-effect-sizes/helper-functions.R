#---------------------------------------------------------------
# parse code chunks with user-specified arguments
#---------------------------------------------------------------

parse_code_chunk <- function(chunk, args) {
  chunk_path <- system.file("shiny-examples/SCD-effect-sizes/code-chunks", paste0(chunk,".R"), package = "SingleCaseES")
  raw_code <- readLines(chunk_path)
  code_chunk <- paste(raw_code, collapse = "\n")
  glue::glue_data(.x = args, code_chunk)
}
