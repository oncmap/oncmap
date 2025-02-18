#
debug_print <- function(text) {
  # TODO: execute when debug enabled
  if (Sys.getenv("ONCMAP_DEBUG") == "1") {
    print(text)
  }
}
