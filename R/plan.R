plan <- drake_plan(
  books = map_dfr(c(1:1), get_books)
)
