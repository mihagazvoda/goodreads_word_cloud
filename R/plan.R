plan <- drake_plan(
  books = get_books("./data/goodreads_books.rds")
)
