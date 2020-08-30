plan <- drake_plan(
  books = map_dfr(c(1:8), get_books),
  weighted_genres = summarise_genres(books),
  word_cloud = plot_word_cloud(weighted_genres)
)
