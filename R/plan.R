plan <- drake_plan(
  books = map_dfr(c(1:1), get_books), # TODO should website url as argument
  weighted_genres = summarise_genres(books),
  word_cloud = plot_word_cloud(weighted_genres)
)
