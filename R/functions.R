# TODO move
goodreads_url <- "https://www.goodreads.com"
start_url <- "https://www.goodreads.com/review/list/31076100-miha-gazvoda"
# TODO add checking robots.txt
# TODO might be good idea to show topics over time
# TODO function to get book descriptions and genres
# get_book_details <- function(book_link) {}

get_books <- function(i) {
  cat(i, "\n")
  url <- str_c(start_url, "?page=", i)
  html <- read_html(url)

  title <- html %>%
    html_nodes(".title a") %>%
    html_text(trim = TRUE)

  # TODO only returns first author
  author <- html %>%
    html_nodes(".author a") %>%
    html_text(trim = TRUE) %>%
    discard(str_detect(., "^\\("))

  rating <- html %>%
    html_nodes("[class=' staticStars notranslate']") %>%
    html_attr("title")

  # date_read <- html %>%
  #   html_nodes("span.date_read_value") %>%
  #   html_text()

  book_links <- html %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    discard(!str_detect(., "^/book/show")) %>%
    na.omit() %>%
    unique()

  description <- book_links %>%
    map_chr(get_description)

  genres <- book_links %>%
    map(get_genres)

  tibble(
    title,
    author,
    rating,
    description,
    genres
  )
}

get_description <- function(book_link) {
  Sys.sleep(0.1)

  url <- str_c(goodreads_url, book_link)
  read_html(url) %>%
    html_node("#descriptionContainer") %>%
    html_text() %>%
    trimws()
}

get_genres <- function(book_link) {
  Sys.sleep(0.1)

  url <- paste0(goodreads_url, book_link)
  html_file <- read_html(url)

  genres <- html_file %>%
    html_nodes(".elementList") %>%
    map(get_genre) %>%
    tibble()
}

# change to iteratoe over book page genre link?
get_genre <- function(html_file) {
  genre_name <- html_file %>%
    html_nodes(".left .bookPageGenreLink") %>%
    html_text()

  n_shelved <- html_file %>%
    html_nodes(".right .bookPageGenreLink") %>%
    html_text() %>%
    parse_number() %>%
    as.integer()

  if (!is.na(genre_name[3])) {
    stop("Error: Add one more level of genres!")
  }

  tibble(
    genre_category = genre_name[1],
    genre_subcategory = genre_name[2],
    n_shelved
  )
}

summarise_genres <- function(books) {
  # TODO does it make sense to filter some words? 
  
  unnested_books <- books %>%
    unnest(genres) %>%
    unnest(cols = c(.)) %>%
    mutate(
      rating = recode_rating(rating),
      genre = coalesce(genre_subcategory, genre_category)
    )

  weighted_genres <- unnested_books %>%
    filter(!is.na(rating)) %>%
    group_by(author, title) %>%
    mutate(
      p_genre = n_shelved / sum(n_shelved),
      genre_weight = p_genre * rating
    ) %>%
    group_by(genre) %>%
    summarise(weight = sum(genre_weight))

  weighted_genres
}

recode_rating <- function(rating) {
  recode(
    rating,
    "did not like it" = 1L,
    "it was ok" = 2L,
    "liked it" = 3L,
    "really liked it" = 4L,
    "it was amazing" = 5L,
    .default = NA_integer_
  )
}

plot_word_cloud <- function(weighted_genres) {
  ggplot(weighted_genres, aes(label = genre, size = weight)) +
    geom_text_wordcloud(eccentricity = 1.25) +
    theme_minimal()
}
