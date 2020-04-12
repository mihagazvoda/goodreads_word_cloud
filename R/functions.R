goodreads_url <- "https://www.goodreads.com"
start_url <- "https://www.goodreads.com/review/list/31076100-miha-gazvoda"

# TODO function to get book descriptions and genres
# TODO add function of rates
# get_book_details <- function(book_link) {}

get_books <- function(path) {
  books <- if (file.exists(path)) {
    readr::read_rds(path)
  } else {
    c(1:1) %>%
      map_dfr(get_books_from_page) %>%
      save_rds(path)
  }

  books
}

get_books_from_page <- function(i) {
  cat(i, "\n")
  url <- str_c(start_url, "?page=", i, "&shelf=read")

  html <- read_html(url)

  title <- html %>%
    html_nodes(".title a") %>%
    html_text(trim = TRUE)

  author <- html %>%
    html_nodes(".author a") %>%
    html_text(trim = TRUE) %>%
    discard(str_detect(., "^\\("))

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
    book_description,
    book_genres
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

  # TODO there can be multiple genres in one row
  genre <- html_file %>%
    html_nodes(".left .bookPageGenreLink") %>%
    html_text()

  n_shelved <- html_file %>%
    html_nodes(".right .bookPageGenreLink") %>%
    html_text() %>%
    parse_number() %>%
    as.integer()

  list(genre, n_shelved)
}

save_rds <- function(file, path) {
  readr::write_rds(file, path = path)
  invisible(file)
}
