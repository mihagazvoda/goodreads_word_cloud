source("R/packages.R")
source("R/functions.R")

# TODO change to while loop
goodreads_books <- map_dfr(c(1:1), get_books)

write_rds(goodreads_books, path = "./data/goodreads_books.rds")
# TODO do the same for authors as for topics
