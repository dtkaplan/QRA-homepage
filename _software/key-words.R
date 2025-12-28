
#' @params type the filetype suffix, e.g. qmd
#' @params what - a function that takes a file name as input and returns a list or 
#' data frame giving the results from that file.

for_all_files <- function(what, type = "qmd", debug = TRUE) {
  files <- dir(pattern = paste0(type, "$"), recursive = TRUE, full.name = TRUE) 
  if (debug) files <- head(files, 3)
  lapply(files, what) |> dplyr::bind_rows()
}

# just for development purposes
countlines <- function(fname) {
  Lines <- readLines(fname)
  tibble::tibble(name = fname, nlines = length(Lines))
}

# find definitions
definitions <- function(fname) {
  Lines <- readLines(fname)
  Lines <- Lines[grepl("xf_definition\\(", Lines)]
  Lines <- unlist(strsplit(Lines, split = "`r xf_", fixed=TRUE))
  Lines <- Lines[grepl("^definition", Lines)]
  Lines <- gsub('")`.*$', "", Lines)
  Lines <- gsub("^definition\\(\"", "", Lines) |> tolower()
  # tibble::tibble(name = fname, defines = length(Lines))
  
  tibble::tibble(name = fname, definition = Lines)
}


# list of definitions and the files they come from
Defines <- for_all_files(what = definitions, debug=FALSE)

has_words <- function(fname) {
  names(Defines)[1] <- "defined_in"
  Lines <- readLines(fname) |> tolower()
  # break them up to avoid missing multiple uses on one line
  Lines <- strsplit(Lines, " and | or | the | to | from |, |\\. ")
  # start counting
  Defines <- Defines |> dplyr::mutate(count = 0)
  for (k in 1:nrow(Defines)) { # one word at a time
    search_phrase <- paste0("[^a-z]", Defines$definition[k], "[\\. \\-]")
    Defines$count[k] <- sum(grepl(search_phrase, Lines))
  }
  
  Defines |> dplyr::filter(count > 0) |> dplyr::mutate(used_in = fname)
}

Cross_uses <- for_all_files(what = has_words, debug = FALSE)

Total_uses <- Cross_uses |>
  summarize(sum(count), .by = c(definition, defined_in))
