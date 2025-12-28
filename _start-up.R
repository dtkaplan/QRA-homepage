library(mosaicCalc)
library(LSTbook)
library(ggformula)
library(dplyr)
library(gt)
library(devoirs)
library(ggplot2)
library(ggformula)

# Definitions of storybook functions
source("_software/storybook-functions.r")

# facilities for cross references in the web-site version

# Read in the index. See xref.R

load("XREFS.rda")

# Function to add a cross-reference

xf <- function(id, suppress_label = FALSE, label = NULL) {
  if (!exists("XREFS")) load("XREFS.rda", envir = parent.frame(n = 2))
  id <- tolower(id)
  id <- gsub("^@", "", id) # Remove @ just in case it was used in the ID
  this_one <- XREFS |> 
    filter(ID == id)
  text <- if (nrow(this_one) == 0) {
    paste("UNRESOLVED", id)
  } else if (nrow(this_one) > 1) {
    paste("MULTIPLE DEFS of", id)
  } else {
    if (suppress_label) {
      # Just keep the number
      gsub("\\[[a-zA-Z]* ", "[", this_one$link)
    } else {
      # if a label has been given explicitly, use it.
      if (!is.null(label)) {
        gsub("\\[.*\\]", paste0("[", label, "]"), this_one$link)
      } else { 
        this_one$link
      }
    }
  }
  
  text
}

xf_anchor <- function(ID, label="unlabelled") {
  glue::glue('[ ☜]{{id="finger-{ID}" data-label="{label}"}}')
}

xf_definition <- function(word){
  word2 <- tolower(gsub(" ", "-", word))
  # for HTML
  glue::glue('☞ **{word}**[ ☜]{{id="finger-{word2}-definition" data-label="{word}"}}')
  # for PDF
  # glue::glue('$\\Rightarrow$**{word}**[$\\Leftarrow$]{{id="finger-{word2}-definition" data-label="{word}"}}')
}

# A cross-reference to a previous definition

xf_to_def <- function(word, label = word) {
  word <- tolower(word) # Just in case
  word <- gsub(" ", "-", word)
  # if a label was given, use 
  xf(glue::glue("finger-{word}-definition"), label = label)
}

xf_new_words <- function(chapter) {
  if (!exists("XREFS")) load("XREFS.rda", envir = parent.frame(n = 2))
  chap_name <- glue::glue("Chap-{chapter}")
  refs_in_chapter <- XREFS |> dplyr::filter(grepl(chap_name, file))
  definitions <- refs_in_chapter |>
    dplyr::filter(grepl("-definition", ID))
  
  paste(paste("- [ ] ", sort(definitions$link)), collapse = "\n\n") 
}  
