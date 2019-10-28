bib <- "bib/pubs.bib"

all_authors <- function(author, last_first = TRUE) {
  author %>%
    stringr::str_split(" and ") %>%
    purrr::flatten_chr() %>%
    purrr::map_chr(format_author, last_first = last_first) %>%
    knitr::combine_words(and = " & ")
}

format_author <- function(author, last_first = TRUE) {
  parts <- stringr::str_split(author, " ") %>%
    purrr::flatten_chr()
  
  last <- parts[length(parts)]
  first <- parts[-length(parts)] %>%
    purrr::map_chr(function(x) {paste0(stringr::str_sub(x, 1, 1), ".")}) %>%
    paste(collapse = " ")
  
  if (last_first) {
    paste0(last, ", ", first)
  } else {
    paste0(first, " ", last)
  }
}

cite_article <- function(ref_info) {
  authors <- ref_info %>%
    pull(author) %>%
    all_authors()
  
  ref_info %>%
    dplyr::mutate(full_author = authors) %>%
    glue::glue_data(
      "{full_author} ({stringr::str_replace_all(year, '-[0-9]*$', '')}). {title}. *{journal}, {volume}*, {pages}. doi:{doi}"
    )
}

cite_incollection <- function(ref_info) {
  authors <- ref_info %>%
    pull(author) %>%
    all_authors()
  
  ref_info %>%
    dplyr::mutate(full_author = authors) %>%
    dplyr::mutate_all(~stringr::str_replace_all(.x, "\\{|\\}", "")) %>%
    glue::glue_data(
      "{full_author} ({stringr::str_replace_all(year, '-[0-9]*$', '')}). {title}. In {editor} (Ed.) *{booktitle}* (pp. {pages}). {address}: {publisher}. doi:{doi}"
    )
}

format_citation <- function(type, ref_info) {
  func <- rlang::sym(paste0("cite_", tolower(type)))
  check <- rlang::call2(func, rlang::expr(ref_info))
  eval(check)
}

format_bib <- function(bib, emphasize = "Thompson, W. J.") {
  all_bib <- vitae::bibliography_entries(bib) %>%
    tibble::as_tibble() %>%
    tidyr::nest(info = -c(key, bibtype)) %>%
    dplyr::mutate(citation = purrr::map2_chr(bibtype, info, format_citation),
                  citation = stringr::str_replace_all(citation, emphasize, glue::glue("**{emphasize}**"))) %>%
    tidyr::unnest(cols = c(info))
  
  return(all_bib)
}
