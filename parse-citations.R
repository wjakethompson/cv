bib <- "bib/conf.bib"

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

format_pkg_title <- function(title) {
  tibble::tibble(title = title) %>%
    dplyr::mutate(title = stringr::str_to_lower(title),
                  title = stringr::str_replace_all(title, " atlas", " ATLAS"),
                  title = stringr::str_replace_all(title, " dlm", " DLM"),
                  title = stringr::str_replace_all(title,
                                                   "university of kansas",
                                                   "University of Kansas"),
                  title = stringr::str_replace_all(title,
                                                   "dynamic learning maps",
                                                   "Dynamic Learning Maps")) %>%
    tidyr::separate(title, c("part1", "part2"), sep = ": ", fill = "right",
                    extra = "merge") %>%
    dplyr::mutate(part2 = paste0(stringr::str_to_upper(stringr::str_sub(part2, 1L, 1L)),
                                 stringr::str_sub(part2, 2L, -1L)),
                  title = paste0(part1, ": ", part2)) %>%
    dplyr::pull(title)
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
    dplyr::pull(author) %>%
    all_authors()
  
  ref_info %>%
    dplyr::mutate(full_author = authors) %>%
    dplyr::mutate_all(~stringr::str_replace_all(.x, "\\{|\\}", "")) %>%
    glue::glue_data(
      "{full_author} ({stringr::str_replace_all(year, '-[0-9]*$', '')}). {title}. In {editor} (Ed.) *{booktitle}* (pp. {pages}). {address}: {publisher}. doi:{doi}"
    )
}

cite_inproceedings <- function(ref_info) {
  authors <- ref_info %>%
    dplyr::pull(author) %>%
    all_authors()
  
  cite_info <- ref_info %>%
    dplyr::mutate(full_author = authors) %>%
    dplyr::mutate_all(~stringr::str_replace_all(.x, "\\{|\\}", "")) %>%
    tidyr::separate(year, c("year", "month"), convert = TRUE) %>%
    dplyr::mutate(month = month.name[month])
    
  if (is.na(cite_info$booktitle))  {
    cite <- cite_info %>%
      glue::glue_data(
        "{full_author} ({year}, {month}). *{title}*. {eventtitle}, {location}."
      )
  } else {
    cite <- cite_info %>%
      glue::glue_data(
        "{full_author} ({year}, {month}). {title}. In {editora} ({ratlas::rat_cap_words(editoratype)}), *{booktitle}*. {eventtitle}, {location}."
      )
  }
  
  cite
}

cite_techreport <- function(ref_info) {
  authors <- ref_info %>%
    dplyr::pull(author) %>%
    all_authors()
  
  ref_info %>%
    dplyr::mutate(full_author = authors) %>%
    dplyr::mutate_all(~stringr::str_replace_all(.x, "\\{|\\}", "")) %>%
    glue::glue_data(
      "{full_author} ({stringr::str_replace_all(year, '-[0-9]*$', '')}). *{title}*. ({type} No. {number}). {location}: {publisher}."
    )
}

cite_proceedings <- function(ref_info) {
  authors <- ref_info %>%
    dplyr::pull(author) %>%
    all_authors()
  
  ref_info %>%
    dplyr::mutate(full_author = authors) %>%
    dplyr::mutate_all(~stringr::str_replace_all(.x, "\\{|\\}", "")) %>%
    tidyr::separate(year, c("year", "month"), convert = TRUE) %>%
    dplyr::mutate(month = month.name[month]) %>%
    glue::glue_data(
      "{full_author} ({year}, {month}). *{title}*. {eventtitle}, {location}. {ifelse(is.na(url), '', url)}"
    )
}

cite_manual <- function(ref_info) {
  authors <- ref_info %>%
    dplyr::pull(author) %>%
    all_authors()
  
  ref_info %>%
    dplyr::mutate(full_author = authors) %>%
    dplyr::mutate_all(~stringr::str_replace_all(.x, "\\{|\\}", "")) %>%
    dplyr::mutate(title = purrr::map_chr(title, format_pkg_title)) %>%
    glue::glue_data(
      "{full_author} ({stringr::str_replace_all(year, '-[0-9]*$', '')}). *{title}*. {note}. {ifelse(is.na(url), '', url)}"
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
