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
  
  if (last_first) {
    last <- parts[1] %>%
      stringr::str_replace(",$", "")
    first <- parts[-1] %>%
      purrr::map_chr(function(x) {paste0(stringr::str_sub(x, 1, 1), ".")}) %>%
      paste(collapse = " ")
  } else {
    last <- parts[length(parts)]
    first <- parts[-length(parts)] %>%
      purrr::map_chr(function(x) {paste0(stringr::str_sub(x, 1, 1), ".")}) %>%
      paste(collapse = " ")
  }
  
  paste0(last, ", ", first)
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
    dplyr::pull(author) %>%
    purrr::flatten_chr() %>%
    all_authors()
  
  ref_info %>%
    dplyr::mutate(full_author = authors,
                  across(where(is.character),
                         ~stringr::str_replace_all(.x, "\\{|\\}", ""))) %>%
    glue::glue_data(
      "{full_author} ({year}). {title}. *{journal}, {volume}*, {pages}. https://doi.org/{doi}"
    )
}

cite_incollection <- function(ref_info) {
  authors <- ref_info %>%
    dplyr::pull(author) %>%
    purrr::flatten_chr() %>%
    all_authors()
  
  ref_info %>%
    dplyr::mutate(full_author = authors,
                  across(where(is.character),
                         ~stringr::str_replace_all(.x, "\\{|\\}", ""))) %>%
    glue::glue_data(
      "{full_author} ({year}). {title}. In {editor} (Ed.) *{booktitle}* (pp. {pages}). {publisher}. https://doi.org/{doi}"
    )
}

cite_thesis <- function(ref_info) {
  authors <- ref_info %>%
    dplyr::pull(author) %>%
    purrr::flatten_chr() %>%
    all_authors()
  
  ref_info %>%
    dplyr::mutate(full_author = authors,
                  across(where(is.character),
                         ~stringr::str_replace_all(.x, "\\{|\\}", ""))) %>%
    glue::glue_data(
      "{full_author} ({year}). {title} ({type} No. {number}) [{titleaddon}]. {publisher}."
    )
}

cite_presentation <- function(ref_info) {
  authors <- ref_info %>%
    dplyr::pull(author) %>%
    purrr::flatten_chr() %>%
    all_authors()
  
  dates <- ref_info %>%
    dplyr::pull(eventdate) %>%
    stringr::str_split("/") %>%
    purrr::flatten_chr() %>%
    lubridate::ymd()
  
  if (length(dates) == 1) {
    print_date <- glue::glue("{lubridate::year(dates)}, ",
                             "{lubridate::month(dates, label = TRUE, abbr = FALSE)} ",
                             "{lubridate::day(dates)}")
  } else {
    if (lubridate::month(dates[1]) == lubridate::month(dates[2])) {
      print_date <- glue::glue("{lubridate::year(dates[1])}, ",
                               "{lubridate::month(dates[1], label = TRUE, abbr = FALSE)} ",
                               "{lubridate::day(dates[1])}--{lubridate::day(dates[2])}")
    } else {
      print_date <- glue::glue("{lubridate::year(dates[1])}, ",
                               "{lubridate::month(dates[1], label = TRUE, abbr = FALSE)} ",
                               "{lubridate::day(dates[1])}--",
                               "{lubridate::month(dates[2], label = TRUE, abbr = FALSE)} ",
                               "{lubridate::day(dates[2])}")
    }
  }
  
  cite_info <- ref_info %>%
    dplyr::mutate(full_author = authors,
                  across(where(is.character),
                         ~stringr::str_replace_all(.x, "\\{|\\}", "")),
                  full_date = print_date)

  if (is.na(cite_info$maintitle))  {
    cite <- cite_info %>%
      glue::glue_data(
        "{full_author} ({full_date}). *{title}* [{titleaddon}]. {eventtitle}, {venue}."
      )
  } else {
    cite <- cite_info %>%
      glue::glue_data(
        "{full_author} ({full_date}). {title}{ifelse(is.na(titleaddon), '', paste0(' [', titleaddon, ']'))}. In {editora} ({ratlas::rat_cap_words(editoratype)}), *{maintitle}* [{maintitleaddon}]. {eventtitle}, {venue}."
      )
  }
  
  if (!is.na(cite_info$url)) {
    cite <- glue::glue("{cite} {cite_info$url}")
  }
  
  if (("addendum" %in% colnames(cite_info)) && !is.na(cite_info$addendum)) {
    cite <- glue::glue("{cite} {cite_info$addendum}")
  }
  
  cite
}

cite_report <- function(ref_info) {
  authors <- ref_info %>%
    dplyr::pull(author) %>%
    purrr::flatten_chr() %>%
    all_authors()
  
  cite_info <- ref_info %>%
    dplyr::mutate(full_author = authors,
                  across(where(is.character),
                         ~stringr::str_replace_all(.x, "\\{|\\}", "")),
                  full_title = case_when(is.na(subtitle) ~ title,
                                         TRUE ~ paste0(title, ": ", subtitle)))
  
  cite <- cite_info %>%
    glue::glue_data(
      "{full_author} ({year}). *{full_title}*. ({type} No. {number}). {publisher}."
    )
  
  if (!is.na(cite_info$doi)) {
    cite <- glue::glue("{cite} https://doi.org/{cite_info$doi}")
  } else if (!is.na(cite_info$url)) {
    cite <- glue::glue("{cite} {cite_info$url}")
  }
  
  return(cite)
}

cite_manual <- function(ref_info) {
  authors <- ref_info %>%
    dplyr::pull(author) %>%
    purrr::flatten_chr() %>%
    all_authors(last_first = FALSE)
  
  url_present <- "url" %in% colnames(ref_info)
  
  new_info <- ref_info %>%
    dplyr::mutate(full_author = authors,
                  across(where(is.character),
                         ~stringr::str_replace_all(.x, "\\{|\\}", ""))) %>%
    dplyr::mutate(title = purrr::map_chr(title, format_pkg_title))
    
  if (!url_present) {
    new_info <- add_column(new_info, url = NA_character_)
  }
  
  new_info %>%
    select(full_author, year, title, note, url) %>%
    mutate(url = case_when(str_detect(note, "https") ~ note,
                           TRUE ~ url),
           note = case_when(!is.na(url) & !str_detect(url, "CRAN") ~ NA_character_,
                            is.na(url) ~ NA_character_,
                            TRUE ~ note),
           url = map_chr(url,
                         function(u) {
                           if (is.na(u) | !str_detect(u, ",")) return(u)
                           
                           str_split(u, ",") %>%
                             flatten_chr() %>%
                             str_subset("github.com", negate = TRUE)
                         })) %>%
    glue::glue_data(
      "{full_author} ({year}). *{title}*. {ifelse(is.na(note), '', paste0(note, '.'))} {ifelse(is.na(url), '', url)}"
    )
}

format_citation <- function(type, ref_info) {
  func <- rlang::sym(paste0("cite_", tolower(type)))
  check <- rlang::call2(func, rlang::expr(ref_info))
  eval(check)
}

format_bib <- function(bib, emphasize = "Thompson, W. J.") {
  all_bib <- bib2df::bib2df(bib) %>%
    janitor::clean_names() %>%
    dplyr::mutate(category = stringr::str_to_lower(category)) %>%
    tidyr::nest(info = -c(bibtexkey, category)) %>%
    dplyr::mutate(citation = purrr::map2_chr(category, info, format_citation),
                  citation = stringr::str_replace_all(citation, emphasize, glue::glue("**{emphasize}**"))) %>%
    tidyr::unnest(cols = c(info))
  
  return(all_bib)
}
