bib <- "bib/pubs.bib"

all_authors <- function(author, last_first = TRUE) {
  author %>%
    stringr::str_split(" and ") %>%
    purrr::flatten_chr() %>%
    purrr::map_chr(format_author, last_first = last_first) %>%
    knitr::combine_words(and = " & ")
}

format_author <- function(author, last_first = TRUE) {
  if (author == "Accessible Teaching, Learning, & Assessment Systems") {
    return("Accessible Teaching, Learning, and Assessment Systems")
  }
  
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
  close_loc <- str_locate_all(title, "\\}") %>% 
    pluck(1) %>% 
    as_tibble() %>% 
    mutate(first = case_when(start == min(start) ~ "first"),
           last = case_when(start == max(start) ~ "last")) %>% 
    select(-end) %>% 
    pivot_longer(-start, names_to = "loc", values_to = "position") %>% 
    select(position, start) %>% 
    filter(!is.na(position)) %>% 
    deframe()
  start_loc <- str_locate_all(title, "\\{") %>% 
    pluck(1) %>% 
    as_tibble() %>% 
    mutate(first = case_when(start == min(start) ~ "first"),
           last = case_when(start == max(start) ~ "last")) %>% 
    select(-end) %>% 
    pivot_longer(-start, names_to = "loc", values_to = "position") %>% 
    select(position, start) %>% 
    filter(!is.na(position)) %>% 
    deframe()
  
  if (close_loc["first"] < start_loc["first"]) {
    title <- paste0("{", title)
  }
  if (start_loc["last"] > close_loc["last"]) {
    title <- paste0(title, "}")
  }
  
  tibble::tibble(title = title) %>%
    dplyr::mutate(title = gsub("^(.*?)\\{", "\\L\\1", title, perl = TRUE),
                  title = gsub("\\}(.*?)\\{", "\\L\\1", title, perl = TRUE),
                  title = gsub("\\}(.*)$", "\\L\\1", title, perl = TRUE),
                  title = gsub("(?<=\\: )([a-z])", "\\U\\1", title,
                               perl = TRUE)) %>% 
    dplyr::pull(title)
}

combine_eds <- function(editor) {
  knitr::combine_words(editor) %>%
    stringr::str_replace_all(" and ", " & ")
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
      "{full_author} ({year}). {title}{ifelse(is.na(titleaddon), '', paste0(' [', titleaddon, ']'))}. *{journal}{ifelse(is.na(volume), '', paste0(', ', volume))}*{ifelse(is.na(number), '', paste0('(', number, ')'))}{ifelse(is.na(pages), '', paste0(', ', pages))}. {ifelse(is.na(doi), url, paste0('https://doi.org/', doi))}"
    )
}

cite_incollection <- function(ref_info) {
  authors <- ref_info %>%
    dplyr::pull(author) %>%
    purrr::flatten_chr() %>%
    all_authors()
  
  editors <- ref_info %>%
    dplyr::pull(editor) %>%
    purrr::flatten_chr()
  
  ref_info %>%
    dplyr::mutate(full_author = authors,
                  full_editor = dplyr::case_when(length(editors) > 1 ~
                                                   paste0("In ", combine_eds(editors), " (Eds.) "),
                                                 is.na(editors) ~ "",
                                                 TRUE ~ paste0("In ", editors, " (Ed.) ")),
                  dplyr::across(where(is.character),
                                ~stringr::str_replace_all(.x, "\\{|\\}", "")),
                  edition = as.numeric(edition)) %>%
    dplyr::select(full_author, year, title, full_editor, booktitle, edition, pages,
                  publisher, doi) %>%
    glue::glue_data(
      "{full_author} ({year}). {title}. {full_editor}*{booktitle}* ({ifelse(is.na(edition), '', paste0(scales::ordinal(edition), ' ed., '))}pp. {pages}). {publisher}. https://doi.org/{doi}"
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
      "{full_author} ({year}). *{title}*{ifelse(is.na(type), '', paste0(' (', type, ' No. ', number, ')'))} [{titleaddon}]. {publisher}.{ifelse(is.na(doi), '', paste0(' https://doi.org/', doi))}"
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
        "{full_author} ({full_date}). *{title}{ifelse(is.na(subtitle), '', paste0(': ', subtitle))}* [{titleaddon}]. {eventtitle}, {venue}."
      )
  } else {
    cite <- cite_info %>%
      glue::glue_data(
        "{full_author} ({full_date}). {title}{ifelse(is.na(subtitle), '', paste0(': ', subtitle))}{ifelse(is.na(titleaddon), '', paste0(' [', titleaddon, ']'))}. In {editora} ({ratlas::rat_cap_words(editoratype)}), *{maintitle}* [{maintitleaddon}]. {eventtitle}, {venue}."
      )
  }
  
  if (!is.na(cite_info$url)) {
    final_url <- ifelse(stringr::str_detect(cite_info$url, "\\.pdf"),
                        glue("[<a href={cite_info$url}>PDF</a>]"),
                        glue("{cite_info$url}"))
    
    cite <- glue::glue("{cite} {final_url}")
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
      "{full_author} ({year}). *{full_title}* ({type} No. {number}). {publisher}."
    )
  
  if (!is.na(cite_info$doi)) {
    cite <- glue::glue("{cite} https://doi.org/{cite_info$doi}")
  } else if (!is.na(cite_info$url)) {
    cite <- glue::glue("{cite} [<a href={cite_info$url}>PDF</a>]")
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
                  title = purrr::map_chr(title, format_pkg_title))
    
  if (!url_present) {
    new_info <- tibble::add_column(new_info, url = NA_character_)
  }
  
  new_info %>%
    select(full_author, year, date, title, note, url) %>%
    mutate(url = case_when(str_detect(note, "https") ~ note,
                           TRUE ~ url),
           note = case_when(is.na(url) ~ "Internal R package",
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
