download_data <- function(name, ...) {
  about_ <- about
  within_ <- within
  name <- "dd-sportf-aman-2342"
  message("Fetching datasets...")
  # Function to take in JSON and return a dataframe
  jsonReader <- function(json_link) {
    json_file <- json_link[[1]]
    x <- jsonlite::fromJSON(json_file) %>%
      tibble::as_tibble() %>%
      dplyr::select(-.data$id) %>%
      unique()
    return(x)
  }

  files <-
    httr::GET(
      "https://api.github.com/repos/thedivtagguy/desidatasets/git/trees/master?recursive=1"
    )
  httr::stop_for_status(files)

  # Filter only DESCRIPTION and CSV Files
  fileList <- httr::content(files)$tree %>%
    purrr::map_dfr(., dplyr::bind_cols) %>%
    dplyr::filter(.data$type == "blob") %>%
    dplyr::mutate(
      dataset_category = dplyr::case_when(
        grepl('categories', .data$path) ~ 'Categories',
        grepl('states', .data$path) ~ 'States',
        grepl('cities', .data$path) ~ 'Cities',
      )
    ) %>%
    dplyr::mutate(
      download_url = paste(
        "https://raw.githubusercontent.com/thedivtagguy/desidatasets/master/",
        .data$path,
        sep = ""
      )
    ) %>%
    dplyr::select(-.data$mode,
                  -.data$type,
                  -.data$sha,
                  -.data$size,
                  -.data$url) %>%
    dplyr::mutate(
      file_type = dplyr::case_when(
        grepl('.csv', .data$path) ~ 'CSV',
        grepl('DESCRIPTION', .data$path) ~ 'DESCRIPTION',
        grepl('DICTIONARY', .data$path) ~ 'DICTIONARY'

      ),
      id = stringr::str_extract(.data$path, "dd-([^/]+)")
    ) %>%
    dplyr::filter(.data$id == name) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$id) %>%
    dplyr::select(.data$download_url, .data$id) %>%
    tidyr::nest()

  urls <- fileList$data[[1]][[1]]

  download.file(urls[1])


  # descriptions <- fileList %>%
  #   # dplyr::filter(.data$file_type == "DESCRIPTION") %>%
  #  # dplyr::select(.data$download_url, .data$id) %>%
  #   dplyr::group_by(.data$id) %>%
  #   tidyr::nest() %>%
  #   dplyr::mutate(desc = purrr::map(.data$data, jsonReader)) %>%
  #   dplyr::select(-.data$data) %>%
  #   tidyr::unnest(.data$desc) %>%
  #   dplyr::mutate(
  #     within = stringr::str_to_title(.data$category),
  #     about = stringr::str_to_title(.data$about)
  #   ) %>%
  #   dplyr::select(-.data$category) %>%
  #   dplyr::filter(.data$id == name)

}
