#' Get All Categories
#'
#' @param within Main category to search. Defaults to `categories`. Arguments can include `categories`, `cities`, `states`.
#' @param about Sub-category to search within the main category. Can include categories like `economics`, `health`, city names like `delhi` or names of states like `assam`.
#'
#' @return 	a character vector of items.
#' @export
#'
#' @examples
#'
#' get_categories()
#'
#' get_categories(within = "states")
#'
#' get_categories(within = "states", about = "assam")
#'

get_categories <- function(within = "categories", about = FALSE) {

  urls <- data.frame()

  if (within == "categories") {
    req <-
      httr::GET(
        "https://api.github.com/repos/thedivtagguy/desidatasets/git/trees/a33042a5a4baad807261d33d4e25c40bd897fca6"
      )
    httr::stop_for_status(req)

    category_tree <- httr::content(req)$tree %>%
      purrr::map_dfr(., dplyr::bind_cols) %>%
      dplyr::filter(.data$type == "tree") %>%
      dplyr::filter(.data$path == "categories") %>%
      dplyr::select(.data$url)

    categories <- httr::GET(category_tree$url)
    httr::stop_for_status(categories)

    urls <- httr::content(categories)$tree %>%
      purrr::map_dfr(., dplyr::bind_cols) %>%
      dplyr::filter(.data$type == "tree") %>%
      dplyr::select(.data$path)


  }

  else if (within != "categories" && about == FALSE) {
    req <-
      httr::GET(
        "https://api.github.com/repos/thedivtagguy/desidatasets/git/trees/a33042a5a4baad807261d33d4e25c40bd897fca6"
      )

    urls <- httr::content(req)$tree %>%
      purrr::map_dfr(., dplyr::bind_cols) %>%
      dplyr::filter(.data$type == "tree") %>%
      dplyr::filter(.data$path == within) %>%
      dplyr::select(.data$url)

    about <- httr::GET(urls$url)
    httr::stop_for_status(about)

    urls <- httr::content(about)$tree %>%
      purrr::map_dfr(., dplyr::bind_cols) %>%
      dplyr::filter(.data$type == "tree") %>%
      dplyr::select(.data$path)


  } else if (within != "categories" && about != FALSE) {
    req <-
      httr::GET(
        "https://api.github.com/repos/thedivtagguy/desidatasets/git/trees/a33042a5a4baad807261d33d4e25c40bd897fca6"
      )

    urls <- httr::content(req)$tree %>%
      purrr::map_dfr(., dplyr::bind_cols) %>%
      dplyr::filter(.data$type == "tree") %>%
      dplyr::filter(.data$path == "states") %>%
      dplyr::select(.data$url)

    about_ <- httr::GET(urls$url)
    httr::stop_for_status(about_)

    urls <- httr::content(about_)$tree %>%
      purrr::map_dfr(., dplyr::bind_cols) %>%
      dplyr::filter(.data$type == "tree") %>%
      dplyr::filter(.data$path == about) %>%
      dplyr::select(.data$url)

    within <- httr::GET(urls$url)
    httr::stop_for_status(within)

    urls <- httr::content(within)$tree %>%
      purrr::map_dfr(., dplyr::bind_cols) %>%
      dplyr::filter(.data$type == "tree") %>%
      dplyr::select(.data$path)

  }


  cat_list <- data.frame(urls$path) %>%
    dplyr::rename("categories" = "urls.path")

  return(cat_list)
}
