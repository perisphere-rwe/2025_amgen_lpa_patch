#' Extends dplyr::summarize
#'
#'  The extensions are:
#'
#'  1. An overall result is always included.
#'  2. Multiple groups can be summarized over.
#'
#' either pass a grouped data frame or provide `groups` input,
#'   and this function will run summarize based on what is provided
#'   in `...` overall and in each group, separately.
#'
#' @param data a data frame
#'
#' @param ... R expressions to run in `summarize`
#'
#' @param groups character vector with data names to use as groups
#'
#' @param include_overall logical or character value. If `TRUE`, then
#'   the summary will be computed in each group as well as overall, and
#'   the overall group will be called ".overall". If `FALSE`, no overall
#'   stat is included. If `include_overall` is provided as a character value,
#'   e.g., `include_overall = "foo"`, then the summary will be computed
#'   in each group as well as overall, and the overall group will be called
#'   `"foo"`.
#'
#' @return a tibble
#'
#' @export
#'
#' @examples
#'
#' library(dplyr, quietly = TRUE)
#'
#' summarize_each_group(iris,
#'                      groups = c("Species"),
#'                      across(c(matches("Sepal|Petal")), mean))
#'
#'
#' iris %>%
#'   group_by(Species) %>%
#'   summarize_each_group(n = n())


summarize_each_group <- function(data, ..., groups = NULL, include_overall = TRUE){

  # convert to character vector
  groups <- groups %||%
    dplyr::groups(data) %>%
    purrr::map_chr(as.character)

  checkmate::assert_character(groups)

  result_groups <- vector(mode = 'list', length = length(groups)) %>%
    purrr::set_names(groups)

  for(i in seq_along(groups)){

    result_groups[[i]] <- data %>%
      dplyr::group_by(.data[[groups[i]]]) %>%
      dplyr::summarize(...) %>%
      # need a standardized name to stack results
      dplyr::rename_with(.fn = ~ ".group_level",
                         .cols = dplyr::all_of(groups[i])) %>%
      # safe to stack results if we coerce group cats to character
      dplyr::mutate(.group_level = as.character(.group_level))

  }

  result_overall <- NULL
  .overall_label <- ".overall"

  if(is.character(include_overall)){
    checkmate::assert_character(include_overall, len = 1)
    .overall_label <- include_overall
    include_overall <- TRUE
  }

  checkmate::assert_logical(include_overall,
                            len = 1,
                            null.ok = FALSE,
                            any.missing = FALSE)

  if(include_overall){

    result_overall <- dplyr::summarize(dplyr::ungroup(data), ...) %>%
      dplyr::mutate(.group_variable = .overall_label,
                    .group_level = .overall_label, .before = 1)

  }


  dplyr::bind_rows(
    result_overall,
    dplyr::bind_rows(result_groups, .id = '.group_variable')
  )

}
