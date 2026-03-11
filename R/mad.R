#' Mean Absolute Difference
#'
#' @description
#' Computes the mean absolute difference from subgroup values to a specified
#' reference value (typically the overall mean).
#'
#' @details
#' The mean absolute difference (MAD) is defined as
#'
#' \deqn{
#'   MAD = \frac{1}{K} \sum_{k=1}^{K} |x_k - r|
#' }
#'
#' where \eqn{x_k} denotes the outcome value for subgroup \eqn{k},
#' \eqn{r} the reference value, and \eqn{K} is the number of subgroups.
#'
#' If a weight variable is supplied, a weighted version is computed:
#'
#' \deqn{
#'   MAD_w = \frac{\sum_{k=1}^{K} w_k |x_k - r|}
#'                 {\sum_{k=1}^{K} w_k}
#' }
#' where \eqn{w_k} represents the subgroup weights.
#'
#' If \code{reference_var} is \code{NULL}, the reference \eqn{r} is defined
#' as the overall mean of the subgroup outcome values.
#' Otherwise, the supplied reference variable is used.
#'
#' The function is designed for grouped data where each row represents a
#' subgroup. If \code{groupby} is specified, MAD is calculated separately
#' within each group defined by that variable. (e.g., countries, years, indicators)
#'
#' MAD quantifies dispersion relative to a reference and is expressed in the
#' same units as the outcome variable.
#'
#' @param data A data.frame or tibble containing the variables.
#' @param outcome_var Outcome variable (unquoted column name).
#' @param reference_var Optional reference variable. If NULL, the code uses the mean of the subgroups as the reference.
#' @param weight_var Optional weight variable (unquoted column name).
#'   If \code{NULL}, equal weights are assumed.
#' @param groupby Optional grouping variable. Use it if your data frame has multiple countries/years/indicators..
#' @importFrom dplyr summarise group_by %>%
#' @importFrom tibble as_tibble
#' @importFrom stats weighted.mean
#' @examples
#' data(example_data2)
#' mad(
#'   data = example_data2,
#'   outcome_var = r,
#'   reference_var = r_mean
#' )
#' @return
#' A tibble with the Mean Absolute Difference (MAD).
#'
#' If \code{groupby} is provided, the tibble contains one row per group.
#' Otherwise, it contains a single row with the overall MAD.

#' @export
mad <- function(data,
                outcome_var,
                reference_var = NULL,
                weight_var = NULL,
                groupby = NULL) {

  df <- data
  if (!is.data.frame(data)) stop("data must be a data.frame")

  ref_q    <- rlang::enquo(reference_var)
  outcome_q <- rlang::enquo(outcome_var)
  weight_q  <- rlang::enquo(weight_var)
  groupby_q <- rlang::enquo(groupby)

  df <- df %>%  dplyr::mutate(y = !!outcome_q)
  if (!rlang::quo_is_null(ref_q))  df <- df %>%  dplyr::mutate(ref = !!ref_q)
  if (!rlang::quo_is_null(weight_q)) df <- df %>%  dplyr::mutate(w = !!weight_q)
  if (!rlang::quo_is_null(groupby_q)) df <- df %>%  dplyr::mutate(groups = !!groupby_q)

  # set weighting variable, weight = 1 otherwise
  if (rlang::quo_is_null(weight_q)) {
    df$w <- 1
  } else {
    df$w <- ifelse(df$w == 0, .Machine$double.eps, df$w)
  }

  # Reference: from column or weighted mean
  if (rlang::quo_is_null(ref_q)) {
    if (rlang::quo_is_null(groupby_q)) {
      df <- df %>% dplyr::group_by(.data$groups) %>% dplyr::mutate(ref = stats::weighted.mean(.data$y, .data$w, na.rm = TRUE)) %>% dplyr::ungroup()
    } else {
      df <- df %>% dplyr::mutate(ref= stats::weighted.mean(.data$y, .data$w, na.rm = TRUE))
    }
  }

  if (!rlang::quo_is_null(groupby_q)) {
    mad <- df %>%
      dplyr::group_by(.data$groups) %>%
      dplyr::summarise(mad = sum(.data$w * abs(.data$y - .data$ref), na.rm = TRUE) / sum(.data$w, na.rm = TRUE), .groups = "drop")
  } else {
     mad = sum(df$w * abs(df$y - df$ref), na.rm = TRUE) / sum(df$w, na.rm = TRUE)
  }

  return(as_tibble(mad))
}
