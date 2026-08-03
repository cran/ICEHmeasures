#' SII and RII estimation using logistic regression
#'
#' Calculates the slope index for inequality (SII), an absolute measure of inequality expressed as the difference between the extremes of the ranking variable. It can also compute the relative index of inequality (RII).
#'
#' @details
#' The Slope Index of Inequality (SII) is an absolute measure of inequality
#' that represents the difference in predicted coverage between the most
#' advantaged and most disadvantaged individuals, based on the full
#' distribution of an ordered inequality dimension (e.g., wealth quintiles).
#'
#' This implementation is primarily designed for coverage indicators
#' bounded between 0 and 1 (e.g., service utilization, intervention coverage).
#' A logistic regression model is used to ensure predicted values remain
#' within the (0, 1) range.
#'
#' The function is intended for ordered dimensions such as wealth quintiles,
#' education levels, or other ranked stratification variables.
#'
#' Interpretation: A positive SII indicates higher coverage among more
#' advantaged groups, while a negative SII indicates higher coverage among
#' disadvantaged groups. An SII of zero reflects no absolute inequality.
#' The magnitude represents the absolute percentage-point difference in
#' predicted coverage between the extremes of the distribution of the inequality dimension.
#'
#' Important assumption: The SII assumes a relatively linear relationship
#' between the subgroups of the inequality dimension and the outcome of interest.
#' If the pattern of coverage across subgroups is highly non-linear, the SII may not
#' adequately summarize inequality.
#'
#' @references
#' World Health Organization (2013).
#' Handbook on Health Inequality Monitoring.
#'
#' @param data A data.frame or tibble containing the variables.
#' @param rank_var Ranking variable (unquoted column name; e.g. \code{wealth}).
#' @param outcome_var Outcome variable (unquoted column name).
#' @param weight_var Optional weight variable (unquoted column name).
#'   If \code{NULL}, equal weights are assumed.
#' @param cluster_var Optional cluster variable for variance estimation
#'   (unquoted column name). If provided, clustered standard errors are computed
#'   using \code{svydesign()} from the \pkg{survey} package.
#' @param rii Logical. If \code{TRUE}, computes the Relative Index of Inequality (RII).
#' @param graph Logical. If \code{TRUE}, draws a plot of the fitted model.
#' @return A tibble with a single row and two columns: sii (slope index of inequality) and sii_se (standard error).
#'  If \code{rii=TRUE}, the tibble contains two additional columns: rii (relative index of inequality), rii_se (standard error).
#' @examples
#' data(example_data)
#' siilogit(
#'   data = example_data,
#'   rank_var = wiq,
#'   outcome_var = stunt5,
#'   weight_var = sweight,
#'   cluster_var = cluster,
#'   rii = TRUE,
#'   graph = TRUE
#' )
#' @importFrom dplyr filter arrange mutate row_number n %>%
#' @importFrom ggplot2 ggplot geom_point geom_line labs aes scale_x_continuous scale_y_continuous
#' @importFrom survey svydesign svyglm
#' @importFrom tibble as_tibble tibble
#' @importFrom car deltaMethod
#' @importFrom stats as.formula glm.control quasibinomial weighted.mean
#' @importFrom rlang enquo
#' @export
#'
siilogit <- function(data,
                     rank_var,
                     outcome_var,
                     weight_var = NULL,
                     cluster_var = NULL,
                     rii = FALSE,
                     graph = FALSE) {

  stopifnot(is.data.frame(data))

  df <- data

  rank_q    <- rlang::enquo(rank_var)
  outcome_q <- rlang::enquo(outcome_var)
  weight_q  <- rlang::enquo(weight_var)
  cluster_q <- rlang::enquo(cluster_var)

  if (!rlang::as_name(rank_q) %in% names(df) || !rlang::as_name(outcome_q) %in% names(df)) {
    stop("Invalid outcome or rank var")
  }

  if (!rlang::quo_is_null(weight_q) &&
      !rlang::as_name(weight_q) %in% names(df)) {
    stop("Invalid weight_var")
  }
  if (!rlang::quo_is_null(cluster_q) &&
      !rlang::as_name(cluster_q) %in% names(df)) {
    stop("Invalid cluster_var")
  }

  df <- df %>%  dplyr::mutate(y = !!outcome_q, rank = !!rank_q)

  if (!is.numeric(df$y)) stop("Outcome must be numeric")

  # drop missings from rank and outcome
  df <- df %>% filter(!is.na(.data$rank), !is.na(.data$y))
  if (nrow(df) == 0) stop("No observations after removing missing values.")

  if (any(df$y > 1 | df$y < 0))  stop("Outcome is outside the 0-1 interval.")
  # replace extreme values to prevent convergence issues
  df$y <- pmin(pmax(df$y, 0.01), 0.99)

  # set weighting variable, weight = 1 otherwise
  if (rlang::quo_is_null(weight_q)) {
    df$w <- 1
  } else {
    df <- df %>%  dplyr::mutate(w = !!weight_q)
    if (!is.numeric(df$w)) stop("weight must be numeric")
    df$w <- ifelse(df$w == 0, .Machine$double.eps, df$w)
  }

  # create ranking variable
  df <- df %>%
    mutate(.row = row_number()) %>%
    arrange(.data$rank, .row) %>%
    mutate(
      rfi = .data$w / sum(.data$w),
      rfcum = cumsum(.data$rfi)) %>%
    group_by(.data$rank) %>% mutate(rkmidp = mean(.data$rfcum)) %>% ungroup() %>%
    select(-.row)

  # set survey design with cluster and/or weights
  if (!rlang::quo_is_null(cluster_q)) {
    df <- df %>%  dplyr::mutate(cluster = !!cluster_q)
    df$cluster <- as.factor(df$cluster)
    if (anyNA(df$cluster)) {
      stop("Cluster variable contains missing values.")
    }
    des <- svydesign(ids = ~cluster,
                     weights = ~w, data = df, nest = TRUE)
  } else {
    des <- svydesign(ids = ~1, weights = ~w, data = df)
  }

  # fit SII model
  fit <- svyglm(as.formula("y ~ rkmidp"),
                design = des,
                family = quasibinomial(),
                control = glm.control(maxit = 100, epsilon = 1e-10))

  # SII estimates
  sii_values  <- deltaMethod(
    object = fit,
    g = "(exp((Intercept)+rkmidp) / (1 + exp((Intercept)+rkmidp))) - (exp((Intercept)) / (1 + exp((Intercept))))"
  )
  # RII estimates
  if (rii) {
    rii_values  <- deltaMethod(
      object = fit,
      g = "(exp((Intercept)+rkmidp) / (1 + exp((Intercept)+rkmidp))) / (exp((Intercept)) / (1 + exp((Intercept))))"
    )
  }
  else {
    rii_values <- list(
      Estimate = NA,
      SE = NA
    )
  }

  if (graph) {
    fit_cat <- survey::svyglm(y ~ factor(rank), design = des,
                              family = quasibinomial(),
                              control = glm.control(maxit = 100, epsilon = 1e-8))
    df$pred_cat_link  <- stats::predict(fit_cat, newdata = df, type = "link")
    grid <- data.frame(rkmidp = seq(0,1, length.out = 200))
    grid$pred_line_link <- stats::predict(fit, newdata = grid, type = "link")

    ngroups <- length(unique(df$rank))

    if (ngroups <= 50) {
      group_df <- df %>%
        group_by(.data$rank) %>%
        summarise(rank_mean  = weighted.mean(.data$rkmidp, .data$w, na.rm = TRUE),
                  pred_link  = mean(.data$pred_cat_link, na.rm = TRUE),
                  n_obs      = n(),
                  .groups = "drop")
    } else {
      df <- df %>% mutate(rank_grp = dplyr::ntile(.data$rank, 5))
      group_df <- df %>%
        group_by(.data$rank_grp) %>%
        summarise(rank_mean = weighted.mean(.data$rkmidp, .data$w, na.rm = TRUE),
                  pred_link = mean(.data$pred_cat_link, na.rm = TRUE),
                  .groups = "drop")
    }

    y_min <- min(c(group_df$pred_link, grid$pred_line_link), na.rm = TRUE)
    y_max <- max(c(group_df$pred_link, grid$pred_line_link), na.rm = TRUE)
    padding <- 0.1 * (y_max - y_min)

    p <- ggplot() +
      geom_point(data = group_df, aes(x = .data$rank_mean, y = .data$pred_link), size = 3) +
      geom_line(data = grid, aes(x = .data$rkmidp, y = .data$pred_line_link), color = "blue") +
      labs(x = "Fractional rank", y = "Logit (linear predictor)") +
      scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) +
      scale_y_continuous(limits = c(y_min - padding, y_max + padding))
    print(p)
  }
  message("Slope Index of Inequality: ", sprintf("%.4f", sii_values$Estimate),
      " (SE = ", sprintf("%.4f", sii_values$SE), ")\n")
  if (rii) {
    message("Relative Index of Inequality: ", sprintf("%.4f", rii_values$Estimate),
        " (SE = ", sprintf("%.4f", rii_values$SE), ")\n")
  }
  if (rii) {
    return(tibble(
   sii = sii_values$Estimate,
   sii_se  = sii_values$SE,
   rii = rii_values$Estimate,
   rii_se = rii_values$SE
  ))
  } else {
    return(tibble(
      sii = sii_values$Estimate,
      sii_se  = sii_values$SE,
    ))
  }
}
