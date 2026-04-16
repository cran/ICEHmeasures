#' Concentration index (relative) and Erreygers corrected index
#'
#' Calculates the relative concentration index, a relative measure of inequality expressed as the cumulative concentration of the outcome across the ranking variable distribution
#'
#' @details
#' The Concentration Index (CI) is a relative measure of socioeconomic
#' inequality that quantifies the extent to which a health variable is
#' unequally distributed across an ordered inequality dimension.
#'
#' The CI is defined as twice the area between the concentration curve
#' and the line of equality. It can equivalently be expressed as twice
#' the covariance between the health variable and the fractional rank in
#' the socioeconomic distribution, divided by the mean of the health
#' variable. The CI ranges theoretically between -1 and 1.
#'
#' This function is designed for use with ordered dimensions such as the
#' wealth index. The ranking variable must represent a meaningful ordering
#' from the most disadvantaged to the most advantaged.
#'
#' The implementation follows the original formulation proposed by
#' Wagstaff et al. (1991). Estimation is performed using a convenient
#' regression-based approach as described by O'Donnell et al. (2008).
#' The Erreygers correction is optionally available.
#'
#' Interpretation: A positive CI indicates that the health variable is
#' concentrated among more advantaged groups, while a negative CI indicates
#' concentration among disadvantaged groups. A value of zero reflects no
#' relative socioeconomic inequality. The magnitude reflects the degree
#' of relative inequality across the entire distribution.
#'
#' @references
#' Wagstaff A, Paci P, van Doorslaer E (1991).
#' On the measurement of inequalities in health.
#' Social Science & Medicine, 33(5), 545–557.
#'
#' O'Donnell O, van Doorslaer E, Wagstaff A, Lindelow M (2008).
#' Analyzing Health Equity Using Household Survey Data:
#' A Guide to Techniques and Their Implementation.
#' The World Bank.
#'
#' Erreygers G (2009).
#' Correcting the concentration index.
#' Journal of Health Economics, 28(2), 504–515.
#'
#' @param data A data.frame or tibble containing the variables.
#' @param rank_var Ranking variable (unquoted column name; e.g. \code{wealth}).
#' @param outcome_var Outcome variable (unquoted column name).
#' @param weight_var Optional weight variable (unquoted column name).
#'   If \code{NULL}, equal weights are assumed.
#' @param cluster_var Optional cluster variable for variance estimation
#'   (unquoted column name). If provided, clustered standard errors are computed
#'   using \code{svydesign()} from the \pkg{survey} package.
#' @param corrected Logical. If \code{TRUE}, computes Erreygers corrected index.
#' @param graph Logical. If \code{TRUE}, draws the concentration curve.
#' @param quant Optional integer containing the number of quantiles to use for plotting (grouped plot). If \code{NULL}, it attempts to find the optional number of groups

#' @importFrom dplyr mutate arrange filter summarise group_by ungroup transmute select rename n %>%
#' @importFrom stats coef vcov lm
#' @importFrom ggplot2 ggplot aes geom_line geom_segment labs theme_minimal
#' @importFrom tibble as_tibble
#' @return A tibble with a single row and two columns: cix (concentration index) and cix_se (standard error).
#'  If \code{corrected=TRUE}, the tibble contains two additional columns: ccix (corrected concentration index), ccix_se (standard error).
#' @examples
#' data(example_data)
#' cixr(
#'   data = example_data,
#'   rank_var = wic,
#'   outcome_var = stunt5,
#'   weight_var = sweight,
#'   cluster_var = cluster,
#'   graph = TRUE
#' )
#' @export
cixr <- function(data,
                 rank_var,
                 outcome_var,
                 weight_var = NULL,
                 cluster_var = NULL,
                 corrected = FALSE,
                 graph = FALSE,
                 quant = NULL) {

  df <- data

  # Check inputs
  if (!is.data.frame(data)) stop("data must be a data.frame")

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

  df <- df %>%  dplyr::mutate(outcome = !!outcome_q, rank = !!rank_q)

  if (!is.numeric(df$outcome)) stop("Outcome must be numeric")

  df <- df %>% filter(!is.na(.data$rank), !is.na(.data$outcome))

  if (nrow(df) == 0) stop("No observations after removing missing values.")
  if (any(df$outcome < 0, na.rm = TRUE)) stop("Outcome must be non-negative.")

  # default weight
  if (!rlang::quo_is_null(weight_q)) {
    df$weight <- 1
  } else {
    df <- df %>%  dplyr::mutate(w = !!weight_q)
    if (!is.numeric(df$w)) stop("weight must be numeric")
    df$weight <- ifelse(df$weight == 0, .Machine$double.eps, df$weight)
  }

  # Decide if grouped or microdata based on unique ranks
  n_unique <- df %>% dplyr::distinct(.data$rank) %>% nrow()
  grouped <- (n_unique == nrow(df))


  if (grouped) {
    # --- GROUPED DATA METHOD ---
    df <- df %>% arrange(.data$rank)
    total_weight <- sum(df$weight)
    df <- df %>% mutate(wrel = .data$weight / total_weight,
                        cumwrel = cumsum(.data$wrel))


    conc_mean <- sum(df$outcome * df$wrel)
    df <- df %>% mutate(concxwrel = .data$outcome * .data$wrel,
                        concxwrel_sum = cumsum(.data$concxwrel),
                        y_i = .data$concxwrel_sum / conc_mean,
                        y_im1 = dplyr::lag(.data$y_i, default = 0),
                        x_im1 = dplyr::lag(.data$cumwrel, default = 0),
                        a_i = (.data$y_i + .data$y_im1) * (.data$cumwrel - .data$x_im1) / 2)


    area <- sum(df$a_i, na.rm = TRUE)
    cix <- 1 - 2 * area

    df <- df %>% mutate(a = (.data$outcome / conc_mean) * (2 * (dplyr::lag(.data$cumwrel, default = 0) + 0.5 * .data$wrel) - 1 - cix) + 2 - .data$y_i - .data$y_im1,
                        fa2 = .data$wrel * .data$a^2)
    fa2sum <- sum(df$fa2, na.rm = TRUE)
    cix_se <- sqrt(1 / (nrow(df) - 1) * (fa2sum - (1 + cix)^2))


    results <- list(cix = as.numeric(cix), cix_se = as.numeric(cix_se))


    if (graph) {
      g <- ggplot(df, aes(x = .data$cumwrel, y = .data$y_i)) +
        geom_line(color = "steelblue", size = 1) +
        geom_segment(x = 0, y = 0, xend = 1, yend = 1, linetype = "dashed", color = "gray40", linewidth = 0.8) +
        labs(x = "Cumulative percent ranked by economic status",
             y = "Cumulative percent",
             title = "Concentration Curve (Grouped Data)") +
        theme_minimal()
      print(g)
    }


  } else {
    # --- MICRODATA METHOD ---
    df <- df %>% arrange(.data$rank)
    total_weight <- sum(df$weight)
    df <- df %>% mutate(wi = .data$weight / total_weight,
                        cumwi = cumsum(.data$wi),
                        rank_rel = .data$cumwi - 0.5 * .data$wi)


    if (!rlang::quo_is_null(cluster_q)) {
      design <- survey::svydesign(ids = ~1, weights = ~weight, data = df)
    } else {
      df <- df %>%  dplyr::mutate(cluster = !!cluster_q)
      df$cluster <- as.factor(df$cluster)
      if (anyNA(df$cluster)) {
        stop("Cluster variable contains missing values.")
      }
      design <- survey::svydesign(ids = ~cluster, weights = ~weight, data = df)
    }


    model <- survey::svyglm(outcome ~ rank_rel, design = design)
    b <- coef(model)
    v <- vcov(model)

    # Stata-style weighted variance of rank
    w <- df$weight
    r <- df$rank_rel
    mu_r <- sum(w * r) / sum(w)
    var_rank <- sum(w * (r - mu_r)^2) / sum(w)

    # Correct denominator
    b0 <- b["(Intercept)"]
    b1 <- b["rank_rel"]
    mu_hat <- b0 + 0.5 * b1

    cix <- (2 * var_rank / mu_hat) * b1

    D <- c(
      d_b0 = -2 * var_rank * b1 / mu_hat^2,
      d_b1 =  2 * var_rank / mu_hat -
        var_rank * b1 / mu_hat^2
    )

    V <- v[c("(Intercept)", "rank_rel"),
           c("(Intercept)", "rank_rel")]

    cix_se <- .delta_method(D, V)

    results <- list(cix = as.numeric(cix), cix_se = as.numeric(cix_se))


    if (corrected) {
      min_outcome <- min(df$outcome, na.rm = TRUE)
      max_outcome <- max(df$outcome, na.rm = TRUE)
      scale <- 8 * var_rank / (max_outcome - min_outcome)
      # Point estimate
      ccix <- scale * b["rank_rel"]
      # Delta method gradient (only wrt beta_rank)
      grad_ccix <- c(rank_rel = scale)
      # Relevant vcov
      V_ccix <- v["rank_rel", "rank_rel", drop = FALSE]
      # Standard error
      ccix_se <- .delta_method(grad_ccix, V_ccix)

      results$ccix <- as.numeric(ccix)
      results$ccix_se <- as.numeric(ccix_se)
    }


    if (graph) {
      df <- df %>% mutate(cum_outcome = cumsum(.data$outcome * .data$weight),
                          cum_outcome_rel = .data$cum_outcome / max(.data$cum_outcome),
                          cum_rank_rel = .data$cumwi)


      g <- ggplot(df, aes(x = .data$cum_rank_rel, y = .data$cum_outcome_rel)) +
        geom_line(color = "steelblue", linewidth = 1) +
        geom_segment(x = 0, y = 0, xend = 1, yend = 1, linetype = "dashed", color = "gray40", linewidth = 0.8) +
        labs(x = "Cumulative proportion ranked by status", y = "Cumulative proportion of outcome",
             title = "Concentration Curve (Microdata)") +
        theme_minimal()
      print(g)
    }
  }


  message("Concentration Index: ", sprintf("%.4f", results$cix),
      " (SE = ", sprintf("%.4f", results$cix_se), ")\n")
  if (corrected && !grouped) cat("Corrected (Erreygers) Index:", sprintf("%.4f", results$ccix),
                                 "(SE =", sprintf("%.4f", results$ccix_se), ")\n")


  return(as_tibble(results))
}
# End of file
