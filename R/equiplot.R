# ============================================================================ #
# ICEH Adaptive Color Palettes
# ============================================================================ #

#' ICEH Adaptive Color Palettes
#'
#' @param type character: "wealth", "educ", "area", or "viridis".
#' @param n integer, optional. Number of colors to return. If `NULL`, the base
#'   palette is returned.
#'
#' @return A character vector of hexadecimal color codes.
#' If `n` is `NULL`, the function returns the base palette corresponding
#' to the selected `type`. If `n` is specified, the function returns
#' `n` interpolated colors generated from the base palette using
#' `grDevices::colorRampPalette()`.
#'
#' @details
#' When `type = "viridis"`, the palette is generated using
#' `viridisLite::viridis()`. The package `viridisLite` must be installed
#' to use this option.
#'
#' @export
iceh_palette <- function(type = c("wealth", "educ", "area", "viridis"), n = NULL) {
  type <- match.arg(type)

  base_cols <- switch(type,
                      "wealth" = c("#15353B", "#005866", "#46919D", "#FFDA83", "#FFB300"),
                      "educ"   = c("#515151", "#005866", "#4A7D87", "#45AABE"),
                      "area"   = c("#727376", "#A8CF46"),
                      "viridis" = NULL
  )

  if (type == "viridis") {
    if (!requireNamespace("viridisLite", quietly = TRUE)) {
      stop("Package 'viridisLite' is required for type = 'viridis'. ",
           "Please install it with install.packages('viridisLite').")
    }
    if (is.null(n)) stop("Please provide 'n' for viridis palette.")
    return(viridisLite::viridis(n))
  }

  if (is.null(n)) return(base_cols)
  return(grDevices::colorRampPalette(base_cols)(n))
}

# ============================================================================ #
# Equiplot Function
# ============================================================================ #

# ============================================================================ #
#' Equiplot Function
#'
#' The function creates an equiplot graph to visualize disaggregated health indicator estimates
#' across population subgroups defined by an inequality dimension.
#'
#'
#' An equiplot is a graphical tool used to display disaggregated estimates
#' of a health indicator across population subgroups defined by an inequality
#' dimension (e.g., wealth quintile, education level, place of residence).
#' It provides a clear visual representation of absolute differences between subgroups
#' and facilitates the identification of inequality patterns.
#'
#' It supports both **long** and **wide** data formats.
#' In the wide format, all columns except the grouping variable are assumed to be
#' stratification variables and are automatically reshaped into long format.
#'
#' The outcome scale is controlled through the `proportion` argument.
#' When `proportion = TRUE`, outcomes expressed as proportions (0–1)
#' are converted to percentages (0–100). The default (`proportion = FALSE`)
#' keeps the original outcome scale, enabling use with proportions,
#' percentages, rates, or counts.
#'
#' Interpretation: Each point represents the outcome value for a specific subgroup of the stratifier variable (e.g., wealth quintiles, place of residence).
#' The distance between points reflects the absolute inequality between these subgroups, the greater the distance, the larger the disparity.
#' Equiplots facilitate visual comparison of inequality patterns across multiple groups simultaneously.
#'
#' @param data A data.frame containing the data.
#' @param group_var Unquoted column name for the grouping variable (y-axis).
#' @param outcome_var Unquoted column name for the outcome variable (x-axis).
#' @param strat_var Unquoted column name for the stratifier (color groups).
#' @param wide Logical. If TRUE, assumes each stratum is in a separate column.
#' @param palette "wealth", "educ", "area", or "viridis".
#' @param order "alphabetical", "ascending", or "descending".
#' @param order_ref Specific stratum used to order groups.
#' @param proportion Logical. If TRUE converts outcome from 0–1
#'   proportions to percentages. Default = FALSE.
#' @param xlim Numeric vector of x-axis limits.
#' @param point_size Size of points.
#' @param line_color Line color connecting strata.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param legend_title Legend title.
#' @return A ggplot object representing an Equiplot.
#'   Because the function returns a standard ggplot object, users can further
#'   customize the Equiplot by adding layers and adjustments using the `+`
#'   operator (e.g., themes, scales, labels, or annotations).
#'
#'
#' @examples
#'
#' # Example 1: 5 Wealth Quintiles, Wide Format, Sorted by "Poorest" Descending
#' # Goal: Highlight countries with the best results for their lowest quintile
#' # Values already expressed as percentages (0–100)
#'
#' df_wealth <- data.frame(
#'   country = c("Angola", "Brazil", "Vietnam", "Peru", "Egypt"),
#'   Poorest = c(10, 20, 45, 15, 35),
#'   Q2 = c(25, 35, 55, 30, 45),
#'   Q3 = c(40, 50, 65, 45, 60),
#'   Q4 = c(60, 70, 80, 65, 75),
#'   Richest = c(85, 90, 95, 85, 92)
#' )
#'
#' equiplot(
#'   df_wealth,
#'   country,
#'   wide = TRUE,
#'   palette = "wealth",
#'   order = "descending",
#'   order_ref = "Poorest",
#'   proportion = FALSE,
#'   xlab = "DTP3 Coverage (%)",
#'   legend_title = "Wealth Quintile"
#' )
#'
#'
#' # Example 2: Education Categories, Long Format
#' # Goal: Example using proportions (0–1) converted automatically to %
#'
#' df_educ <- data.frame(
#'   country = rep(c("Zambia", "Bolivia", "Albania"), each = 3),
#'   education = rep(c("None", "Primary", "Secondary+"), 3),
#'   value = c(0.60, 0.75, 0.90,
#'             0.40, 0.60, 0.85,
#'             0.80, 0.85, 0.95)
#' )
#'
#' equiplot(
#'   df_educ,
#'   country,
#'   value,
#'   education,
#'   palette = "educ",
#'   order = "alphabetical",
#'   proportion = TRUE,
#'   xlab = "Antenatal care 4+ visits (%)"
#' )
#'
#'
#' # Example 3: Urban vs Rural (Area), Wide Format
#' # Goal: Identify the lowest overall coverage
#' # Values already expressed as percentages
#'
#' df_area <- data.frame(
#'   region = c("North", "South", "East", "West"),
#'   Rural = c(30, 55, 20, 45),
#'   Urban = c(60, 75, 50, 65)
#' )
#'
#' equiplot(
#'   df_area,
#'   region,
#'   wide = TRUE,
#'   palette = "area",
#'   order = "ascending",
#'   proportion = FALSE,
#'   xlab = "Outcome (%)",
#'   legend_title = "Residence"
#' )
#'
#' @importFrom dplyr where
#' @importFrom rlang .data
#'
#' @export
#'
equiplot <- function(
    data,
    group_var,
    outcome_var = NULL,
    strat_var = NULL,
    wide = FALSE,
    palette = "wealth",
    order = c("alphabetical", "ascending", "descending"),
    order_ref = NULL,
    proportion = FALSE,
    xlim = NULL,
    point_size = 4,
    line_color = "black",
    xlab = "Outcome",
    ylab = "",
    legend_title = "Stratifier"
) {

  order <- match.arg(order)

  # -------------------------
  # 1. DATA PREP
  # -------------------------
  if (wide) {

    group_col <- rlang::ensym(group_var)

    data_plot <- data %>%
      dplyr::select(!!group_col, dplyr::where(is.numeric)) %>%
      tidyr::pivot_longer(
        cols = -!!group_col,
        names_to = "str_internal",
        values_to = "out_internal"
      ) %>%
      dplyr::rename(grp_internal = !!group_col)

  } else {

    grp_n <- rlang::as_string(rlang::ensym(group_var))
    str_n <- rlang::as_string(rlang::ensym(strat_var))
    out_n <- rlang::as_string(rlang::ensym(outcome_var))

    data_plot <- data %>%
      dplyr::transmute(
        grp_internal = as.character(.data[[grp_n]]),
        str_internal = as.character(.data[[str_n]]),
        out_internal = as.numeric(.data[[out_n]])
      )
  }

  # -------------------------
  # 2. PROPORTION CONVERSION
  # -------------------------
  if (isTRUE(proportion)) {
    data_plot$out_internal <- data_plot$out_internal * 100
  }

  # -------------------------
  # 3. ORDER GROUPS
  # -------------------------
  if (order == "alphabetical") {

    ordered_levels <- rev(sort(unique(data_plot$grp_internal)))

  } else {

    if (!is.null(order_ref)) {

      order_data <- data_plot %>%
        dplyr::filter(.data$str_internal == order_ref)

      if (nrow(order_data) == 0) {
        stop("order_ref category not found in stratifier.")
      }

    } else {

      order_data <- data_plot %>%
        dplyr::group_by(.data$grp_internal) %>%
        dplyr::summarise(
          out_internal = mean(.data$out_internal, na.rm = TRUE),
          .groups = "drop"
        )
    }

    ordered_levels <- order_data %>%
      dplyr::arrange(
        if (order == "descending")
          dplyr::desc(.data$out_internal)
        else
          .data$out_internal
      ) %>%
      dplyr::pull(.data$grp_internal)
  }

  data_plot$grp_internal <-
    factor(data_plot$grp_internal, levels = ordered_levels)

  # -------------------------
  # 4. COLORS
  # -------------------------
  n_strata <- length(unique(data_plot$str_internal))
  colors_vec <- iceh_palette(palette, n_strata)

  # -------------------------
  # 5. X SCALE
  # -------------------------

  if (!is.null(xlim)) {

    scale_x <- ggplot2::scale_x_continuous(
      limits = xlim,
      expand = ggplot2::expansion(mult = c(0, 0.05))
    )

  } else if (isTRUE(proportion)) {

    # proportions shown as percentages
    scale_x <- ggplot2::scale_x_continuous(
      limits = c(0, 100),
      expand = ggplot2::expansion(mult = c(0, 0))
    )

  } else {

    # counts or rates
    scale_x <- ggplot2::scale_x_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    )
  }

  # -------------------------
  # 6. PLOT
  # -------------------------
  ggplot2::ggplot(
    data_plot,
    ggplot2::aes(
      x = .data$out_internal,
      y = .data$grp_internal,
      color = .data$str_internal,
      group = .data$grp_internal
    )
  ) +
    ggplot2::geom_line(color = line_color, alpha = 0.6) +
    ggplot2::geom_point(size = point_size) +
    scale_x +
    ggplot2::scale_color_manual(values = colors_vec) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      color = legend_title
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey90"),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black")
    )
}
