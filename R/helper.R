##########################################################
# Project: Some utility scripts for my Quarto books
# Author: Peter Baumgartner
# Edit date: January 29, 2024
##########################################################





##########################################################
# glance_data: Glance at a specified number of random data
# Purpose:
  # To prevent possible bias with head()/tail() or
  # other function that print some data excerpts
# Used in "Statistics with R"
# See: https://bookdown.org/pbaumgartner/swr-harris/
##########################################################

# df   = dataframe or tibble
# N    = number of records chosen randomly
# seed = set.seed for reproducibility

glance_data <- function(df, N = 8, seed = 42){
    df_temp <- first_and_last_row(df)

    set.seed(seed)
    df |>
        dplyr::mutate(obs = dplyr::row_number()) |>
        dplyr::relocate(obs) |>
        dplyr::slice_sample(n = N) |>
        dplyr::bind_rows(df_temp) |>
        dplyr::arrange(obs)
}

first_and_last_row <-  function(df) {
    df |>
        dplyr::mutate(obs = dplyr::row_number()) |>
        dplyr::filter(dplyr::row_number() %in% base::c(1, dplyr::n()))
}

##########################################################
# hist_dnorm: Create histogram with overlaid dnorm curve
# Purpose:
# Generate histogram from a data frame
# and overlay the density normal distribution
# Author: Peter Baumgartner
# Used in "Statistics with R"
# See: https://bookdown.org/pbaumgartner/swr-harris/
##########################################################

# df        = data.frame or tibble
# v         = numerical column of data.frame: syntax for call df$v
# x_label   = title for x-axis
# nbins     = number of bins
# col_fill  = fill color
# col_color = border color of bins
# col       = color of dnorm curve

hist_dnorm <- function(df, v, n_bins = 20,
                       col_fill = "gray90",
                       col_color = "black",
                       col_dnorm = "tomato") {
    p <- df |>
        ggplot2::ggplot(ggplot2::aes(v)) +
        ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(density)),
            bins = n_bins,
            fill = col_fill,
            color = col_color) +
        ggplot2::stat_function(fun = dnorm,
                               args = c(mean = mean(v),
                                        sd = sd(v)),
                               col = col_dnorm) +
        ggplot2::theme_bw()
    p

}
