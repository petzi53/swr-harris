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
# Used in my personal notes on "Statistics with R"
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


################################################################
# list_plotter: Plot color list as a palette
# Purpose:
# Display different color palettes for easy comparison
# Author: Emil Hvitfeldt
# Developed for r-color-palettes and {paletteer} package
# See: https://github.com/EmilHvitfeldt/r-color-palettes/blob/main/R/list_plotter.R
# I have used it in my personal notes on "Statistics with R"
# # See: https://bookdown.org/pbaumgartner/swr-harris/
################################################################



list_plotter <- function(color_list, names, package_name) {
    par(mar = c(0, 0, 0, 0) + 0.1)

    plot(
        0,
        0,
        type = "n",
        axes = FALSE,
        bty = "n",
        xlab = "",
        ylab = "",
        xlim = c(0, 1),
        ylim = c(-length(color_list) - 1, 0)
    )

    title(package_name, line = -3)
    for (i in seq_len(length(color_list))) {
        colors_len <- length(color_list[[i]])
        breaks <- seq(from = 0,
                      to = 1,
                      length = colors_len + 1)


        text(0, -i, names[i], pos = 4)
        rect(
            xleft = breaks[1:colors_len],
            xright = breaks[1:colors_len + 1],
            ytop = -0.15 - i,
            ybottom = -0.8 - i,
            col = color_list[[i]],
            border = NA
        )
    }
}
