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
# v         = character: numerical column of data.frame:
#             syntax for call = df$v NO NA's !!!!
# x_label   = character: title for x-axis
# nbins     = numeric: number of bins
# col_fill  = character: fill color
# col_color = character: border color of bins
# col       = character: color of dnorm curve

hist_dnorm <- function(df, v, n_bins = 20,
                       col_fill = "gray90",
                       col_color = "black",
                       col_dnorm = "tomato",
                       x_label = "x") {
    p <- df |>
        # tidyr::drop_na(tidyselect::all_of(v)) |>
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
        ggplot2::theme_bw() +
        ggplot2::xlab(x_label)

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


################################################################
# save_data_file: Save data file for the specified chapter
# Purpose:
# If folder not exists, create it and save object as .rds file
# Author: Peter Baumgartner
# chapter_folder = character: folder inside "data" folder
#                  example "chap05"
# object = data to save
# file_name = character: example "xy_object.rds"
# I have used the function in my notes on "Statistics with R"
# # See: https://bookdown.org/pbaumgartner/swr-harris/
################################################################

save_data_file <- function(chapter_folder, object, file_name){
    data_folder <- base::paste0(here::here(), "/data/")
    if (!base::file.exists(data_folder))
    {base::dir.create(data_folder)}

    chap_folder <-
        base::paste0(
            here::here(),
            paste0("/data/", chapter_folder, "/")
        )
    if (!base::file.exists(chap_folder))
    {base::dir.create(chap_folder)}

    base::saveRDS(object = object,
                  file = paste0(chap_folder, "/", file_name))
}


################################################################
# pkgs_downloads: Get number of downloads from RStudio CRAN Mirror
# Purpose:
# Compare popularity of different packages
# Author: Peter Baumgartner
# pkgs = character vector of package names
# period = "last-day" "last-week", "last-month"
# days: period days = 1, 7, 30
# returns: tibble with packages sorted by download figures
# I have used the function in my notes on "Statistics with R"
# # See: https://bookdown.org/pbaumgartner/swr-harris/
################################################################
pkgs_dl <-  function(pkgs, period = "last-week", days = 7) {
    dl_pkgs <- cranlogs::cran_downloads(when = period, packages = pkgs)

    start_date = base::min(dl_pkgs$date)
    end_date = base::max(dl_pkgs$date)

    dl_pkgs |>
        dplyr::group_by(package) |>
        dplyr::summarize(average = trunc(sum(count) / days)) |>
        dplyr::arrange(desc(average)) |>
        dplyr::mutate(
            from = start_date,
            to = end_date
            )
}
