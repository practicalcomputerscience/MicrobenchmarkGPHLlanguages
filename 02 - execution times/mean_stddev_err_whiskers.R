# mean_stddev_err_whiskers.R
#
# 2025-05-31, 2025-06-10/15, 2025-09-28/29, 2025-10-20, 2025-11-16
# 2026-01-12: print a sorted list from lowest mean to highest
# 2026-01-26: split the "master diagram" into 2 halfs for better overview
# 2026-02-01: new diagram type for web programming languages
# 2026-02-08: Dart execution forms
#
# env: R version 4.5.2 (2025-10-31 ucrt) -- "[Not] Part in a Rumble"
#      Platform: x86_64-w64-mingw32/x64
#
# test:
#


library(tidyquant)  # loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR, %>%, theme_tq()
library(tibble)
library(ggplot2)
library(webshot2)   # to save gt tables as images
library(gt)         # for gt tables for nicely formatted tables
library(dplyr)


##########################
#
# user switch:
plot_type <-  0
            # 0 = Master diagram
            # 1 = Java native languages Scala, Kotlin and Clojure and their speedup with the GraalVM
            # 2 = Tested Scheme dialects
            # 3 = only virtual machine (VM) languages
            # 4 = Prolog systems for the map coloring problem of Germany-benchmark
            # 5 = web programming languages
            # 6 = Dart execution forms
#
##########################


sec_to_ms <- 1000  # raw data is in seconds => convert to milliseconds for better presentation here

if (plot_type < 4 || plot_type == 5) {
  if (plot_type == 0) {  # "master diagram in two parts"
    plot_title <- paste("Microbenchmark: execution speeds ('wall clock') of a
hobby project program in different programming languages -- part 1/2")
  } else {
    plot_title <- paste("Microbenchmark: execution speeds ('wall clock') of a
hobby project program in different programming languages")
  }
} else if (plot_type == 4) {
    plot_title <- paste("Microbenchmark: execution speeds ('wall clock') of the
map coloring problem of Germany with 16 states and 4 colors")
} else if (plot_type == 6) {
    plot_title <- paste("Microbenchmark: execution speeds ('wall clock') of a
hobby project program in different Dart execution forms")
}


date_time  <- paste(format(Sys.Date()), format(Sys.time(), "%H:%M"))

if (plot_type < 5) {
    sub_title0 <- paste("using Linux perf-stat with 20 individual runs of each program (on Ubuntu 24 LTS)")
    sub_title <- paste(sub_title0, "\nbest mean out of 3 shell command runs with perf-stat -- plot version", date_time)    
} else if (plot_type == 5) {
    sub_title0 <- paste("using Linux multitime with 20 individual runs of each program (on Ubuntu 24 LTS)")
    sub_title <- paste(sub_title0, "\nbest mean out of 3 shell command runs with multitime -- plot version", date_time)
} else if (plot_type == 6) {
    sub_title0 <- paste("using Linux multitime with 20 individual runs of each execution form (on Ubuntu 24 LTS)")
    sub_title <- paste(sub_title0, "\nplot version", date_time)
}


setwd("e:/zzz_Scripts/R/comparison plot with only mean and standard deviations")


# dat1 class: data.frame
if (plot_type == 0) {
    dat1 <- read.csv("programming_languages_exe_speeds.csv", header = T, dec = ",", sep = ";", comment.char = '%')
  } else if (plot_type == 1) {
    dat1 <- read.csv("programming_languages_exe_speeds_GraalVM.csv", header = T, dec = ",", sep = ";", comment.char = '%')
  } else if (plot_type == 2) {
    dat1 <- read.csv("programming_languages_exe_speeds_Scheme.csv", header = T, dec = ",", sep = ";", comment.char = '%')
  } else if (plot_type == 3) {
    dat1 <- read.csv("programming_languages_exe_speeds_VM.csv", header = T, dec = ",", sep = ";", comment.char = '%')
  } else if (plot_type == 4) {
    dat1 <- read.csv("programming_languages_exe_speeds_Prolog.csv", header = T, dec = ",", sep = ";", comment.char = '%')
  } else if (plot_type == 5) {
    dat1 <- read.csv("programming_languages_exe_speeds_web_programming.csv", header = T, dec = ",", sep = ";", comment.char = '%')
  } else if (plot_type == 6) {
    dat1 <- read.csv("programming_languages_exe_speeds_Dart.csv", header = T, dec = ",", sep = ";", comment.char = '%')
  }

ylabel <-  paste("mean, +/- standard deviation in milliseconds")
# xlabel <- paste("programming language")
xlabel <- paste("")

# numerically sort data frame according to mean
# problem: data frame factor is not ordered! --> convert to tibble to preserve ordering!
dat1_sort <- as_tibble(dat1[order(dat1$mean),])

dat1_sort
#  # A tibble: 33 × 4
#     language     mean   std_dev date
#     <chr>       <dbl>     <dbl> <chr>
#   1 Zig       0.00335 0.0000609 2025-12-18
#   2 Crystal   0.00768 0.000957  2025-12-17
#   3 C         0.00784 0.000058  2025-12-17
#   4 Rust      0.00862 0.0000635 2025-12-17
#   5 Odin      0.0139  0.000136  2026-01-08
#   6 Go        0.0151  0.000118  2025-12-17
#   7 C3        0.0158  0.000138  2025-12-17
#   8 FreeBASIC 0.0172  0.000682  2025-12-17
#   9 OCaml     0.0176  0.000136  2025-12-21
#  10 V         0.0180  0.0000972 2025-12-18
#  # ℹ 23 more rows
#  # ℹ Use `print(n = ...)` to see more rows


# cut this tibble into 2 halfs for the otherwise too crowded master diagram:
if (plot_type == 0) {
  dat1_sort_len      <- nrow(dat1_sort)  # number of data rows
  dat1_sort_len_half <- ceiling(dat1_sort_len / 2)
  dat1a_sort         <- head(dat1_sort, dat1_sort_len_half)
  dat1b_sort         <- tail(dat1_sort, dat1_sort_len_half)
}


#----------------------------------------------------------------
# master diagram: make and print a nice table too:
if (plot_type == 0) {
  dat1_nice_sort <- as_tibble(dat1_sort$language)
  dat1_nice_sort$mean <- dat1_sort$mean * 1000  # for milliseconds
  dat1_nice_sort$date <- dat1_sort$date

  # rename columns:
  dat1_nice_sort <- dat1_nice_sort %>%
          rename(
            language = value,  # rename(<new> = <old>)
            'mean in milliseconds'  = mean    # rename(<new> = <old>)
          )

  dat1_nice_sort %>%
    gt() %>%
    tab_header(title = "Execution times of the master diagram") %>%
    gtsave("exe_times_of_the_master_diagram.png", zoom = 1.0)
}
#----------------------------------------------------------------



if (plot_type == 0) {
  y_break_max = 100  # milliseconds
  y_tick = 10  # milliseconds
} else if (plot_type == 4) {
  y_break_max = 3500
  y_tick = 200  # milliseconds
} else if (plot_type == 5) {
  y_break_max = 300
  y_tick = 50  # milliseconds
} else {
  y_break_max = 1100  # milliseconds
  y_tick = 100  # milliseconds
}


if (plot_type == 0) {  # master diagram": part 1/2
  dat1_sort <- dat1a_sort
}

# problem: ggplotting with x = language will sort it alphanumerically before!
# --> solution: reorder() it according to mean BEFORE!
bar_plot1 <- ggplot(dat1_sort,
               # aes(x = language)) +
               aes(x = reorder(language,  mean*sec_to_ms),
                   y =  mean*sec_to_ms)) +

               geom_errorbar(aes(ymin = (mean - std_dev)*sec_to_ms,
                                 ymax = (mean + std_dev)*sec_to_ms),
                                 width=.1) +

               geom_point(aes(y = mean*sec_to_ms)) +

               scale_y_continuous(
                 breaks = seq(0, y_break_max, y_tick)
               ) +

               labs(title = plot_title,
                    subtitle = sub_title,
                    x = xlabel,
                    y = ylabel) +

               theme(text = element_text(size = 12),
                     axis.text.x = element_text(angle = 90, size = 12))

print(bar_plot1)


####################
# make a manual copy here!!
####################


# master diagram": part 2/2
if (plot_type == 0) {
  y_break_max = 1100  # milliseconds
  y_tick      = 100  # milliseconds
  plot_title <- paste("Microbenchmark: execution speeds ('wall clock') of a
hobby project program in different programming languages -- part 2/2")

  bar_plot1b <- ggplot(dat1b_sort,
                 # aes(x = language)) +
                 aes(x = reorder(language,  mean*sec_to_ms),
                     y =  mean*sec_to_ms)) +

                 geom_errorbar(aes(ymin = (mean - std_dev)*sec_to_ms,
                                   ymax = (mean + std_dev)*sec_to_ms),
                                   width=.1) +

                 geom_point(aes(y = mean*sec_to_ms)) +

                 scale_y_continuous(
                   breaks = seq(0, y_break_max, y_tick)
                 ) +

                 labs(title = plot_title,
                      subtitle = sub_title,
                      x = xlabel,
                      y = ylabel) +

                 theme(text = element_text(size = 12),
                       axis.text.x = element_text(angle = 90, size = 12))

  print(bar_plot1b)
}


# end of mean_stddev_err_whiskers.R
