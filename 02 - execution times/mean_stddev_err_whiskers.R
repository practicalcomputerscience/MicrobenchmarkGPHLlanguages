# mean_stddev_err_whiskers.R
#
# 2025-05-31, 2025-06-10/15, 2025-09-28/29, 2025-10-20, 2025-11-16
#
# env: R version 4.5.0 (2025-04-11 ucrt) -- "How About a Twenty-Six"
#      Platform: x86_64-w64-mingw32/x64
#
# test: OK


library(tidyquant)  # loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR, %>%, theme_tq()
library(tibble)
library(ggplot2)


##########################
#
# user switch:
plot_type <-  0
            # 0 = Master diagram
            # 1 = Java native languages Scala, Kotlin and Clojure and their speedup with the GraalVM
            # 2 = Tested Scheme dialects
            # 3 = only virtual machine (VM) languages
            # 4 = Prolog systems for the map coloring problem of Germany-benchmark
#
##########################


sec_to_ms <- 1000  # raw data is in seconds => convert to milliseconds for better presentation here

if (plot_type < 4) {
plot_title <- paste("Microbenchmark: execution speeds ('wall clock') of a
hobby project program in different programming languages")
} else if (plot_type == 4) {
plot_title <- paste("Microbenchmark: execution speeds ('wall clock') of the
map coloring problem of Germany with 16 states and 4 colors")
}

sub_title0 <- paste("using Linux perf-stat with 20 individual runs of each program (on Ubuntu 24 LTS)")

date_time  <- paste(format(Sys.Date()), format(Sys.time(), "%H:%M"))

sub_title <- paste(sub_title0, "\nbest mean out of 3 shell command runs with perf-stat -- plot version", date_time)

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
  }

ylabel <-  paste("mean, +/- standard deviation in milliseconds")
# xlabel <- paste("programming language")
xlabel <- paste("")

# numerically sort data frame according to mean
# problem: data frame factor is not ordered! --> convert to tibble to preserve ordering!
dat1_sort <- as_tibble(dat1[order(dat1$mean),])

dat1_sort
# # A tibble: 30 × 4
#    language     mean  std_dev date 
#    <chr>       <dbl>    <dbl> <chr>
#  1 C         0.00981 0.00016  ""   
#  2 Rust      0.0106  0.000156 ""   
#  3 Crystal   0.0116  0.000445 "" 


if (plot_type < 4) {
  y_break_max = 1100  # milliseconds
  y_tick = 100  # milliseconds
} else if (plot_type == 4) {
  y_break_max = 3500
  y_tick = 200  # milliseconds
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


# end of mean_stddev_err_whiskers.R
