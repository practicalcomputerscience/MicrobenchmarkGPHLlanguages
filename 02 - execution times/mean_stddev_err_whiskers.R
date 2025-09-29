# mean_stddev_err_whiskers.R
#
# 2025-05-31, 2025-06-10/15, 2025-09-28/29
#
# env: R version 4.5.0 (2025-04-11 ucrt) -- "How About a Twenty-Six"
#      Platform: x86_64-w64-mingw32/x64
#
# test: OK
#
#
#
# sources:
#   DAX volatility outlier anlysis.R
#   https://stats.stackexchange.com/questions/177628/plotting-results-having-only-mean-and-standard-deviation
#   http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/


library(tidyquant)  # loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR, %>%, theme_tq()
library(tibble)
library(ggplot2)


##########################
#
# user switch:
plot_type <-- 0
            # 0 = Master diagram   
            # 1 = Java native languages Scala, Kotlin and Clojure and their speedup with the GraalVM
            # 2 = Tested Scheme dialects
#
##########################


sec_to_ms <- 1000  # raw data is in seconds => convert to milliseconds for better presentation here

plot_title <- paste("Microbenchmark: execution speeds ('wall clock') of a
hobby project program in different programming languages")

sub_title0 <- paste("using Linux perf-stat with 20 individual runs of each program (on Ubuntu 24 LTS)")

date_time  <- paste(format(Sys.Date()), format(Sys.time(), "%H:%M"))

sub_title <- paste(sub_title0, "\nbest mean out of 3 shell command runs with perf-stat -- plot version", date_time)

setwd("e:/zzz_Scripts/R/comparison plot with only mean and standard deviations")


# dat1 class: data.frame
if (plot_type > 1) {
    dat1 <- read.csv("programming_languages_exe_speeds_Scheme.csv", header = T, dec = ",", sep = ";", comment.char = '%')
  } else if (plot_type < 1) {
    dat1 <- read.csv("programming_languages_exe_speeds.csv", header = T, dec = ",", sep = ";", comment.char = '%')
  } else {
    dat1 <- read.csv("programming_languages_exe_speeds_GraalVM.csv", header = T, dec = ",", sep = ";", comment.char = '%')
  }


ylabel <-  paste("mean, +/- standard deviation in milliseconds")
# xlabel <- paste("programming language")
xlabel <- paste("")

# numerically sort data frame according to mean
# problem: data frame factor is not ordered! --> convert to tibble to preserve ordering!
dat1_sort <- as_tibble(dat1[order(dat1$mean),])

dat1_sort
# # A tibble: 2 × 3
#   language    mean   std_dev
#   <chr>      <dbl>     <dbl>
# 1 C        0.00790 0.0000327
# 2 C3       0.0155  0.000109 


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
                 breaks = seq(0, 1100, 100)
               ) +
               
               labs(title = plot_title,
                    subtitle = sub_title,
                    x = xlabel,
                    y = ylabel) +
               
               theme(text = element_text(size = 12),
                     axis.text.x = element_text(angle = 90, size = 12))

print(bar_plot1)


# end of mean_stddev_err_whiskers.R
