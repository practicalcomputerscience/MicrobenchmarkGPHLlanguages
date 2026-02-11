# mean_stddev_err_whiskers_recursive_Fibonacci_numbers.R
#
# 2026-02-11
#
# env: R version 4.5.2 (2025-10-31 ucrt) -- "[Not] Part in a Rumble"
#      Platform: x86_64-w64-mingw32/x64
#
# test:


library(tidyquant)  # loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR, %>%, theme_tq()
library(tibble)
library(ggplot2)


##########################
#
# user switch:

#
##########################


sec_to_ms <- 1.0  # no conversion to milliseconds here

plot_title <- paste("Microbenchmark: execution speeds ('wall clock') of virtual machines and
speed effects of interpretation, just-in-time and ahead-of-time compilation")

sub_title0 <- paste("when calculating the 47th Fibonacci number in an inefficient (recursive) manner")

date_time  <- paste(format(Sys.Date()), format(Sys.time(), "%H:%M"))

sub_title <- paste(sub_title0, "\none run of each program (on Ubuntu 24 LTS) -- plot version", date_time)

setwd("e:/zzz_Scripts/R/comparison plot with only mean and standard deviations")


# dat1 class: data.frame
dat1 <- read.csv("programming_languages_exe_speeds_recursive_Fibonacci_numbers.csv", header = T, dec = ",", sep = ";", comment.char = '%')


ylabel <-  paste("mean, +/- standard deviation in seconds")
# xlabel <- paste("programming language")
xlabel <- paste("")

# numerically sort data frame according to mean
# problem: data frame factor is not ordered! --> convert to tibble to preserve ordering!
dat1_sort <- as_tibble(dat1[order(dat1$mean),])

dat1_sort
# # A tibble: 3 × 3
#   language             mean std_dev
#   <chr>               <dbl>   <dbl>


y_break_max = 200  # seconds
y_tick = 50  # seconds


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
                 limits = c(0, y_break_max),
                 breaks = c(seq(0,y_break_max,y_tick))
               ) +

               labs(title = plot_title,
                    subtitle = sub_title,
                    x = xlabel,
                    y = ylabel) +

               theme(text = element_text(size = 12),
                     axis.text.x = element_text(angle = 90, size = 12))

print(bar_plot1)


# end of mean_stddev_err_whiskers_recursive_Fibonacci_numbers.R
