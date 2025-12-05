# mean_stddev_err_whiskers_Oz.R
#
# 2025-12-05
#
# env: R version 4.5.1 (2025-06-13 ucrt) -- "Great Square Root"
#      Platform: x86_64-w64-mingw32/x64
#
# test: OK


library(tidyquant)  # loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR, %>%, theme_tq()
library(tibble)
library(ggplot2)


##########################
#
# user switch:

#
##########################


sec_to_ms <- 1000  # raw data is in seconds => convert to milliseconds for better presentation here

plot_title <- paste("Microbenchmark: execution speeds ('wall clock') of the map coloring
problem of Germany in Mozart-Oz 1.4.0 versus Python (Miniconda3)")

sub_title0 <- paste("PowerShell cmdlet for execution time measurement in Windows 11")

date_time  <- paste(format(Sys.Date()), format(Sys.time(), "%H:%M"))

sub_title <- paste(sub_title0, "\nbest out of 3 manual runs of each program -- plot version", date_time)

setwd("e:/zzz_Scripts/R/comparison plot with only mean and standard deviations")


# dat1 class: data.frame
dat1 <- read.csv("programming_languages_exe_speeds_Oz_Python.csv", header = T, dec = ",", sep = ";", comment.char = '%')


ylabel <-  paste("mean, +/- standard deviation in milliseconds")
# xlabel <- paste("programming language")
xlabel <- paste("")

# numerically sort data frame according to mean
# problem: data frame factor is not ordered! --> convert to tibble to preserve ordering!
dat1_sort <- as_tibble(dat1[order(dat1$mean),])

dat1_sort
# # A tibble: 3 × 3
#   language             mean std_dev
#   <chr>               <dbl>   <dbl>
# 1 Oz ozengine         0.286       0
# 2 Oz executable       0.304       0
# 3 Python (CP package) 0.328       0


y_break_max = 500  # milliseconds
y_tick = 100  # milliseconds


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


# end of mean_stddev_err_whiskers_Oz.R
