# runway 0.0.0.9000

* Added additional statistics available for output in `threshperf(statistics=)`. The sensitivity and specificity are calculated using the {yardstick} package. All other statistics are then calculated using these values and the prevalence of the outcome. Calculating the statistics with this method allows a user to pass the prevalence and the performance statistics can also be calculated with a case-control design. (@ddsjoberg, #12)
