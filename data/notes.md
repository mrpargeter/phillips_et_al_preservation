 - When reading in CSV, read_csv is preferred, it's faster than read.csv and does not result in factors. Then we set any factors we need explicitly later.
 
 - combined_arc_exp_data only has 10 cols, yet you try to remove some non-existant columns with combined_arc_exp_data[, -c(7, 11:13, 15)]
 
- with the functions, it would be preferred to return a list that includes the test results and the plot object, rather than simply print the test results.

- For the createMovementPlot fn, facet_grid would simplify

- multiplot.R might be better replaced by ggplot2::facet_wrap or functions from cowplot, gridExtra or patchwork pkgs

- I'm not sure about the use of the permKS for directional data. We need to use a test specifically for directional data.

- in chies(), don't hard-code the total N value, use sum() - 1 on the table that goes into chi.sq

- for chies(chi_sq_2_3_4$statistic, ...) should n be 199 rather than 1193? Because all the item in the series_2_3_4_cores_loss_tbl table only sum to 199. 

- there are a few sections of repitition in the code, these should be avoided, and lapply or purrr::map functions used with list objects instead (I've used map here).

- I'm curious about gamlss(), how did you find it, and why did you choose it?

- object series_2_3_loss_tbl should probably be named series_1_2_3_loss_tbl
