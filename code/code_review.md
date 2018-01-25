# Comments on R code in Phillips et al. Open Air 

To organise this code review, I have used the guidelines in Eglen, S. J., Marwick, B., Halchenko, et al. (2017). Toward standard practices for sharing computer code and programs in neuroscience. _Nature Neuroscience_, 20(6), 770-773. https://www.nature.com/articles/nn.4550.

For more information about the criteria of this review, please refer to that paper, and references therein, especially this one: 

Stodden, V. & Miguez, S., (2014). Best Practices for Computational Science: Software Infrastructure and Environments for Reproducible and Extensible Research. _Journal of Open Research Software_ 2(1), p.e21. DOI: http://doi.org/10.5334/jors.ay 

## Is enough code, data and documentation to allow at least one key figure from your manuscript to be reproduced?

Observations: I didn't see the manuscript, but it does look like the code and data reproduces many figures that appear in the manuscript. 

Recommendations: Circulate the code and manuscript together is a compendium.

## Was a version control system (such as Git) used to develop the code?

Observations: I didn't see any indications of version control

Recommendations: Use Git for version control, and host the code on GitHub or GitLab or similar.

## Is there a stable URL (such as a DOI) for key versions of your software. 

Observations: No.

Recommendations: Use version control, and deposit key versions (e.g. first submit, final acceptance) at a trustworthy repository such as osf.io, zenodo.org, figshare.com, or your university data repository. These services give you a DOI to link to your files. Then include the DOI for your files in your manuscript to link to them. 

## Is there a suitable license for your code to assert how you wish others to reuse your code?

Observations: No. 

Recommendations: Paste in to the mains source document the text of a widely used open-source license, such as MIT. Or add the license text as a separate file to the compendium. 

## Is there a minimal README file to describe what the code does and how to run it?

Observations: No

Recommendations: Add a short README file to the compendium. 

## Are modern, widely used software tools used in the paper to making your computational research reproducible?

Observations: Some, yes. Extensive use of functional programming is excellent. 

Recommendations: Use knitr to create an executible, reproducible document. A step further would be to creat an R package and place the functions in `/R` so you can take advantage of the quality control tools for package building. Using a package will also help a lot with managing the dependencies as they will be installed when the package is installed. 

## Are (community) standards, where appropriate, used, non-proprietary formats to enable long-term accessibility?

Observations: Yes, CSV files are used for the data. The code is in an R script file. A potential weak link is the code for the multiplot function, which is downloaded from a private webpage. If this webpage disappears, the code for this function will be lost, and the script wont work. 

Recommendations: Depend only on trustworthy repositories for code. 

## Are dependencies specified by name and version?

Observations: Dependent packages are named in the script. But package versions are not, the and version of R used is not noted. 

Recommendations: Add the output of devtools::session_info() to the output to capture the package names and version numbers of the last good run of the code. 

## Language-specific comments

Observations & Recommendations:  

- Excellent code style overall, really interesting work!

 - When reading in CSV, read_csv is preferred, it's faster than read.csv and does not result in factors. Then we set any factors we need explicitly later.
 
 - combined_arc_exp_data only has 10 cols, yet you try to remove some non-existant columns with combined_arc_exp_data[, -c(7, 11:13, 15)]
 
- with the functions, it would be preferred to return a list that includes the test results and the plot object, rather than simply print the test results.

- For the createMovementPlot fn, facet_grid would simplify

- multiplot.R might be better replaced by ggplot2::facet_* or functions from cowplot, gridExtra or patchwork pkgs, or a local custom version of that function. That website could disappear without warning. 

- I'm not sure about the use of the permKS for directional data. We need to use a test specifically for directional data.

- in chies(), don't hard-code the total N value, use sum() - 1 on the table that goes into chi.sq. Hard coding values is bad style and can lead to mistakes. 

- for chies(chi_sq_2_3_4$statistic, ...) should n be 199 rather than 1193? Because all the item in the series_2_3_4_cores_loss_tbl table only sum to 199. 

- there are a few sections of repitition in the code, these should be avoided, and lapply or purrr::map functions used with list objects instead (I've used map here).

- I'm curious about gamlss(), how did you find it, and why did you choose it?

- object series_2_3_loss_tbl should probably be named series_1_2_3_loss_tbl, no?

- If we use and Rmd, we can cache the results of long-running code, such as the simulations so we don't need to rerun them each time we update other parts. 





