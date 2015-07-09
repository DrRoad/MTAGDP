# MTAGDP
Modelled Territorial Authority Gross Domestic Product for New Zealand

This repository hosts a snapshot of the Ministry of Business, Innovation and Employment's code repository used to create modelled estimates of Territorial Authority Gross Domestic Product for New Zealand.

The code will not run end to end because of dependencies on the MBIE environment.  It is presented here for transparency reasons and to help methodological debate, if it arises.  The Ministry is working on a range of open data initiatives that will make it easier for projects such as this to be truly reproducible.  If you are interested in this, contact us at info@mbie.govt.nz 

The Ministry does not regard these estimates as an end state, and there is an active project to improve them further, by making aggregate figures available in a more timely fashion and numerous improvements to issues such as the 'commuter correction' for allocating residence-based earnings to place of production.

The data, full methodology, and interactive web app for exploring the data are available from http://www.mbie.govt.nz.

Feel free to raise an issue in this GitHub repository for a public discussion of methodology or questions that may be of wider interest.  

This project would not have been possible without the [R statistical computing environment](http://www.r-project.org/), and in particular the [{survey} package by Thomas Lumley](http://cran.r-project.org/web/packages/survey/index.html), [{dplyr}](http://cran.r-project.org/web/packages/dplyr/index.html) and [{ggplot2}](http://cran.r-project.org/web/packages/ggplot2/index.html) by Hadley Wickham, and [{shiny}](http://cran.r-project.org/web/packages/shiny/index.html) by the RStudio team.

Special thanks to Statistics New Zealand and to the New Zealand Institute of Economic Research, both of whom hosted seminars at which valuable feedback was provided on work in progress.  Statistics New Zealand also provided a custom dataset to improve the results, with confidentiality applied, under conditions designed to give effect to the security and confidentiality provisions of the Statistics Act 1975.  That dataset is available in this repository, but is of most interest as an interim step to the final results.  All other data sources have been previously published.

Caveats and disclaimers:

* These estimates are at a more detailed level of granularity than available in the Statistics New Zealand official Tier 1 regional GDP series. They are experimental in nature and should be used with caution.	The data are modelled and produced by the Ministry of Business Innovation and Employment (MBIE) (not by Statistics New Zealand), according to the methods outlined in [link to come].
* These estimates are not a Tier 1 statistic and have been created by MBIE for research purposes. While various Statistics New Zealand collections form the source data, Statistics New Zealand will not be held accountable for any error, inaccurate findings or interpretation within the publication.
* MBIE is not responsible for the results of any actions taken on the basis of this information.