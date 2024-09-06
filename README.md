# A Workflow for Modeling Mortality

The Individual Life Experience Committee (ILEC) is pleased to offer a reference framework for applying predictive analytics techniques to mortality data as it is typically encountered by a working life experience studies actuary. Of the many techniques to model such data, we have opted to demonstrate and discuss the application of generalized linear models (GLMs), gradient-boosted decision trees (GBDTs), and elastic net regression. To focus the discussion in the reference framework, we analyze mortality differences by product.

## Data

ILEC maintains and analyzes a dataset of industry mortality experience. This data is regularly collected by a designated statistical agent, and reporting has been mandatory for nearly all companies since 2011. The data can be found [here](https://www.soa.org/resources/research-reports/2021/2017-mortality-experience/). As of this date (July 10, 2024), the latest release is through experience year 2017. As the new statistical agent (NAIC) ramps up its processing capabilities, more recent data will be released in the coming months.

The ILEC data as of this date is a CSV of over 39 million rows. Loading this data requires 30-50GB of working memory, which is beyond the capability of the typical user. Therefore, we have prepared summarized versions for this framework. These can be found in the datafiles subfolder with the .parquet suffixes.

## Findings

While we encourage our audience to read the HTML output and work with the framework themselves, we also provide the following findings for the impatient:

-   The relative spread of preferred mortality differs by product.
    -   For 2-class preferred systems, the residual standard mortality is much higher than preferred for term than for for other products.
    -   The spread for UL/VL/ULSG/VLSG for 4-class preferred systems is much wider than for other products.
-   There are divergences in the spread of face amount factors for xl, Perm, and Term, with xL narrowing relative to Term.
-   The issue age slope appears to be steeper for Term than Perm and xL under age 65. However, differences emerge above issue age 65, with the slope for Perm steepening relative to xL.
-   There are differences among the products in durations 1 and 2.
-   Since issues years 1990-1999, there has been a small but steady increase in relative mortality for xL vs Term, with xL now approaching Term.

# Running the Framework

We are pleased to offer two options to work with this repository:

-   Using Github Codespaces on Github's cloud
-   Running it on your own system

## Using Github Codespaces

Github offers the Codespaces service. This allows you to start your own virtual environment running on Github's servers. The virtual environment is a Docker container, which has been customized to run the framework out-of-the-box. The most basic and least expensive container using 2 CPUs can run the framework as is. However, if you want to experiment with models or plots, you will need a more capable instance. THe GLM can benefit from as many as 32 cores, while the elastic net portion requires 10 cores.

The document [GeneratingReport.md](https://github.com/SOA-ILEC-Demo/RILEC/blob/main/GeneratingReport.md) provides the steps you need to use Codespaces for this repository.

Note that Github may charge a fee for instances with more than the basic CPU provisioning.

## Running on Your Own System

Running it on your own system is a little more technically involved. There are at least two options to do so:

-   Use VSCode to run the Github Codespace functionality on your own machine. In this case, once you have cloned the repository to your own system and have Docker installed, you should be able to use VSCode to create and connect to the Docker container locally.
-   Set up the R environment on your own terms

### R Setup

The code assumes R 4.2. It will work with later versions, subject to some adjustments for changes in package versions. For example, the "predcontrib" argument of lightgbm::predict will generate an error in more recent versions of the package.

For setting up your own R environment, you will need to install the following R packages:

-   groundhog
-   pre
-   lightgbm
-   data.table
-   lmtest
-   glmnet
-   dplyr
-   EIX
-   ggplot2
-   tidyr
-   doParallel
-   tidyverse
-   magrittr
-   dtplyr
-   flextable
-   ftExtra
-   arrow
-   here
-   shapviz
-   patchwork
-   Matrix
-   MatrixModels

For installing arrow under Linux, you will need to also have the environment variable ARROW_USE_PKG_CONFIG=false. In R, this can be accomplished with

``` r
Sys.setenv(ARROW_USE_PKG_CONFIG="false")
```

``` r
install.packages(c("groundhog","pre","lightgbm","data.table","lmtest","glmnet","dplyr","EIX","ggplot2","tidyr","doParallel","tidyverse","magrittr","dtplyr","flextable","ftExtra","arrow","here","shapviz","patchwork","Matrix","MatrixModels"))
```

### Windows Clean Install

Download and install R. Rtools are recommended but not required. Install RStudio, and then install the packages listed above. This can be done either using the install.packages in R or letting RStudio prompt to download.

### Linux

For a clean Linux install, we assume that you have already chosen your Linux environment. There are at least three routes to running a Linux environment:

-   Native Linux installation
-   WSL2
-   Docker Image (e.g., [NGC Tensorflow](https://catalog.ngc.nvidia.com/orgs/nvidia/containers/tensorflow))

For what follows, we assume that you are running an Ubuntu-based Linux environment that has not had R, RStudio, or its supporting libraries installed before.

The following packages must also be installed via your preferred method. Many are required for tidyverse. Since we used an Ubuntu-based environment, we reproduce the commands using apt-get.

-   libopenblas-dev
-   libssl-dev
-   zlib1g-dev
-   libcurl4-openssl-dev
-   libxml2-dev
-   libfontconfig1-dev
-   libharfbuzz-dev
-   libfribidi-dev
-   libfreetype6-dev
-   libpng-dev
-   libtiff5-dev
-   libjpeg-dev
-   libcairo2-dev

``` bash
sudo apt-get install libopenblas-dev libssl-dev zlib1g-dev libcurl4-openssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libjpeg-dev libtiff5-dev libcairo2-dev
```

R can be installed using the instructions at [R Project](https://www.r-project.org). The same goes for [R Studio](https://posit.co/).

### Running the R Markdown Document

#### Preparation

To run the code in its current state, you will need summarized ILEC data. This can be found in the ./datafiles folder.

R objects which require significant memory or computing resources are saved in the ./objectcache folder.

#### "Knitting" the Entire Document

If the data files are in place, then you should be able to "knit" the markdown files.

#### Running the Code Interactively

To run the code interactively, you will need to run each code block manually and in sequence. To do so, there is a green play arrow in the upper right-hand corner of each code block. For more information, Posit maintains documentation and tutorials on working with R markdown files.

#### Using Your Own Data

You can use your own summarized data. Data should be summarized in such a way that all predictor variables are factor variables, and there should be a "response" variable (e.g., claim count) and "offset" (e.g., expected claim count or policies exposed). Some parts of the code may have to be disabled, as they are specific to the included ILEC datasets. The core of the framework should work with any data which is structured like the included ILEC datasets.
