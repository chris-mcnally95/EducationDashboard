Christopher McNally

<!-- README.md is generated from README.Rmd. Please edit that file -->

# EducationDashboardGolem

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of EducationDashboardGolem is to overview and monitor COVID-19
cases within school-aged children in Northern Ireland.

It was developed by the Data Analyst Team at the Public Health Agency
using R within the
[{golem}](https://cran.r-project.org/web/packages/golem/index.html)
package.

# Instructions for Development Team

This application has been modularised to fit into the {golem} framework.
Golem does a good job of explaining things within the package
environment but I will include some instructions to help you navigate
this particular environment.

The modules themselves can be found in the /R folder, along with other
key function scripts.

When running a local version of this repo, to utilise the {golem}
framework, you will have to alter the working directory of the
application to that of your own. This can be done by going to /inst and
then golem-config.yml and changing the working directory to your own
path. Be sure to then place this file name within your .gitignore so it
does not get updated within the master branch.

In order for the dynamic loading of the data from the Synapse server,
the data_prep.R file resides in the base directory of the app and is
sourced within the app_server.R file upon app initilisation.

The environment can be confusing if you are unfamiliar with the R
package development process but the most important files are as follows:

-   **app.R** - This file is where you deploy the application on the PHA
    R Studio Connect server

-   **The /R folder** - This is where all the scripts that run the app
    are stored. These should be modularised and called from the
    app_server.R and app_ui.R files (See the 02_dev.R file in /dev for
    information on making new modules)

-   **run_dev.R** - This file is located within the /dev folder inside
    the main directory. Running this script will allow you to launch the
    application locally

-   **The /tests folder** - This folder is where the unit tests for the
    application are stored. The tests still require some tuning but
    there are some standard scripts that test the over all operation of
    the application. See next point for running tests.

-   **03_deploy.R** - The file is located in the /dev folder and
    contains a function called devtools::check(). Running this will
    execute all the unit test scripts within the /tests folder and
    display the results.

## Unit Testing

I have researched and implemented various testing techniques within this
application.

A package called “shinytest” records an operational session of the
application and compares your test version to that session snapshot. An
issue with snapshot comparison testing with this application is that the
data is dynamic and as such will flag up new updated data changes when
comparing to an old snapshot. Therefore, if utilising this package,
check the results and determine if the only error are the infoboxes on
the Home page.

I have also utilised “testthat”. My scripts for this package are located
within /tests/module_tests.R. The theory here is that it will test a
mini server session of the called module and test what is the expected
output. For example, an expected output type for a DT:datatable and
ggplot2:ggplot is of the type “list”, therefore if anything fails and a
datatable or a plot is not generated, the test fails. Warning, for some
reason these tests can only be ran upon the running of
devtools::check(), so build your tests and then run this.

Several pre-constructed test scripts also reside in the /tests folder.

## Resources

The resources used to build this application came from the [Engineering
Production-Grade Shiny Apps](https://engineering-shiny.org/) book by
Colin Fay.

## Code of Conduct

Please note that the EducationDashboardGolem project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
