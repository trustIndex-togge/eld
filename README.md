# eld

This repository contains two Shiny apps that perform meta-analysis using Bayesian and Frequentist approaches, and a Data Validation app. 
The Shiny apps are hosted online and can be accessed through the ELD platform website, in the Apps tab. The website itself is also still under development.
https://metahag.github.io/CAMA-platform-website/

**Note:** all apps are still under development and results might not be correct, the apps can be accessed and tested but note that some outputs might not be accurate. This is an ongoing project.

# Bayesian Meta-analysis App

The Bayesian app allows users to perform a meta-analysis using a Bayesian approach based on Stan models. Users can select the dataset, effect type, prior distributions for the model parameters, and the number of iterations for the Stan model. The app provides a summary of the results, forest plots, funnel plots, and diagnostic plots.

# Frequentist Meta-analysis App

The Frequentist app allows users to perform a meta-analysis using a frequentist approach based on the metafor R package. Users can select the dataset, effect type, meta-analysis model, study design, outcomes, grade, and risk of bias. The app provides a summary of the results, forest plots, funnel plots, and a table of the results.

# Data Validation app

The data validation app currently performs a simple check of all columns necessary to conduct analyses and correct datatype of the columns, as well as links to the dataset template.
