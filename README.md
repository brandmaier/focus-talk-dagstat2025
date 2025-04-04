# Focus! SEM Trees and Forests for Identifying Moderators in Structural Equation Models

## Abstract

> In the social and behavioral sciences, recursive partitioning methods have gained traction as robust non-parametric techniques for developing predictive models. Among these, Structural Equation Model (SEM) trees and forests are notable as a model-based recursive partitioning approach specifically tailored to SEMs. SEMs are deeply rooted in confirmatory modeling, providing a structured framework for estimating and understanding complex relationships among observed and latent variables. By merging SEMs and recursive partitioning, SEM trees and forests offer a non-parametric, exploratory approach to identifying moderators in theory-based structural equation models. SEM trees begin with a single SEM fit to the entire sample and then recursively identify key predictors or moderators that best explain heterogeneity in the SEM parameters. Informative predictors can be identified by examining individual trees or averaging variable importance across an entire forest. I introduce the concept of focus parameters, which enables researchers to pinpoint a subset of model parameters, which solely influence predictor selection in trees. The resulting trees and forests prioritize predictors that best explain heterogeneity in these focus parameters. In addition, I present an algorithm for computing variable importance in SEM forests with focus parameters. I argue that the use of focus parameters aids researchers both in finding interesting patterns and in amending their theories.

## Files
This repository contains the sources for my invited talk at the DagStat 2025 in Berlin. The presentation slides are found in file 
[presentation.pdf](https://github.com/brandmaier/focus-talk-dagstat2025/blob/main/presentation.pdf).

The presentation source code is presentation.Rmd

R codes for producing results are in directory `R/`, computed results are in `data/`, and various images are in directory `img/` and `unsplash/`.

Note that the Unsplash license applies to the images in directory `unsplash/`. 
