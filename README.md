# ASMR: Anchored Score-Matching Representative in Distributed Machine Learning

## Overview
The **ASMR** package implements methods for constructing **Anchored Score-Matching Representatives (ASMR)** and **Transformed Mean Representatives (TMR)** for distributed datasets in logistic, Poisson, and linear regression settings. These methods are designed to provide efficient, representative-based inference in generalized linear models (GLMs) and extend to other smooth loss functions.

This package is currently under active development and will be expanded with further functionality and improvements over time.

## Key Features
- **ASMR (Anchored Score-Matching Representative)**: A method to handle distributed datasets, capturing local data structure more accurately by anchoring the intercept and matching gradients across nodes.
- **TMR (Transformed Mean Representative)**: An extension of the Mean Representative (MR) method, designed to incorporate non-linear transformations through the link function in GLMs.
- Supports **logistic regression**, **Poisson regression**, and **linear regression** models.
- **Iterative updates** for more precise parameter estimation in distributed machine learning environments.
- Subpartitioning strategies for improved representativeness in heterogeneous data.

## Installation
Currently, the package is under development and not yet available on CRAN. You can install the development version from GitHub:

```r
# Install devtools if necessary
install.packages("devtools")

# Install the ASMR package from GitHub
devtools::install_github("kerenli/ASMR")
