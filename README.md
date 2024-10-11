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
```

## Example Usage

### Logistic Regression with ASMR

```r
# Load sample dataset
data <- data.frame(node = rep(1:10, each = 100),
                   x1 = rnorm(1000),
                   x2 = rnorm(1000),
                   y = rbinom(1000, 1, 0.5))

# Apply ASMR for logistic regression
beta_amsr <- rep.amsr.logit(data, names.x = c("x1", "x2"), names.y = "y", iter = 5)

# View the coefficients from each iteration
print(beta_amsr)
```

### Poisson Regression with TMR

```r
# Load sample dataset
data <- data.frame(node = rep(1:10, each = 100),
                   x1 = rnorm(1000),
                   x2 = rnorm(1000),
                   y = rpois(1000, lambda = 2))

# Apply TMR for Poisson regression
beta_tmr <- rep.poisson(data, names.x = c("x1", "x2"), names.y = "y", iter = 5)

# View the coefficients from each iteration
print(beta_tmr)
```

## Functions

- `rep.amsr.logit()`: Perform ASMR on distributed datasets for logistic regression.
- `rep.poisson()`: Perform TMR/ASMR for Poisson regression models.
- `mr.generic()`: Construct a generic Mean Representative (MR) for local data.
- `asmr.logit()`: Construct Anchored Score-Matching Representative (ASMR) for logistic regression.
- `asmr.poisson()`: Construct Anchored Score-Matching Representative (ASMR) for Poisson regression.
