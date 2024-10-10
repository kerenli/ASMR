# ASMR and TMR: Representative Learning in Distributed Machine Learning

This repository contains the implementation of **Anchored Score-Matching Representative (ASMR)** and **Transformed Mean Representative (TMR)**, developed for distributed machine learning and generalized linear models (GLMs). These methods provide robust alternatives to traditional federated learning by handling data heterogeneity, improving communication efficiency, and enabling further statistical inference.

## Features

- **Transformed Mean Representative (TMR)**: Extends the Mean Representative (MR) by incorporating transformations based on the link function in GLMs, reducing bias in non-linear models.
- **Anchored Score-Matching Representative (ASMR)**: An enhancement of Score-Matching Representative (SMR) that anchors the intercept, ensuring better interpretability, stability, and representativeness in distributed environments.

## Contents

- `TMR/`: Contains the implementation of the **TMR** method, including handling continuous and generalized linear model (GLM) data.
- `ASMR/`: Includes the implementation of the **ASMR** method, which improves model fitting in distributed learning by ensuring gradient-based representative updates.
- `data/`: Sample datasets used to test the TMR and ASMR methods.
- `docs/`: Documentation on how to use the methods and code structure.
- `examples/`: Example scripts to demonstrate how to run TMR and ASMR on simulated data.

## Installation

Clone this repository to your local machine:

```bash
git clone https://github.com/kerenli/ASMR-tmr.git
