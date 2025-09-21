# A novel spatial heteroscedastic generalized additive distributed lag model (SHGADLM) for the spatiotemporal relation between PM2.5 and cardiovascular hospitalization

This repository contains the R codes used in the analysis for the paper:  

**"A novel spatial heteroscedastic generalized additive distributed lag model for the spatiotemporal relation between PM2.5 and cardiovascular hospitalization"**

---

## 📂 Repository Structure
- **R/**  
  Contains the main R scripts for Missing Data Imputation, IDW Interpolation, GADL model, SGADL model.  
- **data/**  
  Includes example datasets (if publicly shareable).  
- **figures/**  
  Contains generated figures from the analyses.  
- **requirements.txt**  
  List of R packages required to run the code.  
- **README.md**  
  Documentation and instructions for usage.

---

## ⚙️ Requirements
The analysis was performed in **R (>=4.2)**.  
The following R packages are required (see `requirements.txt` for the full list):

- `mgcv`  
- `spdep`  
- `Matrix`  
- `MASS`  
- `dlnm`  
- `ggplot2`  
- `sf`  
- `dplyr`
- `tidyverse`
- `splines`

You can install them via:
```R
install.packages(c("mgcv", "spdep", "Matrix", "MASS", "dlnm", "ggplot2", "sf","dplyr","tidyverse"))
