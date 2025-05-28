# MetaboCrates - R Package for Biocrates® Metabolomics Data

MetaboCrates is designed for early analysis of data obtained from Biocrates® kits. It simplifies and streamlines the data processing workflow, allowing you to efficiently analyze metabolite data. Here's a brief overview of its key features:


## Features

**1. Data Import:** Easily import metabolomics data generated from Biocrates® platforms.

**2. Preprocessing:** Perform data preprocessing tasks such as metabolites selection and <LOD imputation. 

   - Automatically identify and remove metabolites with a high Limit of Detection (LOD) proportion.
   - Enhance the accuracy of your data by eliminating unreliable measurements.
   - Complete missing data points for metabolites based on LOD values.
   - Ensure that your dataset is comprehensive and suitable for analysis.

**3. Quality Control:** Quality control checks on your data.

**4. Analysis:** Calculate descriptive statistics.

**5. Save your work:** Save your progress, allowing you to resume your analysis at a later time.

  - Track and manage multiple projects effortlessly.
  - Easily share your findings with colleagues or collaborators.
  


## Installation

Execute following commands in the R console:

```
devtools::install_github("BioGenies/MetaboCrates")
```
