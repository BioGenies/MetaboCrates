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
devtools::install_github("KrystynaGrzesiak/MetaboCrates")
```

# Run MetaboCrates

If you have successfully installed the package, run the GUI in your web browser using:

```
MetaboCrates::
```

## TODO

- [x] report
- [x] fix PCA plot
- [x] fix biplot
- [x] logspline imputation
- [ ] ?correct logspline imputation?
- [x] all metabolites allowed on correlation heatmap
- [ ] correlation heatmap check on big screen + helper
- [ ] NA ratios plot reactive height
- [ ] multiple grouping column
- [ ] group plot change
- [ ] group counts table
- [x] group levels to character
- [x] extra space on the bottom
- [ ] colors to missing heatmap
- [ ] extra space under missing heatmap
- [ ] venn description
- [ ] download buttons
