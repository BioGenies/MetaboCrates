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
- [x] all metabolites allowed on correlation heatmap
- [x] correlation heatmap check on big screen + helper
- [ ] NA ratios plot reactive height
- [x] !multiple grouping column!
- [x] !group plot change!
- [x] !group counts table!
- [x] group levels to character
- [x] extra space on the bottom
- [x] colors to missing heatmap
- [ ] extra space under missing heatmap
- [x] venn description
- [ ] download buttons !!!
- [x] histogram change
- [x] PCA plots and correlations heatmap to new panel
- [x] correlations heatmap before and after imputation
- [x] thresholds steps to 5%
- [x] all summary info in excel
- [x] info panel to markdown
- [x] information to impute data to see plots
- [ ] ?adjustable correlations plot size?
- [x] disable picking columns in tables
- [x] new imputation panel layout
- [ ] scaled random imputation
- [ ] colored column height
- [ ] ?fix report?
- [ ] ?correct logspline imputation?
- [ ] legends to the top
- [ ] downloading check !!!
- [ ] color of spinners in DT
- [x] spinners color
- [ ] ?DT spinners color?
- [ ] buttons color
- [x] spinners placement
- [ ] spinners delay
- [ ] plots descriptions
- [x] handling data without missing values
- [ ] missing heatmap colors
- [ ] PCA plots for all sample types
- [ ] change descriptions appearance
- [ ] automatic imputation methods
- [ ] group PCA to another panel
- [ ] different color with different histogram type
