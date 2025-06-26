
# MetaboCrates

*MetaboCrates* is designed for early analysis of data obtained from
targeted metabolomics, e.g., Biocrates® kits. It simplifies and
streamlines the data processing workflow, allowing you to efficiently
analyze metabolite data. Here’s a brief overview of its key features:

## Features

**1. Data Import:** Easily import metabolomics data generated from
Biocrates® platforms.

**2. Preprocessing:** Perform data preprocessing tasks such as
metabolites selection and \<LOD imputation.

- Automatically identify and remove metabolites with a high Limit of
  Detection (LOD) proportion.
- Enhance the accuracy of your data by eliminating unreliable
  measurements.
- Complete missing data points for metabolites based on LOD values.
- Ensure that your dataset is comprehensive and suitable for analysis.

**3. Quality Control:** Quality control checks on your data.

**4. Analysis:** Calculate descriptive statistics.

**5. Save your work:** Save your progress, allowing you to resume your
analysis at a later time.

- Track and manage multiple projects effortlessly.
- Easily share your findings with colleagues or collaborators.

## Web server

The *MetaboCrates* web server can be accessed through our [web
server](http://biogenies.info/metabocrates-ws).

## Installation

To install *MetaboCrates* you need to have *R* version \>= 4.2.0.

``` r
devtools::install_github("BioGenies/MetaboCrates")
```

## Reproducibility

To reproduce our environment you need to git clone our repo and activate
renv.

``` bash
git clone https://github.com/BioGenies/metaborates.git
```

``` r
renv::activate()
renv::restore()
```

## Run MetaboCrates

To run *MetaboCrates* type the following command into an R console.

``` r
MetaboCrates::MetaboCrates_gui()
```

#### How to cite?

Krystyna Grzesiak, Joanna Pokora, Jarosław Chilimoniuk, Adrian
Godlewski, Mariia Solovianova, Rafał Kolenda, Adam Krętowski, Michał
Ciborowski, Michał Burdukiewicz (2025). MetaboCrates :An open-source
pipeline for quality-aware analysis of targeted metabolomics data.####
Contact

If you have any questions, suggestions or comments, contact [Michal
Burdukiewicz](mailto:michalburdukiewicz@gmail.com).

#### Funding and acknowledgements

We want to thank the Clinical Research Centre (Medical University of
Białystok) members for fruitful discussions. K.G. wants to acknowledge
grant no. 2021/43/O/ST6/02805 (National Science Centre). J. P. and M. B.
wants to acknowledge grant no. 2023/51/D/NZ7/02847 (National Science
Centre). We also acknowledge the Center for Artificial Intelligence at
the Medical University of Białystok (funded by the Ministry of Health of
the Republic of Poland).![](readme_files/funding.png)<!-- -->
