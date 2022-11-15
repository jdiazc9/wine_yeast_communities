# wine_yeast_communities

The ./data directory contains experimental data:

**functional_effects_final.csv**  Measured functions of the communities assembled in the experiment, formatted to readily analyze functional effects.

As well as data files generated from the analysis script:

**predicted_functions.csv**  Predictions for the function of every out-of-sample community, obtained using the method described in [[1](https://doi.org/10.1101/2022.06.21.496987)].

**slopes.csv**  Slopes and intercepts of the Functional Effect Equations for each of the 12 species (2 Saccharomyces + 10 non-Saccharomyces) in the study.

**Predicted_FEE_Sugar.csv**  Slopes and intercepts  for the same species, estimated using phylogenetic inference with leave-one-out cross-validation.

The ./scripts directory contains the R scripts for the analyses and plots.

**eFunctions.R**  Auxiliary functions for data preprocessing (described in [[1](https://doi.org/10.1101/2022.06.21.496987)]).

**analyzeData.R**  Runs analyses, generates plots and data files.

Plots are saved under the ./plots directory.

[1] Juan Diaz-Colunga, Abigail Skwara, Jean C. C. Vila, Djordje Bajic, Alvaro Sanchez (2022). Emergent ecosystem functions follow simple quantitative rules. *biorXiv*. DOI: [10.1101/2022.06.21.496987](https://doi.org/10.1101/2022.06.21.496987)