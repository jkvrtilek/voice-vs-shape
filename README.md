# voice-vs-shape
Which parameters are flexible/stable during convergence?

### This project is in progress. Please contact Julia Vrtilek (jkvrtilek@gmail.com) and Gerald Carter (gcarter1640@gmail.com) for more information.
### README last updated August 2026.

<img src="https://user-images.githubusercontent.com/13193023/92195916-49f4b600-ee2b-11ea-90f3-75c0eea7e1b0.png" width="50px"/>

Scripts used for analyses are provided here in R format (.R extension).
Data to reproduce these results is available on figshare:
Vrtilek, Julia K.; Smith-Vidaurre, Grace; Carter, Gerald (2025). Data for "Vocal convergence during formation of cooperative relationships in vampire bats". figshare. Dataset. https://doi.org/10.6084/m9.figshare.29191334

This folder or directory has 3 scripts that should be run sequentially:

1. 01_familiar_DFA.R

	- INPUTS: metadata.csv, vampire_call_measures_transformed.csv (figshare)
	- Uses only post-introduction calls. Within each group of familiar bats, classifies calls to bat using a single dfa without cross validation.
	- PRODUCTS: familiar-dfa-loadings-zoo.csv, familiar-dfa-loadings-2016.csv, familiar-dfa-loadings-2019.csv, familiar_bats_used.csv

2. 02_never_met_DFA.R

	- INPUTS: metadata.csv, vampire_call_measures_transformed.csv (figshare)
	- Separates bats into 4 groups that may be familiar within-group but CANNOT be familiar across groups.
	- Lists all possible permutations of pulling one bat from each group.
	- Classifies calls to bat within 1000 randomly selected combinations of one bat from each group.
	- PRODUCTS: never-met-dfa-loadings_1000.csv, never_met_bats_used.csv
	
3. get/make loading distributions/table

	- INPUTS: familiar-dfa-loadings-zoo.csv, familiar-dfa-loadings-2016.csv, familiar-dfa-loadings-2019.csv, never-met-dfa-loadings_1000.csv
	- Not sure which of these is the best version, but they compare the loading distributions of all call variables for familiar vs. never-met bats.

