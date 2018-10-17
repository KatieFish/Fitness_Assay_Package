### Fitness_Assay_Package

##To use the Lang Lab Fitness Assay Package: 

from the R console 
`library(devtools)`  

`install_github("LangYeastEvoLab/Fitness_Assay_Package")`

- devtools is installed on the lab computer, but may need to be installed on your personal computer

##Package dependencies
- reshape2
- ggplot2

##Input data fromat

This script is based on inputs generated by FlowJo in the following format: 
*Note - when reading in data - stringsAsFactors=FALSE!!

Sample Count Gated_pop'n
A02..  1000  xxxx

A Well Key must be provided in the following format (column names must match): 

Well_ID		Experimental	Reference	Gated.on
A02...		strainID	strainID	strainID

A Well_key and FC_data template can be found as txt files in this repository. 


##Functions

#`output <- Analyze_Fitness_Data(your well_key, your_flow_data)` 
- cacluates the coefficient of selection of a query strain using time-course compettive growth assay flow cytometry data
- output is a table of selection coefficient, standard error, and 95% CI for each competition or replicate group

`Group replicates? (TRUE/FALSE):` 

- The well key supplied will be used to look for any duplicate competitions. If `TRUE`, grouping replicates will treat all competitions with identical strain identifiers as biological replicates (columns 2-4 in the well  key are identical). The coefficient of selection for each replicate group will be found by fitting a linear model based on all data points within a replicate. 
- If `FALSE`, each sample will be treated individually. Coefficient of selection will be found by fitting a linear model for each individual sample. 
*Replicates MUST be named identically in the well key. 

`Calculate error? (TRUE/FALSE):` 

- If you have multiple replicates (`TRUE` for `Group replicates?`), the script will find the standard error of regression and 95% CI by fitting a linear model based on all data points within a replicate. 
*Replicates MUST be named identically in the well key. 
- If you do not have replicates of individual competitions (`FALSE` for `Group replicates?`), this method will assign each fitness measurement an error based on the error of regression for each individual sample. 

`Plot results? (TRUE/FALSE):` 

- For no replicates, each sample will be plotted individually with error bars corresponding to 95% CI. 
- If you have replicates, each replicate will be plotted as one point with error bars corresponding to 95% CI

#`Fitness_ANCOVA(your well_key, your_flow_data)` 
- Uses an ANCOVA (analysis of covariance) to look for statistical differences in slope between competitions.
- User options remain the same as `Analyze_Fitness_Data`. 



