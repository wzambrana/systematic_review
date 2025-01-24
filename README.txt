README for Data and R code for a systematic review project regarding human viruses on fomites (inanimate objects).

Author: WZ
Associated publication: https://doi.org/10.1021/acsenvironau.3c00025

FILES
1)	systematic_review.Rproj: 		R project file needed to run main R script
2)	main_systematic_review.R:		Main R script to run
3)	data_systematic_review.xlsx: 		Data used in main R script 
4)	classification_systematic_review: 	Data used in main R script

1) systematic_review.Rproj
Necessary to run main R script. Save all files (includes this one) in the same directory before running main_systematic_review.R.

2) main_systematic_review.R
The main script for the analysis. This script is used to analyze the results of a systematic review regarding the occurrence of human viruses on fomites in the environment.

3) data_systematic_review.xlsx
[Original data source: https://purl.stanford.edu/rg368zb3188?ref=PDF]
Full.Citation: 			Source of dataset
Virus:				Virus type as reported by the study
Viral.Family:			Viral family of virus, used for analysis and reporting
Location:			Sampling location as reported by the study
Location.Category:		Sampling location category, used for analysis and reporting
Implement:			Implement used to collect samples (i.e., dacron swabs)
Implement.Category:		Implement type category, used for reporting
Eluent:				Liquid used to elute any captured virus from the implement after a fomite was swabbed
Eluent.Category:		Eluent type category, used for reporting
Pre.Moisturizer:		Liquid used to pre-moisten the implement prior to sampling the fomite
Pre.Moisturizer.Category:	Pre-moisturizer type category, used for reporting
Area.Swabbed:			Fomite area swabbed as reported by the study for most of their fomite samples or as their main target area
Fomites.Sampled:		List of fomites in the dataset that were sampled at least once
Fomites.Positive:		List of fomites in the dataset that were found positive at least once
Detection.Method:		How the study measured the virus (i.e., molecular methods)
Format.of.Resuts.Reported:	How the study reported their results (i.e., presence/absence)
Other.Units:			Units reported by the study, if it their reporting format was classified as "Other" in the Format.of.Resuts.Reported column
Comparison.to.health.data:	Type of comparison between fomite data and health data (i.e., quantitative (statistical), non-quantitative (not statistical), not conducted)
Number.of.Samples:		Total number of fomite samples
Number.of.Positive.Samples:	Total number of fomite samples found positive
Positivity.Rate:		Percent of fomites sampled that were positive (Number.of.Positive.Samples/Number.of.Samples)

4) classification_systematic_review.xlsx
Label: 				Category name, used for reporting and quantification
Value:				Observations as found in raw data
--
Sheet Name
Location_Classify: 		Sampling location, categories and observations.
Location_Subclassify2: 		Sampling location, subcategories and observations.
Eluent_Prem_Classify:		Pre-moisturizer and eluent, categories and observations.
Implement_Classify:		Implement, categories and observations.
HTS_Classify:			High touch surfaces (or fomites), categories and observations.
VirusFamily_Classify:		Virus family target, categories and observations.
Virus_Classify:			Virus target, categories and observations.
Area_Classify:			Area, categories and observations.
