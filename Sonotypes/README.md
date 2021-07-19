# Tadarida-C for bat sonotypes

## Summary

This is a variation of Tadarida-C designed to study bat sonotypes around the world.
A classifier is provided and can be downloaded at https://doi.org/10.6084/m9.figshare.14340341.v1
The architecture and interaction between scripts is summarized [here](https://drive.google.com/open?id=1LV-Li36kZvC18UaklBbJp0hjeumf1fCh) (first 8 slides for building your own classifier and apply it, or 2nd slide for applying the classifier provided)
Some files required to run scripts can be found in Tadarida-C/ tadaridaC_src
See sections ‘Building a classifier’ and ‘Using the classifier’ (at the bottom below) for detailed explanations.

## Updates

### 19/02/2021
Release of the scripts and of the classifier (compilation date 11 December 2020).

## Software Installation

Tadarida-C is a set of scripts running under [R environment](https://www.r-project.org/). These scripts need the installation of two packages:
-randomForest
-data.table
-plyr
-dplyr
-beepr
-Hmisc
-ggplot2
If you use R in command line, you can run the “init.r” script provided on the Github repository (https://github.com/YvesBas/Tadarida-C) to install those two libraries.
If asked, you should prefer to install those libraries in a “personal libraries” folder.


## Software function 1 - build classifiers

### write_tabase3HF_Sonotype.r
This script produces a table from the RSDB (Reference Sound DataBase) to be used as a direct input for buildClassif_HF_Sonotype.r
#### Inputs:
It requires 4 arguments: 
- RSDB: (character) the path where the RSDB from Tadarida-L has been saved
- VarSel: (optional, character) a csv file indicating which sound features should be subsetted (see VarSel.csv example at Tadarida-C/ tadaridaC_src/other_inputs/)
- SpeciesList: (optional, character) a path indicating a table listing the potential species and grouping, and/or a filter excluding some taxa, according to geographical occurrence for example. If no “SpeciesList” is provided, all taxa will be included in the classifier without any grouping.
- GeoFilter: (optional, character) a header of SpeciesList indicating which species list should be selected (= according to geographical occurrence)

#### Outputs:
a csv table used as an input for buildClassif_HF_Sonotype.r (see below)

### buildClassif_HF_Sonotype.r
Build the main classifier of Tadarida-C
#### Inputs:
- MRF: (character) the path where Modified_randomForest.r has been stored (Tadarida-C/ tadaridaC_src/)
- VarSel: (character) a csv file indicating which sound features should be subsetted (see VarSel.csv example at Tadarida-C/ tadaridaC_src/other_inputs/)
- GeoFilter: (character) which geographical zone should be selected (if none, use "") (write_tabase3HF.r should have been run first)
- SubSamp: (numeric) level of minimum subsampling (= X times average number of calls per species)
- GradientSamp: (numeric) gradient strength (must be negative)
- Table_TAXREF_Sonotype: (character) the path where Liste_especes_DOM-TOM.csv is and that associates species with sonotypes.

#### Outputs:
- a classifier file to be used by ClassifC1_Sonotype.r (see below)
- a ProbEspXXX.csv file compiling an independent out-of-bag votes that could be used to evaluate classifier performance, model error risk according to score, etc.

## Software function 2 - apply classifiers
### Ta_Tc_Sonotype.r
This script calls subsequently ClassifC1_Sonotype.r, AggContacts_Sonotype.r and AggNbSp_Sonotype.r-project
#### Inputs:
The same as the 3 scripts below +:
- args[7]: (numeric) block size (number of .ta files to be classified at the same time - save memory down to 100)

#### Outputs:
The same as AggNbSp_Sonotype.r (see below)


### ClassifC1_Sonotype.r
#### Inputs:
It requires 6 arguments: 
- args[1]: (character) the path of a folder containing .ta files (Tadarida-D/L outputs)
- args[2]: (character) the path of the classifier to be used
- args[3]: (character) (optional, and still undocumented) the path of a reference file giving the rank scale to convert features in linear discriminants
- args[4]: (numeric) the high pass filter in kHz to filter out low frequency signals
- args[10]: SpeciesList: (character) a path indicating a table listing the potential species and grouping, and/or a filter excluding some taxa, according to geographical occurrence for example.
- args[19]: The variable used to build modes, i.e. to build groups of calls. Default is FreqMP.
- args[20]: the threshold of PED (probability of the "most probable species") used to separate secondary species. Default is 0.05.

#### Outputs:
-	A IdTot_TOTAL_summary_vXX.csv file which gives a summary of potential sonotypes found.
-	Files with a probability matrix of each sound events (in lines) belonging to each potential sonotype (in rows) named according to the first treated .ta file and with a _ProbEsp.csv suffix
-	Files in which sound events are grouped by potential sonotype (column SpMaxF2) and by args[19] mode (column MainMode), associated to the probability score (column Ind) named according to the first treated .ta file and with a _IdTot.csv suffix
-	A IdTot_TOTAL_VXX.csv file which is the concatenation of all IdTot files.

### AggContacts_Sonotype.r
#### Inputs:
It requires 3 arguments: 
- args[1]: (character) the path of a folder containing the ClassifC1_Sonotype.r outputs (_ProbEsp.csv files) (=args[1]) or of the votes from buildClassif.r (ProbEspXXX.csv file)
- args[6]: (boolean) if outputs should be splitted in .tc files (1 file per wave file) or not (in this case outputs are written in one single table IdTot.csv and shorter summary IdShort.csv)
- args[10]: (character) the path of a reference table giving the list of species codes

#### Outputs:
either splitted .tc files or two big tables (IdTot.csv and IdShort.csv), see above (args[6])


### AggNbSp_Sonotype.r
#### Inputs:
It requires 4 arguments: 
- args[6]: (boolean) if outputs should be splitted in .tc files (1 file per wave file) or not (in this case outputs are written in one single table IdTri.csv and shorter summary IdShort.csv)
- args[10]: (character) the path of a reference table giving the list of species codes
- args[11]: (character) the path of the boolean classifier which will decide if a calls group is an additionnal species or not (output of buildClassifNbSp.r)
- args[12]: (boolean) if calls group should be pooled or not

#### Outputs:
either splitted .tc files or two big tables (IdTri.csv and IdShort.csv), see above (args[6])

## Tadarida-C implementation and architecture

Tadarida-C handles the classification of DSEs, based on features extracted by Tadarida-D and RSDB collected by Tadarida-L, and thus provides the final output of the Tadarida toolbox.
It contains two R-script: one building a classifier (for expert users) and another implementing it (for end users, see Fig. 2). Tadarida-C has been developed in R because it allows the use and optimisation of Random Forests (RF) algorithm. Both scripts use randomForest and data.table packages. This latter package is only used through its “rbindlist” function that allows fast aggregation of large data frames.

### Building a classifier

The “buildClassif_HF_Sonotype.r” script should be run each time user’s RSDB has been significantly improved or when there is any need for a new classifier (different sonotype list, settings,etc). It first aggregates and merges label data (.eti files, see Tadarida-L) and features data (.ta files) from a specified RSDB (Fig. 2).
This data frame may then be filtered on a sonotype list (if provided) and is finally used to build a series of 50 RFs containing each 10 classification trees. Authors indeed found out that combining trees with different subsampling levels better handles the trade-off between classification error rates on common sonotypes (i.e. sonotypes with a large number of labelled DSEs in the RSDB) and error rates on rare sonotypes. Thus, a gradient of subsampling is set so that the first trees of the series basically use most of the available DSEs in the RSDB, whereas last trees use an equal number of DSEs for most sonotypes. The strength of the subsampling can be tuned through two input settings (see Tadarida-C manual).
The “site” field of the label data is also used for subsampling, each RF using 63% of available sites. This makes train and test independent for each tree. To implement this subsampling, authors have slightly modified the randomForest function so that size of sample class (sampsize) could be equal to 0.
All other RF settings were kept to default. Note that authors did optimize the number of features to use at each node (mtry) and found out that default setting was optimal (i.e. square root of the total number of features).
Output file is named “ClassifEspHF3.learner” to be used as input in the implementation script (Fig. 2).


### Using the classifier

The “Ta_Tc_Sonotype.r" script aims at returning to the user the list of sonotypes present on each treated .wav file, and a confidence score of each automatic identification. For that purpose, it applies previously built RF classifier (ClassifEspHF3.learner) to any list of .ta files (output from Tadarida-D or Tadarida-L, see Fig. 2). 
With the “predict” function from randomForest R package, it first converts features values extracted on each DSE to a matrix of class probabilities (see ClassifC1_Sonotype.r above).
Most users will not be interested to an identification on such a small scale since many DSEs within a .wav file could come from the same source. Thus, the next scripts (AggContacts_Sonotype.r and AggNbSp_Sonotype.r) aim at (1) generally summarizing the information and (2) more specifically defining how many sonotypes are present.
Sonotypes list is defined following this simple and robust loop:
The maximum score within the class probability matrix gives the identity of the first sonotype automatically identified (sonotypes A)
DSEs giving a low probability for sonotype A are considered as not being from this sonotype and this subset goes through step 1 again. If this condition is not met for any DSE, the loop is ended.
Some ancillary data is also computed for end users to get summary information about the sonotypes probably present in their recordings, where in time and frequency the identified vocalizations occur, how confident these identifications are (see .tc outputs below), etc.

## Ancillary data
Outputs of Ta_Tc_Sonotype.r, AggContacts_Sonotype.r and AggNbSp_Sonotype.r give some ancillary data which are a summary of features extracted by Tadarida-D/L, in addition to probability of classification among potential sonotypes
- FreqM: median of the median frequency ((Fmin+Fmax)/2)
- FreqP: peak frequency 
- FreqC: characteristic freqency (frequency of the flattest part)
- Tstart: time of the first sound event
- Tend: time of the last sound event
- NbCris: number of calls
- DurMed: median duration of calls
- Dur90: 90%-quantile of call durations
- Ampm50: median of call maximum amplitude
- Ampm90: 90%-quantile of call maximum amplitude
- AmpSMd: median of amplitude standard deviation among 4 quarters in duration
- DiffME: median amplitude difference between the peak and the end of the call
- SR: sample rate
- Order: order in which the species were identified (iterative loop of AggContacts_Sonotype.r)
- MainMode: principal mode of arg[19]
- Ramp90: 90%-quantile of maximal amplitude ratios between dominant harmonic and potential harmonic.

## Note concerning harmonics
Ramp90 may be used to filter out harmonics that were unsuccessfully removed in the classification process. Positive values are generally associated with harmonics.
Ramp90 may also be used to distinguish fundamental calls that do not present visible harmonics (generally very low negative values) from fundamental calls that present visible harmonics (generally negative values close to 0). 

