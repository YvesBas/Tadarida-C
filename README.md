# Tadarida-C

##Updates

###09/07/2018
TadaridaC.r is now obsolete and replaced by Ta_Tc.r which is more modular and calls sequentially 3 scripts:
-ClassifC1.r which applies the species classifier to each calls
-AggContacts.r which applies an ad hoc clustering algorithms to group calls which have similar probabilities to belong to a specific species
- AggNbSp.r which calls a new classifier (see below) which determines which groups of calls are really an additional species or more likely a peculiar group of calls of an already detected species (e.g. social and echolocation calls of the same species)
To run this last script we added a “buildClassifNbSp.r” which use files where ALL species have been identified to train a classifier which will decide which calls group correspond to an additionnal species or not.

##Software Installation

Tadarida-C is a set of scripts running under [R environment](https://www.r-project.org/). These scripts need the installation of two packages:
-randomForest
-data.table
If you use R in command line, you can run the “init.r” script provided on the Github repository (https://github.com/YvesBas/Tadarida-C) to install those two libraries.
If asked, you should prefer to install those libraries in a “personal libraries” folder.


##Software function
###buildClassif.r
####Inputs:
The builder has two required inputs that should be indicated by editing the first lines of the script: 
-	a path indicating where is the reference sound database (RSDB) built with Tadarida-L (Note that a sample for test is available at https://github.com/YvesBas/Tadarida-C) 
-	a path indicating where the “Modified_randomForest.R” script was locally copied (this script is in the source folder at https://github.com/YvesBas/Tadarida-C)
Additionally, users can also provide a path indicating a “SpeciesList” table giving potential grouping of similar taxa, and/or a filter excluding some taxa, according to geographical occurrence for example. If no “SpeciesList” is provided, all taxa will be included in the classifier without any grouping.
Format needs to follow this example: 
https://github.com/YvesBas/Tadarida-C/other_inputs/SpeciesList.csv
Groupings are indicated by values given in “Nesp” column, while following columns give one or several filters. Taxa must have a “x” value in each column if they are to be selected.
Finally, if one filter is to be used, line 7 of the script must be edited indicating the corresponding column name. 
Settings:
Two variables are provided at the beginning of the script to balance unevenness in the number of detected sound events (DSEs) per species in the RSDB.
They allow to mix a gradient of trees in the Random Forest (RF). First will benefit from the largest part of the RSDB (but uneven in number of DSEs per species), and following will be more and more even in the number of DSEs used per species at the cost of a smaller part of the RSDB used.
“SubSamp” defines the minimum level of subsampling (=at the start of the loop). Default is 11, corresponding to a subsampling equal to 11 times the average number of DSE per species. 
“GradientSamp” defines the strength of the gradient and must be negative. Default is -0.1.

####Outputs:
The builder output is a RF classifier named “ClassifEspHF3.learner” to be used as input for the following script.
You can find at https://github.com/YvesBas/Tadarida-C, the classifier file produced on the basis of the RSDB sample provided on the same page. However, since this RSDB sample is small, please note that the performance of this classifier is evidently poor.

###Ta_Tc.r
This script replace TadaridaC.r and calls subsequentely ClassifC1.r, AggContacts.r and AggNbSp.r-project
####Inputs:
The same as the 3 scripts below +:
- args[7]: (numeric) block size (number of .ta files to be classified at the same time - save memory down to 100)

####Outputs:
The same as AggNbSp.r (see below)


###ClassifC1.r
####Inputs:
It requires 6 arguments: 
- args[1]: (character) the path of a folder containing .ta files (Tadarida-D/L outputs)
- args[2]: (character) the path of the classifier to be used
- args[3]: (character) (optional, and still undocumented) the path of a reference file giving the rank scale to convert features in linear discriminants
- args[4]: (numeric) the high pass filter in kHz to filter out low frequency signals
- args[8]: (numeric) the Nth file to start with
- args[9]: (numeric) the Nth to end with

####Outputs:
It gives a probability matrix of each sound events (in lines) belonging to each potential species (in rows) named according to the first treated .ta file and with a _ProbEsp.csv suffix

###AggContacts.r
####Inputs:
It requires 3 arguments: 
- args[1]: (character) the path of a folder containing the ClassifC1.r outputs (_ProbEsp.csv files) (=args[1]) or of the votes from buildClassif.r (ProbEspXXX.csv file)
- args[6]: (boolean) if outputs should be splitted in .tc files (1 file per wave file) or not (in this case outputs are written in one single table IdTot.csv and shorter summary IdShort.csv)
- args[10]: (character) the path of a reference table giving the list of species codes

####Outputs:
either splitted .tc files or two big tables (IdTot.csv and IdShort.csv), see above (args[6])

###AggNbSp.r-project
####Inputs:
It requires 4 arguments: 
- args[6]: (boolean) if outputs should be splitted in .tc files (1 file per wave file) or not (in this case outputs are written in one single table IdTri.csv and shorter summary IdShort.csv)
- args[10]: (character) the path of a reference table giving the list of species codes
- args[11]: (character) the path of the boolean classifier which will decide if a calls group is an additionnal species or not (output of buildClassifNbSp.r)
- args[12]: (boolean) if calls group should be pooled or not


###TadaridaC.r

####Inputs:
This script uses 2 inputs:
- a path indicating where ClassifEspHF3.learner is stored (the classifier produced by the builder script - see above)
- a path indicating a directory containing the .ta files to be classified.

####Outputs:
For each .ta files, this script will give a .tc file containing a table. Each line corresponds to a part of the DSEs considered to be from the same species. An iterative algorithm indeed filters out DSEs if they have incompatible classification scores, i.e. very low scores on some DSEs for the species which got the best score on all DSEs. In that way, several different DSE groups can be separated, rarely more than 5.
For each DSE groups, following values are given:
- maximum score for each potential species
- median frequency of DSEs
- time of the first DSE
- time of the last DSE
TadaridaD/L and TadaridaC versions are also computed
Note: if the .ta file is empty (=no DSE), no .tc is produced

Examples of .tc files corresponding to test files are provided at https://github.com/YvesBas/Tadarida-C




##Tadarida-C implementation and architecture

Tadarida-C handles the classification of DSEs, based on features extracted by Tadarida-D and RSDB collected by Tadarida-L, and thus provides the final output of the Tadarida toolbox.
It contains two R-script: one building a classifier (for expert users) and another implementing it (for end users, see Fig. 2). Tadarida-C has been developed in R because it allows the use and optimisation of Random Forests (RF) algorithm. Both scripts use randomForest and data.table packages. This latter package is only used through its “rbindlist” function that allows fast aggregation of large data frames.

###Building a classifier

The “buildClassif.r” script should be run each time user’s RSDB has been significantly improved or when there is any need for a new classifier (different species list, settings,etc). It first aggregates and merges label data (.eti files, see Tadarida-L) and features data (.ta files) from a specified RSDB (Fig. 2).
This data frame may then be filtered on a species list (if provided) and is finally used to build a series of 50 RFs containing each 10 classification trees. Authors indeed found out that combining trees with different subsampling levels better handles the trade-off between classification error rates on common species (i.e. species with a large number of labelled DSEs in the RSDB) and error rates on rare species. Thus, a gradient of subsampling is set so that the first trees of the series basically use most of the available DSEs in the RSDB, whereas last trees use an equal number of DSEs for most species. The strength of the subsampling can be tuned through two input settings (see Tadarida-C manual).
The “site” field of the label data is also used for subsampling, each RF using 63% of available sites. This makes train and test independent for each tree. To implement this subsampling, authors have slightly modified the randomForest function so that size of sample class (sampsize) could be equal to 0.
All other RF settings were kept to default. Note that authors did optimize the number of features to use at each node (mtry) and found out that default setting was optimal (i.e. square root of the total number of features).
Output file is named “ClassifEspHF3.learner” to be used as input in the implementation script (Fig. 2).


###Using the classifier

The “Ta_Tc.r" script aims at returning to the user the list of species present on each treated .wav file, and a confidence score of each automatic identification. For that purpose, it applies previously built RF classifier (ClassifEspHF3.learner) to any list of .ta files (output from Tadarida-D or Tadarida-L, see Fig. 2). 
With the “predict” function from randomForest R package, it first converts features values extracted on each DSE to a matrix of class probabilities (see ClassifC1.r above).
Most users will not be interested to an identification on such a small scale since many DSEs within a .wav file could come from the same source. Thus, the next scripts (AggContacts.r and AggNbSp.r) aim at (1) generally summarizing the information and (2) more specifically defining how many species are present.
Species list is defined following this simple and robust loop:
The maximum score within the class probability matrix gives the identity of the first species automatically identified (species A)
DSEs giving a low probability for species A are considered as not being from this species and this subset goes through step 1 again. If this condition is not met for any DSE, the loop is ended.
Some ancillary data is also computed for end users to get summary information about the species probably present in their recordings, where in time and frequency the identified vocalizations occur, how confident these identifications are (see .tc outputs below), etc.

##Ancillary data
Outputs of Ta_Tc.r, AggContacts.r and AggNbSp.r give some ancillary data which are a summary of features extracted by Tadarida-D/L, in addition to probability of classification among potential species
FreqM: median of the median frequency ((Fmin+Fmax)/2)
FreqP: peak frequency 
FreqC: characteristic freqency (frequency of the flattest part)
Tstart: time of the first sound event
Tend: time of the last sound event
NbCris: number of calls
DurMed: median duration of calls
Dur90: 90%-quantile of call durations
Ampm50: median of call maximum amplitude
Ampm90: 90%-quantile of call maximum amplitude
AmpSMd: median of amplitude standard deviation among 4 quarters in duration
DiffME: median amplitude difference between the peak and the end of the call
SR: sample rate
Order: order in which the species were identified (iterative loop of AggContacts.r)

