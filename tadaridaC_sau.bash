#! /bin/sh

if [ -z "$RSCRIPT" ]
then
    RSCRIPT=Rscript
fi

if [ -z "$TADARIDAC_DIR" ]
then
    TADARIDAC_DIR=$VIGIECHIRO_DIR/Tadarida-C
fi
if [ -z "$TADARIDAC_LEARNER_NAME" ]
then
    TADARIDAC_LEARNER_NAME=ClassifEspFrance180303.learner
fi
TADARIDAC_LEARNER_PATH=$TADARIDAC_DIR/$TADARIDAC_LEARNER_NAME

if ( [ "$#" -eq 0 ] )
then
    echo 'usage: tadaridaC <directory>'
    exit 1
else
    INPUT_FILE=`readlink -f $1`
    if ( [ "$?" -ne 0 ] )
    then
        echo "$1 is not a valid file"
        exit 1
    fi
fi


# Finally start tadaridaC R script
cd $TADARIDAC_DIR && $RSCRIPT ./tadaridaC_src/Ta_Tc.r $INPUT_FILE $TADARIDAC_LEARNER_PATH N 8 F T 500 0 0 ./other_inputs/SpeciesList.csv ./other_inputs/CNS_tabase3HF_France_IdConc.learner T ./other_inputs/Referentiel_seuils_ProbEspHF_.csv
