[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/d-alex-hughes/persistent-bias/master)

This repository builds a compute environment, and executes code against data in support of the publication _Persistent Bias Among Local Election Officials_. The paper was published in the Journal of Experimental Political Science in 2019. Upon having a citation issued by the journal, it will be included here.

# Repository Structure
This repository contains the same data that is included in the most current version of the Dataverse data repository, which is available [here](http:dataverse.harvard.edu).

The `Dockerfile` builds the compute, `./analysis/experimentAnalysis.ipynb` conducts all analysis reported in the publication, `./data/` contains all data used in the publication, and `.figures-tables` includes all plots and table included in publication and through EDA. 

```
.
├── Dockerfile
├── README.md
├── analysis
│   └── experimentAnalysis.ipynb
├── data
│   ├── County_Level_Election_Results_12-16
│   │   └── 2016_US_County_Level_Presidential_Results.csv
│   ├── replication_data.csv
│   ├── replication_data_variable_definitions.txt
│   ├── replication_tracker.csv
│   └── whiteNathanFaller_replicationFiles
│       ├── Icon\r
│       ├── MANIFEST.TXT
│       ├── Replication_SI_September2014_final.R
│       ├── Replication_main_September2014_Finalv2.R
│       ├── coppock_comment_WNF.pdf
│       ├── voterIDexp_data_sept2014.RData
│       ├── voterIDexp_data_sept2014.csv
│       ├── voterIDexp_data_sept2014.tab
│       └── voterIDexp_data_wparty_sept2014.tab
├── figures-tables
│   ├── VRAmodels.tex
│   ├── arabPopRescale.tex
│   ├── districtHTEminorityTable.tex
│   ├── districtHTEminorityTable1.tex
│   ├── fig1.pdf
│   ├── hteRobustArab.pdf
│   ├── hteRobustBlack.pdf
│   ├── hteRobustLatino.pdf
│   ├── hteTrumpAll.pdf
│   ├── hteTrumpArab.pdf
│   ├── hteTrumpBlack.pdf
│   ├── hteTrumpLatino.pdf
│   ├── interference_states.tex
│   ├── marginal_arab_effect.pdf
│   ├── rateOfResponse.pdf
│   ├── rateOfResponsePerCondition.pdf
│   ├── rateOfResponsePerConditionAllStates.pdf
│   ├── responseHistogram.pdf
│   ├── romney_arab_hte.pdf
│   ├── romney_black_hte.pdf
│   ├── romney_latino_hte.pdf
│   ├── survivalModels.tex
│   ├── table1Minorities.tex
│   ├── table1Minority.tex
│   ├── timeToResponse.pdf
│   ├── trackerHitModel.tex
│   ├── trumpHTEtable.tex
│   ├── trump_arab_hte.pdf
│   ├── trump_black_hte.pdf
│   └── trump_latino_hte.pdf
└── src
└── plot_function.R
```

# Building Compute

There are three ways to build the compute environment for this project.

1. Build a version on your host machine, using the docker file. To do so will require a working Docker client. To build in this way issue the following calls:

```
docker build -t persistent-bias
```

Then, launch the image using:

```
docker run -p 8888:8888 persistent-bias
```

2. Pull a version from dockerhub.

```
docker pull dalexhughes/persistent-bias
docker run -p 8888:8888 dalexhughes/persistent-bias
```

3. Or, most easily, click the following button [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/d-alex-hughes/persistent-bias/master) to launch a mybinder.org instance on cloud compute resources. 

 
