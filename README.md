# Salamander-Demography
This repository holds the R script and data used for analyses in "Stage-specific demographic effects of hydrologic variation in a stream salamander." This script was created by Madaline Cochrane November, 2023. 

The Hydrology-Salamander-Script, includes R code to run both a robust-design Pradel model and robust-design multi-state Cormack-Jolly-Seber model using RMark. The script begins by manipulating salamander capture data into encounter histories and characterizing annual flow metrics. Then this data is used to estimate different demographic vital rates, first with a Pradel model and then with a multi-state CJS model.

The salamander mark-recapture data used in this analysis are previously published with the Environmental Data Initiative Data Portal at https://doi.org/10.6073/pasta/cd5f5a03df194930bf87eb12157b8182 (Lowe 2022). Headwater stream discharge data is also already available from the Environmental Data Initiative Data Portal at https://doi.org/10.6073/pasta/15b300e96c2d2f9785d0155b3e18b0e9 (USDA Forest Service 2022). 

However, we included all data called in R script in this repository. Salamander capture-recapture data is found at HubbardBrook_SalamanderCaptureRecapture.csv. The file HB_Salamander_robustCJS_edit.csv contains the same capture-recapture information, except  is edited according to the assumptions of a multi-state CJS model instead of a Pradel model. Specifically, this includes changing the stage of salamanders on 7 ocassions so that they did not change from a larvae to an adult across secondary surveys. The column descriptions for these data files include: 
ElastomerID: visual implant elastomer ID (NA = no elastomer ID for this individual)
PITTagID: PIT tag ID (NA = no pittag for this individual)
FinalID: final ID used to create encounter histories (includes the elastomer ID if available, otherwise is the pittag ID for each individual)
OldNew:
Date:
Stream:
Reach:
SurNum:
Primary:
Species:
Sex:
RawLongLoc:
CorrLongLoc:
Stage:
Meso:
LatLoc:
SubSize:
SubType:
Metamorph:
TailRemoved
TailRegrown
PhotoSVL
MassMg
Head
