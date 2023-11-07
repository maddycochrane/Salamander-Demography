# Salamander-Demography
This repository holds the R script and data used for analyses in "Stage-specific demographic effects of hydrologic variation in a stream salamander." This script was created by Madaline Cochrane November, 2023. 

The Hydrology-Salamander-Script, includes R code to run both a robust-design Pradel model and robust-design multi-state Cormack-Jolly-Seber model using RMark. The script begins by manipulating salamander capture data into encounter histories and characterizing annual flow metrics. Then this data is used to estimate different demographic vital rates, first with a Pradel model and then with a multi-state CJS model.

The salamander mark-recapture data used in this analysis are previously published with the Environmental Data Initiative Data Portal at https://doi.org/10.6073/pasta/cd5f5a03df194930bf87eb12157b8182 (Lowe 2022). Headwater stream discharge data is also already available from the Environmental Data Initiative Data Portal at https://doi.org/10.6073/pasta/15b300e96c2d2f9785d0155b3e18b0e9 (USDA Forest Service 2022). 

However, we included all data called in R script in this repository. Salamander capture-recapture data is found at HubbardBrook_SalamanderCaptureRecapture.csv. The file HB_Salamander_robustCJS_edit.csv contains the same capture-recapture information, except  is edited according to the assumptions of a multi-state CJS model instead of a Pradel model. Specifically, this includes changing the stage of salamanders on 7 ocassions so that they did not change from a larvae to an adult across secondary surveys. The column descriptions for these data files include: 

ElastomerID: visual implant elastomer ID (NA = no elastomer ID for this individual)

PITTagID: PIT tag ID (NA = no pittag ID for this individual)

FinalID: final ID used to create encounter histories (includes the elastomer ID if available, otherwise is the pittag ID for each individual)

OldNew: Indicates whether animal is a recapture (O) or new individual (N)

Date: Date when salamander was captured

Stream: Stream where salamander was captured (Bear, Paradise, or ZigZag)

Reach: Reach where salamander was captured (Lower or Upper)

SurNum: Site specific survey number (NA = missing information)

Primary: Primary occasion number (NA = missing information)

Species: Species of salamanader (GP = Gyrinophilus porphyriticus)

Sex: Sex of salamander (F = female, A = adult, U = unknown; NA = missing information)

RawLongLoc: Location along stream reach where salamander was captured (meters from bottom end of reach; NA = missing information)

CorrLongLoc: Location along stream reach where salamander was captured (meters from confluence with Hubbard Brook/bottom of downstream reach; NA = missing information)

Stage: Developmental stage of salamander (L = larvae; M = metamorph, A = adult)

Meso: Meso-habitat where salamander was captured (RF = riffle; PL = pool; RN = run; C = cascade; NA = missing information)

LatLoc: Lateral stream location (Thalweg = T; wetted edge = WE; bank = B; NA = missing information)

SubSize: Substrate size (mm) under which salamander was captured (length longest dimension)

SubType: Substrate type under which salamander was captured (R = rock; W = wood, NA = missing information)

Metamorph: Metamorphic individual (yes or no)

TailRemoved: % of tail missing (NA = missing information)

TailRegrown: % of tail regrown (NA = missing information)

PhotoSVL: Snout-vent-length (mm; NA = missing information)

MassMg: Mass of salamander (mg; NA = missing information)

Headwidth: Head width (mm; NA = missing information)

TrunkLength: Trunk length (mm; NA = missing information)

TrunkWidth:Trunk width (mm; NA = missing information)

HumerousLength: Humerous length (mm; NA = missing information)

FemurLength: Femur length (mm; NA = missing information)

TailWidth: Tail width (mm; NA = missing information)

TailHeight:Tail height (mm; NA = missing information)

Photo: PhotoID (NA = missing information)

Photo2: PhotoID 2 if applicable (NA = missing information)

Photo3: PhotoID 3 if applicable (NA = missing information)

Notes: Comments/notes

Remove: Remove capture ocassion due to incorrect ID or incomplete information


