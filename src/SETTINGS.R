##### Global settings file
# Author: Katherine Lauck
# Last updated: 18 January 2019
#
#

# package dependencies
#
#install.packages("rio")
require("rio")
#install.packages("plyr")
require("plyr")
#install.packages("devtools")
require("devtools")
#install_github("psolymos/pbapply", ref = "fork-cluster-speedup")
require("pbapply")
#install.packages("parallel")
require("parallel")
#source_gist("4676064", filename = "as.data.frame.list.R")
#install("/Volumes/GoogleDrive/My Drive/CAGN occupancy/Analysis/R/Rpresence/RPresence")
library("RPresence")
#install.packages("MASS")
require("MASS")
#install.packages("dplyr")
require("dplyr")
#install.packages("lattice")
require("lattice")
#install.packages("grid")
require("grid")
#install.packages("ggplot2")
require("ggplot2")
#install.packages("plyr")
require("plyr")
#install.packages("profvis")
#require("profvis")
#install.packages("foreach")
#require("foreach")
require('scales')
require('tidyr')
require('stringr')
require('foreach')
require('doParallel')
#install.packages('car')
require('car')
#install.packages('ggpubr')
require('ggpubr')
#install.packages('stargazer')
require('stargazer')

# choose from 2 options for DATASET.
# "dummy": includes 120 dummy points and dummy site covariate values, as well as
# 120x5 blank rows corresponding to survey covariate values.
# "original" : no dummy data

DATASET <- "dummy"

PROJECT_DIRECTORY <- getwd()

# Choose where the input is. This should correspond to a project directory containing
# a data/ directory and a results/ directory.
INPUT_DIRECTORY <- paste0(PROJECT_DIRECTORY,'/results/data')

# Choose where the output should be stored.
#OUTPUT_DIRECTORY <- "G:/My Drive/projects/songbird-occupancy/results/presenceOutput"
OUTPUT_DIRECTORY <- paste0(PROJECT_DIRECTORY,"/results/presenceOutput")

# choose which spp are analyzed. Two options are available.
# NAMES_ENOUGH_DATA : a list of spp that probably have enough data, or almost
# enough data, to be analyzed.
# NAMES_YPI_PRESENTATION : a list of spp that will be analyzed for an upcoming
# YPI presentation. These species are generally of conservation concern and have
# enough data.
#


NAMES_ENOUGH_DATA <- c(
  "Aegithina viridissima",
  "Aethopyga temminckii",
  "Alcippe brunneicauda",
  "Alophoixus phaeocephalus",
  "Alophoixus tephrogenys",
  "Anthreptes malacensis",
  "Anthreptes rhodolaemus",
  "Anthreptes simplex",
  "Arachnothera longirostra",
  "Arachnothera modesta",
  "Argusianus argus",
  "Blythipicus rubiginosus",
  "Cacomantis merulinus",
  "Calyptomena viridis",
  "Centropus sinensis",
  "Chloropsis cyanopogon",
  "Chloropsis moluccensis",
  "Chloropsis sonnerati",
  "Chrysococcyx xanthorhynchus",
  "Copsychus malabaricus",
  "Cuculus micropterus",
  "Culicicapa ceylonensis",
  "Cyanoderma erythropterum",
  "Cyornis umbratilis",
  "Dicaeum minullum",
  "Dicaeum trigonostigma",
  "Dicrurus paradiseus",
  "Erpornis zantholeuca",
  "Eupetes macrocerus",
  "Eurylaimus javanicus",
  "Eurylaimus ochromalus",
  "Harpactes diardii",
  "Harpactes duvaucelii",
  "Harpactes kasumba",
  "Hypothymis azurea",
  "Iole charlottae",
  "Irena puella",
  "Ixos malaccensis",
  "Lacedo pulchella",
  "Lalage fimbriata",
  "Loriculus galgulus",
  "Macronus ptilosus",
  "Malacopteron affine",
  "Malacopteron cinereum",
  "Malacopteron magnirostre",
  "Malacopteron magnum",
  "Micropternus brachyurus",
  "Microtarsus atriceps",
  "Microtarsus eutilotus",
  "Nyctyornis amictus",
  "Oriolus xanthonotus",
  "Orthotomus atrogularis",
  "Orthotomus ruficeps",
  "Orthotomus sericeus",
  "Pellorneum bicolor",
  "Philentoma pyrhoptera",
  "Platylophus galericulatus",
  "Platysmurus aterrimus",
  "Pomatorhinus bornensis",
  "Prinia flaviventris",
  "Psilopogon australis",
  "Psilopogon chrysopogon",
  "Psilopogon henricii",
  "Psilopogon mystacophanos",
  "Pycnonotus brunneus",
  "Pycnonotus erythropthalmos",
  "Pycnonotus plumosus",
  "Rhinortha chlorophaea",
  "Rhipidura perlata",
  "Spilornis cheela",
  "Stachyris maculata",
  "Stachyris nigriceps",
  "Staphida everetti",
  "Surniculus lugubris",
  "Terpsiphone affinis",
  "Treron curvirostra",
  "Turdinus sepiarius")

NAMES_YPI_PRES <- c(
  "Alophoixus tephrogenys",
  "Platylophus galericulatus",
  "Platysmurus aterrimus",
  "Loriculus galgulus",
  "Argusianus argus",
  "Irena puella",
  "Chloropsis cyanopogon",
  "Chloropsis moluccensis",
  "Chloropsis sonnerati",
  "Copsychus malabaricus",
  "Alcippe brunneicauda"
)

SPECIES_TO_BE_ANALYZED <- NAMES_ENOUGH_DATA
