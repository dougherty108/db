# Script for compiling the PAR data from the buoys (Odyssey brand)

source(here::here("functions", "00_libraries.R"))
source(here::here("functions", "05_OdysseyPAR.R"))


#NOTE FROM BELLA: this doesn't run for me yet, will need to come back later 2026-03-03
LOC_deployment1_a <- read.csv("/Users/isol5015/Library/CloudStorage/OneDrive-UCB-O365/Research/Data/R/sensor_db/data/Sensors/Odyssey PAR/LOC/LOC_1.5_BOT_20240815_20241017.CSV")


LOC_deployment1_a <- process_par("/Users/isol5015/Library/CloudStorage/OneDrive-UCB-O365/Research/Data/R/sensor_db/data/Sensors/Odyssey PAR/LOC_1.5_BOT_20240815_20241017.CSV")
LOC_deployment2_a <- process_par("/Users/isol5015/Library/CloudStorage/OneDrive-UCB-O365/Research/Data/R/sensor_db/data/Sensors/Odyssey PAR/LOC/LOC_1.5m_BOT_20241025_20250522_Serial16684.CSV")
LOC_deployment2_b <- process_par("/Users/isol5015/Library/CloudStorage/OneDrive-UCB-O365/Research/Data/R/sensor_db/data/Sensors/Odyssey PAR/LOC/LOC_1m_BOT_20241025_20250522_Serial16691.CSV")
LOC_deployment2_c <- process_par("/Users/isol5015/Library/CloudStorage/OneDrive-UCB-O365/Research/Data/R/sensor_db/data/Sensors/Odyssey PAR/LOC/LOC_2m_BOT_20241025_20250522_Serial16694.CSV")
