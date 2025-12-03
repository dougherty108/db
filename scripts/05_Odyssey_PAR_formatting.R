########## Odyssey PAR Formatting 

# pull the process_par function from the functions library
source(here::here("functions", "05_OdysseyPAR.R"))

data_path = "~/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/sensors/Odyssey PAR/GL4"

# create a list of files in the data path
files = list.files(data_path)

# create an empty list object to 
par <- list()

# loop to concatonate all the data into one file
for(i in 1:length(files)) {
  obj <- process_par(paste0(data_path, "/", files[[i]]))
  
  par[[i]] = obj
}

par_df = bind_rows(par)
