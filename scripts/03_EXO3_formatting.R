########################  03_EXO3 ######################## 

# uses the process_EXO function to compile all the EXO data into one DF for the Loch. 

source(here::here("functions", "00_libraries.R"))
source(here::here("functions", "07_EXO3.R"))

# input your local path to the MLL Data folder where the EXO data is held
data_path = "~/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/sensors/YSI EXO3/Field Deployment"

# list all the files in the folder to be looped through
files = list.files(data_path)

# there are multiple different files for different deployments of the EXO
# there is already a processing script in the functions folder called process_EXO

output = list()

for(i in 1:length(files)) {
  obj <- process_EXO(paste0(data_path, "/", files[[i]]))
  
  output[[i]] = obj
}

output_df = bind_rows(output)
