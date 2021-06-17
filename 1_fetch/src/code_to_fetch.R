## ---------------------------
##
## Script name: code_to_fetch.R 
##
## Purpose of script: test out the Targets package!
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes:
##     ~ contains all functions involved in fetching data 
## ---------------------------


download_data <- function(out_filepath)
{ # Get the data from Science Base 
  mendota_file <- file.path(out_filepath, 'model_RMSE.csv')
  item_file_download('5d925066e4b0c4f70d0d0599', names = 'me_RMSE.csv',
                     destinations = mendota_file, overwrite_file = TRUE)
  return(mendota_file)
}


process_data <- function(in_filepath)
{ # Prepare the data for plotting
  eval_data <- readr::read_csv(in_filepath, col_types = 'iccd') %>%
    filter(str_detect(exper_id, 'similar_[0-9]+')) %>%
    mutate(col = case_when(
      model_type == 'pb' ~ '#1b9e77',
      model_type == 'dl' ~'#d95f02',
      model_type == 'pgdl' ~ '#7570b3'
    ), pch = case_when(
      model_type == 'pb' ~ 21,
      model_type == 'dl' ~ 22,
      model_type == 'pgdl' ~ 23
    ), n_prof = as.numeric(str_extract(exper_id, '[0-9]+')))
  return(eval_data)
}