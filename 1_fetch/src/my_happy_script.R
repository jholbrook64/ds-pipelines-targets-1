## ---------------------------
##
## Script name: the happy script
##
## Purpose of script:  targets training
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes:
##      
## ---------------------------

library(dplyr)
library(remotes)
library(tidyverse)
library(readr)
library(stringr)
library(sbtools)
library(whisker)

# Get the data from Science Base    
getCSV <- function(out_folder, complete_path){
  mendota_file <- file.path(out_folder, 'model_RMSE.csv')
  item_file_download('5d925066e4b0c4f70d0d0599', names = 'me_RMSE.csv',
                     destinations = mendota_file, overwrite_file = TRUE)
  return(mendota_file)
}

# Prepare the data for plotting
eval_data <- function(in_csv){
  out_data <- readr::read_csv(in_csv, col_types = 'iccd') %>%
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
  return(out_data)
}


# Create a plot, this function won't return anything
createPlot <- function(evalData, plotOutput){

fp <-   file.path(plotOutput, 'figure_1.png')
png(file = fp, width = 8, height = 10, res = 200, units = 'in')
par(omi = c(0,0,0.05,0.05), mai = c(1,1,0,0), las = 1, mgp = c(2,.5,0), cex = 1.5)
# I would like to assining this to a vriable but 
plot(NA, NA, xlim = c(2, 1000), ylim = c(4.7, 0.75),
     ylab = "Test RMSE (°C)", xlab = "Training temperature profiles (#)", log = 'x', axes = FALSE)
# log x axis vector
n_profs <- c(2, 10, 50, 100, 500, 980)
# set axis, this cannot be a function becuase 
axis(1, at = c(-100, n_profs, 1e10), labels = c("", n_profs, ""), tck = -0.01)
axis(2, at = seq(0,10), las = 1, tck = -0.01)
# slight horizontal offsets so the markers don't overlap:
offsets <- data.frame(pgdl = c(0.15, 0.5, 3, 7, 20, 30)) %>%
  mutate(dl = -pgdl, pb = 0, n_prof = n_profs)

# loop: do this for each type of model in the analysis
for (mod in c('pb','dl','pgdl')){
  
  #outer
  # use function parameter here:
  mod_data <- filter(evalData, model_type == mod) 
  mod_profiles <- unique(mod_data$n_prof) 
  
  #inner
  for (mod_profile in mod_profiles) 
  {
    d <- filter(mod_data, n_prof == mod_profile) %>% 
      summarize(y0 = min(rmse), y1 = max(rmse), col = unique(col))
    x_pos <- offsets %>% filter(n_prof == mod_profile) %>% pull(!!mod) + mod_profile
    # this is its own thing this line adds the values to a line for the plot
    lines(c(x_pos, x_pos), c(d$y0, d$y1), col = d$col, lwd = 2.5)
  }
  
  #outer
  d <- group_by(mod_data, n_prof) %>% 
    summarize(y = mean(rmse), col = unique(col), pch = unique(pch)) %>%
    rename(x = n_prof) %>% arrange(x)
  # adds lines
  lines(d$x + tail(offsets[[mod]], nrow(d)), d$y, col = d$col[1], lty = 'dashed')
  # adds points
  points(d$x + tail(offsets[[mod]], nrow(d)), d$y, pch = d$pch[1], col = d$col[1], bg = 'white', lwd = 2.5, cex = 1.5)
  
}

points(2.2, 0.79, col = '#7570b3', pch = 23, bg = 'white', lwd = 2.5, cex = 1.5)
text(2.3, 0.8, 'Process-Guided Deep Learning', pos = 4, cex = 1.1)
points(2.2, 0.94, col = '#d95f02', pch = 22, bg = 'white', lwd = 2.5, cex = 1.5)
text(2.3, 0.95, 'Deep Learning', pos = 4, cex = 1.1)
points(2.2, 1.09, col = '#1b9e77', pch = 21, bg = 'white', lwd = 2.5, cex = 1.5)
text(2.3, 1.1, 'Process-Based', pos = 4, cex = 1.1)
#writes it to the png file
dev.off()
}

# Save the model diagnostics, write to the processes folder 
dataRender <-  function(eval_data, textOutput){    
render_data <- list(pgdl_980mean = filter(eval_data, model_type == 'pgdl', exper_id == "similar_980") %>% pull(rmse) %>% mean %>% round(2),
                    dl_980mean = filter(eval_data, model_type == 'dl', exper_id == "similar_980") %>% pull(rmse) %>% mean %>% round(2),
                    pb_980mean = filter(eval_data, model_type == 'pb', exper_id == "similar_980") %>% pull(rmse) %>% mean %>% round(2),
                    dl_500mean = filter(eval_data, model_type == 'dl', exper_id == "similar_500") %>% pull(rmse) %>% mean %>% round(2),
                    pb_500mean = filter(eval_data, model_type == 'pb', exper_id == "similar_500") %>% pull(rmse) %>% mean %>% round(2),
                    dl_100mean = filter(eval_data, model_type == 'dl', exper_id == "similar_100") %>% pull(rmse) %>% mean %>% round(2),
                    pb_100mean = filter(eval_data, model_type == 'pb', exper_id == "similar_100") %>% pull(rmse) %>% mean %>% round(2),
                    pgdl_2mean = filter(eval_data, model_type == 'pgdl', exper_id == "similar_2") %>% pull(rmse) %>% mean %>% round(2),
                    pb_2mean = filter(eval_data, model_type == 'pb', exper_id == "similar_2") %>% pull(rmse) %>% mean %>% round(2))
     
template_1 <- 'resulted in mean RMSEs (means calculated as average of RMSEs from the five dataset iterations) of {{pgdl_980mean}}, {{dl_980mean}}, and {{pb_980mean}}°C for the PGDL, DL, and PB models, respectively.
  The relative performance of DL vs PB depended on the amount of training data. The accuracy of Lake Mendota temperature predictions from the DL was better than PB when trained on 500 profiles 
  ({{dl_500mean}} and {{pb_500mean}}°C, respectively) or more, but worse than PB when training was reduced to 100 profiles ({{dl_100mean}} and {{pb_100mean}}°C respectively) or fewer.
  The PGDL prediction accuracy was more robust compared to PB when only two profiles were provided for training ({{pgdl_2mean}} and {{pb_2mean}}°C, respectively). '

# since this is inside the dataRender function, this should be able to use the local variable render_data
whisker.render(template_1 %>% str_remove_all('\n') %>% str_replace_all('  ', ' '), render_data) %>% cat(file = file.path(textOutput, 'model_diagnostic_text.txt'))
}

########
# DIRECTIONS :
# run this code block after adding each function to the functions in your global environement:

# obtain data from URL:
# create new output directory
Visualize <- '3_Visualize'
visualize_output_dir <- '3_Visualize/out'   
visualize_src <- '3_Visualize/src'   
process_output_dir <- '2_process/out'
dir.create(visualize_output_dir)
dir.create(process_output_dir)
dir.create(Visualize)
dir.create(visualize_src)

#include a parameter for the path to the output folder from tHe project directory:
out_folderFetch <-  '1_fetch/out/'
file_out <- getCSV(out_folderFetch)

#create object for evaluated data:
evalData <- eval_data(file_out)

# writes to '3_Visualize/out'  
createPlot(evalData, plotOutput = visualize_output_dir)   
                                   
# writes to '2_process/out'  
dataRender(evalData, textOutput = process_output_dir)

# Save the processed data
readr::write_csv(evalData, file = file.path(process_output_dir, 'model_summary_results.csv'))

#~######~#
