library(targets)
files <- c("1_fetch/src/code_to_fetch.R", "2_process/src/code_to_process.R")
for (file in files) {
  source(file)
}
tar_option_set(packages = c("tidyverse", "stringr", "sbtools", "whisker"))
# list separate targets:
list(
  # Get the data from ScienceBase
  tar_target(
    model_RMSEs_csv,
    download_data(out_filepath = "1_fetch/src/"),   # removed file name, see if this works in next tar_make() 4:11pm
    format = "file"
  ), 
  # Prepare the data for plotting
  tar_target(
    eval_data,
    process_data(in_filepath = model_RMSEs_csv),
  ),
  # Create a plot
  tar_target(
    figure_1_png,
    make_plot(out_filepath = "3_Visualize/out/", data = eval_data),
    format = "file"
  ),
  # Save the processed data
  tar_target(
    model_summary_results_csv,
    write_csv(eval_data, file = "2_process/src/"), 
    format = "file"
  ),
  # Save the model diagnostics
  tar_target(
    model_diagnostic_text_txt,
    generate_model_diagnostics(out_filepath = "2_process/src/", data = eval_data), 
    format = "file"
  )
)

