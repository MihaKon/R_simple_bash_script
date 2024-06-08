WARN_THRESHOLD_PERCENTAGE = 50
GROUP_COLUMN = 'grupa'
REQUIRED_LIBRARIES <- c(
 "argparse"
)

install_libraries <- function(libs) {
  for (lib in libs) {
    if(!require(lib, character.only=TRUE)){
      cat("Installing ",lib, "\n")
      install.packages(lib, dependencies = TRUE)
    }
    library(lib, character.only = TRUE)
  }
}

get_parser <- function() {
  parser <- ArgumentParser(description = "Process arguments...")
  parser$add_argument(
    "--input_filename",
    help = "Input file",
    type = "character",
    required = TRUE
  )
  parser$add_argument(
    "--output",
    help = "Output file, when parsed output raport will be created",
    type = "character",
    default = FALSE
  )
  parser$add_argument(
    "--fill_avg",
    help = "Fill missing data with average values per column",
    action = "store_true",
    default = FALSE
  )
  return(parser)
}

get_avg_per_column <- function(data) {
  numeric_columns <- data[, sapply(data, is.numeric)]
  averages <- colMeans(numeric_columns, na.rm = TRUE)
  names(averages) <- names(numeric_columns)
  return(averages)
}

get_avg_per_column_and_group <- function(data, group_column = GROUP_COLUMN) {
  numeric_columns <- data[, sapply(data, is.numeric)]
  averages <- aggregate(numeric_columns[, -1], by = list(data[[group_column]]), FUN = mean, na.rm = TRUE) 
  colnames(averages)[-1] <- paste0("", colnames(averages)[-1])
  return(averages)
}

fill_na <- function(data, fill_values, group_column = GROUP_COLUMN) {
  report <- data.frame(
    Group = character(),
    Column = character(),
    Row = integer(),
    Filled_value = numeric(),
    stringsAsFactors = FALSE
  ) 
  
  numeric_columns <- data[, sapply(data, is.numeric)]
  for (col_name in names(numeric_columns)) {
    for (group in data[[group_column]]) {
     
      group_data <- data[,data[[group_column]] == group, ]
      group_avg <- fll_values(fill_values[[group_column]])
      
      if(is.na(group_avg)) {
        group_avg <- fill_values[1, col_name, drop = TRUE]
      }
      
      na_indices <- which(data[[group_column]] == group & is.na(data[[col_name]]))
      data[na_indices, col_name] <- group_avg
      
      if(length(na_indices) > 0) {
        report <- rbind(report, data.frame(
          Group = rep(group, length(na_indices)),
          Column = rep(col_name, length(na_indices)),
          Row = rep(na_indices, length(na_indices)),
          Filled_value = rep(group_avg, length(na_indices)),
        ))
      }
      
    }
  }
  
  return(list(data, report))
}

main <- function() {
  install_libraries(REQUIRED_LIBRARIES)
  parser <- get_parser()
  args <- parser$parse_args()
  
  data <- read.csv(args$input, sep = ";", dec = ",", header = TRUE)
  if(args$fill_avg) {
    avg_per_column <- get_avg_per_column(data)
    fill_na(data, avg_per_column)
  } else {
    avg_per_col_per_group <- get_avg_per_column_and_group(data)
    list(filled_data, filled_data_report) <- fill_na(data, avg_per_col_per_group)
  }
}

if (interactive() == FALSE) {
  main()
}
