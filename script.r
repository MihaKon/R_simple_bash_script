GROUP_COLUMN <- "grupa"
REQUIRED_LIBRARIES <- c(
  "argparse",
  "Hmisc",
  "base",
  "stats",
  "dplyr",
  "magrittr",
  "rlang",
  "ggpubr"
)
CORRELATION_MAPPING <- data.frame(
  lower_bound = c(-1, -0.7, -0.5, -0.3, -0.2, 0.2, 0.3, 0.5, 0.7),
  upper_bound = c(-0.7, -0.5, -0.3, -0.2, 0.2, 0.3, 0.5, 0.7, 1),
  correlation = c(
    "very strong negative correlation",
    "strong negative correlation",
    "moderate negative correlation",
    "weak negative correlation",
    "no correlation",
    "weak positive correlation",
    "moderate positive correlation",
    "strong positive correlation",
    "very strong positive correlation"
  )
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
    "--input",
    help = "Input file",
    type = "character",
    required = TRUE
  )
  parser$add_argument(
    "--grouping_column",
    type = "character",
    help = "Name of the column used for grouping data", 
    required = TRUE
  )
  parser$add_argument(
    "--save_plot",
    type = "logical",
    help = "Whether to save the plots as PNG files (TRUE or FALSE)",
    default = FALSE
  )
  parser$add_argument(
    "--output",
    help = "Output file, when parsed output raport will be created",
    type = "character",
    default = FALSE
  )
  parser$add_argument(
    "--quiet",
    help = "Hide command line warnings",
    type = "logical",
    default = FALSE
  )
  return(parser)
}

fill_na <- function(data, group_column = GROUP_COLUMN) {
  report <- data.frame(
    Column = character(),
    Row = integer(),
    Old_value = numeric(),
    New_value = numeric()
  )
  
  for (column in names(data)) {
    if(!is.numeric(data[[column]])) {
      next
    }
    na_indices <- which(is.na(data[[column]]))
    old_values <- data[na_indices, column]
    data[[column]] <- with(data, Hmisc::impute(data[[column]], mean))
    new_values <- data[na_indices, column]
    
    if(length(na_indices) > 0) {
      report <- rbind(report, data.frame(
        Column = rep(column, length(na_indices)),
        Row = na_indices,
        Old_value = old_values,
        New_value = new_values
      ))
    }
  }
  return(list(data = data, report = report))
}

get_shapiro_test_results <- function(data, grouping_column) {
  report <- data.frame(
    Group = character(),
    Column = character(),
    W = numeric(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )

  for (column in names(data)) {
    if(!is.numeric(data[[column]])) {
      next
    }
    
    for (group in unique(data[[grouping_column]])) {
      group_data <- data[data[[grouping_column]] == group, column]
      
      if (length(group_data) > 3) {
        test_result <- shapiro.test(group_data)
        
        report <- rbind(report, data.frame(
          Group = group,
          Column = column,
          W = test_result$statistic,
          P_Value = test_result$p.value
        ))
      }
    }
  }
  
  return(report)
}


get_column_characteristic <- function(data, group_column_name, target_column_name) {
  formula <- as.formula(paste(target_column_name, "~", group_column_name))
  
  aggregated_data <- aggregate(
    formula, 
    data, 
    function(x) c(
      mean = mean(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      IQR = IQR(x, na.rm = TRUE)
    )
  )
  stats_data <- do.call(data.frame, aggregated_data)
  names(stats_data)[-1] <- paste(names(stats_data)[-1], c("mean", "min", "max", "IQR"), sep = "_")
  
  return(stats_data)
}

print_group_correlation <- function(data, grouping_column, control_group_name) {
  control_data <- data %>% dplyr::filter(!!rlang::sym(grouping_column) == control_group_name)
  
  
  
  for (column in names(data)) {
    kruskal_test_p_value <- kruskal.test(column ~ grupa, data = data)$p_value
  }
}

main <- function() {
  install_libraries(REQUIRED_LIBRARIES)
  parser <- get_parser()
  args <- parser$parse_args()
  
  data <- read.csv(args$input, sep = ";", dec = ",", header = TRUE)
  
  fill_na_result <- fill_na(data = data)
  data <- fill_na_result$data
  
  outliers_report <- get_shapiro_test_results(data, args$grouping_column)


  for (column in names(data)) {
    column_characteristic <- get_column_characteristic(data, args$grouping_column, column)
    column_characteristics_list[[column]] <- column_characteristic

    visualize_outliers(data, args$grouping_column, column, save_plot = args$save_plot)
    anova_result <- get_anova_result(data, args$grouping_column, column)
  }
  
  if (!args$quiet) {
    print("Filled values") 
    print(fill_na_result$report)
    print("Outliers")
    print(outliers_report)
    print("Column Characteristics")
    for (column in names(column_characteristics_list)) {
      print(paste("Characteristics for", column))
      print(column_characteristics_list[[column]])
    }
    print("ANOVA test")
    print(anova_result)
  }
}

if (interactive() == FALSE) {
  main()
}