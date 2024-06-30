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
    default = "test.csv"
  )
  parser$add_argument(
    "--grouping_column",
    type = "character",
    help = "Name of the column used for grouping data", 
    default = "grupa"
  )
  parser$add_argument(
    "--control_group_name",
    type = "character",
    help = "Whether to save the plots as PNG files (TRUE or FALSE)",
    default = "KONTROLA"
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

perform_statistical_analysis <- function(data, group_column_name, target_column_name) {
  if (target_column_name == group_column_name) {
    return(NULL)
  }
  kruskal_test_result <- kruskal.test(as.formula(paste(target_column_name, "~", group_column_name)), data = data)
  
  if (kruskal_test_result$p.value < 0.05) {
    post_hoc_result <- pairwise.wilcox.test(data[[target_column_name]], data[[group_column_name]], p.adjust.method = "BH")
    return(list("kruskal" = kruskal_test_result, "post_hoc" = post_hoc_result))
  } else {
    return(list("kruskal" = kruskal_test_result))
  }
}

plot_group_comparisons <- function(data, group_column_name, target_column_name) {
  library(ggplot2)
  
  ggplot(data, aes_string(x = group_column_name, y = target_column_name, color = group_column_name)) +
    geom_point() +
    theme_minimal() +
    ggtitle(paste("Comparison of", target_column_name, "across", group_column_name)) +
    theme(plot.title = element_text(hjust = 0.5))
}

save_comparison_plot <- function(data, group_column_name, target_column_name) {
  plot <- plot_group_comparisons(data, group_column_name, target_column_name)
  ggsave(paste0(target_column_name, ".png"), plot)
}

find_correlation_description <- function(correlation_coefficient) {
  for (i in 1:nrow(CORRELATION_MAPPING)) {
    if (correlation_coefficient >= CORRELATION_MAPPING$lower_bound[i] && correlation_coefficient <= CORRELATION_MAPPING$upper_bound[i]) {
      return(CORRELATION_MAPPING$correlation[i])
    }
  }
  return("Correlation out of range")
}

print_group_correlation <- function(data, grouping_column, control_group_name) {
  if (!grouping_column %in% names(data)) {
    stop("grouping_column does not exist in the dataframe")
  }
  
  if (!control_group_name %in% unique(data[[grouping_column]])) {
    stop("control_group_name is not found in the grouping_column")
  }

  control_data <- data %>% dplyr::filter(!!rlang::sym(grouping_column) == control_group_name)
  
  groups <- unique(data[[grouping_column]])
  groups <- groups[groups != control_group_name]
  
  for (target_column_name in colnames(data)) {
    if (target_column_name != grouping_column) {
      
      print(paste("Analyzing", target_column_name, "column:"))
      
      for (group in groups) {
        group_data <- data %>% dplyr::filter(!!rlang::sym(grouping_column) == group)
        combined_data <- rbind(control_data, group_data)
        
        kruskal_test_result <- kruskal.test(as.formula(paste(target_column_name, "~", grouping_column)), data = combined_data)
        correlation_description <- find_correlation_description(kruskal_test_result$p.value)
        print(paste("Kruskal-Wallis test for", group, "vs", control_group_name, "in", target_column_name, ":", correlation_description))
      }
    }
  }
}

main <- function() {
  install_libraries(REQUIRED_LIBRARIES)
  parser <- get_parser()
  args <- parser$parse_args()
  
  data <- read.csv(args$input, sep = ";", dec = ",", header = TRUE)
  
  fill_na_result <- fill_na(data = data)
  fille_data <- fill_na_result$data

  write.csv(fille_data, file = "filled_data.csv", row.names = FALSE)
  
  outliers_report <- get_shapiro_test_results(fille_data, args$grouping_column)

  column_characteristics_list <- list()
  for (column in names(fille_data)) {
    column_characteristic <- get_column_characteristic(fille_data, args$grouping_column, column)
    column_characteristics_list[[column]] <- column_characteristic
    
    analysis_result <- perform_statistical_analysis(fille_data, args$grouping_column, column)
    save_comparison_plot(fille_data, args$grouping_column, column)
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
    print("Analysis Result")
    print(analysis_result)
    print("Group Correlation")
    print_group_correlation(fille_data, args$grouping_column, args$control_group_name)
  }
}

main()