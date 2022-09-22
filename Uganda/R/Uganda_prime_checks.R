pacman::p_load("dplyr", "tidyr", "readxl")

# Load dataframes
flank_prime <- read.csv("Uganda/Data/flanker_prime.csv")
face_prime <- read.csv("Uganda/Data/facetask_prime.csv")

Uganda_TwoTests <- "Uganda/Data/Uganda_TwoTests.xlsx"
flanker_post <- read_excel(path = Uganda_TwoTests, sheet = 4)
facetask_post <- read_excel(path = Uganda_TwoTests, sheet = 3)

#' Takes original facetask or flanker dataframes
#' Returns list of dataframes for each individual subject

split_BRACE_df <- function(df){
      df <- subset(df, subject != "ug-practice")
      subjects <- unique(df$subject)
      df_list <- group_split(df, subject)
      names(df_list) <- subjects
      return(df_list)
}

# Format dataframes
format_df <- function(df){
      #Split into two dfs - accurate, inaccurate
      num_accurate <- sum(df$accuracy)
      
      new_df <- data.frame(subject = unique(df$subject), 
                           block = unique(df$block), 
                           date = NA,
                           NumCompletions = 1,
                           test = which,
                           duration = "idk",
                           n=100,
                           n_Accurate =  num_accurate,
                           n_Inaccurate = 100-num_accurate,
                           PercentAccurate_Overall = num_accurate/100,
                           responseTime_Overall = mean(df$response.time..ms.),
                           responseTime_std_Overall = sd(df$response.time..ms.)
                           )
      return(new_df)
}

flank_list <- split_BRACE_df(flank_prime)
ug0001 <- flank_list[[1]]
ug0001_formatted <- format_df(ug001) 

#' To be used inside format_df
#' Takes a type such as congruent and spits out summary stats (overall, std_overall, accurate)
summary_stats <- function(df, type, type_string){
      names <- c(paste(type_string, "_n"), 
                 paste(type_string, "_n_Accurate"), paste(type_string, "_n_Inaccurate"),
                 paste(type_string, "_PercentAccurate_Overall"),
                 paste(type_string, "_responseTime_Overall"), paste(type_string, "__responseTime_std_Overall"),
                 paste(type_string, "_responseTime_Accurate"), paste(type_string, "_responseTime_std_Accurate"),
                 paste(type_string, "_responseTime_Inaccurate"), paste(type_string, "_responseTime_std_Inaccurate")
                 )
  
      type_df <- subset(df, type == type_string) # Ex: df with only "congruent" in type column
      accurate_df <- subset(type_df, accuracy == 1)
      inaccurate_df <- subset(type_df, accuracy == 0)
      
      new_df <- data.frame(
            sum(df$type == type_string),
            sum(accurate_df$accuracy), sum(inaccurate_df$type == type_string),
            sum(accurate_df$type == type_string)/sum(df$type == type_string),
            mean(type_df$response.time..ms.), sd(type_df$response.time..ms.),
            mean(accurate_df$response.time..ms.), sd(accurate_df$response.time..ms.),
            mean(inaccurate_df$response.time..ms.), sd(inaccurate_df$response.time..ms.)
            
      )
      names(new_df) <- names
      
      return(new_df)
}

ug0001_formatted <- summary_stats(ug001, type = congruent, type_string = "congruent")

# Loop over
formatted_flank <- lapply(flank_list, format_df)
