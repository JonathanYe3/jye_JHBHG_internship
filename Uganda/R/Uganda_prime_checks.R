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
format_df <- function(df, which = "Flanker"){
      variant <- ifelse(which == "Flanker", "nih-hiv-cns3-p2", "pad-jhu-brace")
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
ug001 <- flank_list[[1]]
ug001_formatted <- format_df(ug001) 



