###########################################################################################
# Data Preparation function
###########################################################################################
# The objective of this function is to pass a dataframe through numerous steps of 
# Data processing for the ML Fitting phase of this project.
###########################################################################################
# Input: Dataframe of interest 
# Output: One hot encoded and correctly formated dataframe
###########################################################################################

datprep <- function(Dataset){
  #One hot encoding function requires Caret
  require(caret)
  
  # Create new features based on other values using dplyr
  
  ## freq_formal_finance is 0 for low frequence and 1 for high frequence 
  ## tech_proficiency is 0 for not proficient and 1 for Proficient
  ## personal_investment is 0 for absent and 1 present
  ## country_offering is 0 if the country does not have active_mm_user and 1 for active_mm_user
  ## both_mm_bank is 0 is reg_mm_acct and reg_bank_acct are both 1 and 1 if they are both true
  
  ## https://stackoverflow.com/questions/39405628/how-do-i-create-a-new-column-based-on-multiple-conditions-from-multiple-columns
  
  Dataset  <- Dataset %>%
    mutate(freq_formal_finance = ifelse(as.numeric(as.character(num_formal_institutions_last_year) < 3) &
                                          as.numeric(as.character(num_financial_activities_last_year) < 5), "0","1")) %>%
    mutate(tech_proficiency = ifelse(can_use_internet == "TRUE" & can_text == "TRUE", "1","0")) %>%
    mutate(personal_investment = ifelse(has_investment == "TRUE" & income_own_business_last_year == "TRUE", "1","0")) %>%
    mutate(country_offering = ifelse(active_mm_user == "TRUE" & country %in% c("A", "B", "C", "D", "E","F","G","H","I","J"), "1","0")) %>%
    mutate(both_mm_bank = ifelse(reg_mm_acct == "TRUE" & reg_bank_acct == "TRUE", "1","0"))
  
  ## Extract all the logical and character variables for the training data
  dat_class <-
    Dataset[, sapply(Dataset, class) %in% c('character', 'logical')]
  ## Extract all the numeric variables for the training data
  dat_num <-
    Dataset[, sapply(Dataset, class) %in% c('numeric')]
  ## Extract potential factor variables from default numeric for the training data
  potential_fact <-
    dat_num %>% select(
      education_level,
      share_hh_income_provided,
      num_times_borrowed_last_year,
      borrowing_recency,
      num_shocks_last_year,
      phone_technology,
      num_informal_institutions_last_year
    )
  ## Keep the remaining numeric variables seperate for the training data
  num_dat <-
    dat_num[,-which(
      names(dat_num) %in% c(
        "education_level",
        "share_hh_income_provided",
        "num_times_borrowed_last_year",
        "borrowing_recency",
        "num_shocks_last_year",
        "phone_technology",
        "num_informal_institutions_last_year"
      )
    )]
  
  ## Bind the potential factor variables for the training data
  datframe <- cbind(dat_class, potential_fact)
  
  ## One hot encode the logical and character variables for the training data
  dmy <- dummyVars(" ~ .", data = datframe)
  trsf <- data.frame(predict(dmy, newdata = datframe))

  ## Create the final training dataset 
  final_df <- cbind(trsf,num_dat)
  return(final_df)
}

###########################################################################################
# Cramer V's Correlation for categorical variables function
###########################################################################################
# Function that accepts matrix for coefficients and data and returns a correlation matrix
# Source: https://stackoverflow.com/questions/44070853/association-matrix-in-r
###########################################################################################
# Input: Dataframe of interest (containing only categorical variables) and empty matrix 
# Output: Cramer's V correlation between categorical variables 
###########################################################################################

calculate_cramer <- function(m, df) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
    }
  }
  return(m)
}


