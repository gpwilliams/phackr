# sample data and make a tibble with DV and ID based on input group sample size
# this is fixed at means of 100 and sd of 15 for each group
sample_groups <- function(n_group){
  group_one <- rnorm(n = n_group, mean = 100, sd = 15)
  group_two <- rnorm(n = n_group, mean = 100, sd = 15)
  
  data <- tibble(
    dv = c(group_one, group_two),
    id = c(rep("group_one", n_group),
           rep("group_two", n_group)
    )
  )
}

# run a t-test and extract the p-value for a tibble
# columns must be named dv and id for dependent variables and group ID
test_groups <- function(tibble_data) {
  test <- t.test(dv ~ id, data = tibble_data, paired = FALSE)
  test$p.value
}

# optional stopping without correction:
# sample data and extract p-values from a t-test
# if non-significant, sample additional people and retest
# sample up to a certain number of additional blocks of subjects
# using loop_total
p_hack <- function(additions = 1, original_n = 30, additional_n = 10){
  run <- 0
  original_data <- sample_groups(original_n)
  p_val <- test_groups(original_data)
  
  while (p_val > .05 & run < additions) {
    new_data <- rbind(original_data, sample_groups(additional_n))
    p_val <- test_groups(new_data)
    run <- run + 1
  }
  p_val
}