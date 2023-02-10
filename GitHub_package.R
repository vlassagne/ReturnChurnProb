# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


return_churn_prob <- function(data,customerId){
  if (customerId %in% data$CustomerId){
    churned_customer <- glm(
      Exited ~ CreditScore +
        Gender +
        Age +
        Tenure +
        Balance +
        NumOfProducts +
        HasCrCard +
        IsActiveMember +
        EstimatedSalary,
      data = data,
      family = "binomial")

    data$Exited <- as.factor(data$Exited)
    data$Gender <- as.factor(data$Gender)
    data[, prediction := predict(churned_customer, data, type="response")]

    x <- data[CustomerId == customerId, prediction]
    return(x)
  } else {
    return("Customer ID does not exist")
  }
}
