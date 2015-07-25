pml_train <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
pml_test <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

str(pml_train)
str(pml_test)

use_data(pml_train)
use_data(pml_test)
