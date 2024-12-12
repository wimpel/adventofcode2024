library(RcppAlgos)
library(data.table)
library(stringr)

input <- readLines("./input/day07_sample.txt")
input <- readLines("./input/day07.txt")

# part 1
outcome <- sapply(strsplit(input, ": "), `[[`, 1) |> as.numeric()
numbers <- sapply(strsplit(input, ": "), `[[`, 2) 

operators.possible <- c("+", "*")
# function to get all permutation sof n possible operators
operator.options <- function(x) permuteGeneral(operators.possible, x, repetition = TRUE, FUN = paste, FUN.VALUE = NULL)

final <- vector(mode="logical", length = length(input))

for (i in seq.int(numbers)) {
  desired <- outcome[i]
  operator.poss <- operator.options(str_count(numbers[i], " "))
  tempnumbers <- strsplit(numbers[i], " ") |> unlist()
  answer <- vector(mode = "numeric", length = length(operator.poss))
  for (j in seq.int(operator.poss)) {
    operators.used <- operator.poss[j] |> unlist()
    for (k in seq.int(tempnumbers)) {
      if (k == 1) {
        ans <- tempnumbers[k] 
      } else {
        to.parse <- paste0(ans, operators.used[k-1], tempnumbers[k])
        ans <- eval(parse(text = to.parse))
      }
    }
    answer[j] <- ans
  }
  if (desired %in% answer) final[i] <- TRUE
  print(i)
}

sum(outcome[final])


################################

input <- readLines("./input/day07_sample.txt")
### part 2

outcome <- sapply(strsplit(input, ": "), `[[`, 1) |> as.numeric()
numbers <- sapply(strsplit(input, ": "), `[[`, 2) 

operators.possible <- c("+", "*", "||")
# function to get all permutation sof n possible operators
operator.options <- function(x) permuteGeneral(operators.possible, x, repetition = TRUE, FUN = paste, FUN.VALUE = NULL)

final <- vector(mode="logical", length = length(input))

for (i in seq.int(numbers)) {
  desired <- outcome[i]
  operator.poss <- operator.options(str_count(numbers[i], " "))
  tempnumbers <- strsplit(numbers[i], " ") |> unlist()
  answer <- vector(mode = "numeric", length = length(operator.poss))
  for (j in seq.int(operator.poss)) {
    operators.used <- operator.poss[j] |> unlist()
    for (k in seq.int(tempnumbers)) {
      if (k == 1) {
        ans <- tempnumbers[k] 
      } else {
        if (operators.used[k-1] == "||") {
          ans <- paste0(ans)
        }
        to.parse <- paste0(ans, operators.used[k-1], tempnumbers[k])
        ans <- eval(parse(text = to.parse))
      }
    }
    answer[j] <- as.numeric(ans)
  }
  if (desired %in% answer) final[i] <- TRUE
  print(i)
}

sum(outcome[final])