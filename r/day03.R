input <- readLines("./input/day03_sample.txt")
input <- readLines("./input/day03.txt")

library(stringr)

#part 1
test <- str_extract_all(input, "(?<=mul\\()[0-9]+,[0-9]+(?=\\))", simplify = TRUE)
sapply(strsplit(test, ","), function(x) {
  as.numeric(x[1]) * as.numeric(x[2])
}) |> sum()

#part2
input <- readLines("./input/day03_sample2.txt")
# remove all parts between don't and do
L <- str_remove_all(input,  "(?<=don't).*?(?=do)") |>
  str_extract_all("(?<=mul\\()[0-9]+,[0-9]+(?=\\))", simplify = TRUE) |>
  unlist() |>
  strsplit(",")

sapply(L, function(x) {
    as.numeric(x[1]) * as.numeric(x[2])
  }) |> sum()
  
