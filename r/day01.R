library(data.table)
DT <- fread("./input/day01.txt", header = FALSE)
# part 1
sum(data.table(loc1 = sort(DT$V1), loc2 = sort(DT$V2))[, diff := abs(loc2 - loc1)]$diff)
# part 2
lookup <- as.data.table(table(DT$V2))[, V1 := as.numeric(V1)]
sum(DT[lookup, on = .(V1)][!is.na(V2), V1 * N])

