# read data
read_data <- function(filename) {
  read.csv(filename, comment.char="#")
}

clustered.data <- function() {
  read_data("data/targets_clustered_006.csv")
}

power.data <- function() {
  read_data("data/targets_power_005.csv")
}

random.data <- function() {
  read_data("data/targets_random_005.csv")
}
