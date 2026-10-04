# load the tidyverse package for data manipulation and plotting
library(tidyverse)
library(ggplot2)

set.seed(123)

# proportion of red/black
theta <- 0.6 # declare probability parameter for the binomial model

# Sequence of trials
trials <- seq(10, 1000, by = 10)

# Number of simulation draws from the model
nsims <- 1000 # number of of simulations from the binomial model

# Helper function for getting the proportions
binom_gen <- function(trials,theta,nsims){
    df <-  as.data.frame(rbinom(nsims,trials,theta)/trials) |> mutate(nsims = nsims,trials = trials)
    colnames(df) <- c("Proportions","Nsims","Trials")
  return(df)
}

# Create a data frame containing the draws for each number of trials
proportion_60 <- do.call(rbind, lapply(trials, binom_gen, theta, nsims)) # lapply applies elements in trials column to binom_gen function, which is then rowbound via do.call

# Inspect the result
print(dim(proportion_60))
print(head(proportion_60))

summary_60 <- proportion_60 |>
  group_by(Trials) |>
  summarise(mean = mean(Proportions), sd = sd(Proportions), .groups = "drop")
print(summary_60 |> filter(Trials %in% c(10, 50, 100, 500, 1000)))

# Plot the simulated proportions against the number of trials
p <- ggplot(proportion_60, aes(x = Trials, y = Proportions)) +
  geom_point(alpha = 0.05, size = 0.5) +
  geom_hline(yintercept = theta, colour = "red") +
  labs(title = "Simulated proportions (theta = 0.6)",
       x = "Number of trials", y = "Proportion of successes")
ggsave("proportion_60.png", p, width = 7, height = 4.5, dpi = 120)
