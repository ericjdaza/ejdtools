# Compare pwr::pwr.t.test and stats:power.t.test.
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, pwr)



# Set parameters.
alpha <- 0.05
power <- 0.80
delta <- 10
sigma <- 3
sample_size <- 3
icc <- 0.5



# Check for identical sample sizes.


## one sample / one group
pwr::pwr.t.test(
  d = delta / sigma,
  sig.level = alpha,
  power = power,
  type = "one.sample",
  alternative = "two.sided"
)
stats::power.t.test(
  delta = delta,
  sd = sigma,
  sig.level = alpha,
  power = power,
  type = "one.sample",
  alternative = "two.sided"
)

### FINDING: Pass.


## two samples / two groups
pwr::pwr.t.test(
  d = delta / sigma,
  sig.level = alpha,
  power = power,
  type = "two.sample",
  alternative = "two.sided"
)
stats::power.t.test(
  delta = delta,
  sd = sigma,
  sig.level = alpha,
  power = power,
  type = "two.sample",
  alternative = "two.sided"
)

### FINDING: Pass.



# Check for identical delta.


## one sample / one group
pwr::pwr.t.test(
  n = sample_size,
  sig.level = alpha,
  power = power,
  type = "one.sample",
  alternative = "two.sided"
)
stats::power.t.test(
  n = sample_size,
  sig.level = alpha,
  power = power,
  type = "one.sample",
  alternative = "two.sided"
)

### FINDING: Pass.


## two samples / two groups
pwr::pwr.t.test(
  n = sample_size,
  power = power,
  type = "two.sample",
  alternative = "two.sided"
)
stats::power.t.test(
  n = sample_size,
  sig.level = alpha,
  power = power,
  type = "two.sample",
  alternative = "two.sided"
)

### FINDING: Pass.



# Does changing group size help?
test_n1 <- 10
test_n2 <- pwr::pwr.t2n.test(
  d = 1.5,
  n1 = test_n1,
  sig.level = 0.01,
  power = 0.9,
  alternative = "two.sided"
)$n2
test_n2
pwr::pwr.t.test(
  n = (test_n1 + test_n2)/2,
  sig.level = 0.01,
  power = 0.9,
  type = "two.sample",
  alternative = "two.sided"
)
