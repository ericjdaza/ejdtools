# Quick function to calculate sample sizes.
#   Inputs: mu1, mu0, sd
#   Output: sample_size, power, alpha
sampSizes <- function(
  mu1,
  mu0,
  sd,
  alpha_start = 0.01,
  alpha_end = 0.10,
  alpha_increment = 0.01,
  power_start = 0.70,
  power_end = 0.99,
  power_increment = 0.01
) {
  
  tbl_out <- dplyr::tibble(
    sig_level = round(
      seq(alpha_start, alpha_end, alpha_increment) %x% rep(1, length(seq(power_start, power_end, power_increment))),
      as.numeric(
        substr(
          x = format(alpha_increment, scientific = TRUE),
          start = regexpr("e", format(alpha_increment, scientific = TRUE))[1] + 2,
          stop =  regexpr("e", format(alpha_increment, scientific = TRUE))[1] + 3
        )
      )
    ),
    power = round(
      rep(1, length(seq(alpha_start, alpha_end, alpha_increment))) %x% seq(power_start, power_end, power_increment),
      as.numeric(
        substr(
          x = format(power_increment, scientific = TRUE),
          start = regexpr("e", format(power_increment, scientific = TRUE))[1] + 2,
          stop =  regexpr("e", format(power_increment, scientific = TRUE))[1] + 3
        )
      )
    )
  )
  tbl_out$sample_size_raw <- apply(
    X = tbl_out,
    MARGIN = 1,
    FUN = function(x) pwr::pwr.t.test(
      d = (mu1 - mu0) / sd,
      power = x[2],
      sig.level = x[1],
      type = "paired",
      alternative="two.sided"
    )$n
  )
  tbl_out$sample_size <- ceiling(tbl_out$sample_size_raw)
  tbl_out %>%
    dplyr::rename(
      `Support Level (alpha)` = sig_level,
      `Power (1 - beta)` = power,
      `Sample Size (Raw)` = sample_size_raw,
      `Sample Size` = sample_size
    )
  
}



# Quick function to calculate mean differences.
#   Inputs: sd, n1, n0
#   Output: mean_diff, power, alpha
meanDiffs <- function(
  sd,
  n1,
  n0,
  alpha_values = NA, # must also specify power_values
  alpha_start = 0.01,
  alpha_end = 0.10,
  alpha_increment = 0.01,
  power_values = NA, # must also specify alpha_values
  power_start = 0.70,
  power_end = 0.99,
  power_increment = 0.01
) {
  
  if (anyNA(alpha_values) & anyNA(power_values)) tbl_out <- dplyr::tibble(
    sig_level = round(
      seq(alpha_start, alpha_end, alpha_increment) %x% rep(1, length(seq(power_start, power_end, power_increment))),
      as.numeric(
        substr(
          x = format(alpha_increment, scientific = TRUE),
          start = regexpr("e", format(alpha_increment, scientific = TRUE))[1] + 2,
          stop =  regexpr("e", format(alpha_increment, scientific = TRUE))[1] + 3
        )
      )
    ),
    power = round(
      rep(1, length(seq(alpha_start, alpha_end, alpha_increment))) %x% seq(power_start, power_end, power_increment),
      as.numeric(
        substr(
          x = format(power_increment, scientific = TRUE),
          start = regexpr("e", format(power_increment, scientific = TRUE))[1] + 2,
          stop =  regexpr("e", format(power_increment, scientific = TRUE))[1] + 3
        )
      )
    )
  )
  if (!anyNA(alpha_values) & !anyNA(power_values)) tbl_out <- dplyr::tibble(
    sig_level = alpha_values %x% rep(1, length(power_values)),
    power = rep(1, length(alpha_values)) %x% power_values
  )

  tbl_out$mean_diff <- apply(
    X = tbl_out,
    MARGIN = 1,
    FUN = function(x) pwr::pwr.t2n.test(
      n1 = n0,
      n2 = n1,
      power = x[2],
      sig.level = x[1],
      alternative="two.sided"
    )$d * sd
  )
  
  tbl_out %>% return
  
}