# Project: stat-sci-support: Power and Sample Size Calculations based on Comparing Expectations
#   Description: Power and sample size justification for common requests with vague research
#     hypotheses involving comparisons (e.g., one-sample, two-sample) and possible multiple
#     testing (i.e., multiplicity).



# Preliminary processing


## Clear environment and install/load packages.
rm(list=ls())
if (!require("pacman")) install.packages("pacman") # just to make sure "pacman" is installed
pacman::p_load(tidyverse, knitr, ggpubr) # e.g., naniar, extrafont, tidyverse, reshape2, janitor, lubridate, jsonlite, arsenal, knitr, feather


## Set global parameters.
scalar_seed <- # e.g., set to your current datetime
  output_path <- "/LocalPath/data/MyProjectFolder/" # your local path and project folder within the "data" folder



# Simulate data.
sampsize_n <- 100 # set as needed
num_of_tests <- 50 # set as needed, but use same sampsize_n for all values used; common values are 1 and 50 to demonstrate 1 test or 50 simultaneous tests (controlling for multiplicity)

tbl_sims_1 <- dplyr::tibble(
  
  sig_level = rep(1, 2) %x% c(0.05, 0.01, 0.001) %x% rep(1, 2) / num_of_tests,
  power_value = rep(1, 2) %x% rep(1, 3) %x% c(0.90, 0.99),
  one_two_sample = c(rep("one.sample", 6), rep("two.sample", 6)),
  sampsize = c(rep(sampsize_n, 6), rep(sampsize_n / 2, 6)),
  sampsize_n = sampsize_n,
  num_of_tests = num_of_tests
  
)
tbl_sims_1$cohens_d <- apply(
  X = tbl_sims_1,
  MARGIN = 1,
  FUN = function(x) pwr::pwr.t.test(
    n = x["sampsize"] %>% as.numeric,
    sig.level = x["sig_level"] %>% as.numeric,
    power = x["power_value"] %>% as.numeric,
    type = x["one_two_sample"],
    alternative = "two.sided"
  )$d
)
tbl_sims_2 <- tbl_sims_1 %>%
  dplyr::mutate(
    
    fixed_association_size = 10,
    needed_sd = fixed_association_size / cohens_d
    
  )
tbl_sims_3 <- tbl_sims_2 %>%
  dplyr::mutate(
    
    # fixed_sd = median(tbl_sims_2$needed_sd),
    fixed_sd = 50,
    needed_association_size = cohens_d * fixed_sd,
    Scenario = seq(1:nrow(tbl_sims_2))
    
  )

tbl_sims_final <- tbl_sims_3
tbl_sims_final %>% readr::write_csv(paste0(output_path, "tbl_sims_final_ntests", num_of_tests, ".csv"))

tbl_sims_fancy <- tbl_sims_final %>%
  dplyr::mutate(
    
    string_one_two_sample = ifelse(
      one_two_sample == "one.sample",
      "one-sample difference from zero",
      "difference between two samples"
    ),
    Description = paste0(
      "With ",
      (1 - sig_level) * 100,
      "% confidence, there is at least a ",
      power_value * 100,
      "% chance of detecting an average ",
      string_one_two_sample,
      " of at least ",
      round(fixed_association_size, 2),
      " or greater with an outcome SD of ",
      round(needed_sd, 2),
      ", or a difference of at least ",
      round(needed_association_size, 2),
      " or greater with an outcome SD of ",
      round(fixed_sd, 2),
      "."
    )
  ) %>%
  dplyr::select(Scenario, Description)
tbl_sims_fancy %>% readr::write_csv(paste0(output_path, "tbl_sims_fancy_ntests", num_of_tests, ".csv"))



# Plot data.
element_text_size <- 10
fig_height <- 600
fig_width <- 800


## Function
plotScenario <- function(
    Scenario,
    ylim_touse = NA
) {
  
  i <- Scenario
  
  set.seed(scalar_seed + i)
  title_touse <- paste0(
    # ifelse(tbl_sims_final$one_two_sample[i] == "one.sample", "One Sample: ", "Two Samples: "),
    "Scenario ",
    tbl_sims_final$Scenario[i],
    ": Conf. ",
    (1 - tbl_sims_final$sig_level[i]) * 100,
    "%, Power ",
    tbl_sims_final$power_value[i] * 100,
    "%"
  )
  tbl_plot <- dplyr::tibble(
    
    association_size = c(
      rep(
        round(tbl_sims_final$fixed_association_size[i], 2),
        sampsize_n
      ),
      rep(
        round(tbl_sims_final$needed_association_size[i], 2),
        sampsize_n
      )
    ),
    sd = c(
      rep(
        round(tbl_sims_final$needed_sd[i], 2),
        sampsize_n
      ),
      rep(
        round(tbl_sims_final$fixed_sd[i], 2),
        sampsize_n
      )
    ),
    x = c(
      rep(
        paste0(
          "Avg. Diff. = ",
          round(tbl_sims_final$fixed_association_size[i], 2),
          ", SD = ",
          round(tbl_sims_final$needed_sd[i], 2)
        ),
        sampsize_n
      ),
      rep(
        paste0(
          "Avg. Diff. = ",
          round(tbl_sims_final$needed_association_size[i], 2),
          ", SD = ",
          round(tbl_sims_final$fixed_sd[i], 2)
        ),
        sampsize_n
      )
    ) %>% factor,
    y = c(
      rnorm(n = sampsize_n / 2, ifelse(tbl_sims_final$one_two_sample[i] == "one.sample", tbl_sims_final$fixed_association_size[i], 0), tbl_sims_final$needed_sd[i]),
      rnorm(n = sampsize_n / 2, tbl_sims_final$fixed_association_size[i], tbl_sims_final$needed_sd[i]),
      rnorm(n = sampsize_n / 2, ifelse(tbl_sims_final$one_two_sample[i] == "one.sample", tbl_sims_final$needed_association_size[i], 0), tbl_sims_final$fixed_sd[i]),
      rnorm(n = sampsize_n / 2, tbl_sims_final$needed_association_size[i], tbl_sims_final$fixed_sd[i])
    ),
    mean = c(
      rep(round(ifelse(tbl_sims_final$one_two_sample[i] == "one.sample", tbl_sims_final$fixed_association_size[i], 0), 2), sampsize_n / 2),
      rep(round(tbl_sims_final$fixed_association_size[i], 2), sampsize_n / 2),
      rep(round(ifelse(tbl_sims_final$one_two_sample[i] == "one.sample", tbl_sims_final$needed_association_size[i], 0), 2), sampsize_n / 2),
      rep(round(tbl_sims_final$needed_association_size[i], 2), sampsize_n / 2)
    ),
    Group = c(
      rep(ifelse(tbl_sims_final$one_two_sample[i] == "one.sample", paste0("n = ", sampsize_n), paste0("A (n = ", sampsize_n / 2, ")")), sampsize_n / 2),
      rep(ifelse(tbl_sims_final$one_two_sample[i] == "one.sample", paste0("n = ", sampsize_n), paste0("B (n = ", sampsize_n / 2, ")")), sampsize_n / 2),
      rep(ifelse(tbl_sims_final$one_two_sample[i] == "one.sample", paste0("n = ", sampsize_n), paste0("A (n = ", sampsize_n / 2, ")")), sampsize_n / 2),
      rep(ifelse(tbl_sims_final$one_two_sample[i] == "one.sample", paste0("n = ", sampsize_n), paste0("B (n = ", sampsize_n / 2, ")")), sampsize_n / 2)
    )
    
  )
  tbl_plot$x <- relevel(
    tbl_plot$x,
    paste0(
      "Avg. Diff. = ",
      round(tbl_sims_final$fixed_association_size[i], 2),
      ", SD = ",
      round(tbl_sims_final$needed_sd[i], 2)
    )
  )
  
  xlab_touse <- ifelse(tbl_sims_final$one_two_sample[i] == "two.sample", "Group", "")
  ggp_out <- tbl_plot %>%
    ggplot2::ggplot(aes(x = Group, y = y)) +
    ggplot2::theme_classic() +
    ggplot2::geom_hline(yintercept = 0, color = "grey") +
    ggplot2::geom_jitter(height = 0, alpha = 0.2) +
    ggplot2::geom_point(aes(y = mean), size = 5) +
    ggplot2::geom_errorbar(
      aes(
        ymin = mean - qnorm(1 - tbl_sims_final$sig_level[i]/2) * sd / sqrt(sampsize_n),
        ymax = mean + qnorm(1 - tbl_sims_final$sig_level[i]/2) * sd / sqrt(sampsize_n)
      )
      # , size = 2
    ) +
    ggplot2::ggtitle(title_touse) +
    ggplot2::xlab(xlab_touse) +
    ggplot2::ylab("Outcome") +
    ggplot2::theme(text = element_text(size = element_text_size)) +
    ggplot2::facet_grid(cols = vars(x))
  if (!anyNA(ylim_touse)) ggp_out <- ggp_out +
    ggplot2::ylim(ylim_touse)
  
  # for plotting uniform y-axis range across all scenarios
  scenario_ylim <- c(min(tbl_plot$y), max(tbl_plot$y))
  
  return(
    list(
      tbl_plot = tbl_plot,
      ggp_out = ggp_out,
      scenario_ylim = scenario_ylim
    )
  )
  
}


## Plot scenarios.

### Get y min and max values across all scenarios.
ylim_omnibus <- c(
  min(
    plotScenario(1)$scenario_ylim %>% min,
    plotScenario(2)$scenario_ylim %>% min,
    plotScenario(3)$scenario_ylim %>% min,
    plotScenario(4)$scenario_ylim %>% min,
    plotScenario(5)$scenario_ylim %>% min,
    plotScenario(6)$scenario_ylim %>% min,
    plotScenario(7)$scenario_ylim %>% min,
    plotScenario(8)$scenario_ylim %>% min,
    plotScenario(9)$scenario_ylim %>% min,
    plotScenario(10)$scenario_ylim %>% min,
    plotScenario(11)$scenario_ylim %>% min,
    plotScenario(12)$scenario_ylim %>% min
  ),
  max(
    plotScenario(1)$scenario_ylim %>% max,
    plotScenario(2)$scenario_ylim %>% max,
    plotScenario(3)$scenario_ylim %>% max,
    plotScenario(4)$scenario_ylim %>% max,
    plotScenario(5)$scenario_ylim %>% max,
    plotScenario(6)$scenario_ylim %>% max,
    plotScenario(7)$scenario_ylim %>% max,
    plotScenario(8)$scenario_ylim %>% max,
    plotScenario(9)$scenario_ylim %>% max,
    plotScenario(10)$scenario_ylim %>% max,
    plotScenario(11)$scenario_ylim %>% max,
    plotScenario(12)$scenario_ylim %>% max
  )
)

### Set y min and max values across all scenarios.
ylim_omnibus <- c(-100, 150)

### Plot one- and two-sample cases.
title_suffix <- " Statistical Test"
if (num_of_tests > 1) title_suffix <- paste0(title_suffix, "s")

#### Only consider one-sample case when num_of_tests == 1.
png(
  filename = paste0(output_path, "figs_one_sample_ntests", num_of_tests, ".png"),
  height = fig_height,
  width = fig_width
)
ggpubr::ggarrange(
  plotScenario(1, ylim_touse = ylim_omnibus)$ggp_out,
  plotScenario(3, ylim_touse = ylim_omnibus)$ggp_out,
  plotScenario(5, ylim_touse = ylim_omnibus)$ggp_out,
  plotScenario(2, ylim_touse = ylim_omnibus)$ggp_out,
  plotScenario(4, ylim_touse = ylim_omnibus)$ggp_out,
  plotScenario(6, ylim_touse = ylim_omnibus)$ggp_out,
  ncol = 3,
  nrow = 2
  # ) %>%
  #   ggpubr::annotate_figure(top = text_grob(paste0("One Sample: ", num_of_tests, title_suffix), size = element_text_size * 1.5))
)
dev.off()

png(
  filename = paste0(output_path, "figs_two_sample_ntests", num_of_tests, ".png"),
  height = fig_height,
  width = fig_width
)
ggpubr::ggarrange(
  plotScenario(7, ylim_touse = ylim_omnibus)$ggp_out,
  plotScenario(9, ylim_touse = ylim_omnibus)$ggp_out,
  plotScenario(11, ylim_touse = ylim_omnibus)$ggp_out,
  plotScenario(8, ylim_touse = ylim_omnibus)$ggp_out,
  plotScenario(10, ylim_touse = ylim_omnibus)$ggp_out,
  plotScenario(12, ylim_touse = ylim_omnibus)$ggp_out,
  ncol = 3,
  nrow = 2
  # ) %>%
  #   ggpubr::annotate_figure(top = text_grob(paste0("Two Samples: ", num_of_tests, title_suffix), size = element_text_size * 1.5))
)
dev.off()