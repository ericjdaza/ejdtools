# Filenames are anonymized here and are not pushed to GitHub.
# If applicable, this code must be run for 0-get code to work.



# Define mapping.
anonymizeFilenames_original <- c(
  "[file01.csv]",
  "[file02.json]",
  ...
)
anonymizeFilenames_anonymized <- c(
  "input01",
  "input02",
  ...
)



# Create mapping.
anonymizeFilenames_mapping <- setNames(
  anonymizeFilenames_original,
  anonymizeFilenames_anonymized
)
rm(
  anonymizeFilenames_original,
  anonymizeFilenames_anonymized
)



# # Check that mapping is correct.
# anonymizeFilenames_mapping
