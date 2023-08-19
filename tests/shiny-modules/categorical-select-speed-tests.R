# When working with monstrous (in samples) dataset, like the UKBB, the covarite
# filters really slow things down. Let's see if we can unwind the steps and
# make things faster.
library(FacileData)
library(dplyr)
library(dbplyr)
devtools::load_all(".")

# The Nightingale dataset has 117,947 samples and 10 covariates
xfds <- FacileDataSet("~/workspace/facilebio/data/FacileNightingaleDataSet")

# These are some things that happen when a new set of samples rolls its way
# through the covariate picker.

# Bring up the samples facile_frame:
system.time(samples.all <- samples(xfds) |> collect(n = Inf))
#  user  system elapsed 
# 0.330   0.076   0.612 

# How long does it take to test that this samples.all frame belongs to
# the FDS: 0.1s
system.time(test_sample_subset(samples.all, xfds))
#  user  system elapsed 
# 0.120   0.008   0.130 

# Sequence of events that updates the sample covariates: -----------------------

# 1a. retrieve all the sample covariate names.
#    the first time, this takes ~7s, but subsequent calls are closer to 2.5s
system.time(acovs <- fetch_sample_covariates(xfds, samples.all))
#  user  system elapsed 
# 3.930   0.823   7.629 
#  user  system elapsed 
# 2.530   0.098   2.635 

# Writing a new function to make covariate summarization quick
# I could have sworn I've done this before :-/
# 
# Just the names:
system.time({
  acovs.fds <- xfds |> 
    sample_covariate_summary(categorical_only = TRUE, with_levels = FALSE,
                             verbose = TRUE)
})
# 3.8s first time
# 1.5s subsequent times

# With levels:
system.time({
  acovs.lvls <- xfds |> 
    sample_covariate_summary(categorical_only = TRUE, with_levels = TRUE)
})
# 3.8s first time
# 1.5s subsequent times


# Passing in a tbl to use for a semi_join is a NON-STARTER =====================
if (FALSE) {
  # Let's not accidentally run this, it takes forever and I had to kill R to
  # come back.
  system.time({
    acovs.samples <- sample_covariate_summary(samples.all, verbose = TRUE)
  })
}

# Let's leverage the database to get DISTINCT sample covariates
cov.lvs <- xfds |> 
  sample_covariate_tbl() |> 
  filter(class == "categorical") |> 
  distinct(variable) |> 
  show_query()
