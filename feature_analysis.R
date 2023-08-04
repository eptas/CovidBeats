library(haven)
library(readxl)
library(dplyr)
library(writexl)

setwd("C:/Users/esthe/Desktop/Scientific Reports/Featureanalyse")

merged_data <- readxl::read_excel(excel_file)


# LMMS pleasure > social provision

model_pleasure_sum <- lm(sps5_sum ~ pleasure, data = merged_data)
summary(model_pleasure_sum)
confint(model_pleasure_sum)

# LMMS pleasure > reward

model_pleasure_reward <- lm(reassurance ~ pleasure, data = merged_data)
summary(model_pleasure_reward)
confint(model_pleasure_reward)

# LMMS pleasure > attachment

model_pleasure_attach <- lm(attachment ~ pleasure, data = merged_data)
summary(model_pleasure_attach)
confint(model_pleasure_attach)

# LMMS pleasure > reliance

model_pleasure_reliable <- lm(sense_reliable_alliance ~ pleasure, data = merged_data)
summary(model_pleasure_reliable)
confint(model_pleasure_reliable)


# Acoustical features > DV might be replaced by sps5_sum, reassurance, attachment or sense_reliable_alliance


model_melf <- lm (pleasure ~ melbands_flatness_db.stdev, data = merged_data)
summary(model_melf)
confint(model_melf)


model_bpm <- lm (pleasure ~ bpm, data = merged_data)
summary(model_bpm)
confint(model_bpm)


model_onset <- lm (pleasure ~ onset_rate, data = merged_data)
summary(model_onset)
confint(model_onset)


# quadratic fit

model_speccompsq <- lm (pleasure ~ poly(spectral_complexity.mean, degree = 2), data = merged_data)
summary(model_speccompsq)
confint(model_speccompsq)


model_zero <- lm (pleasure ~  zerocrossingrate.stdev, data = merged_data)
summary(model_zero)
confint(model_zero)


model_flux <- lm (pleasure ~ spectral_flux.stdev, data = merged_data)
summary(model_flux)
confint(model_flux)


model_cent <- lm (pleasure ~ spectral_centroid.max, data = merged_data)
summary(model_cent)
confint(model_cent)


# quadratic fit

model_dyn <- lm (pleasure ~ poly(dynamic_complexity, degree = 2), data = merged_data)
summary(model_dyn)
confint(model_dyn)


model_mfc1 <- lm (pleasure ~ mfc1, data = merged_data)
summary(model_mfc1)
confint(model_mfc1)


# quadratic fit

model_speccompsq2 <- lm (pleasure ~ poly(spectral_complexity.median, degree = 2), data = merged_data)
summary(model_speccompsq2)
confint(model_speccompsq2)

