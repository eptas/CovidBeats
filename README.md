# Data Analyses
**Paper title:** The vibe of musical and social reward: Listening to beat-based music as a surrogate for socioemotional support during the Covid-19 pandemic across Europe 

**Date:** 19/03/2026  

## Authors
* **Lena Esther Ptasczynski** [Charité - Universitätsmedizin Berlin, lena-esther.ptasczynski@charite.de]
* **Patrick Blättermann** [Institute of Sound and Vibration Engineering, Hochschule Düsseldorf, patrick.blaettermann@hs-duesseldorf.de]
* **Fabian Greb** [independent researcher, fabian.greb@gmx.de]
* **Philipp Sterzer** [University Psychiatric Clinics Basel, philipp.sterzer@upk.ch]
* **Jochen Steffens** [Institute of Sound and Vibration Engineering, Hochschule Düsseldorf, jochen.steffens@hs-duesseldorf.de]

---

## Libraries Required
The following R libraries are required for this analysis:
`dplyr`, `tibble`, `lme4`, `lmerTest`, `mediation`, `purrr`, `knitr`, `DT`, `performance`.

---

## Script Files and Reports

| File | Description |
| :--- | :--- |
| `analyses.Rmd` | Pre/post Covid-19 and mediation analyses described in the Method section of the paper. |
| `analyses.md` | Compiled report (Markdown) of the pre/post Covid-19 and mediation analyses script. It can be regenerated using `analyses.Rmd` and `data/data_all_europe_st.rds`, `data/data_all_stress_europe.rds`, and `data/data_all_stress_europe_st.rds` |
| `docs/analyses.html` | Compiled report (HTML) of the Pre/post Covid-19 and mediation analyses script. It can be regenerated using `analyses.Rmd` and `data/data_all_europe_st.rds`, `data/data_all_stress_europe.rds`, and `data/data_all_stress_europe_st.rds` |

### Data Files
* `data/data_all_europe_st.rds`: Data frame required by `analyses.Rmd` script.
* `data/data_all_stress_europe.rds`: Data frame required by `analyses.Rmd` script.
* `data/data_all_stress_europe_st.rds`: Data frame required by `analyses.Rmd` script.

---
