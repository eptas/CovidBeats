The vibe of musical and social reward: Listening to beat-based music as
a surrogate for socioemotional support during the Covid-19 pandemic
across Europe
================
Lena Esther Ptasczynski \[Charité - Universitätsmedizin Berlin,
<lena-esther.ptasczynski@charite.de>\]Patrick Blättermann \[Institute of
Sound and Vibration Engineering, Hochschule Düsseldorf,
<patrick.blaettermann@hs-duesseldorf.de>\]Fabian Greb \[independent
researcher, <fabian.greb@gmx.de>\]Philipp Sterzer \[University
Psychiatric Clinics Basel, <philipp.sterzer@upk.ch>\]Jochen Steffens
\[Institute of Sound and Vibration Engineering, Hochschule Düsseldorf,
<jochen.steffens@hs-duesseldorf.de>\]
20 März, 2026

- [Description](#description)
- [Load libraries](#load-libraries)
- [Helper functions](#helper-functions)
- [Load dataframes](#load-dataframes)
- [Calculate means and SEM](#calculate-means-and-sem)
- [Pre/post analysis](#prepost-analysis)
- [Create analysis plan for
  mediation](#create-analysis-plan-for-mediation)
- [Fit mediation models](#fit-mediation-models)
- [Mediation results](#mediation-results)
- [Mediation results (CI)](#mediation-results-ci)
- [Session info](#session-info)

# Description

This script runs mixed-effects mediation analyses for danceability,
valence, and energy, with the Stringency Index as predictor, temperature
as covariate, and multiple mediators. Additionally, it performs a
pre/post analysis comparing the period before versus after the onset of
the Covid-19 pandemic, and computes country-level descriptive
statistics.

<style>
.main-container {
  max-width: 95% !important;
  margin-left: 20px !important;
  margin-right: 20px !important;
}
&#10;.container-fluid {
  max-width: 95% !important;
}
&#10;body {
  max-width: 100%;
}
</style>

# Load libraries

``` r
# Install libraries if not installed already
# and load libraries
for (package in c("dplyr", "tibble", "performance", "lme4", "lmerTest", "mediation", "purrr", "knitr", "DT")) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}
```

# Helper functions

``` r
build_formula <- function(response, predictors) {
  as.formula(
    paste(response, "~", paste(predictors, collapse = " + "))
  )
}

format_effect <- function(est, p, sig, digits = 3) {
  p_txt <- ifelse(
    is.na(p),
    "NA",
    format.pval(p, digits = digits, eps = .001)
  )

  out <- sprintf(
    paste0("%.", digits, "f%s (p = %s)"),
    est, sig, p_txt
  )

  out[is.na(est)] <- NA_character_
  out
}

p_to_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < .001) return("***")
  if (p < .01)  return("**")
  if (p < .05)  return("*")
  if (p < .1)   return(".")
  ""
}

extract_coef_info <- function(model, term) {
  coef_tab <- coef(summary(model))

  if (!term %in% rownames(coef_tab)) {
    return(tibble::tibble(
      estimate = NA_real_,
      p_value = NA_real_,
      sig = NA_character_
    ))
  }

  p_col <- grep("^Pr\\(", colnames(coef_tab), value = TRUE)

  p_val <- if (length(p_col) == 1) coef_tab[term, p_col] else NA_real_

  tibble::tibble(
    estimate = unname(coef_tab[term, "Estimate"]),
    p_value = unname(p_val),
    sig = p_to_stars(unname(p_val))
  )
}

extract_model_effect_info <- function(model,
                                      term = "Stringency_index",
                                      conf_level = 0.95,
                                      ci_method = "Wald") {

  coef_tab <- coef(summary(model))

  if (!term %in% rownames(coef_tab)) {
    return(tibble::tibble(
      estimate = NA_real_,
      p_value = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      sig = NA_character_
    ))
  }

  p_col <- grep("^Pr\\(", colnames(coef_tab), value = TRUE)
  p_val <- if (length(p_col) == 1) coef_tab[term, p_col] else NA_real_

  ci_vals <- tryCatch(
    {
      ci_obj <- suppressMessages(
        confint(model, parm = term, level = conf_level, method = ci_method)
      )
      as.numeric(ci_obj[1, ])
    },
    error = function(e) c(NA_real_, NA_real_)
  )

  tibble::tibble(
    estimate = unname(coef_tab[term, "Estimate"]),
    p_value = unname(p_val),
    ci_lower = ci_vals[1],
    ci_upper = ci_vals[2],
    sig = p_to_stars(unname(p_val))
  )
}

extract_med_info <- function(med_object,
                             total_model = NULL,
                             total_term = "Stringency_index") {
  s <- summary(med_object)

  indirect_est <- if ("d.avg" %in% names(s)) s$d.avg else if ("d0" %in% names(s)) s$d0 else NA_real_
  indirect_p   <- if ("d.avg.p" %in% names(s)) s$d.avg.p else if ("d0.p" %in% names(s)) s$d0.p else NA_real_

  direct_est   <- if ("z.avg" %in% names(s)) s$z.avg else if ("z0" %in% names(s)) s$z0 else NA_real_
  direct_p     <- if ("z.avg.p" %in% names(s)) s$z.avg.p else if ("z0.p" %in% names(s)) s$z0.p else NA_real_

  if (!is.null(total_model)) {
    total_info <- extract_model_effect_info(total_model, term = total_term)
    total_est <- total_info$estimate
    total_p   <- total_info$p_value
  } else {
    total_est <- if ("tau.coef" %in% names(s)) s$tau.coef else NA_real_
    total_p   <- if ("tau.p" %in% names(s)) s$tau.p else NA_real_
  }

  tibble::tibble(
    indirect_effect = indirect_est,
    indirect_p = indirect_p,
    indirect_sig = p_to_stars(indirect_p),

    direct_effect = direct_est,
    direct_p = direct_p,
    direct_sig = p_to_stars(direct_p),

    total_effect = total_est,
    total_p = total_p,
    total_sig = p_to_stars(total_p)
  )
}

make_summary_row <- function(result, analysis_id, section_title, outcome, mediator,
                             treatment = "Stringency_index") {

  med_info <- extract_med_info(
    med_object = result$med,
    total_model = result$m3_lmtest,
    total_term = treatment
  )

  a_info <- extract_coef_info(result$m2_lmtest, treatment)
  b_info <- extract_coef_info(result$m1_lmtest, mediator)

  tibble::tibble(
    analysis_id = analysis_id,
    section_title = section_title,
    outcome = outcome,
    mediator = mediator,

    indirect_effect = med_info$indirect_effect,
    indirect_p = med_info$indirect_p,
    indirect_sig = med_info$indirect_sig,

    direct_effect = med_info$direct_effect,
    direct_p = med_info$direct_p,
    direct_sig = med_info$direct_sig,

    total_effect = med_info$total_effect,
    total_p = med_info$total_p,
    total_sig = med_info$total_sig,

    a_path = a_info$estimate,
    a_p = a_info$p_value,
    a_sig = a_info$sig,

    b_path = b_info$estimate,
    b_p = b_info$p_value,
    b_sig = b_info$sig
  )
}

fit_model_set <- function(data, outcome, mediator,
                          treatment = "Stringency_index",
                          group = "Country",
                          covariate = "Temp_mean") {

  # define formulas
  f_m1 <- build_formula(outcome, c(treatment, mediator, paste0("(1|", group, ")"), covariate))
  f_m2 <- build_formula(mediator, c(treatment, paste0("(1|", group, ")")))
  f_m3 <- build_formula(outcome, c(treatment, paste0("(1|", group, ")"), covariate))
  f_m4 <- build_formula(outcome, c(mediator, paste0("(1|", group, ")"), covariate))

  # lme4 models
  m1 <- lme4::lmer(f_m1, data = data)
  m2 <- lme4::lmer(f_m2, data = data)
  m3 <- lme4::lmer(f_m3, data = data)
  m4 <- lme4::lmer(f_m4, data = data)

  # mediation model
  set.seed(21)
  med <- mediation::mediate(
    model.m = m2,
    model.y = m1,
    treat = treatment,
    mediator = mediator
  )

  # lmerTest models for significance tests
  m1_lmtest <- lmerTest::lmer(f_m1, data = data)
  m2_lmtest <- lmerTest::lmer(f_m2, data = data)
  m3_lmtest <- lmerTest::lmer(f_m3, data = data)
  m4_lmtest <- lmerTest::lmer(f_m4, data = data)

  list(
    m1 = m1, m2 = m2, m3 = m3, m4 = m4,
    med = med,
    m1_lmtest = m1_lmtest, m2_lmtest = m2_lmtest,
    m3_lmtest = m3_lmtest, m4_lmtest = m4_lmtest
  )
}

get_first_existing <- function(x, candidates) {
  for (nm in candidates) {
    if (!is.null(x[[nm]])) return(x[[nm]])
  }
  NULL
}

extract_med_effect_from_object <- function(med_object,
                                           effect = c("indirect", "direct", "total"),
                                           conf_level = 0.95) {
  effect <- match.arg(effect)

  alpha <- 1 - conf_level
  probs <- c(alpha / 2, 1 - alpha / 2)

  if (effect == "indirect") {
    effect_label <- "Indirect effect (ACME)"
    est <- get_first_existing(med_object, c("d.avg", "d0"))
    p   <- get_first_existing(med_object, c("d.avg.p", "d0.p"))
    sims <- get_first_existing(med_object, c("d.avg.sims", "d0.sims"))
  } else if (effect == "direct") {
    effect_label <- "Direct effect (ADE)"
    est <- get_first_existing(med_object, c("z.avg", "z0"))
    p   <- get_first_existing(med_object, c("z.avg.p", "z0.p"))
    sims <- get_first_existing(med_object, c("z.avg.sims", "z0.sims"))
  } else {
    effect_label <- "Total effect"
    est <- get_first_existing(med_object, c("tau.coef"))
    p   <- get_first_existing(med_object, c("tau.p"))
    sims <- get_first_existing(med_object, c("tau.sims"))
  }

  ci_lower <- NA_real_
  ci_upper <- NA_real_

  if (!is.null(sims)) {
    ci_vals <- stats::quantile(as.numeric(sims), probs = probs, na.rm = TRUE)
    ci_lower <- unname(ci_vals[1])
    ci_upper <- unname(ci_vals[2])
  }

  tibble::tibble(
    Effect = effect_label,
    Estimate = as.numeric(est)[1],
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = as.numeric(p)[1],
    Significance = p_to_stars(as.numeric(p)[1])
  )
}

format_ci <- function(lower, upper, digits = 3) {
  out <- sprintf(
    paste0("[%.", digits, "f, %.", digits, "f]"),
    lower, upper
  )
  out[is.na(lower) | is.na(upper)] <- NA_character_
  out
}

make_med_ci_table <- function(result,
                              digits = 3,
                              total_term = "Stringency_index",
                              total_ci_method = "Wald") {

  indirect <- extract_med_effect_from_object(result$med, "indirect")

  direct <- extract_med_effect_from_object(result$med, "direct")

  total_model_info <- extract_model_effect_info(
    model = result$m3_lmtest,
    term = total_term,
    ci_method = total_ci_method
  ) %>%
    dplyr::transmute(
      Effect = "Total effect",
      Estimate = estimate,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_value,
      Significance = sig
    )

  dplyr::bind_rows(
    indirect,
    direct,
    total_model_info
  ) %>%
    dplyr::mutate(
      `95% CI` = format_ci(ci_lower, ci_upper, digits = digits),
      Estimate = round(Estimate, digits),
      `p-value` = round(p_value, digits)
    ) %>%
    dplyr::select(
      Effect,
      Estimate,
      `95% CI`,
      `p-value`,
      Significance
    )
}
```

# Load dataframes

``` r
data_all_europe_st <- readRDS("data/data_all_europe_st.RDS")

data_all_stress_europe <- readRDS("data/data_all_stress_europe.RDS")

data_all_stress_europe_st <- readRDS("data/data_all_stress_europe_st.RDS")
```

# Calculate means and SEM

``` r
means <- data_all_stress_europe %>%
            group_by(Country) %>%
            summarise(
              Mean_danceability = mean(Danceability_mean, na.rm=TRUE),
              Mean_stringency_index = mean(Stringency_index, na.rm=TRUE),
              Mean_sps_sum = mean(SPS_SUM_mean, na.rm=TRUE),
              Mean_sps_si = mean(SPS_SI_mean, na.rm=TRUE),
              Mean_sps_row = mean(SPS_ROW_mean, na.rm=TRUE),
              Mean_sps_a = mean(SPS_A_mean, na.rm=TRUE),
              Mean_sps_sora = mean(SPS_SORA_mean, na.rm=TRUE),
              Mean_sps_g = mean(SPS_G_mean, na.rm=TRUE),
              SEM_danceability = sd(Danceability_mean, na.rm=TRUE)/sqrt(sum(!is.na(Danceability_mean))),
              SEM_stringency_index = sd(Stringency_index, na.rm=TRUE)/sqrt(sum(!is.na(Stringency_index))),
              SEM_sps_sum = sd(SPS_SUM_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_SUM_mean))),
              SEM_sps_si = sd(SPS_SI_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_SI_mean))),
              SEM_sps_row = sd(SPS_ROW_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_ROW_mean))),
              SEM_sps_a = sd(SPS_A_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_A_mean))),
              SEM_sps_sora = sd(SPS_SORA_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_SORA_mean))),
              SEM_sps_g = sd(SPS_G_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_G_mean)))
  )

means
```

    ## # A tibble: 11 × 17
    ##    Country Mean_danceability Mean_stringency_index Mean_sps_sum Mean_sps_si Mean_sps_row Mean_sps_a Mean_sps_sora Mean_sps_g SEM_danceability SEM_stringency_index SEM_sps_sum SEM_sps_si
    ##    <chr>               <dbl>                 <dbl>        <dbl>       <dbl>        <dbl>      <dbl>         <dbl>      <dbl>            <dbl>                <dbl>       <dbl>      <dbl>
    ##  1 be                  0.689                  79.3         48.7        9.76         9.20       9.88         10.1        9.81         0.000615                0.362       0.889     0.181 
    ##  2 ch                  0.717                  67.8         51.6       10.5          9.91      10.4          10.8        9.96         0.000768                0.809       0.569     0.121 
    ##  3 de                  0.732                  71.0         50.8       10.4          9.65      10.3          10.6        9.86         0.000621                0.908       0.517     0.0983
    ##  4 dk                  0.738                  68.6         51.7       10.5          9.64      10.6          10.8       10.3          0.000924                0.427       0.500     0.105 
    ##  5 es                  0.747                  81.5         50.3       10.2          9.68       9.98         10.5        9.98         0.000493                0.659       0.772     0.187 
    ##  6 fi                  0.689                  67.4         50.4       10.3          9.51      10.3          10.4        9.83         0.00120                 0.478       0.348     0.0673
    ##  7 fr                  0.738                  84.5         48.2        9.80         8.91       9.85         10.1        9.47         0.000905                0.662       0.502     0.109 
    ##  8 gb                  0.694                  77.2         49.7        9.92         9.55      10.3          10.3        9.69         0.000614                0.470       0.597     0.161 
    ##  9 it                  0.707                  82.7         49.3        9.93         9.55       9.93          9.96       9.94         0.000946                1.23        0.579     0.137 
    ## 10 nl                  0.694                  76.5         50.8       10.3          9.73      10.2          10.5       10.1          0.00114                 0.422       0.554     0.104 
    ## 11 pt                  0.707                  77.0         51.1       10.2          9.77      10.3          10.6       10.3          0.000497                1.00        0.618     0.185 
    ## # ℹ 4 more variables: SEM_sps_row <dbl>, SEM_sps_a <dbl>, SEM_sps_sora <dbl>, SEM_sps_g <dbl>

# Pre/post analysis

``` r
model_period_dance_lmtest = lmerTest::lmer(Danceability_mean ~ relevel(Period, ref="pre")+Temp_mean+(1|Country), data = data_all_europe_st)
model_period_dance = lme4::lmer(Danceability_mean ~ relevel(Period, ref="pre")+Temp_mean+(1|Country), data = data_all_europe_st)

set.seed(21)
model_period_dance_CI = confint(model_period_dance, method="boot", nsim=1000)

summary(model_period_dance_lmtest)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
    ## Formula: Danceability_mean ~ relevel(Period, ref = "pre") + Temp_mean +      (1 | Country)
    ##    Data: data_all_europe_st
    ## 
    ## REML criterion at convergence: 1394.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1820 -0.5990  0.0260  0.7277  2.7760 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Country  (Intercept) 0.8626   0.9288  
    ##  Residual             0.1532   0.3915  
    ## Number of obs: 1364, groups:  Country, 11
    ## 
    ## Fixed effects:
    ##                                    Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)                        -0.08277    0.28044   10.02433  -0.295    0.774    
    ## relevel(Period, ref = "pre")post    0.16555    0.02143 1351.09122   7.727 2.14e-14 ***
    ## Temp_mean                           0.09155    0.01559 1354.93775   5.871 5.45e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) r(Pr="
    ## rlv(P,r="") -0.038       
    ## Temp_mean    0.006 -0.145

``` r
performance::r2(model_period_dance_lmtest)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.852
    ##      Marginal R2: 0.016

``` r
performance::icc(model_period_dance_lmtest)
```

    ## # Intraclass Correlation Coefficient
    ## 
    ##     Adjusted ICC: 0.849
    ##   Unadjusted ICC: 0.835

``` r
model_period_dance_CI
```

    ##                                        2.5 %    97.5 %
    ## .sig01                            0.52000638 1.3160095
    ## .sigma                            0.37644551 0.4064301
    ## (Intercept)                      -0.64171923 0.4977600
    ## relevel(Period, ref = "pre")post  0.12072797 0.2066064
    ## Temp_mean                         0.06202185 0.1234839

``` r
##Check regression Assumptions (visual)
plot(model_period_dance_lmtest)
```

![](analyses_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
qqnorm(resid(model_period_dance_lmtest))
```

![](analyses_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
hist(resid(model_period_dance_lmtest))
```

![](analyses_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

# Create analysis plan for mediation

``` r
analysis_plan <- tribble(
  ~analysis_id,                ~section_title,                                                  ~outcome,              ~mediator,                     ~med_summary_output, ~med_plot,
  "string_sps_dance",          "Stringency -> SPS SUM Score -> Dance",                          "Danceability_mean",   "SPS_SUM_mean",                NA_character_,       FALSE,
  "string_si_dance",           "Stringency -> SPS Social Integration -> Dance",                 "Danceability_mean",   "SPS_SI_mean",                 NA_character_,       FALSE,
  "string_row_dance",          "Stringency -> SPS Reassurance of worth score -> Dance",         "Danceability_mean",   "SPS_ROW_mean",                NA_character_,       FALSE,
  "string_a_dance",            "Stringency -> SPS Attachment -> Dance",                         "Danceability_mean",   "SPS_A_mean",                  NA_character_,       FALSE,
  "string_sora_dance",         "Stringency -> SPS Sense of reliable alliance -> Dance",         "Danceability_mean",   "SPS_SORA_mean",               NA_character_,       FALSE,
  "string_g_dance",            "Stringency -> SPS Guidance -> Dance",                           "Danceability_mean",   "SPS_G_mean",                  NA_character_,       FALSE,
  "string_slon_dance",         "Stringency -> Scale SLON SUM -> Dance",                         "Danceability_mean",   "Scale_SLON_SUM_mean",         NA_character_,       FALSE,
  "string_pss_dance",          "Stringency -> Scale PSS10 UCLA SUM_mean -> Dance",              "Danceability_mean",   "Scale_PSS10_UCLA_SUM_mean",   NA_character_,       FALSE,

  "string_sps_valence",        "Stringency -> SPS SUM Score -> Valence",                        "Valence_mean",        "SPS_SUM_mean",                NA_character_,       FALSE,
  "string_si_valence",         "Stringency -> SPS Social Integration -> Valence",               "Valence_mean",        "SPS_SI_mean",                 NA_character_,       FALSE,
  "string_row_valence",        "Stringency -> SPS Reassurance of worth score -> Valence",       "Valence_mean",        "SPS_ROW_mean",                NA_character_,       FALSE,
  "string_a_valence",          "Stringency -> SPS Attachment -> Valence",                       "Valence_mean",        "SPS_A_mean",                  NA_character_,       FALSE,
  "string_sora_valence",       "Stringency -> SPS Sense of reliable alliance -> Valence",       "Valence_mean",        "SPS_SORA_mean",               NA_character_,       FALSE,
  "string_g_valence",          "Stringency -> SPS Guidance -> Valence",                         "Valence_mean",        "SPS_G_mean",                  NA_character_,       FALSE,

  "string_sps_energy",         "Stringency -> SPS SUM Score -> Energy",                         "Energy_mean",         "SPS_SUM_mean",                NA_character_,       FALSE,
  "string_si_energy",          "Stringency -> SPS Social Integration -> Energy",                "Energy_mean",         "SPS_SI_mean",                 NA_character_,       FALSE,
  "string_row_energy",         "Stringency -> SPS Reassurance of worth score -> Energy",        "Energy_mean",         "SPS_ROW_mean",                NA_character_,       FALSE,
  "string_a_energy",           "Stringency -> SPS Attachment -> Energy",                        "Energy_mean",         "SPS_A_mean",                  NA_character_,       FALSE,
  "string_sora_energy",        "Stringency -> SPS Sense of reliable alliance -> Energy",        "Energy_mean",         "SPS_SORA_mean",               NA_character_,       FALSE,
  "string_g_energy",           "Stringency -> SPS Guidance -> Energy",                          "Energy_mean",         "SPS_G_mean",                  NA_character_,       FALSE
)

kable(
  analysis_plan %>%
    dplyr::select(section_title, outcome, mediator),
  col.names = c("Analysis", "Outcome", "Mediator")
)
```

| Analysis                                                  | Outcome           | Mediator                  |
|:----------------------------------------------------------|:------------------|:--------------------------|
| Stringency -\> SPS SUM Score -\> Dance                    | Danceability_mean | SPS_SUM_mean              |
| Stringency -\> SPS Social Integration -\> Dance           | Danceability_mean | SPS_SI_mean               |
| Stringency -\> SPS Reassurance of worth score -\> Dance   | Danceability_mean | SPS_ROW_mean              |
| Stringency -\> SPS Attachment -\> Dance                   | Danceability_mean | SPS_A_mean                |
| Stringency -\> SPS Sense of reliable alliance -\> Dance   | Danceability_mean | SPS_SORA_mean             |
| Stringency -\> SPS Guidance -\> Dance                     | Danceability_mean | SPS_G_mean                |
| Stringency -\> Scale SLON SUM -\> Dance                   | Danceability_mean | Scale_SLON_SUM_mean       |
| Stringency -\> Scale PSS10 UCLA SUM_mean -\> Dance        | Danceability_mean | Scale_PSS10_UCLA_SUM_mean |
| Stringency -\> SPS SUM Score -\> Valence                  | Valence_mean      | SPS_SUM_mean              |
| Stringency -\> SPS Social Integration -\> Valence         | Valence_mean      | SPS_SI_mean               |
| Stringency -\> SPS Reassurance of worth score -\> Valence | Valence_mean      | SPS_ROW_mean              |
| Stringency -\> SPS Attachment -\> Valence                 | Valence_mean      | SPS_A_mean                |
| Stringency -\> SPS Sense of reliable alliance -\> Valence | Valence_mean      | SPS_SORA_mean             |
| Stringency -\> SPS Guidance -\> Valence                   | Valence_mean      | SPS_G_mean                |
| Stringency -\> SPS SUM Score -\> Energy                   | Energy_mean       | SPS_SUM_mean              |
| Stringency -\> SPS Social Integration -\> Energy          | Energy_mean       | SPS_SI_mean               |
| Stringency -\> SPS Reassurance of worth score -\> Energy  | Energy_mean       | SPS_ROW_mean              |
| Stringency -\> SPS Attachment -\> Energy                  | Energy_mean       | SPS_A_mean                |
| Stringency -\> SPS Sense of reliable alliance -\> Energy  | Energy_mean       | SPS_SORA_mean             |
| Stringency -\> SPS Guidance -\> Energy                    | Energy_mean       | SPS_G_mean                |

# Fit mediation models

``` r
all_results_list <- vector("list", nrow(analysis_plan))

pb <- txtProgressBar(min = 0, max = nrow(analysis_plan), style = 3)
```

    ##   |                                                                                                                                                                                            |                                                                                                                                                                                    |   0%

``` r
for (i in seq_len(nrow(analysis_plan))) {
  all_results_list[[i]] <- fit_model_set(
    data = data_all_stress_europe_st,
    outcome = analysis_plan$outcome[[i]],
    mediator = analysis_plan$mediator[[i]]
  )

  setTxtProgressBar(pb, i)
}
```

    ##   |                                                                                                                                                                                            |=========                                                                                                                                                                           |   5%  |                                                                                                                                                                                            |==================                                                                                                                                                                  |  10%  |                                                                                                                                                                                            |===========================                                                                                                                                                         |  15%  |                                                                                                                                                                                            |====================================                                                                                                                                                |  20%  |                                                                                                                                                                                            |=============================================                                                                                                                                       |  25%  |                                                                                                                                                                                            |======================================================                                                                                                                              |  30%  |                                                                                                                                                                                            |===============================================================                                                                                                                     |  35%  |                                                                                                                                                                                            |========================================================================                                                                                                            |  40%  |                                                                                                                                                                                            |=================================================================================                                                                                                   |  45%  |                                                                                                                                                                                            |==========================================================================================                                                                                          |  50%  |                                                                                                                                                                                            |===================================================================================================                                                                                 |  55%  |                                                                                                                                                                                            |============================================================================================================                                                                        |  60%  |                                                                                                                                                                                            |=====================================================================================================================                                                               |  65%  |                                                                                                                                                                                            |==============================================================================================================================                                                      |  70%  |                                                                                                                                                                                            |=======================================================================================================================================                                             |  75%  |                                                                                                                                                                                            |================================================================================================================================================                                    |  80%  |                                                                                                                                                                                            |=========================================================================================================================================================                           |  85%  |                                                                                                                                                                                            |==================================================================================================================================================================                  |  90%  |                                                                                                                                                                                            |===========================================================================================================================================================================         |  95%  |                                                                                                                                                                                            |====================================================================================================================================================================================| 100%

``` r
close(pb)
```

``` r
all_results <- analysis_plan %>%
  dplyr::mutate(result = all_results_list)

mediation_summary_table <- purrr::pmap_dfr(
  list(
    result = all_results$result,
    analysis_id = all_results$analysis_id,
    section_title = all_results$section_title,
    outcome = all_results$outcome,
    mediator = all_results$mediator
  ),
  make_summary_row
) %>%
  dplyr::mutate(
    dplyr::across(
      c(
        indirect_effect, indirect_p,
        direct_effect, direct_p,
        total_effect, total_p,
        a_path, a_p,
        b_path, b_p
      ),
      ~ round(.x, 4)
    )
  )
```

# Mediation results

``` r
mediation_summary_table_display <- mediation_summary_table %>%
  dplyr::transmute(
    Analysis = section_title,
    Outcome = outcome,
    Mediator = mediator,
    `Indirect effect (ACME)` = format_effect(indirect_effect, indirect_p, indirect_sig),
    `Direct effect (ADE)`   = format_effect(direct_effect, direct_p, direct_sig),
    `Total effect`          = format_effect(total_effect, total_p, total_sig),
    `a path: Stringency → Mediator` = format_effect(a_path, a_p, a_sig),
    `b path: Mediator → Outcome`    = format_effect(b_path, b_p, b_sig)
  )

if (is_html_output) {
  DT::datatable(
    mediation_summary_table_display,
    rownames = FALSE,
    filter = "top",
    options = list(
      paging = FALSE,
      scrollX = TRUE,
      scrollY = "600px",
      scrollCollapse = TRUE,
      autoWidth = TRUE
    ),
    caption = "Summary of mediation analyses"
  )
} else {
  knitr::kable(
    mediation_summary_table_display,
    caption = "Summary of mediation analyses"
  )
}
```

| Analysis                                                  | Outcome           | Mediator                  | Indirect effect (ACME) | Direct effect (ADE)        | Total effect               | a path: Stringency → Mediator | b path: Mediator → Outcome |
|:----------------------------------------------------------|:------------------|:--------------------------|:-----------------------|:---------------------------|:---------------------------|:------------------------------|:---------------------------|
| Stringency -\> SPS SUM Score -\> Dance                    | Danceability_mean | SPS_SUM_mean              | 0.004\* (p = 0.038)    | -0.141\*\*\* (p = \<0.001) | -0.117\*\*\* (p = \<0.001) | -0.148\* (p = 0.0119)         | -0.028\* (p = 0.0271)      |
| Stringency -\> SPS Social Integration -\> Dance           | Danceability_mean | SPS_SI_mean               | 0.002 (p = 0.212)      | -0.138\*\*\* (p = \<0.001) | -0.117\*\*\* (p = \<0.001) | -0.115. (p = 0.0517)          | -0.018 (p = 0.1565)        |
| Stringency -\> SPS Reassurance of worth score -\> Dance   | Danceability_mean | SPS_ROW_mean              | 0.003. (p = 0.098)     | -0.140\*\*\* (p = \<0.001) | -0.117\*\*\* (p = \<0.001) | -0.100. (p = 0.0918)          | -0.032\*\* (p = 0.0090)    |
| Stringency -\> SPS Attachment -\> Dance                   | Danceability_mean | SPS_A_mean                | 0.006\* (p = 0.028)    | -0.141\*\*\* (p = \<0.001) | -0.117\*\*\* (p = \<0.001) | -0.232\*\*\* (p = \<0.001)    | -0.027\* (p = 0.0276)      |
| Stringency -\> SPS Sense of reliable alliance -\> Dance   | Danceability_mean | SPS_SORA_mean             | 0.004. (p = 0.052)     | -0.141\*\*\* (p = \<0.001) | -0.117\*\*\* (p = \<0.001) | -0.139\* (p = 0.0186)         | -0.026\* (p = 0.0353)      |
| Stringency -\> SPS Guidance -\> Dance                     | Danceability_mean | SPS_G_mean                | 0.000 (p = 0.692)      | -0.138\*\*\* (p = \<0.001) | -0.117\*\*\* (p = \<0.001) | -0.044 (p = 0.4662)           | -0.011 (p = 0.3726)        |
| Stringency -\> Scale SLON SUM -\> Dance                   | Danceability_mean | Scale_SLON_SUM_mean       | 0.000 (p = 0.766)      | -0.131\*\*\* (p = \<0.001) | -0.117\*\*\* (p = \<0.001) | 0.019 (p = 0.7497)            | 0.013 (p = 0.2564)         |
| Stringency -\> Scale PSS10 UCLA SUM_mean -\> Dance        | Danceability_mean | Scale_PSS10_UCLA_SUM_mean | 0.000 (p = 0.878)      | -0.132\*\*\* (p = \<0.001) | -0.117\*\*\* (p = \<0.001) | 0.028 (p = 0.6436)            | 0.004 (p = 0.7728)         |
| Stringency -\> SPS SUM Score -\> Valence                  | Valence_mean      | SPS_SUM_mean              | -0.003 (p = 0.128)     | 0.122\*\*\* (p = \<0.001)  | 0.064\*\*\* (p = \<0.001)  | -0.148\* (p = 0.0119)         | 0.021 (p = 0.1236)         |
| Stringency -\> SPS Social Integration -\> Valence         | Valence_mean      | SPS_SI_mean               | -0.001 (p = 0.436)     | 0.117\*\*\* (p = \<0.001)  | 0.064\*\*\* (p = \<0.001)  | -0.115. (p = 0.0517)          | 0.010 (p = 0.4690)         |
| Stringency -\> SPS Reassurance of worth score -\> Valence | Valence_mean      | SPS_ROW_mean              | -0.002 (p = 0.202)     | 0.121\*\*\* (p = \<0.001)  | 0.064\*\*\* (p = \<0.001)  | -0.100. (p = 0.0918)          | 0.020 (p = 0.1358)         |
| Stringency -\> SPS Attachment -\> Valence                 | Valence_mean      | SPS_A_mean                | -0.005 (p = 0.114)     | 0.120\*\*\* (p = \<0.001)  | 0.064\*\*\* (p = \<0.001)  | -0.232\*\*\* (p = \<0.001)    | 0.021 (p = 0.1238)         |
| Stringency -\> SPS Sense of reliable alliance -\> Valence | Valence_mean      | SPS_SORA_mean             | -0.002 (p = 0.418)     | 0.118\*\*\* (p = \<0.001)  | 0.064\*\*\* (p = \<0.001)  | -0.139\* (p = 0.0186)         | 0.010 (p = 0.4641)         |
| Stringency -\> SPS Guidance -\> Valence                   | Valence_mean      | SPS_G_mean                | -0.001 (p = 0.508)     | 0.121\*\*\* (p = \<0.001)  | 0.064\*\*\* (p = \<0.001)  | -0.044 (p = 0.4662)           | 0.029\* (p = 0.0319)       |
| Stringency -\> SPS SUM Score -\> Energy                   | Energy_mean       | SPS_SUM_mean              | 0.001 (p = 0.698)      | -0.008 (p = 0.730)         | -0.055\*\*\* (p = \<0.001) | -0.148\* (p = 0.0119)         | -0.005 (p = 0.6668)        |
| Stringency -\> SPS Social Integration -\> Energy          | Energy_mean       | SPS_SI_mean               | 0.001 (p = 0.514)      | -0.009 (p = 0.686)         | -0.055\*\*\* (p = \<0.001) | -0.115. (p = 0.0517)          | -0.007 (p = 0.4833)        |
| Stringency -\> SPS Reassurance of worth score -\> Energy  | Energy_mean       | SPS_ROW_mean              | 0.000 (p = 0.718)      | -0.008 (p = 0.736)         | -0.055\*\*\* (p = \<0.001) | -0.100. (p = 0.0918)          | -0.004 (p = 0.6713)        |
| Stringency -\> SPS Attachment -\> Energy                  | Energy_mean       | SPS_A_mean                | -0.001 (p = 0.766)     | -0.008 (p = 0.718)         | -0.055\*\*\* (p = \<0.001) | -0.232\*\*\* (p = \<0.001)    | 0.002 (p = 0.8480)         |
| Stringency -\> SPS Sense of reliable alliance -\> Energy  | Energy_mean       | SPS_SORA_mean             | 0.000 (p = 0.700)      | -0.009 (p = 0.676)         | -0.055\*\*\* (p = \<0.001) | -0.139\* (p = 0.0186)         | -0.004 (p = 0.6703)        |
| Stringency -\> SPS Guidance -\> Energy                    | Energy_mean       | SPS_G_mean                | 0.000 (p = 0.822)      | -0.008 (p = 0.740)         | -0.055\*\*\* (p = \<0.001) | -0.044 (p = 0.4662)           | -0.005 (p = 0.6018)        |

Summary of mediation analyses

# Mediation results (CI)

``` r
all_mediation_ci_table <- purrr::pmap_dfr(
  list(
    result = all_results$result,
    section_title = all_results$section_title,
    outcome = all_results$outcome,
    mediator = all_results$mediator
  ),
  function(result, section_title, outcome, mediator) {
    make_med_ci_table(result) %>%
      dplyr::mutate(
        Analysis = section_title,
        Outcome = outcome,
        Mediator = mediator,
        .before = 1
      )
  }
)

if (is_html_output) {
  DT::datatable(
    all_mediation_ci_table,
    width = "100%",
    rownames = FALSE,
    filter = "top",
    class = "display nowrap",
    options = list(
      dom = "lfrtip",
      paging = TRUE,
      lengthChange = TRUE,
      pageLength = 20,
      lengthMenu = list(
        c(10, 20, 50, 100, -1),
        c("10", "20", "50", "100", "All")
      ),
      scrollX = TRUE,
      scrollY = "500px",
      scrollCollapse = TRUE,
      autoWidth = TRUE
    ),
    caption = "Confidence intervals for all mediation models"
  )
} else {
  knitr::kable(
    all_mediation_ci_table,
    caption = "Confidence intervals for all mediation models"
  )
}
```

| Analysis                                                  | Outcome           | Mediator                  | Effect                 | Estimate | 95% CI             | p-value | Significance |
|:----------------------------------------------------------|:------------------|:--------------------------|:-----------------------|---------:|:-------------------|--------:|:-------------|
| Stringency -\> SPS SUM Score -\> Dance                    | Danceability_mean | SPS_SUM_mean              | Indirect effect (ACME) |    0.004 | \[0.000, 0.010\]   |   0.038 | \*           |
| Stringency -\> SPS SUM Score -\> Dance                    | Danceability_mean | SPS_SUM_mean              | Direct effect (ADE)    |   -0.140 | \[-0.182, -0.099\] |   0.000 | \*\*\*       |
| Stringency -\> SPS SUM Score -\> Dance                    | Danceability_mean | SPS_SUM_mean              | Total effect           |   -0.117 | \[-0.147, -0.086\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Social Integration -\> Dance           | Danceability_mean | SPS_SI_mean               | Indirect effect (ACME) |    0.002 | \[-0.001, 0.006\]  |   0.212 |              |
| Stringency -\> SPS Social Integration -\> Dance           | Danceability_mean | SPS_SI_mean               | Direct effect (ADE)    |   -0.138 | \[-0.180, -0.097\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Social Integration -\> Dance           | Danceability_mean | SPS_SI_mean               | Total effect           |   -0.117 | \[-0.147, -0.086\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Reassurance of worth score -\> Dance   | Danceability_mean | SPS_ROW_mean              | Indirect effect (ACME) |    0.003 | \[-0.001, 0.009\]  |   0.098 | .            |
| Stringency -\> SPS Reassurance of worth score -\> Dance   | Danceability_mean | SPS_ROW_mean              | Direct effect (ADE)    |   -0.140 | \[-0.181, -0.098\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Reassurance of worth score -\> Dance   | Danceability_mean | SPS_ROW_mean              | Total effect           |   -0.117 | \[-0.147, -0.086\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Attachment -\> Dance                   | Danceability_mean | SPS_A_mean                | Indirect effect (ACME) |    0.006 | \[0.001, 0.013\]   |   0.028 | \*           |
| Stringency -\> SPS Attachment -\> Dance                   | Danceability_mean | SPS_A_mean                | Direct effect (ADE)    |   -0.141 | \[-0.183, -0.100\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Attachment -\> Dance                   | Danceability_mean | SPS_A_mean                | Total effect           |   -0.117 | \[-0.147, -0.086\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Sense of reliable alliance -\> Dance   | Danceability_mean | SPS_SORA_mean             | Indirect effect (ACME) |    0.004 | \[-0.000, 0.009\]  |   0.052 | .            |
| Stringency -\> SPS Sense of reliable alliance -\> Dance   | Danceability_mean | SPS_SORA_mean             | Direct effect (ADE)    |   -0.141 | \[-0.182, -0.099\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Sense of reliable alliance -\> Dance   | Danceability_mean | SPS_SORA_mean             | Total effect           |   -0.117 | \[-0.147, -0.086\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Guidance -\> Dance                     | Danceability_mean | SPS_G_mean                | Indirect effect (ACME) |    0.000 | \[-0.001, 0.003\]  |   0.692 |              |
| Stringency -\> SPS Guidance -\> Dance                     | Danceability_mean | SPS_G_mean                | Direct effect (ADE)    |   -0.138 | \[-0.180, -0.096\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Guidance -\> Dance                     | Danceability_mean | SPS_G_mean                | Total effect           |   -0.117 | \[-0.147, -0.086\] |   0.000 | \*\*\*       |
| Stringency -\> Scale SLON SUM -\> Dance                   | Danceability_mean | Scale_SLON_SUM_mean       | Indirect effect (ACME) |    0.000 | \[-0.002, 0.003\]  |   0.766 |              |
| Stringency -\> Scale SLON SUM -\> Dance                   | Danceability_mean | Scale_SLON_SUM_mean       | Direct effect (ADE)    |   -0.131 | \[-0.171, -0.091\] |   0.000 | \*\*\*       |
| Stringency -\> Scale SLON SUM -\> Dance                   | Danceability_mean | Scale_SLON_SUM_mean       | Total effect           |   -0.117 | \[-0.147, -0.086\] |   0.000 | \*\*\*       |
| Stringency -\> Scale PSS10 UCLA SUM_mean -\> Dance        | Danceability_mean | Scale_PSS10_UCLA_SUM_mean | Indirect effect (ACME) |    0.000 | \[-0.001, 0.002\]  |   0.878 |              |
| Stringency -\> Scale PSS10 UCLA SUM_mean -\> Dance        | Danceability_mean | Scale_PSS10_UCLA_SUM_mean | Direct effect (ADE)    |   -0.132 | \[-0.172, -0.092\] |   0.000 | \*\*\*       |
| Stringency -\> Scale PSS10 UCLA SUM_mean -\> Dance        | Danceability_mean | Scale_PSS10_UCLA_SUM_mean | Total effect           |   -0.117 | \[-0.147, -0.086\] |   0.000 | \*\*\*       |
| Stringency -\> SPS SUM Score -\> Valence                  | Valence_mean      | SPS_SUM_mean              | Indirect effect (ACME) |   -0.003 | \[-0.009, 0.001\]  |   0.128 |              |
| Stringency -\> SPS SUM Score -\> Valence                  | Valence_mean      | SPS_SUM_mean              | Direct effect (ADE)    |    0.122 | \[0.076, 0.167\]   |   0.000 | \*\*\*       |
| Stringency -\> SPS SUM Score -\> Valence                  | Valence_mean      | SPS_SUM_mean              | Total effect           |    0.064 | \[0.029, 0.099\]   |   0.000 | \*\*\*       |
| Stringency -\> SPS Social Integration -\> Valence         | Valence_mean      | SPS_SI_mean               | Indirect effect (ACME) |   -0.001 | \[-0.006, 0.002\]  |   0.436 |              |
| Stringency -\> SPS Social Integration -\> Valence         | Valence_mean      | SPS_SI_mean               | Direct effect (ADE)    |    0.117 | \[0.071, 0.163\]   |   0.000 | \*\*\*       |
| Stringency -\> SPS Social Integration -\> Valence         | Valence_mean      | SPS_SI_mean               | Total effect           |    0.064 | \[0.029, 0.099\]   |   0.000 | \*\*\*       |
| Stringency -\> SPS Reassurance of worth score -\> Valence | Valence_mean      | SPS_ROW_mean              | Indirect effect (ACME) |   -0.002 | \[-0.007, 0.001\]  |   0.202 |              |
| Stringency -\> SPS Reassurance of worth score -\> Valence | Valence_mean      | SPS_ROW_mean              | Direct effect (ADE)    |    0.121 | \[0.076, 0.167\]   |   0.000 | \*\*\*       |
| Stringency -\> SPS Reassurance of worth score -\> Valence | Valence_mean      | SPS_ROW_mean              | Total effect           |    0.064 | \[0.029, 0.099\]   |   0.000 | \*\*\*       |
| Stringency -\> SPS Attachment -\> Valence                 | Valence_mean      | SPS_A_mean                | Indirect effect (ACME) |   -0.005 | \[-0.012, 0.001\]  |   0.114 |              |
| Stringency -\> SPS Attachment -\> Valence                 | Valence_mean      | SPS_A_mean                | Direct effect (ADE)    |    0.120 | \[0.074, 0.165\]   |   0.000 | \*\*\*       |
| Stringency -\> SPS Attachment -\> Valence                 | Valence_mean      | SPS_A_mean                | Total effect           |    0.064 | \[0.029, 0.099\]   |   0.000 | \*\*\*       |
| Stringency -\> SPS Sense of reliable alliance -\> Valence | Valence_mean      | SPS_SORA_mean             | Indirect effect (ACME) |   -0.001 | \[-0.006, 0.002\]  |   0.418 |              |
| Stringency -\> SPS Sense of reliable alliance -\> Valence | Valence_mean      | SPS_SORA_mean             | Direct effect (ADE)    |    0.118 | \[0.072, 0.163\]   |   0.000 | \*\*\*       |
| Stringency -\> SPS Sense of reliable alliance -\> Valence | Valence_mean      | SPS_SORA_mean             | Total effect           |    0.064 | \[0.029, 0.099\]   |   0.000 | \*\*\*       |
| Stringency -\> SPS Guidance -\> Valence                   | Valence_mean      | SPS_G_mean                | Indirect effect (ACME) |   -0.001 | \[-0.006, 0.002\]  |   0.508 |              |
| Stringency -\> SPS Guidance -\> Valence                   | Valence_mean      | SPS_G_mean                | Direct effect (ADE)    |    0.121 | \[0.075, 0.166\]   |   0.000 | \*\*\*       |
| Stringency -\> SPS Guidance -\> Valence                   | Valence_mean      | SPS_G_mean                | Total effect           |    0.064 | \[0.029, 0.099\]   |   0.000 | \*\*\*       |
| Stringency -\> SPS SUM Score -\> Energy                   | Energy_mean       | SPS_SUM_mean              | Indirect effect (ACME) |    0.001 | \[-0.003, 0.004\]  |   0.698 |              |
| Stringency -\> SPS SUM Score -\> Energy                   | Energy_mean       | SPS_SUM_mean              | Direct effect (ADE)    |   -0.008 | \[-0.043, 0.027\]  |   0.730 |              |
| Stringency -\> SPS SUM Score -\> Energy                   | Energy_mean       | SPS_SUM_mean              | Total effect           |   -0.055 | \[-0.084, -0.025\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Social Integration -\> Energy          | Energy_mean       | SPS_SI_mean               | Indirect effect (ACME) |    0.001 | \[-0.002, 0.004\]  |   0.514 |              |
| Stringency -\> SPS Social Integration -\> Energy          | Energy_mean       | SPS_SI_mean               | Direct effect (ADE)    |   -0.009 | \[-0.044, 0.026\]  |   0.686 |              |
| Stringency -\> SPS Social Integration -\> Energy          | Energy_mean       | SPS_SI_mean               | Total effect           |   -0.055 | \[-0.084, -0.025\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Reassurance of worth score -\> Energy  | Energy_mean       | SPS_ROW_mean              | Indirect effect (ACME) |    0.000 | \[-0.002, 0.003\]  |   0.718 |              |
| Stringency -\> SPS Reassurance of worth score -\> Energy  | Energy_mean       | SPS_ROW_mean              | Direct effect (ADE)    |   -0.008 | \[-0.043, 0.028\]  |   0.736 |              |
| Stringency -\> SPS Reassurance of worth score -\> Energy  | Energy_mean       | SPS_ROW_mean              | Total effect           |   -0.055 | \[-0.084, -0.025\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Attachment -\> Energy                  | Energy_mean       | SPS_A_mean                | Indirect effect (ACME) |   -0.001 | \[-0.005, 0.004\]  |   0.766 |              |
| Stringency -\> SPS Attachment -\> Energy                  | Energy_mean       | SPS_A_mean                | Direct effect (ADE)    |   -0.008 | \[-0.044, 0.027\]  |   0.718 |              |
| Stringency -\> SPS Attachment -\> Energy                  | Energy_mean       | SPS_A_mean                | Total effect           |   -0.055 | \[-0.084, -0.025\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Sense of reliable alliance -\> Energy  | Energy_mean       | SPS_SORA_mean             | Indirect effect (ACME) |    0.001 | \[-0.003, 0.004\]  |   0.700 |              |
| Stringency -\> SPS Sense of reliable alliance -\> Energy  | Energy_mean       | SPS_SORA_mean             | Direct effect (ADE)    |   -0.009 | \[-0.045, 0.026\]  |   0.676 |              |
| Stringency -\> SPS Sense of reliable alliance -\> Energy  | Energy_mean       | SPS_SORA_mean             | Total effect           |   -0.055 | \[-0.084, -0.025\] |   0.000 | \*\*\*       |
| Stringency -\> SPS Guidance -\> Energy                    | Energy_mean       | SPS_G_mean                | Indirect effect (ACME) |    0.000 | \[-0.001, 0.002\]  |   0.822 |              |
| Stringency -\> SPS Guidance -\> Energy                    | Energy_mean       | SPS_G_mean                | Direct effect (ADE)    |   -0.008 | \[-0.043, 0.028\]  |   0.740 |              |
| Stringency -\> SPS Guidance -\> Energy                    | Energy_mean       | SPS_G_mean                | Total effect           |   -0.055 | \[-0.084, -0.025\] |   0.000 | \*\*\*       |

Confidence intervals for all mediation models

# Session info

``` r
sI=sessionInfo()
print(sI, RNG = FALSE, locale = FALSE)
```

    ## R version 4.3.2 (2023-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 11 x64 (build 26200)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] DT_0.34.0          knitr_1.43         purrr_1.0.1        mediation_4.5.1    sandwich_3.1-1     mvtnorm_1.3-2      MASS_7.3-60        performance_0.10.8 tibble_3.2.1      
    ## [10] dplyr_1.1.2        lmerTest_3.1-3     lme4_1.1-34        Matrix_1.6-1.1    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.4        xfun_0.40           ggplot2_3.4.3       htmlwidgets_1.6.4   insight_0.19.6      lattice_0.21-9      numDeriv_2016.8-1.1 vctrs_0.6.5         tools_4.3.2        
    ## [10] generics_0.1.3      fansi_1.0.6         highr_0.10          cluster_2.1.4       pkgconfig_2.0.3     data.table_1.16.2   checkmate_2.3.2     lifecycle_1.0.4     compiler_4.3.2     
    ## [19] stringr_1.5.0       munsell_0.5.0       htmltools_0.5.7     yaml_2.3.7          htmlTable_2.4.2     Formula_1.2-5       pillar_1.9.0        nloptr_2.0.3        Hmisc_5.1-1        
    ## [28] rpart_4.1.21        boot_1.3-28.1       nlme_3.1-163        tidyselect_1.2.0    digest_0.6.33       stringi_1.7.12      splines_4.3.2       cowplot_1.1.2       fastmap_1.1.1      
    ## [37] grid_4.3.2          colorspace_2.1-0    cli_3.6.3           magrittr_2.0.3      base64enc_0.1-3     utf8_1.2.4          withr_3.0.2         foreign_0.8-85      scales_1.2.1       
    ## [46] backports_1.5.0     estimability_1.5.1  rmarkdown_2.24      emmeans_1.10.6      nnet_7.3-19         gridExtra_2.3       zoo_1.8-12          coda_0.19-4.1       evaluate_0.21      
    ## [55] lpSolve_5.6.23      rlang_1.1.4         Rcpp_1.0.11         glue_1.8.0          rstudioapi_0.15.0   minqa_1.2.5         R6_2.5.1
