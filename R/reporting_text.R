# Fixed descriptions and audit notes from the submitted supplement.

supplementary_table_s1 <- tibble::tribble(
  ~source, ~coverage, ~unit, ~time_resolution, ~inclusion_in_other_source, ~analytic_use,
  "COFEPRIS Group 1 controlled-medication surveillance database", "National methadone prescriptions recorded outside CIJ", "Individual prescription-capture record with formulation, quantity, capture date, state, and municipality", "Dated records aggregated by month", "Does not include the CIJ clinic records used in this study", "Primary national time series, geographic analyses, and COFEPRIS-derived population scenarios",
  "Centros de Integración Juvenil (CIJ) administrative data", "Public-sector methadone provision at CIJ clinics in Tijuana, Ciudad Juárez, and Zapopan/Guadalajara, when available", "Unique patient count and methadone volume within site-year", "Calendar year; 2025 through August", "Separate administrative reporting; not included in the COFEPRIS records used in this study", "Annual patient counts and volumes presented separately"
)

supplementary_table_s2 <- tibble::tribble(
  ~component, ~specification,
  "Analysed date and duplicates", "The analysed date was the surveillance-capture date recorded in the extract and was not independently verified as the dispensing date. Duplicates were records with the same standardised medicine-presentation string and identical surveillance-capture timestamp.",
  "Medicine standardisation", "Medicine descriptions were normalised for spelling and units and classified as tablets or oral solution. Milligrams were derived from formulation, strength, concentration, package size, and recorded quantity.",
  "Conversion rules", "Rules used 40 mg per tablet, 100 tablets per box, 10 mg/mL oral solution, and 30 mL per bottle when quantity was recorded as packages. Jurisdiction-specific and date-specific rules accounted for documented changes in whether quantity represented milligrams, tablets, boxes, millilitres, or bottles.",
  "Extreme values", "One record with a recorded quantity greater than 100 000 was excluded as implausible."
)

supplementary_table_s3 <- tibble::tribble(
  ~analysis, ~component, ~specification,
  "Primary interrupted time-series model", "Outcome and study period", "The primary outcome was observed monthly oral methadone volume recorded by COFEPRIS from January 2018 through August 2025. Months without records were assigned zero before log transformation; no national month had zero volume.",
  "Primary interrupted time-series model", "Model specification", "The segmented log-linear dynamic regression estimated immediate level and slope changes at December 2022 and August 2023. It included a linear secular trend, annual sine and cosine terms, and autoregressive errors of order 1.",
  "Primary interrupted time-series model", "Annual percentage changes", "Segment-specific APCs were calculated as 100 × [exp(12βₛ) − 1], where βₛ is the fitted monthly log-volume slope for each segment. Confidence limits were obtained by applying the same transformation to the 95% confidence limits for βₛ. The post-reopening estimate was interpreted alongside the low suspension-period trough and the modelled continuation of the pre-suspension trend.",
  "Primary interrupted time-series model", "Model diagnostics", "Residual autocorrelation was assessed using the Ljung–Box test at 12 months, and residual stationarity using the augmented Dickey–Fuller test.",
  "Population estimation", "Calculation", "Estimated people = monthly methadone mg ÷ (daily dose × days in month × treatment coverage).",
  "Population estimation", "Assumptions and summary", "The central scenario assumed 80 mg/day and 90% monthly treatment coverage. Dose was varied separately to 40 and 120 mg/day while coverage remained 90%; coverage was varied separately to 75% and 95% while dose remained 80 mg/day. Monthly estimates were summarised by the median within the pre-suspension, suspension, and post-reopening periods and rounded to whole people.",
  "Population estimation", "Indication and CIJ data", "Oral methadone used for pain or palliative care may have contributed because indication was unavailable. CIJ annual patient counts were retained at their original annual resolution and presented separately.",
  "Sensitivity analyses", "Alternative specifications", "Sensitivity analyses used ordinary least squares with 12-month Newey-West heteroskedasticity- and autocorrelation-consistent standard errors, added a quadratic secular term, aggregated observed volume by calendar quarter, excluded 482 records on 12 suspected bulk-upload dates, and excluded 14 national records lacking geography.",
  "Sensitivity analyses", "Interruption timing", "Lead and lag interruption dates were not used because the data identify regulatory authorisation rather than production restart or clinic-level availability.",
  "Primary interrupted time-series model", "Model equation", "log(Yₜ + 1) = β₀ + β₁t + β₂Sₜ + β₃(t − k₁)₊ + β₄Rₜ + β₅(t − k₂)₊ + γ₁sin(2πt/12) + γ₂cos(2πt/12) + εₜ, with εₜ = φεₜ₋₁ + uₜ.",
  "Primary interrupted time-series model", "Interruption coding", "Sₜ was 0 before December 2022 and 1 thereafter; Rₜ was 0 before August 2023 and 1 thereafter. The terms (t − k₁)₊ and (t − k₂)₊ were 0 before their respective interruption month and counted months since that interruption thereafter.",
  "Primary interrupted time-series model", "Estimation and confidence intervals", "The model was fitted by maximum likelihood with AR(1) errors. Primary 95% confidence intervals used the model-based standard errors. APC confidence limits were obtained by applying the APC transformation to the corresponding slope limits."
)

supplementary_table_s4 <- tibble::tribble(
  ~epoc_domain, ~risk_of_bias, ~supporting_information_and_interpretation,
  "Was the intervention independent of other changes?", "Unclear risk", "The December 2022 suspension and August 2023 authorisation were discrete external events with documented dates, and no other national regulatory action affecting methadone production was identified at either point. However, the uncontrolled design cannot exclude concurrent changes in production, distribution, clinic operation, demand, use for other indications, or reporting. Findings are therefore interpreted as temporal associations rather than effects attributable exclusively to the regulatory actions.",
  "Was the shape of the intervention effect prespecified?", "Low risk", "The interruption points corresponded to the documented regulatory dates. Before fitting the revised model, we specified immediate level and slope changes at both December 2022 and August 2023, allowing for both an abrupt change and a subsequent change in trend. The dates and effect structure were not selected by comparing alternative interruption dates or effect shapes.",
  "Was the intervention unlikely to affect data collection?", "Low risk", "The COFEPRIS surveillance source, record structure, variables, and extraction method were consistent across the study period, and no change in the national reporting system was identified at either interruption point. The regulatory actions could affect the number of methadone records because dispensing volume was the outcome of interest, but there was no evidence that they changed the source or method of data collection. Possible variation in the timing of surveillance capture was examined through the bulk-upload and quarterly sensitivity analyses.",
  "Was knowledge of the allocated intervention adequately prevented during the study?", "Low risk", "Blinding was not applicable because the primary outcome was objectively derived monthly oral methadone volume from national administrative records. Outcome ascertainment did not depend on participant recall, clinician judgement, or subjective assessment. Recorded volume nevertheless represented surveillance capture rather than independently verified medicine availability, treatment received, or patient access.",
  "Were incomplete outcome data adequately addressed?", "Low risk", "Record inclusion and exclusion are documented in the record-flow table. Thirteen of 155 373 cleaned records lacked a derivable milligram quantity and were excluded from volume analyses. Fourteen records with valid dates and quantities but without geographic information were retained in the national analysis and excluded only from subnational summaries. Excluding these records changed national volume by −0·01%. Months without records were assigned zero before analysis, although no national month had zero recorded volume.",
  "Was the study free of selective outcome reporting?", "Low risk", "All outcomes described in the Methods are reported, including national volume, the three segment-specific slopes, both immediate level changes, annual percentage changes, counterfactual contrasts, geographic summaries, population scenarios, and annual CIJ observations. Primary-model coefficients, diagnostics, and sensitivity analyses are reported in the supplement, and executable code accompanies the analysis. The study was not prospectively registered, so adherence to an independently archived analysis plan could not be assessed.",
  "Were there other risks of bias?", "Unclear risk", "The analysis used observed monthly totals, accounted for secular trend, annual seasonality, and first-order autocorrelation, reported confidence intervals and residual diagnostics, and examined alternative specifications. Remaining risks include the absence of a control series, the short suspension segment, possible concurrent changes, use of surveillance-capture dates, and unavailable prescription indications."
)

supplementary_table_s5 <- tibble::tribble(
  ~stage, ~records, ~details_and_analytic_treatment,
  "Methadone-labelled source records", "156 379", "Records dated Jan 2018 to Aug 2025",
  "Duplicate records removed", "575", "Same standardised presentation and surveillance-capture timestamp",
  "Oral-formulation and quantity-processing exclusions", "431", "Processing exclusions rather than incomplete records; includes one implausible extreme value",
  "Cleaned records", "155 373", "Records remaining after duplicate, product, formulation, and quantity cleaning",
  "Records without derivable milligrams", "13", "All from 2018; excluded from volume analyses",
  "National analysis", "155 360", "Included 14 records missing both state and municipality (10 in 2018, one in 2019, and three in 2020; 13 400 mg)",
  "Subnational analysis", "155 346", "Excluded the 14 records without state and municipality"
)
