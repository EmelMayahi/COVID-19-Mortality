Predictive Modeling of Mortality Risk in Hospitalized COVID-19 Patients
This repository contains the code and resources developed for my master's thesis titled "Longitudinal In-hospital Forecast and Evaluation System for Assessing COVID-19 Mortality and Validation Effectiveness".

📌 Abstract
The acute respiratory disease caused by the COVID-19 virus, which originated in China at the end of 2019, not only placed a significant strain on healthcare resources but also made the efficient management of time and medical resources one of the major challenges during the pandemic era.

This research aimed to design a predictive system for assessing the deterioration of hospitalized patients with COVID-19 using data from Bellvitge University Hospital in Barcelona, Spain. The dataset comprises information from 2,285 patients with 288 variables.

Seven distinct cause-specific Cox regression models were developed for mortality prediction using three different variable selection approaches:

Naive approach (Models 1–2)
Genetic algorithm optimization (Models 3–5)
Regularization techniques (Models 6–7: Ridge and Lasso)
The Lasso-penalized model (Model 7) was selected as the final model, achieving an AUC of 0.854 with excellent calibration (intercept = -0.0069, slope = 0.963). Key predictors identified include:

Age,
Smoking history,
Charlson comorbidity index,
Hypertension,
Dyspnea,
Chronic heart failure and
Reduced oxygen saturation

🤝 Acknowledgments
Bellvitge University Hospital, Barcelona, Spain for providing the clinical dataset
My thesis advisors and research team
Open-source community for the statistical packages used
