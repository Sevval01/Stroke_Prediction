library(plumber)
library(tidymodels)
library(dplyr)     
library(tibble)

col_log <- readRDS("col_log.rds")
col_rand <- readRDS("col_rand.rds")
col_xgb <- readRDS("col_xgb.rds")
model_log <- readRDS("model_log.rds")
model_rand <- readRDS("model_rand.rds")
model_xgb <- readRDS("model_xgb.rds")

model_scores <- data.frame(
  model = c("log", "random_forest", "xgboost"),
  roc_auc = c(
    col_log$mean[col_log$.metric == "roc_auc"],
    col_rand$mean[col_rand$.metric == "roc_auc"],
    col_xgb$mean[col_xgb$.metric == "roc_auc"] ),
  accuracy = c(
    col_log$mean[col_log$.metric == "accuracy"],
    col_rand$mean[col_rand$.metric == "accuracy"],
    col_xgb$mean[col_xgb$.metric == "accuracy"]),
  f1_score =c (
    col_log$mean[col_log$.metric =="f_meas"],
    col_rand$mean[col_rand$.metric =="f_meas"],
    col_xgb$mean[col_xgb$.metric =="f_meas"]
  ))

#* @post /predict
#* @param age:numeric Age of the patient (e.g., 45)
#* @param hypertension:numeric Hypertension status (1 = Yes, 0 = No)
#* @param heart_disease:numeric Heart disease status (1 = Yes, 0 = No)
#* @param avg_glucose_level:numeric Average glucose level (e.g., 120.5)
#* @param bmi:numeric Body Mass Index (e.g., 27.3)
#* @param gender_Male:numeric Gender Male (1 = Yes, 0 = No)
#* @param gender_Other:numeric Gender Other (1 = Yes, 0 = No) (All options 0 = Female)
#* @param ever_married_Yes:numeric Marital status (1 = Married, 0 = Not married)
#* @param work_type_Govt_job:numeric Work type: Government job (1 = Yes, 0 = No)
#* @param work_type_Never_worked:numeric Work type: Never worked (1 = Yes, 0 = No)
#* @param work_type_Private:numeric Work type: Private sector (1 = Yes, 0 = No)
#* @param work_type_Self_employed:numeric Work type: Self-employed (1 = Yes, 0 = No) (All options 0 = children)
#* @param Residence_type_Urban:numeric Residence type: Urban (1 = Yes, 0 = Rural) 
#* @param smoking_status_never_smoked:numeric Smoking status: Never smoked (1 = Yes, 0 = No)
#* @param smoking_status_smokes:numeric Smoking status: Currently smokes (1 = Yes, 0 = No)
#* @param smoking_status_Unknown:numeric Smoking status: Unknown (1 = Yes, 0 = No) (All options 0 = formerly smoked)
#* @response 200 A JSON containing predictions and model comparison results
function(age, hypertension, heart_disease, avg_glucose_level, bmi,
         gender_Male, gender_Other, ever_married_Yes, work_type_Govt_job, work_type_Never_worked,
         work_type_Private, work_type_Self_employed, Residence_type_Urban, smoking_status_never_smoked,
         smoking_status_smokes, smoking_status_Unknown){
  
  tryCatch({
    
    new_data <- data.frame(
      age = as.numeric(age),
      hypertension = as.numeric(hypertension),
      heart_disease = as.numeric(heart_disease),
      avg_glucose_level = as.numeric(avg_glucose_level),
      bmi = as.numeric(bmi),
      gender_Male = as.numeric(gender_Male),
      gender_Other = as.numeric(gender_Other),
      ever_married_Yes = as.numeric(ever_married_Yes),
      work_type_Govt_job = as.numeric(work_type_Govt_job),
      work_type_Never_worked = as.numeric(work_type_Never_worked),
      work_type_Private = as.numeric(work_type_Private),
      work_type_Self_employed = as.numeric(work_type_Self_employed),
      Residence_type_Urban = as.numeric(Residence_type_Urban),
      smoking_status_never_smoked = as.numeric(smoking_status_never_smoked),
      smoking_status_smokes = as.numeric(smoking_status_smokes),
      smoking_status_Unknown = as.numeric(smoking_status_Unknown)
    )
    
    pred_log  <- predict(model_log,  new_data, type = "prob")$.pred_1
    pred_rand <- predict(model_rand, new_data, type = "prob")$.pred_1
    pred_xgb  <- predict(model_xgb,  new_data, type = "prob")$.pred_1
    
    results <- tibble(
      model = c("Logistic Regression", "Random Forest", "XGBoost"),
      probability = c(pred_log, pred_rand, pred_xgb),
      prediction = ifelse(c(pred_log, pred_rand, pred_xgb) > 0.5, 1, 0),
      accuracy = model_scores$accuracy
    )
    
    best_model <- results %>% filter(accuracy == max(accuracy)) %>% slice(1)
    
    list(
      all_models = results,
      best_model = best_model
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}