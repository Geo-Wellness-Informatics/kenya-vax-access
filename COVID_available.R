
data_all <- read.csv("C:/Users/User/Downloads/TravelTime_Urbancenter_Ruralcluster.csv")


data_all$sc_shock_1 <- as.factor(data_all$sc_shock_1)
data_all$as_assets_8 <- as.factor(data_all$as_assets_8)
data_all$pr_govtstmt <- as.factor(data_all$pr_govtstmt)
data_all$ahs_hhinsurance <- as.factor(data_all$ahs_hhinsurance)
data_all$resp_gender_confirm <- as.factor(data_all$resp_gender_confirm)
data_all$currentlocation_code <- as.factor(data_all$currentlocation_code)


data_all$resp_age_update_std <- scale(data_all$resp_age_update)
data_all$Travel_centroid_std <- scale(data_all$Travel_centroid)


data_all$hh_children_cat <- cut(as.numeric(data_all$hh_children),
                                breaks = c(-Inf, 0, 2, Inf),
                                labels = c("No children", "1-2 children", "3+ children"))

data_all$ns_hadcovid_cat <- cut(as.numeric(data_all$ns_hadcovid),
                                breaks = c(-Inf, 0, 2, Inf),
                                labels = c("Never", "1-2 times", "3+ times"))

create_models <- function(data, dependent_var) {
  # Model 1: Base Model
  model1 <- glm(as.formula(paste(dependent_var, "~ resp_age_update_std +
                      ns_hadcovid_cat +
                      sc_shock_1 +
                      hh_children_cat +
                      as_assets_8 +
                      pr_govtstmt +
                      ahs_hhinsurance +
                      Travel_centroid_std +
                      resp_gender_confirm")), 
                data = data, 
                family = "binomial",
                na.action = na.omit)
  
  # Model 2: Quadratic Model
  model2 <- glm(as.formula(paste(dependent_var, "~ resp_age_update_std +
                      ns_hadcovid_cat +
                      sc_shock_1 +
                      hh_children_cat +
                      as_assets_8 +
                      pr_govtstmt +
                      ahs_hhinsurance +
                      Travel_centroid_std +
                      I(Travel_centroid_std^2) +
                      resp_gender_confirm")), 
                data = data, 
                family = "binomial",
                na.action = na.omit)
  
  # Model 3: Interaction Model
  model3 <- glm(as.formula(paste(dependent_var, "~ resp_age_update_std +
                      ns_hadcovid_cat +
                      sc_shock_1 +
                      hh_children_cat +
                      as_assets_8 +
                      pr_govtstmt +
                      ahs_hhinsurance +
                      Travel_centroid_std +
                      I(Travel_centroid_std^2) +
                      Travel_centroid_std:pr_govtstmt +
                      resp_gender_confirm")),
                data = data,
                family = "binomial",
                na.action = na.omit)
  
  return(list(model1 = model1, model2 = model2, model3 = model3))
}

intent_models <- create_models(data_all, "vc_intent")
available_models <- create_models(data_all, "vc_available")


create_html_output <- function() {
  html_file <- "C:/Users/User/Downloads/Travel_time/vaccine_analysis.html"
  

  cat('<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<style>
body {
    font-family: Arial, sans-serif;
    margin: 20px;
    max-width: 1200px;
    margin: 0 auto;
    padding: 20px;
}
table {
    width: 100%;
    border-collapse: collapse;
    margin: 20px 0;
}
th, td {
    padding: 8px;
    text-align: left;
    border: 1px solid #ddd;
}
th {
    background-color: #f5f5f5;
}
tr:nth-child(even) {
    background-color: #f9f9f9;
}
.section {
    margin: 30px 0;
    padding: 20px;
    background-color: white;
    border-radius: 5px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}
.header {
    background-color: #1a73e8;
    color: white;
    padding: 10px;
    margin-bottom: 20px;
    border-radius: 5px;
}
.significant {
    color: #1a73e8;
    font-weight: bold;
}
.model-group {
    margin-top: 40px;
    border-top: 2px solid #1a73e8;
    padding-top: 20px;
}
</style>
</head>
<body>
', file = html_file)
  
  # Sample Statistics
  cat(sprintf('
<div class="section">
<h2 class="header">Sample Characteristics (N=%d)</h2>
<table>
    <tr>
        <th>Metric</th>
        <th>Value</th>
    </tr>
    <tr>
        <td>Vaccine Intent Rate</td>
        <td>%.1f%%</td>
    </tr>
    <tr>
        <td>Vaccine Availability Rate</td>
        <td>%.1f%%</td>
    </tr>
    <tr>
        <td>Mean Travel Time (SD)</td>
        <td>%.1f (%.1f)</td>
    </tr>
</table>
</div>
', 
              nrow(data_all),
              mean(data_all$vc_intent, na.rm=TRUE)*100,
              mean(data_all$vc_available, na.rm=TRUE)*100,
              mean(data_all$Travel_centroid, na.rm=TRUE),
              sd(data_all$Travel_centroid, na.rm=TRUE)
  ), file = html_file, append = TRUE)
  
  # Function to create model results table
  create_model_table <- function(model, outcome_name, model_type) {
    coef_summary <- summary(model)$coefficients
    
    cat(sprintf('
<div class="section">
<h2 class="header">%s - %s</h2>
<table>
    <tr>
        <th>Variable</th>
        <th>Coefficient</th>
        <th>Std. Error</th>
        <th>Z value</th>
        <th>P value</th>
    </tr>
', outcome_name, model_type), file = html_file, append = TRUE)
    
    for(i in 1:nrow(coef_summary)) {
      cat(sprintf('
    <tr>
        <td>%s</td>
        <td>%.3f %s</td>
        <td>%.3f</td>
        <td>%.3f</td>
        <td>%.3f</td>
    </tr>
',
                  rownames(coef_summary)[i],
                  coef_summary[i,1],
                  ifelse(coef_summary[i,4] < 0.001, "***",
                         ifelse(coef_summary[i,4] < 0.01, "**",
                                ifelse(coef_summary[i,4] < 0.05, "*", ""))),
                  coef_summary[i,2],
                  coef_summary[i,3],
                  coef_summary[i,4]
      ), file = html_file, append = TRUE)
    }
    
    # Add model fit statistics
    cat(sprintf('
</table>
<div style="margin-top: 10px;">
    <strong>Model Fit Statistics:</strong><br>
    AIC: %.2f<br>
    BIC: %.2f<br>
    Null Deviance: %.2f on %d degrees of freedom<br>
    Residual Deviance: %.2f on %d degrees of freedom
</div>
</div>
',
                AIC(model),
                BIC(model),
                model$null.deviance,
                model$df.null,
                model$deviance,
                model$df.residual
    ), file = html_file, append = TRUE)
  }
  
  # Vaccine Intent Models
  cat('
<div class="model-group">
<h1 class="header">Vaccine Intent Models</h1>
', file = html_file, append = TRUE)
  
  create_model_table(intent_models$model1, "Vaccine Intent", "Base Model")
  create_model_table(intent_models$model2, "Vaccine Intent", "Quadratic Model")
  create_model_table(intent_models$model3, "Vaccine Intent", "Interaction Model")
  
  # Vaccine Availability Models
  cat('
<div class="model-group">
<h1 class="header">Vaccine Availability Models</h1>
', file = html_file, append = TRUE)
  
  create_model_table(available_models$model1, "Vaccine Availability", "Base Model")
  create_model_table(available_models$model2, "Vaccine Availability", "Quadratic Model")
  create_model_table(available_models$model3, "Vaccine Availability", "Interaction Model")
  
  # Model Comparison Summary
  cat('
<div class="model-group">
<h1 class="header">Model Comparison Summary</h1>
<div class="section">
<table>
    <tr>
        <th>Outcome</th>
        <th>Model Type</th>
        <th>AIC</th>
        <th>BIC</th>
        <th>Residual Deviance</th>
        <th>df</th>
    </tr>
', file = html_file, append = TRUE)
  
  # Add comparison rows
  models_list <- list(
    list(intent_models$model1, "Intent", "Base"),
    list(intent_models$model2, "Intent", "Quadratic"),
    list(intent_models$model3, "Intent", "Interaction"),
    list(available_models$model1, "Availability", "Base"),
    list(available_models$model2, "Availability", "Quadratic"),
    list(available_models$model3, "Availability", "Interaction")
  )
  
  for(model_info in models_list) {
    cat(sprintf('
    <tr>
        <td>%s</td>
        <td>%s</td>
        <td>%.2f</td>
        <td>%.2f</td>
        <td>%.2f</td>
        <td>%d</td>
    </tr>
',
                model_info[[2]],
                model_info[[3]],
                AIC(model_info[[1]]),
                BIC(model_info[[1]]),
                model_info[[1]]$deviance,
                model_info[[1]]$df.residual
    ), file = html_file, append = TRUE)
  }
  
  cat('</table></div>', file = html_file, append = TRUE)
  
  # Notes Section
  cat('
<div class="section">
<h2 class="header">Notes</h2>
<ul>
    <li>*** p < 0.001</li>
    <li>** p < 0.01</li>
    <li>* p < 0.05</li>
    <li>Travel time has been standardized for analysis</li>
    <li>Base Model: Linear effect of travel time</li>
    <li>Quadratic Model: Adds squared term of travel time</li>
    <li>Interaction Model: Adds interaction between travel time and government trust</li>
</ul>
</div>
</div>
</body>
</html>
', file = html_file, append = TRUE)
  
  cat("\nComplete analysis report has been generated at:", html_file, "\n")
}

create_html_output()