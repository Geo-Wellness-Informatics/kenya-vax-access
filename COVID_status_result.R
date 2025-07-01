# 读取数据
data_all <- read.csv("C:/Users/User/Downloads/TravelTime_Urbancenter_Ruralcluster.csv")

# 将相关变量转换为因子型
data_all$sc_shock_1 <- as.factor(data_all$sc_shock_1)
data_all$as_assets_8 <- as.factor(data_all$as_assets_8)
data_all$pr_govtstmt <- as.factor(data_all$pr_govtstmt)
data_all$ahs_hhinsurance <- as.factor(data_all$ahs_hhinsurance)
data_all$resp_gender_confirm <- as.factor(data_all$resp_gender_confirm)

# 标准化年龄和旅行时间变量
data_all$resp_age_update_std <- scale(data_all$resp_age_update)
data_all$Travel_centroid_std <- scale(data_all$Travel_centroid)

# 重新编码变量
# 子女数量重编码
data_all$hh_children_cat <- cut(as.numeric(data_all$hh_children),
                                breaks = c(-Inf, 0, 2, Inf),
                                labels = c("No children", "1-2 children", "3+ children"))

# COVID历史重编码
data_all$ns_hadcovid_cat <- cut(as.numeric(data_all$ns_hadcovid),
                                breaks = c(-Inf, 0, 2, Inf),
                                labels = c("Never", "1-2 times", "3+ times"))

# 基础线性模型 - 无交互项
model1 <- glm(vc_status ~ resp_age_update_std +
                ns_hadcovid_cat +
                sc_shock_1 +
                hh_children_cat +
                as_assets_8 +
                pr_govtstmt +
                ahs_hhinsurance +
                Travel_centroid_std +
                resp_gender_confirm, 
              data = data_all, 
              family = "binomial",
              na.action = na.omit)

# 非线性模型 - 只加入平方项
model2 <- glm(vc_status ~ resp_age_update_std +
                ns_hadcovid_cat +
                sc_shock_1 +
                hh_children_cat +
                as_assets_8 +
                pr_govtstmt +
                ahs_hhinsurance +
                Travel_centroid_std +
                I(Travel_centroid_std^2) +
                resp_gender_confirm, 
              data = data_all, 
              family = "binomial",
              na.action = na.omit)

# 最终模型 - 加入交互项
model3 <- glm(vc_status ~ resp_age_update_std +
                ns_hadcovid_cat +
                sc_shock_1 +
                hh_children_cat +
                as_assets_8 +
                pr_govtstmt +
                ahs_hhinsurance +
                Travel_centroid_std +
                I(Travel_centroid_std^2) +
                Travel_centroid_std:pr_govtstmt +
                resp_gender_confirm,
              data = data_all,
              family = "binomial",
              na.action = na.omit)

# 创建输出文件夹
dir.create("C:/Users/User/Downloads/Travel_time", showWarnings = FALSE)

# 创建分析函数
create_model_analysis <- function(model, model_name) {
  coef_summary <- summary(model)$coefficients
  odds_ratios <- exp(coef(model))
  ci <- exp(confint(model))
  
  results <- data.frame(
    Variable = names(coef(model)),
    Coefficient = coef(model),
    Std_Error = coef_summary[,2],
    Z_value = coef_summary[,3],
    P_value = coef_summary[,4],
    Odds_Ratio = odds_ratios,
    CI_Lower = ci[,1],
    CI_Upper = ci[,2],
    Significance = ifelse(coef_summary[,4] < 0.001, "***",
                          ifelse(coef_summary[,4] < 0.01, "**",
                                 ifelse(coef_summary[,4] < 0.05, "*", "")))
  )
  
  return(results)
}

model1_results <- create_model_analysis(model1, "Base Model")
model2_results <- create_model_analysis(model2, "Quadratic Model")
model3_results <- create_model_analysis(model3, "Interaction Model")

cat('<!DOCTYPE html>
<html>
<head>
<style>
body {
    font-family: Arial, sans-serif;
    margin: 20px;
    line-height: 1.6;
}
.container {
    max-width: 1200px;
    margin: 0 auto;
}
table {
    border-collapse: collapse;
    width: 100%;
    margin: 20px 0;
}
th, td {
    border: 1px solid #ddd;
    padding: 8px;
    text-align: left;
}
th {
    background-color: #f5f5f5;
}
tr:nth-child(even) {
    background-color: #f9f9f9;
}
.model-section {
    margin-bottom: 40px;
}
.significant {
    color: #1a73e8;
    font-weight: bold;
}
.header {
    background-color: #1a73e8;
    color: white;
    padding: 10px;
    margin-bottom: 20px;
}
</style>
</head>
<body>
<div class="container">',
file = "C:/Users/User/Downloads/Travel_time/complete_analysis.html")

# Function to format table rows
format_row <- function(row) {
  sprintf('
    <tr>
      <td>%s</td>
      <td>%.3f %s</td>
      <td>%.3f</td>
      <td>%.3f</td>
      <td>%.3f</td>
      <td>%.3f [%.3f, %.3f]</td>
    </tr>',
          row$Variable,
          row$Coefficient, row$Significance,
          row$Std_Error,
          row$Z_value,
          row$P_value,
          row$Odds_Ratio,
          row$CI_Lower,
          row$CI_Upper
  )
}

# Add results for each model
models <- list(model1_results, model2_results, model3_results)
names <- c("Base Model", "Quadratic Model", "Interaction Model")

for(i in 1:3) {
  cat(sprintf('
    <div class="model-section">
    <h2 class="header">%s Results</h2>
    <table>
      <tr>
        <th>Variable</th>
        <th>Coefficient (Significance)</th>
        <th>Std. Error</th>
        <th>Z value</th>
        <th>P value</th>
        <th>Odds Ratio [95%% CI]</th>
      </tr>', names[i]), file = "C:/Users/User/Downloads/Travel_time/complete_analysis.html", append = TRUE)
  
  # Add each row
  for(j in 1:nrow(models[[i]])) {
    cat(format_row(models[[i]][j,]), file = "C:/Users/User/Downloads/Travel_time/complete_analysis.html", append = TRUE)
  }
  
  cat('</table></div>', file = "C:/Users/User/Downloads/Travel_time/complete_analysis.html", append = TRUE)
}

# Add model comparison metrics
cat('
<div class="model-section">
<h2 class="header">Model Comparison Metrics</h2>
<table>
  <tr>
    <th>Metric</th>
    <th>Base Model</th>
    <th>Quadratic Model</th>
    <th>Interaction Model</th>
  </tr>
  <tr>
    <td>AIC</td>
    <td>', AIC(model1), '</td>
    <td>', AIC(model2), '</td>
    <td>', AIC(model3), '</td>
  </tr>
  <tr>
    <td>BIC</td>
    <td>', BIC(model1), '</td>
    <td>', BIC(model2), '</td>
    <td>', BIC(model3), '</td>
  </tr>
</table>
</div>
', file = "C:/Users/User/Downloads/Travel_time/complete_analysis.html", append = TRUE)

# Add notes
cat('
<div style="margin-top: 20px;">
<p><strong>Notes:</strong></p>
<ul>
  <li>*** p < 0.001</li>
  <li>** p < 0.01</li>
  <li>* p < 0.05</li>
  <li>CI = Confidence Interval</li>
</ul>
</div>
</div>
</body>
</html>
', file = "C:/Users/User/Downloads/Travel_time/complete_analysis.html", append = TRUE)

cat("\nHTML analysis report has been generated at: C:/Users/User/Downloads/Travel_time/complete_analysis.html\n")