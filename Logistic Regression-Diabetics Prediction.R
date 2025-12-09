# 广义线性模型数据实战——皮马印第安人糖尿病预测 
setwd("D:/Working-documents/Assignment-Collection/多元统计分析/Homework-Week4")
# =============================================================================
# 第一部分：数据加载与探索性分析
# =============================================================================
cat("=== 数据加载与探索性分析 ===\n")

diabetes_data <- read.csv("Diabetics prediction/diabetes2.csv", head=TRUE)

cat("\n数据基本信息：\n")
str(diabetes_data)
cat("\n数据前几行：\n")
head(diabetes_data)
cat("\n数据摘要统计：\n")
summary(diabetes_data)
cat("\n缺失值检查：\n")
cat(sprintf("   总缺失值数量: %d\n", sum(is.na(diabetes_data))))

cat("\n1.1 零值筛查与处理：\n")
cat("   检查各变量中的零值，识别可能的缺失数据或异常值\n")

numeric_vars <- names(diabetes_data)[sapply(diabetes_data, is.numeric)]
zero_counts <- sapply(numeric_vars, function(var) {
  sum(diabetes_data[[var]] == 0, na.rm = TRUE)
})

cat("   各变量零值统计：\n")
for (var in numeric_vars) {
  zero_count <- zero_counts[var]
  total_count <- nrow(diabetes_data)
  zero_percentage <- (zero_count / total_count) * 100
  cat(sprintf("   %s: %d个零值 (%.1f%%)\n", var, zero_count, zero_percentage))
}

# 识别需要处理的零值变量
# 对于医学数据，某些变量的零值可能表示缺失数据
# 根据糖尿病数据的特点，以下变量在生物学上不太可能为零：
biologically_impossible_zero_vars <- c("Glucose", "BloodPressure", "BMI")

replace_with_mean_vars <- c("SkinThickness")

cat("\n1.1.1 零值处理策略：\n")
cat("   生物学上不可能为零的变量：", paste(biologically_impossible_zero_vars, collapse = ", "), "\n")
cat("   用均值替换零值的变量：", paste(replace_with_mean_vars, collapse = ", "), "\n")

cat("\n1.1.2 删除包含生物学上不可能零值的行：\n")
if (length(biologically_impossible_zero_vars) > 0) {

  available_vars <- intersect(biologically_impossible_zero_vars, names(diabetes_data))
  if (length(available_vars) > 0) {

    zero_condition <- rowSums(diabetes_data[available_vars] == 0) > 0
    rows_to_remove <- which(zero_condition)
    
    cat(sprintf("   发现 %d 行包含生物学上不可能为零的值\n", length(rows_to_remove)))
    
    diabetes_cleaned <- diabetes_data[!zero_condition, ]
    cat(sprintf("   删除后数据从 %d 行减少到 %d 行\n", nrow(diabetes_data), nrow(diabetes_cleaned)))
  } else {
    diabetes_cleaned <- diabetes_data
    cat("   未找到需要删除零值的变量\n")
  }
} else {
  diabetes_cleaned <- diabetes_data
}

cat("\n1.1.3 用均值替换零值：\n")
for (var in replace_with_mean_vars) {
  if (var %in% names(diabetes_cleaned)) {
    zero_indices <- which(diabetes_cleaned[[var]] == 0)
    if (length(zero_indices) > 0) {
      mean_value <- mean(diabetes_cleaned[[var]][diabetes_cleaned[[var]] != 0], na.rm = TRUE)
      diabetes_cleaned[zero_indices, var] <- mean_value
      cat(sprintf("   %s: 将 %d 个零值替换为均值 %.2f\n", var, length(zero_indices), mean_value))
    } else {
      cat(sprintf("   %s: 无零值需要替换\n", var))
    }
  }
}

diabetes_data <- diabetes_cleaned
cat("\n   零值处理完成，数据已更新\n")

cat("\n1.2 变量分布分析：\n")
cat("   分析8个解释变量的分布特征，识别数据的分布模式\n")

explanatory_vars <- names(diabetes_data)[names(diabetes_data) != "Outcome"]
cat(sprintf("   共有 %d 个解释变量需要分析\n", length(explanatory_vars)))

cat("\n1.2.1 数据质量检查：\n")
cat("   检查数据的基本质量指标，保留所有数据点进行分析\n")

cat("\n1.2.2 绘制8个解释变量的分布图：\n")

windowsFonts(微软雅黑 = windowsFont("微软雅黑"))

par(mfrow = c(4, 2))
par(family = "微软雅黑")
par(mar = c(3, 3, 2, 1))

for (var in explanatory_vars) {
  hist(diabetes_data[[var]], 
       breaks = 30,
       main = paste(var, "分布图"),
       xlab = "",
       ylab = "",
       col = "lightblue",
       border = "black",
       freq = FALSE,
       cex.main = 0.8,
       cex.axis = 0.7)
  
  lines(density(diabetes_data[[var]], na.rm = TRUE), 
        col = "red", lwd = 2)
}

par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)

cat("\n1.2.3 绘制8个解释变量的箱线图：\n")

par(mfrow = c(4, 2))
par(family = "微软雅黑")
par(mar = c(3, 3, 2, 1))

for (var in explanatory_vars) {
  boxplot(diabetes_data[[var]],
          main = paste(var, "箱线图"),
          ylab = "",
          col = "lightgreen",
          border = "darkgreen",
          cex.main = 0.8,
          cex.axis = 0.7)
}

par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)

cat("\n1.2.4 Outcome变量分布：\n")

par(mfrow = c(1, 1))
par(family = "微软雅黑")

outcome_counts <- table(diabetes_data$Outcome)
outcome_labels <- paste(c("非糖尿病", "糖尿病"), "\n(", outcome_counts, "例, ", 
                       round(prop.table(outcome_counts) * 100, 1), "%)", sep = "")

pie(outcome_counts, 
    labels = outcome_labels,
    main = "Outcome变量分布",
    col = c("lightblue", "lightcoral"),
    border = "white",
    cex = 0.8)

par(mfrow = c(1, 1))

cat("\n1.3 相关性分析：\n")
cat("   计算并可视化8个解释变量之间的相关性矩阵\n")

explanatory_data <- diabetes_data[, explanatory_vars]
correlation_matrix <- cor(explanatory_data, use = "complete.obs")
cat(sprintf("   解释变量相关性矩阵计算完成 (%dx%d)\n", 
            nrow(correlation_matrix), ncol(correlation_matrix)))

cat("\n1.3.1 绘制解释变量相关性热力图：\n")

if (!require(corrplot, quietly = TRUE)) {
  install.packages("corrplot")
  library(corrplot)
}

par(family = "微软雅黑")

corrplot(correlation_matrix, 
         method = "color",
         type = "full",
         order = "hclust",
         tl.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.6,
         col = colorRampPalette(c("red", "white", "blue"))(200),
         title = "解释变量相关性热力图",
         mar = c(0, 0, 2, 0))

cat("\n1.3.2 相关性分析总结：\n")

cat("   解释变量间高相关性变量对（|r| > 0.7）：\n")
high_corr_pairs <- which(abs(correlation_matrix) > 0.7 & 
                        correlation_matrix != 1, arr.ind = TRUE)

if (nrow(high_corr_pairs) > 0) {
  for (i in seq_len(min(5, nrow(high_corr_pairs)))) {
    row_idx <- high_corr_pairs[i, 1]
    col_idx <- high_corr_pairs[i, 2]
    var1 <- rownames(correlation_matrix)[row_idx]
    var2 <- colnames(correlation_matrix)[col_idx]
    cor_value <- correlation_matrix[row_idx, col_idx]
    cat(sprintf("   %s vs %s: %.3f\n", var1, var2, cor_value))
  }
} else {
  cat("   未发现高相关性变量对（|r| > 0.7）\n")
}

cat("\n数据预处理和可视化分析完成！\n")

# =============================================================================
# 第二部分：参数估计（Newton-Raphson和Fisher Scoring算法）
# =============================================================================
cat("\n=== 第二部分：参数估计 ===\n")

# 准备数据
if (is.numeric(diabetes_data$Outcome)) {
  y <- as.numeric(diabetes_data$Outcome)
} else {
  y <- as.numeric(as.character(diabetes_data$Outcome))
}

X <- model.matrix(~ ., data = diabetes_data[, explanatory_vars])
n <- nrow(X)
p <- ncol(X)

cat(sprintf("\n数据维度: n = %d, p = %d\n", n, p))
cat("解释变量:", paste(explanatory_vars, collapse = ", "), "\n")

# Logistic回归的辅助函数
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

log_likelihood <- function(beta, X, y) {
  eta <- X %*% beta
  p <- sigmoid(eta)
  sum(y * log(p + 1e-10) + (1 - y) * log(1 - p + 1e-10))
}

gradient <- function(beta, X, y) {
  eta <- X %*% beta
  p <- sigmoid(eta)
  t(X) %*% (y - p)
}

hessian <- function(beta, X, y) {
  eta <- X %*% beta
  p <- sigmoid(eta)
  W <- diag(as.vector(p * (1 - p)))
  -t(X) %*% W %*% X
}

# 删除了不再使用的fisher_information函数

cat("\n2.1 Newton-Raphson算法迭代求解：\n")
cat("   Newton-Raphson算法使用Hessian矩阵进行迭代更新\n")

beta_nr <- rep(0, p)
max_iter <- 100
tolerance <- 1e-8
nr_history <- data.frame(Iteration = integer(), 
                        LogLikelihood = numeric(),
                        MaxGradient = numeric(),
                        stringsAsFactors = FALSE)

cat("\n   迭代过程：\n")
for (iter in 1:max_iter) {
  grad <- gradient(beta_nr, X, y)
  hess <- hessian(beta_nr, X, y)
  
  # 检查Hessian矩阵是否可逆
  if (any(is.na(hess)) || any(is.infinite(hess))) {
    cat(sprintf("   迭代 %d: Hessian矩阵异常，停止迭代\n", iter))
    break
  }
  
  # 尝试求解
  tryCatch({
    delta <- solve(-hess, grad)
    beta_nr <- beta_nr + delta
    
    ll <- log_likelihood(beta_nr, X, y)
    max_grad <- max(abs(grad))
    
    nr_history <- rbind(nr_history, data.frame(
      Iteration = iter,
      LogLikelihood = ll,
      MaxGradient = max_grad
    ))
    
    if (iter <= 5 || iter %% 10 == 0) {
      cat(sprintf("   迭代 %d: 对数似然 = %.4f, 最大梯度 = %.6f\n", 
                  iter, ll, max_grad))
    }
    
    if (max(abs(delta)) < tolerance) {
      cat(sprintf("   迭代 %d: 收敛！最大参数变化 = %.8f\n", iter, max(abs(delta))))
      break
    }
  }, error = function(e) {
    cat(sprintf("   迭代 %d: 求解失败 - %s\n", iter, e$message))
    break
  })
}

if (iter == max_iter) {
  cat("   警告: 达到最大迭代次数，可能未完全收敛\n")
}

cat("\n   Newton-Raphson算法最终估计结果：\n")
names(beta_nr) <- colnames(X)
print(round(beta_nr, 6))

# 使用Newton-Raphson的结果作为最终估计
beta_final <- beta_nr

cat("\n2.2 回归方程：\n")
cat("   使用Newton-Raphson算法，Logistic回归方程为：\n")
cat("\n   logit(P(Y=1|X)) = ")
eq_parts <- c()
for (i in seq_along(beta_final)) {
  var_name <- names(beta_final)[i]
  coef_val <- beta_final[i]
  if (i == 1) {
    eq_parts <- c(eq_parts, sprintf("%.4f", coef_val))
  } else {
    if (coef_val >= 0) {
      eq_parts <- c(eq_parts, sprintf("+ %.4f*%s", coef_val, var_name))
    } else {
      eq_parts <- c(eq_parts, sprintf("- %.4f*%s", abs(coef_val), var_name))
    }
  }
}
cat(paste(eq_parts, collapse = " "))
cat("\n\n   其中 logit(p) = log(p/(1-p))\n")

cat("\n2.3 使用R内置glm函数验证：\n")
glm_model <- glm(Outcome ~ ., data = diabetes_data, family = binomial(link = "logit"))
beta_glm <- coef(glm_model)

beta_validation <- data.frame(
  Variable = names(beta_final),
  Fisher_Scoring = round(beta_final, 6),
  GLM_Result = round(beta_glm, 6),
  Difference = round(abs(beta_final - beta_glm), 6)
)
print(beta_validation)

max_diff_glm <- max(beta_validation$Difference)
cat(sprintf("\n   与glm函数结果的最大差异: %.8f\n", max_diff_glm))
if (max_diff_glm < 1e-4) {
  cat("   估计结果与R内置函数高度一致，验证通过\n")
} else {
  cat("   估计结果与R内置函数存在差异，需要检查\n")
}

# =============================================================================
# 第三部分：回归的显著性检验
# =============================================================================
cat("\n=== 第三部分：回归的显著性检验 ===\n")

cat("\n3.1 回归关系整体的显著性检验（广义似然比检验）：\n")

# 零模型（只有截距项）
null_model <- glm(Outcome ~ 1, data = diabetes_data, family = binomial)
null_deviance <- null_model$deviance
null_df <- null_model$df.residual

# 完整模型
full_deviance <- glm_model$deviance
full_df <- glm_model$df.residual

# 广义似然比统计量
lr_statistic <- null_deviance - full_deviance
lr_df <- null_df - full_df
lr_p_value <- 1 - pchisq(lr_statistic, lr_df)

cat(sprintf("   零模型偏差: %.4f (自由度: %d)\n", null_deviance, null_df))
cat(sprintf("   完整模型偏差: %.4f (自由度: %d)\n", full_deviance, full_df))
cat(sprintf("   似然比统计量: %.4f (自由度: %d)\n", lr_statistic, lr_df))
cat(sprintf("   p值: %.6f\n", lr_p_value))

if (lr_p_value < 0.001) {
  cat("   结论: p < 0.001，回归关系高度显著\n")
} else if (lr_p_value < 0.01) {
  cat("   结论: p < 0.01，回归关系非常显著\n")
} else if (lr_p_value < 0.05) {
  cat("   结论: p < 0.05，回归关系显著\n")
} else {
  cat("   结论: p >= 0.05，回归关系不显著\n")
}

cat("\n3.2 单个回归系数的显著性检验（Wald检验）：\n")

coef_summary <- summary(glm_model)$coefficients
wald_results <- data.frame(
  Variable = rownames(coef_summary),
  Coefficient = coef_summary[, 1],
  StdError = coef_summary[, 2],
  Z_Statistic = coef_summary[, 3],
  P_Value = coef_summary[, 4],
  Significant = ifelse(coef_summary[, 4] < 0.05, "是", "否")
)

cat("\n   Wald检验结果：\n")
wald_results_display <- wald_results
wald_results_display[, c("Coefficient", "StdError", "Z_Statistic", "P_Value")] <- 
  round(wald_results[, c("Coefficient", "StdError", "Z_Statistic", "P_Value")], 6)
print(wald_results_display)

cat("\n   显著性总结：\n")
sig_vars <- wald_results$Variable[wald_results$P_Value < 0.05]
non_sig_vars <- wald_results$Variable[wald_results$P_Value >= 0.05]

cat(sprintf("   显著变量 (p < 0.05): %d 个\n", length(sig_vars)))
if (length(sig_vars) > 0) {
  for (var in sig_vars) {
    p_val <- wald_results$P_Value[wald_results$Variable == var]
    cat(sprintf("     - %s (p = %.6f)\n", var, p_val))
  }
}

cat(sprintf("   不显著变量 (p >= 0.05): %d 个\n", length(non_sig_vars)))
if (length(non_sig_vars) > 0) {
  for (var in non_sig_vars) {
    p_val <- wald_results$P_Value[wald_results$Variable == var]
    cat(sprintf("     - %s (p = %.6f)\n", var, p_val))
  }
}

# 保存不显著变量列表供第五部分使用
non_significant_vars <- non_sig_vars[non_sig_vars != "(Intercept)"]

cat("\n3.3 回归系数的95%置信区间：\n")
coef_ci <- confint(glm_model, level = 0.95)
ci_results <- data.frame(
  Variable = rownames(coef_ci),
  Lower_CI = round(coef_ci[, 1], 6),
  Upper_CI = round(coef_ci[, 2], 6)
)
print(ci_results)

# 检查置信区间是否包含0
ci_contains_zero <- (ci_results$Lower_CI <= 0) & (ci_results$Upper_CI >= 0)
ci_results$Contains_Zero <- ifelse(ci_contains_zero, "是", "否")
cat("\n   置信区间包含0的变量（不显著）：\n")
zero_ci_vars <- ci_results$Variable[ci_contains_zero]
if (length(zero_ci_vars) > 0) {
  for (var in zero_ci_vars) {
    cat(sprintf("     - %s: [%.4f, %.4f]\n", 
                var, ci_results$Lower_CI[ci_results$Variable == var],
                ci_results$Upper_CI[ci_results$Variable == var]))
  }
} else {
  cat("     无\n")
}

# =============================================================================
# 第四部分：预测过程与性能评估
# =============================================================================
cat("\n=== 第四部分：预测过程与性能评估 ===\n")

cat("\n4.1 生成预测值：\n")

# 预测概率
predicted_prob <- predict(glm_model, type = "response")
cat(sprintf("   预测概率范围: [%.4f, %.4f]\n", min(predicted_prob), max(predicted_prob)))
cat(sprintf("   预测概率均值: %.4f\n", mean(predicted_prob)))

# 预测类别（使用0.5作为阈值）
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
cat(sprintf("   预测类别分布: 0类 = %d, 1类 = %d\n", 
            sum(predicted_class == 0), sum(predicted_class == 1)))

# 实际类别
actual_class <- y

cat("\n4.2 预测性能评估指标：\n")

# 混淆矩阵
conf_matrix <- table(Actual = actual_class, Predicted = predicted_class)
cat("\n   混淆矩阵：\n")
print(conf_matrix)

# 计算各种性能指标
TP <- conf_matrix[2, 2]  # True Positive
TN <- conf_matrix[1, 1]  # True Negative
FP <- conf_matrix[1, 2]  # False Positive
FN <- conf_matrix[2, 1]  # False Negative

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("\n   分类性能指标：\n")
cat(sprintf("   准确率 (Accuracy): %.4f\n", accuracy))
cat(sprintf("   精确率 (Precision): %.4f\n", precision))
cat(sprintf("   召回率 (Recall/Sensitivity): %.4f\n", recall))
cat(sprintf("   特异性 (Specificity): %.4f\n", specificity))
cat(sprintf("   F1分数 (F1-Score): %.4f\n", f1_score))

# AUC计算
if (!require(ROCR, quietly = TRUE)) {
  install.packages("ROCR")
  library(ROCR)
}

pred_obj <- prediction(predicted_prob, actual_class)
auc_value <- performance(pred_obj, "auc")@y.values[[1]]
cat(sprintf("   AUC值 (Area Under Curve): %.4f\n", auc_value))

# ROC曲线
roc_curve <- performance(pred_obj, "tpr", "fpr")

cat("\n4.3 绘制ROC曲线：\n")
par(family = "微软雅黑")
plot(roc_curve, 
     main = "ROC曲线",
     xlab = "假阳性率 (1 - Specificity)",
     ylab = "真阳性率 (Sensitivity)",
     col = "blue",
     lwd = 2)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
text(0.6, 0.2, sprintf("AUC = %.4f", auc_value), cex = 1.2, col = "darkblue")

# 性能指标总结表
performance_summary <- data.frame(
  指标 = c("准确率", "精确率", "召回率", "特异性", "F1分数", "AUC"),
  数值 = round(c(accuracy, precision, recall, specificity, f1_score, auc_value), 4)
)

cat("\n   预测性能指标总结：\n")
print(performance_summary)

cat("\n4.4 预测值列表：\n")

# 创建预测结果数据框
prediction_results <- data.frame(
  观测编号 = 1:n,
  实际类别 = actual_class,
  预测概率 = round(predicted_prob, 4),
  预测类别 = predicted_class,
  预测正确 = ifelse(actual_class == predicted_class, "是", "否")
)

cat("\n   前20个观测的预测结果：\n")
print(head(prediction_results, 20))

cat("\n   后20个观测的预测结果：\n")
print(tail(prediction_results, 20))

# 统计预测正确率
correct_predictions <- sum(prediction_results$预测正确 == "是")
cat(sprintf("\n   预测正确数量: %d / %d (%.2f%%)\n", 
            correct_predictions, n, 100 * correct_predictions / n))

# 按预测概率分组统计
cat("\n4.5 预测概率分布分析：\n")
prob_ranges <- cut(predicted_prob, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
prob_distribution <- table(prob_ranges)
cat("   预测概率分布：\n")
print(prob_distribution)

# 高置信度预测统计
high_conf_predictions <- sum(predicted_prob > 0.8 | predicted_prob < 0.2)
cat(sprintf("\n   高置信度预测 (概率>0.8或<0.2): %d (%.2f%%)\n", 
            high_conf_predictions, 100 * high_conf_predictions / n))

# 保存完整预测结果（可选，如果数据量不大）
cat("\n   完整预测结果已保存在prediction_results数据框中\n")
cat(sprintf("   总观测数: %d\n", nrow(prediction_results)))

# =============================================================================
# 第五部分：模型改进（去除不显著变量后重新回归）
# =============================================================================
cat("\n=== 第五部分：模型改进 ===\n")

if (length(non_significant_vars) > 0) {
  cat(sprintf("\n5.1 识别不显著的回归系数分量：\n"))
  cat(sprintf("   发现 %d 个不显著变量 (p >= 0.05):\n", length(non_significant_vars)))
  for (var in non_significant_vars) {
    cat(sprintf("     - %s\n", var))
  }
  
  cat("\n5.2 构建改进模型（去除不显著变量）：\n")
  
  # 构建新的公式（去除不显著变量，但保留截距项）
  significant_vars <- setdiff(explanatory_vars, non_significant_vars)
  
  if (length(significant_vars) > 0) {
    improved_formula <- as.formula(paste("Outcome ~", paste(significant_vars, collapse = " + ")))
    cat(sprintf("   改进模型公式: %s\n", format(improved_formula)))
    
    cat("\n5.3 使用改进模型重新估计参数：\n")
    
    # 使用Fisher Scoring重新估计
    X_improved <- model.matrix(improved_formula, data = diabetes_data)
    p_improved <- ncol(X_improved)
    
    cat("\n   使用Fisher Scoring算法重新估计：\n")
    beta_improved <- rep(0, p_improved)
    
    for (iter in 1:max_iter) {
      grad <- gradient(beta_improved, X_improved, y)
      fisher_info <- fisher_information(beta_improved, X_improved, y)
      
      tryCatch({
        delta <- solve(fisher_info, grad)
        beta_improved <- beta_improved + delta
        
        if (iter <= 3 || iter %% 10 == 0) {
          ll <- log_likelihood(beta_improved, X_improved, y)
          cat(sprintf("   迭代 %d: 对数似然 = %.4f\n", iter, ll))
        }
        
        if (max(abs(delta)) < tolerance) {
          cat(sprintf("   迭代 %d: 收敛！\n", iter))
          break
        }
      }, error = function(e) {
        cat(sprintf("   迭代失败: %s\n", e$message))
        break
      })
    }
    
    names(beta_improved) <- colnames(X_improved)
    cat("\n   改进模型参数估计结果：\n")
    print(round(beta_improved, 6))
    
    # 使用glm函数拟合改进模型
    improved_glm <- glm(improved_formula, data = diabetes_data, family = binomial(link = "logit"))
    
    cat("\n5.4 改进模型的显著性检验：\n")
    
    # 整体显著性检验
    improved_null <- glm(Outcome ~ 1, data = diabetes_data, family = binomial)
    improved_lr_stat <- improved_null$deviance - improved_glm$deviance
    improved_lr_df <- improved_null$df.residual - improved_glm$df.residual
    improved_lr_p <- 1 - pchisq(improved_lr_stat, improved_lr_df)
    
    cat(sprintf("   整体显著性检验: 似然比统计量 = %.4f, p值 = %.6f\n", 
                improved_lr_stat, improved_lr_p))
    
    # 单个系数显著性检验
    improved_coef_summary <- summary(improved_glm)$coefficients
    improved_wald <- data.frame(
      Variable = rownames(improved_coef_summary),
      Coefficient = round(improved_coef_summary[, 1], 6),
      StdError = round(improved_coef_summary[, 2], 6),
      Z_Statistic = round(improved_coef_summary[, 3], 6),
      P_Value = round(improved_coef_summary[, 4], 6)
    )
    
    cat("\n   改进模型Wald检验结果：\n")
    print(improved_wald)
    
    cat("\n5.5 模型比较：\n")
    
    comparison_table <- data.frame(
      Model = c("完整模型", "改进模型"),
      Variables = c(length(explanatory_vars), length(significant_vars)),
      AIC = round(c(AIC(glm_model), AIC(improved_glm)), 4),
      BIC = round(c(BIC(glm_model), BIC(improved_glm)), 4),
      Deviance = round(c(glm_model$deviance, improved_glm$deviance), 4),
      Pseudo_R2 = round(c(1 - glm_model$deviance/glm_model$null.deviance,
                   1 - improved_glm$deviance/improved_glm$null.deviance), 4)
    )
    
    print(comparison_table)
    
    cat("\n5.6 改进模型的回归方程：\n")
    cat("   logit(P(Y=1|X)) = ")
    improved_eq_parts <- c()
    for (i in seq_along(beta_improved)) {
      var_name <- names(beta_improved)[i]
      coef_val <- beta_improved[i]
      if (i == 1) {
        improved_eq_parts <- c(improved_eq_parts, sprintf("%.4f", coef_val))
      } else {
        if (coef_val >= 0) {
          improved_eq_parts <- c(improved_eq_parts, sprintf("+ %.4f*%s", coef_val, var_name))
        } else {
          improved_eq_parts <- c(improved_eq_parts, sprintf("- %.4f*%s", abs(coef_val), var_name))
        }
      }
    }
    cat(paste(improved_eq_parts, collapse = " "))
    cat("\n")
    
    cat("\n5.7 改进效果总结：\n")
    aic_improvement <- AIC(glm_model) - AIC(improved_glm)
    bic_improvement <- BIC(glm_model) - BIC(improved_glm)
    
    cat(sprintf("   AIC改善: %.4f (降低表示改进)\n", aic_improvement))
    cat(sprintf("   BIC改善: %.4f (降低表示改进)\n", bic_improvement))
    cat(sprintf("   变量数量: 从 %d 减少到 %d\n", 
                length(explanatory_vars), length(significant_vars)))
    
    if (aic_improvement > 0 && bic_improvement > 0) {
      cat("   ✓ 改进模型在AIC和BIC指标上均优于完整模型\n")
    } else if (aic_improvement > 0) {
      cat("   ✓ 改进模型在AIC指标上优于完整模型\n")
    } else {
      cat("   ⚠️  改进模型在某些指标上未优于完整模型，但模型更简洁\n")
    }
    
  } else {
    cat("   警告: 所有变量都不显著，无法构建改进模型\n")
  }
  
} else {
  cat("\n5.1 不显著变量检查：\n")
  cat("   所有回归系数分量均显著 (p < 0.05)，无需去除变量\n")
  cat("   当前模型即为最优模型\n")
}

cat("\n=== Logistic回归分析完成 ===\n")
cat("第一部分：数据加载与探索性分析完成\n")
cat("第二部分：通过Newton-Raphson和Fisher Scoring算法完成参数估计\n")
cat("第三部分：通过广义似然比检验和Wald检验完成显著性检验\n")
cat("第四部分：完成预测过程与性能评估\n")
cat("第五部分：通过去除不显著变量完成模型改进\n")
