
#packages
library(performance)
library(lme4)
library(dplyr)
library(MASS)
library(cowplot)
library(ggplot2)
## library(ggeffects)
library(viridis)
library(glmmTMB)
library(car)
library(DHARMa)

#data
webs_complete <- read.csv("network-bias-saved/saved/webs_complete.csv")

savefilepath <- c("network-bias-saved/manuscript/tables")

library(knitr)
library(kableExtra)
library(knitr)
library(kableExtra)

format_glm_table <- function(model, caption = "Regression Results", savefilepath = "network-bias-saved/manuscript/tables") {
  # Extract coefficients
  sum_model <- summary(model)
  coef_table <- as.data.frame(sum_model$coefficients)
  colnames(coef_table) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  
  # Create significance stars
  coef_table$Significance <- symnum(coef_table$`Pr(>|z|)`, 
                                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                    symbols = c("***", "**", "*", ".", " "))
  
  # Convert p-values to formatted strings, handling very small values
  coef_table$`Pr(>|z|)` <- ifelse(coef_table$`Pr(>|z|)` < 2e-16, "< 2e-16", 
                                  formatC(coef_table$`Pr(>|z|)`, format = "e", digits = 2))
  
  # Identify which rows contain scaled variables
  bold_rows <- grepl("scale", rownames(coef_table))
  
  # Generate LaTeX table (without the problematic extra header)
  latex_table <- kable(coef_table, format = "latex", booktabs = TRUE, digits = 3, align = "c",
                       caption = caption) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    row_spec(0, bold = TRUE, extra_latex_after = "\\hline \\hline") %>%
    row_spec(which(bold_rows), bold = TRUE, extra_latex_after = "\\hline") %>%
    row_spec(which(!bold_rows & rownames(coef_table) != "(Intercept)")[1] - 1, extra_latex_after = "\\addlinespace")
  
  # Extract model name and sanitize it for the filename
  model_name <- deparse(substitute(model))
  model_name_safe <- gsub("[^[:alnum:]_]", "_", model_name)
  
  # Save table as a .txt file with the model's name
  write.table(latex_table, file = paste0(savefilepath, "/", model_name_safe, "_table.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
  
  return(latex_table)
}
