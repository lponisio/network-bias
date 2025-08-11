# =========================================================
# Model Table Formatting & Bootstrap Utilities
# =========================================================

# -----------------------------
# Data and output path settings
# -----------------------------

# data
webs_complete <- read.csv("network-bias-saved/saved/webs_complete.csv")

savefilepath <- c("network-bias-saved/manuscript/tables")


# ---------------------------------------------------------
# Formatter: Mixed-effects (lmer / lmerTest) → LaTeX table
# ---------------------------------------------------------
format_lmer_table <- function(model, caption = "Regression Results",
                              savefilepath = "network-bias-saved/manuscript/tables") {
  # Get model summary
  sum_model <- summary(model)
  
  # Extract fixed effects and convert to data frame
  coefs <- coef(sum_model)
  coefs_df <- as.data.frame(coefs)
  
  # Store raw numeric p-values
  raw_p <- coefs_df$`Pr(>|t|)`
  
  # Add significance stars BEFORE formatting p-values
  coefs_df$Significance <- symnum(raw_p,
                                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                  symbols = c("***", "**", "*", ".", " "))
  
  # Format p-values AFTER significance stars
  coefs_df$`Pr(>|t|)` <- ifelse(is.na(raw_p), "NA",
                                ifelse(raw_p < 2e-16, "< 2e-16",
                                       formatC(raw_p, format = "e", digits = 2)))
  
  # Identify scaled terms for bolding
  bold_rows <- grepl("scale", rownames(coefs_df))
  
  # Build LaTeX table
  latex_table <- kable(coefs_df, format = "latex", booktabs = TRUE, digits = 3, align = "c",
                       caption = caption) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    row_spec(0, bold = TRUE, extra_latex_after = "\\hline \\hline") %>%
    row_spec(which(bold_rows), bold = TRUE, extra_latex_after = "\\hline") %>%
    row_spec(which(!bold_rows &
                     rownames(coefs_df) != "(Intercept)")[1] - 1,
             extra_latex_after = "\\addlinespace")
  
  # Save to file
  model_name <- deparse(substitute(model))
  model_name_safe <- gsub("[^[:alnum:]_]", "_", model_name)
  write.table(latex_table, file = paste0(savefilepath, "/",
                                         model_name_safe, "_table.txt"),
              sep = "\t", row.names = FALSE, quote = FALSE)
  
  return(latex_table)
}



# -----------------------------------------------
# Formatter: GLM / glm.nb (MASS) → LaTeX table
# -----------------------------------------------
format_glm_table <- function(model, caption = "Regression Results",
                             savefilepath = "network-bias-saved/manuscript/tables") {
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
                                  formatC(coef_table$`Pr(>|z|)`,
                                          format = "e",
                                          digits = 2))
  
  # Identify which rows contain scaled variables
  bold_rows <- grepl("scale", rownames(coef_table))
  
  # Generate LaTeX table (without the problematic extra header)
  latex_table <- kable(coef_table, format = "latex", booktabs = TRUE,
                       digits = 3,
                       align = "c",
                       caption = caption) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    row_spec(0, bold = TRUE, extra_latex_after = "\\hline \\hline") %>%
    row_spec(which(bold_rows), bold = TRUE, extra_latex_after = "\\hline") %>%
    row_spec(which(!bold_rows & rownames(coef_table) !=
                     "(Intercept)")[1] - 1,
             extra_latex_after = "\\addlinespace")
  
  # Extract model name and sanitize it for the filename
  model_name <- deparse(substitute(model))
  model_name_safe <- gsub("[^[:alnum:]_]", "_", model_name)
  
  # Save table as a .txt file with the model's name
  write.table(latex_table, file = paste0(savefilepath, "/",
                                         model_name_safe,
                                         "_table.txt"),
              sep = "\t", row.names = FALSE, quote = FALSE)
  
  return(latex_table)
}



# -------------------------------------------------------
# Unified model runner: GLMM/GLM/LMM via family keyword
# -------------------------------------------------------
## Runs models based on a formula, family, response variable, and data
mod <- function(forms,
                fam,
                ys,
                dats,
                return.sum = FALSE) {
  
  if (fam == "poisson") {
    out.mod <- glmer(forms,
                     family = fam,
                     data = dats,
                     nAGQ = 10L,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 1e9)))
    
  } else if (fam == "nbinomRandom") {
    out.mod <- glmer.nb(forms,
                        data = dats,
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 1e9),
                                               tolPwrss = 1e-3))
    
  } else if (fam == "gaussian") {
    out.mod <- lmer(forms, data = dats)
    
  } else if (fam == "nbinom") {
    out.mod <- glm.nb(forms, data = dats)
    
  } else {
    stop("Unsupported family. Use 'poisson', 'nbinomRandom', 'gaussian', or 'nbinom'.")
  }
  
  # Return summary or model object
  if (return.sum) {
    return(summary(out.mod))
  } else {
    return(out.mod)
  }
}


# -------------------------------------------------------
# Parametric bootstrap LRT: large vs small model
# -------------------------------------------------------
para.boot <- function(largeModel, smallModel, nsim) {
  # Helper function to perform the bootstrap
  pboot <- function(m1, m0) {
    # Simulate new response values based on small model (m0)
    sims <- simulate(m0, nsim = 1)[[1]]  # Simulate response values
    
    # Update the data with the simulated response
    m0_data <- m0$model
    m0_data[[as.character(formula(m0))[2]]] <- sims  # Update response variable
    
    # Refit the models with the simulated data
    refitted_m0 <- update(m0, data = m0_data)
    refitted_m1 <- update(m1, data = m0_data)
    
    # Calculate log-likelihoods for the refitted models
    L0 <- logLik(refitted_m0)
    L1 <- logLik(refitted_m1)
    
    # Return the likelihood ratio statistic
    return(2 * (L1 - L0))
  }
  
  # Calculate the observed likelihood ratio statistic
  obsval <- 2 * (logLik(largeModel) - logLik(smallModel))
  
  # Perform the bootstrap by replicating the procedure
  pbdist <- replicate(nsim, pboot(m1 = largeModel, m0 = smallModel))
  
  # Calculate p-value by comparing observed vs bootstrap statistics
  pval <- mean(c(obsval, pbdist) >= obsval)
  
  # Return the observed statistic and p-value
  return(c(stat = round(obsval, 3), p.value = round(pval, 5)))
}


# -------------------------------------------------------
# Bootstrap SE for a focal parameter (by simulation)
# -------------------------------------------------------
se.boot <- function(largeModel,
                    smallModel,
                    param = "s.simpson.div", ## xvar of interest
                    nsim){
  p.sim <- function(m1, m0, param){
    sims <- simulateFun(m0)
    new.model <- refit(m1, sims)
    out.param <- coef(summary(new.model))[param,
                                          'Estimate']
    return(out.param)
  }
  pb <-  replicate(nsim, p.sim(m1=largeModel,
                               m0=smallModel,
                               param=param))
  
  if(!is.null(dim(pb))){
    se.param <- apply(pb, 1, sd)
    names(se.param) <- rownames(pb)
  } else {
    se.param <- sd(pb)
  }
  return(se.param)
}


# -------------------------------------------------------
# LaTeX helper: multi-column country list
# -------------------------------------------------------
make_latex_country_table <- function(country_vector, ncol = 3) {
  # Remove NAs
  country_vector <- na.omit(country_vector)
  
  # Calculate number of rows needed
  n <- length(country_vector)
  nrow <- ceiling(n / ncol)
  
  # Create a matrix with nrow rows and ncol columns, filling by column
  mat <- matrix("", nrow = nrow, ncol = ncol)
  mat[1:n] <- country_vector
  
  # Fill matrix by column to get desired column-wise layout
  mat <- matrix(country_vector, ncol = ncol, byrow = FALSE)
  
  # Begin LaTeX tabular
  latex_str <- "\\begin{tabular}{l l l}\n"
  
  for (i in 1:nrow(mat)) {
    row_vals <- mat[i, ]
    row_vals[is.na(row_vals)] <- ""  # clean up any trailing NA
    latex_str <- paste0(latex_str, paste(row_vals, collapse = " & "), " \\\\\n")
  }
  
  latex_str <- paste0(latex_str, "\\end{tabular}")
  
  return(latex_str)
}


