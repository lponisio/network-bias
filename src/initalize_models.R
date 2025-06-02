
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



# 
# 
 #para.boot <- function(largeModel, smallModel, nsim){
#   pboot <- function(m1, m0) {
#   sims <- simulateFun(m0)
#  L0 <- logLik(refit(m0, sims))
#     L1 <- logLik(refit(m1, sims))
#     return(2*(L1 - L0))
#   }
#   obsval <- c(2*(logLik(largeModel) - logLik(smallModel)))
#   pbdist <- replicate(nsim, pboot(m1=largeModel, m0=smallModel))
#   pval <- mean(c(obsval, pbdist) >= obsval)
#   return(c(stat=round(obsval, 3),
#            p.value=round(pval, 5)))
# }
# 
# boot.all <- function(dat.mods, ## data
#                      formulas, ## full model formula
#                      formulas.nest, ## model formula without xvar of interest
#                      fams, ## family
#                      ys, ## response variable
#                      nsim){
#   
#   ## run nested model
#   nest.mods <-  mod(forms= formulas.nest,
#                     fam= fams,
#                     ys= ys,
#                     dats=dat.mods)
#   ## run full model
#   all.mods <-  mod(forms= formulas,
#                    fam= fams,
#                    ys= ys,
#                    dats=dat.mods)
#   ## run bootrap
#   out.vals <- para.boot(largeModel=all.mods,
#                         smallModel=nest.mods,
#                         nsim = nsim)
#   return(out.vals)
# }
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

write_latex_table <- function(df, file, columns = NULL) {
  if (!is.null(columns)) {
    df <- df[, columns, drop = FALSE]
  }
  
  # Replace _ with space, escape &
  clean_latex <- function(x) {
    x <- gsub("&", "\\\\&", x)     # escape &
    x <- gsub("_", " ", x)         # replace _ with space
    return(x)
  }
  
  # Start LaTeX table lines
  lines <- c()
  col_align <- paste(rep("l", ncol(df)), collapse = "")
  lines <- c(lines, paste0("\\begin{tabular}{", col_align, "}"))
  lines <- c(lines, "\\hline")
  
  # Header
  header <- paste(names(df), collapse = " & ")
  lines <- c(lines, paste0(header, " \\\\"))
  lines <- c(lines, "\\hline")
  
  # Data rows
  for (i in 1:nrow(df)) {
    row <- sapply(df[i, ], clean_latex)
    lines <- c(lines, paste0(paste(row, collapse = " & "), " \\\\"))
  }
  
  lines <- c(lines, "\\hline", "\\end{tabular}")
  
  # Write to file
  writeLines(lines, con = file)
}






