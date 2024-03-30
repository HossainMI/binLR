
binLR = function(formula,
                 data,
                 wt = NULL) {
  
  if (is.null(wt)) {
    
    model = glm(formula = formula,
                 data = data,
                 family = "binomial")
    
  } else {
    
    model = glm(formula = formula,
                 data = data,
                 family = "binomial",
                 weights = wt)
  }
  
  summary = summary(model)
  
  model_OR = as.data.frame(round(exp(cbind(OR = coef(model),
                                           confint(model))), 2))
  
  p_values = as.data.frame(round(summary$coefficients[,4], 4))
  
  colnames(p_values) = "P-value"
  
  return(cbind(model_OR,
               p_values))
}

# Demo analysis.

library(Hmisc)

data = spss.get("data_binLR.sav")

formula = DEP~WAGE+EANC+V190+FEDU+MM

binLR(formula,
      data = data,
      wt = NULL)
