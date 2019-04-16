## Code for Logistic Regression and diagnostics in R

# The Model

model <-glm(y ~ x1 + x2 + x3, data = yourdata, family = "binomial")
summary(model)

## ROC Curve

#Extract from the fitted model object the vector of fitted probabilities:
  
predpr <- predict(mod,type=c("response"))
library(pROC)
roccurve <- roc(y ~ predpr)
plot(roccurve)

## Confusion Matrix
confusion.glm <- function(data, model) {
  prediction <- ifelse(predict(model, data, type='response') > 0.5, TRUE, FALSE)
  confusion  <- table(prediction, as.logical(model$y))
  confusion  <- cbind(confusion, c(1 - confusion[1,1]/(confusion[1,1]+confusion[2,1]), 1 - confusion[2,2]/(confusion[2,2]+confusion[1,2])))
  confusion  <- as.data.frame(confusion)
  names(confusion) <- c('FALSE', 'TRUE', 'class.error')
  confusion
}

## Hosmer - Lemeshow goodness of fit test

hl <- hoslem.test(model$y, fitted(model), g=10)
