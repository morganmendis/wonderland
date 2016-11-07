source("clean_pci_data.R")
library(randomForest)

set.seed(240)


model <- randomForest(TARGET_D~.,data = train)

varImpPlot(model)
