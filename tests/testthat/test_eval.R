test_that("eval_config works", {
  learner_id = "classif.rpart"
  task_id = "40981"
  configuration = list(classif.rpart.cp = 0.01, classif.rpart.maxdepth = 6, classif.rpart.minbucket = 74, classif.rpart.minsplit = 46)
  trainsize = 0.5
  result1 = eval_config(learner_id, task_id, configuration, trainsize)
  result2 = eval_config(learner_id, task_id, configuration, trainsize)
  expect_numeric(result1, len = 12L)
  expect_true(all(names(result1) == c("classif.ce", "classif.fbeta", "classif.auc", "classif.logloss", "ram_train", "ram_model",
                                      "ram_predict", "time_train", "time_predict", "mec", "ias", "nf")))
  # fixed original seed by default, therefore same results except for metrics below
  expect_subset(names(which(abs(result1 - result2) > 1e-3)), c("ram_train", "ram_model", "ram_predict", "time_train", "time_predict"))
})

test_that("eval_iaml works", {
  learner = "iaml_glmnet"
  task_id = "41146"
  configuration = list(alpha = 0.1, s = 0.1, trainsize = 0.05)
  result = eval_iaml(learner, task_id, configuration)
  expect_numeric(result, len = 12L)
  expect_true(all(names(result) == c("classif.ce", "classif.fbeta", "classif.auc", "classif.logloss", "ram_train", "ram_model",
                                      "ram_predict", "time_train", "time_predict", "mec", "ias", "nf")))
})

test_that("eval_yahpo works", {
  scenario = "iaml_super"
  for (i in seq_along(sample_x)) {
    result = eval_yahpo(scenario, sample_x[[i]])
    expect_list(result, len = 12L)
    expect_true(all(names(result) == c("mmce", "f1", "auc", "logloss", "ramtrain", "rammodel",
                                       "rampredict", "timetrain", "timepredict", "mec", "ias", "nf")))
   }
})

test_that("reproduces original values", {
  scenario = "iaml_glmnet"
  configuration = list(task_id = "40981", trainsize = 0.05, alpha = 0.8674066, s = 0.4150169)
  result = eval_yahpo(scenario, configuration)
  eps = sum(abs(unlist(result[c("mmce", "f1", "auc", "logloss", "mec", "ias", "nf")]) - c(0.2536232, 0.7874553, 0.7479059, 0.6453319, 0, 0, 0)))
  expect_true(eps < 1e-3)
  configuration$trainsize = 1
  result = eval_yahpo(scenario, configuration)
  eps = sum(abs(unlist(result[c("mmce", "f1", "auc", "logloss", "mec", "ias", "nf")]) - c(0.4449275, 0.7132488, 0.6350481, 0.6863925, 0, 0, 0)))
  expect_true(eps < 1e-3)
})

