make_learner = function(learner_id) {
  preproc = po("subsample") %>>% po("removeconstants") %>>% po("fixfactors") %>>% po("imputesample", affect_columns = selector_type(c("factor", "ordered")))
  
  switch(learner_id,
    ranger = {
      ranger = GraphLearner$new(preproc$clone(deep = TRUE) %>>% lrn("classif.ranger"))
      ranger$param_set$values$classif.ranger.verbose = FALSE
      ranger
    },
    glmnet = {
      glmnet = GraphLearner$new(preproc$clone(deep = TRUE) %>>% po("encodeimpact", affect_columns = selector_type(c("factor", "ordered"))) %>>% lrn("classif.glmnet"))
      glmnet$param_set$values$classif.glmnet.trace.it = 0L
      glmnet
    },
    xgboost = {
      xgboost = GraphLearner$new(preproc$clone(deep = TRUE) %>>% po("encodeimpact", affect_columns = selector_type(c("factor", "ordered"))) %>>% lrn("classif.xgboost"))
      xgboost$param_set$values$classif.xgboost.verbose = 0
      xgboost
    },
    rpart = {
      rpart = GraphLearner$new(preproc$clone(deep = TRUE) %>>% lrn("classif.rpart"))
      rpart
    }
  )
}

