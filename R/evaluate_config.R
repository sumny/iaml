#' @title Run an algorithm
#'
#' @description Run an algorithm.
#'
#' @param learner `character`\cr
#'   Learner name e.g. "classif.glmnet".
#' @param task_id `character`\cr
#'   OpenML data ID.
#' @param configuration `list`\cr
#'   Named list of hyperparameters.
#' @param trainsize `numeric`\cr
#'   Fraction of training set size.
#' @param logfile `character`\cr
#'   Logfile to write to.
#' @param seed `character`\cr
#'   Seed to set. Defaults to `NULL` (no custom seed, using original one).
#' @export
#' @examples
#' learner = "classif.glmnet"
#' task_id = "40981"
#' configuration = list(classif.glmnet.alpha = 0.1, classif.glmnet.s = 0.1)
#' trainsize = 0.05
#' eval_config(learner, task_id, configuration, trainsize)
eval_config = function(learner, task_id, configuration, trainsize, logfile = NULL, seed = NULL) {
  learner_id = assert_choice(learner, paste0("classif.", c("ranger", "glmnet", "xgboost", "rpart")))
  learner_id = gsub("classif.", replacement = "", x = learner_id)
  data_id = as.numeric(assert_choice(task_id, c("40981", "41146", "1489", "1067")))
  assert_list(configuration)
  assert_int(seed, null.ok = TRUE)
  assert_string(logfile, null.ok = TRUE)

  logger = get_logger("eval_logger")$set_threshold("info")
  if (!is.null(logfile)) {
    logger$add_appender(lgr::AppenderFile$new(logfile))
  }
  logger$info(sprintf("Evaluating %s on %s.", learner, task_id))

  fracs = c(0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1)
  frac_interval = fracs[trainsize <= fracs][1L]
  seed = if (is.null(seed)) data_id * 100 * frac_interval
  task = tsk("oml", data_id = data_id)
  set.seed(seed)
  # FIMXE: could also load the splits from iaml_splits.csv
  r = rsmp("cv", folds = 5L)
  task = tsk("oml", data_id = data_id)
  r$instantiate(task)
  frac = trainsize

  logger$info(sprintf("Hyperparameters: %s", config_to_string(configuration)))

  learner = make_learner(learner_id)
  learner$param_set$values$subsample.frac = frac
  learner$predict_type = "prob"
  orig_pv = learner$param_set$values

  learner$param_set$values = insert_named(orig_pv, configuration)

  set.seed(seed)
  rr = resample(task, learner = learner, resampling = r)
  performances = rr$aggregate(msrs(c("classif.ce", "classif.fbeta", "classif.auc", "classif.logloss")))

  gc()
  set.seed(seed)
  ram_train = peakRAM::peakRAM({learner$train(task)})$Peak_RAM_Used_MiB
  ram_model = object.size(learner$model)[1L] / 1000000  # MiB
  ram_predict = peakRAM::peakRAM({p = learner$predict(task)})$Peak_RAM_Used_MiB
  rams = c(ram_train = ram_train, ram_model = ram_model, ram_predict = ram_predict)
  times = c(time_train = learner$state$train_time, time_predict = learner$state$predict_time)

  # NOTE: prediction function is logit of positive class (due to logistic regression etc.)
  pred = Predictor$new(learner, data = as.data.frame(task$data()), y = task$target_names, class = task$positive)
  pred$prediction.function = create_predict_fun_custom(learner, task = "classification")
  set.seed(seed)
  imeasure = FunComplexity$new(pred, max_seg_cat = 9L, max_seg_num = 5L, epsilon = 0.05, grid.size = 200L)
  interpretability = c(mec = imeasure$c_wmean, ias = 1 - imeasure$r2, nf = imeasure$n_features)

  result = c(performances, rams, times, interpretability)
  logger$info(sprintf("Result: %s: %s", names(result), result))

  result
}


#' @title Run an algorithm from iaml
#'
#' @description Run an algorithm from iaml.
#'
#' @param learner `character`\cr
#'   Learner name e.g. "iaml_glmnet".
#' @param task_id `character`\cr
#'   OpenML data ID.
#' @param configuration `list`\cr
#'   Named list of hyperparameters.
#' @param ... `any`\cr
#'   Arguments passed on to `eval_config`.
#' @export
#' @examples
#' learner = "iaml_glmnet"
#' task_id = "40981"
#' configuration = list(alpha = 0.1, s = 0.1, trainsize = 0.05)
#' eval_iaml(learner, task_id, configuration)
eval_iaml = function(learner, task_id, configuration, ...) {
  assert_true(grepl("iaml_", learner))
  trainsize = configuration[["trainsize"]]
  configuration[["trainsize"]] = NULL

  iaml_name = gsub("iaml_", "", learner)
  if (learner == "iaml_super") {
    assert_true(!is.null(configuration$learner))
    learner = paste0("iaml_", configuration$learner)
    learner_short = configuration$learner
    configuration$learner = NULL
    configuration_names = map(strsplit(names(configuration), paste0(learner_short, ".")), 2L)
    names(configuration) = configuration_names
  }
  iaml_name = gsub("iaml_", "", learner)
  learner = gsub("iaml_", "classif.", learner)

  # Filter missing args
  configuration = Filter(Negate(is.na), configuration)
  # Fix up configuration names
  names(configuration) = paste0(learner, ".", names(configuration))
  eval_config(learner, task_id, configuration, trainsize, ...)
}



#' @title Run a YAHPO Gym config
#'
#' @description Run a YAHPO Gym config.
#'
#' @param scenario `character`\cr
#'   Scenario (e.g. `iaml_super`) to evaluate.
#' @param configuration `list`\cr
#'   Named list of hyperparameters including task_id and trainsize.
#' @param ... `any`\cr
#'   Arguments passed on to `eval_config`.
#' @export
#' @examples
#' scenario = "iaml_glmnet"
#' configuration = list(alpha = 0.1, s = 0.1, trainsize = 0.05, task_id = "40981")
#' eval_yahpo(scenario, configuration)
eval_yahpo = function(scenario, configuration, ...) {
  learner = scenario
  task_id = configuration[["task_id"]]
  configuration = configuration[- which(names(configuration) == "task_id")]
  result = eval_iaml(learner, task_id, configuration, ...)
  names_lookup = data.table(old = c("classif.ce", "classif.fbeta", "classif.auc", "classif.logloss", "ram_train", "ram_model",
                                    "ram_predict", "time_train", "time_predict", "mec", "ias", "nf"),
                            new = c("mmce", "f1", "auc", "logloss", "ramtrain", "rammodel",
                                    "rampredict", "timetrain", "timepredict", "mec", "ias", "nf"))
  names(result) = names_lookup[match(names(result), names_lookup$old), ][["new"]]
  as.list(result)
}

