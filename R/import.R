#' @importFrom stats sd rbeta setNames
#' @importFrom utils head read.csv
#' @import ggplot2
#' @import dplyr
#' @import bit64
#' @import happyR
NULL

if (getRversion() >= "2.15.1")  utils::globalVariables(c(
  'Type',
  'Subtype',
  'QQ',
  'METRIC.Recall',
  'METRIC.Precision',
  'Group.Id',
  'Type',
  'N',
  'METRIC.Precision',
  'METRIC.Recall',
  'METRIC.Frac_NA',
  'METRIC.F1_Score',
  'METRIC.Precision.mean',
  'METRIC.Precision.sd',
  'METRIC.Recall.mean',
  'METRIC.Recall.sd',
  'METRIC.Frac_NA.mean',
  'METRIC.Frac_NA.sd',
  'METRIC.F1_Score.mean',
  'METRIC.F1_Score.sd',
  'Replicate.Id',
  'QUERY.TOTAL',
  'TRUTH.TP',
  'TRUTH.FN',
  'QUERY.FP',
  'QUERY.UNK',
  '.Id',
  'Sample.Id',
  'Replicate.Id',
  'Subset'
))