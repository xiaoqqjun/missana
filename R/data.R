#' 医疗研究中的缺失数据示例
#'
#' 一个包含200位患者医疗数据的示例数据集，有一些有意设计的缺失值，
#' 用于演示缺失机制分析函数的使用。
#'
#' @format 数据框，包含600行和18列:
#' \describe{
#'   \item{PatientID}{患者ID}
#'   \item{Gender}{性别 (0=女性, 1=男性)}
#'   \item{Age}{年龄 (岁)}
#'   \item{BMI}{体重指数}
#'   \item{BloodType}{血型 (A, B, AB, O)}
#'   \item{Hemoglobin}{血红蛋白浓度 (g/L)}
#'   \item{Neutrophils}{中性粒细胞数 (10^9/L)}
#'   \item{Platelets}{血小板计数 (10^9/L)}
#'   \item{Lymphocytes}{淋巴细胞计数 (10^9/L)}
#'   \item{WBC}{白细胞数 (10^9/L)}
#'   \item{T_Stage}{T分期 (T1-T4)}
#'   \item{N_Stage}{N分期 (N0-N3)}
#'   \item{M_Stage}{M分期 (M0-M1)}
#'   \item{LymphNodeMetastasis}{淋巴结转移 (0=无, 1=有)}
#'   \item{FollowUpTime}{随访时间 (月)}
#'   \item{SurvivalStatus}{存活状态 (0=存活, 1=死亡)}
#'   \item{DiseaseFreeTime}{无病生存时间 (月)}
#'   \item{QualityOfLife}{生活质量评分 (0-100)}
#' }
#' @source 模拟数据，基于医学研究常见数据结构
"data_test"