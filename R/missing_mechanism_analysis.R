# 缺失机制分析函数 (改进版)
#
# 该脚本提供了完整的缺失值分析功能，可通过source直接加载使用
# 作者：fzj
# 版本：1.0
# 日期：2025-04-13

#' 缺失机制分析函数
#'
#' 这个函数执行一系列分析来确定数据中的缺失机制(MCAR/MAR/MNAR)，
#' 包括缺失值可视化、相关性矩阵、变量间关系检验和Little's MCAR测试
#'
#' @param data 数据框，包含需要分析的数据
#' @param sig_level 显著性水平，默认为0.05
#' @param sample_size 热图样本大小，对于大数据集，默认为100
#' @param high_cor_threshold 认为缺失模式高度相关的阈值，默认为0.5
#' @param output_dir 输出图形和报告的目录，默认为当前目录
#' @param create_plots 是否创建可视化图形，默认为TRUE
#' @param save_plots 是否保存图形，默认为FALSE
#'
#' @return 返回包含各种缺失机制分析结果的列表
#' @export
#'
#' @examples
#' # 基本用法
#' result <- analyze_missing_mechanism(my_data)
#' 
#' # 自定义参数
#' result <- analyze_missing_mechanism(my_data, sig_level = 0.01, 
#'                                    create_plots = TRUE, save_plots = TRUE)
analyze_missing_mechanism <- function(data, 
                                      sig_level = 0.05,
                                      sample_size = 100, 
                                      high_cor_threshold = 0.5,
                                      output_dir = ".", 
                                      create_plots = TRUE,
                                      save_plots = FALSE) {
  
  # 检查必需的包是否可用，如果不可用则尝试安装
  required_packages <- c("ggplot2", "reshape2", "dplyr", "corrplot")
  for(pkg in required_packages) {
    if(!requireNamespace(pkg, quietly = TRUE)) {
      cat(paste0("正在安装必要的包: ", pkg, "\n"))
      install.packages(pkg, quiet = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
  
  # 检查并尝试安装naniar包（用于高级缺失值分析）
  has_naniar <- requireNamespace("naniar", quietly = TRUE)
  if(!has_naniar) {
    cat("naniar包未安装，尝试安装...\n")
    install.packages("naniar", quiet = TRUE)
    has_naniar <- requireNamespace("naniar", quietly = TRUE)
  }
  if(has_naniar) library(naniar)
  
  # 初始化结果列表
  result <- list()
  
  # 数据集基本信息
  result$data_info <- list(
    dim = dim(data),
    n_vars = ncol(data),
    n_obs = nrow(data)
  )
  
  cat("\n========== 开始缺失机制分析 ==========\n")
  cat("数据维度:", nrow(data), "行 x", ncol(data), "列\n\n")
  
  #===========================================
  # 1. 缺失值基本统计
  #===========================================
  
  # 创建缺失值矩阵 (TRUE/FALSE)
  missing_matrix <- is.na(data)
  
  # 计算每个变量的缺失值数量和百分比
  var_missing <- colSums(missing_matrix)
  var_missing_pct <- round(100 * var_missing / nrow(data), 2)
  missing_summary <- data.frame(
    Variable = names(var_missing),
    Missing_Count = var_missing,
    Missing_Percent = var_missing_pct
  )
  missing_summary <- missing_summary[order(-missing_summary$Missing_Count), ]
  
  # 计算每个观测值的缺失值数量
  row_missing <- rowSums(missing_matrix)
  
  # 缺失模式
  pattern_table <- data.frame(
    pattern = apply(missing_matrix, 1, function(x) paste(as.numeric(x), collapse = "")),
    count = 1
  )
  pattern_summary <- aggregate(count ~ pattern, data = pattern_table, sum)
  pattern_summary <- pattern_summary[order(-pattern_summary$count), ]
  pattern_summary$percent <- round(100 * pattern_summary$count / nrow(data), 2)
  
  # 将基本统计信息添加到结果中
  result$missing_summary <- missing_summary
  result$pattern_summary <- pattern_summary
  
  cat("变量缺失情况：\n")
  print(missing_summary)
  
  #===========================================
  # 2. 缺失值热图 (如果需要创建图形)
  #===========================================
  if(create_plots) {
    
    # 如果样本量较大，考虑抽样显示
    if(nrow(data) > sample_size) {
      # 确保包含有缺失值的行
      rows_with_missing <- which(row_missing > 0)
      
      if(length(rows_with_missing) <= sample_size) {
        # 如果缺失行少于样本大小，全部纳入
        sample_rows <- rows_with_missing
        # 如果需要，再随机抽取一些非缺失行
        if(length(sample_rows) < sample_size) {
          non_missing_rows <- setdiff(1:nrow(data), rows_with_missing)
          extra_rows <- sample(non_missing_rows, min(sample_size - length(sample_rows), length(non_missing_rows)))
          sample_rows <- c(sample_rows, extra_rows)
        }
      } else {
        # 如果缺失行很多，随机抽取
        sample_rows <- sample(rows_with_missing, sample_size)
      }
      
      # 对行进行排序
      sample_rows <- sort(sample_rows)
      missing_matrix_sample <- missing_matrix[sample_rows, ]
    } else {
      missing_matrix_sample <- missing_matrix
      sample_rows <- 1:nrow(data)
    }
    
    # 转换为长格式，用于ggplot2
    missing_melted <- reshape2::melt(missing_matrix_sample)
    names(missing_melted) <- c("Row", "Variable", "IsMissing")
    # 添加原始行号
    missing_melted$OriginalRow <- sample_rows[missing_melted$Row]
    
    # 创建缺失值热图
    p_heatmap <- ggplot2::ggplot(missing_melted, ggplot2::aes(x = Variable, y = OriginalRow, fill = IsMissing)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_manual(values = c("steelblue", "tomato"), 
                                 labels = c("存在", "缺失"),
                                 name = "数据状态") +
      ggplot2::labs(title = "缺失值热图",
                    subtitle = "按行显示每个变量的缺失状态",
                    x = "变量", 
                    y = "观测值ID") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     plot.title = ggplot2::element_text(size = 14, face = "bold"),
                     plot.subtitle = ggplot2::element_text(size = 10))
    
    # 保存热图到结果
    result$plots$heatmap <- p_heatmap
    
    # 显示热图
    print(p_heatmap)
    
    # 保存图形
    if(save_plots) {
      ggplot2::ggsave(file.path(output_dir, "missing_heatmap.png"), p_heatmap, width = 10, height = 8)
    }
    
    # 使用naniar包的高级缺失值可视化
    if(has_naniar) {
      # 缺失值可视化，带聚类
      tryCatch({
        p_vis_miss <- naniar::vis_miss(data, cluster = TRUE) +
          ggplot2::labs(title = "缺失值模式热图 (带聚类)")
        
        # 将图形添加到结果
        result$plots$vis_miss <- p_vis_miss
        
        # 显示图形
        print(p_vis_miss)
        
        # 保存图形
        if(save_plots) {
          ggplot2::ggsave(file.path(output_dir, "missing_vis_miss.png"), p_vis_miss, width = 10, height = 8)
        }
      }, error = function(e) {
        cat("创建vis_miss图形时出错:", e$message, "\n")
      })
      
      # 尝试创建组合图
      tryCatch({
        p_upset <- naniar::gg_miss_upset(data)
        
        # 保存为结果对象 (但不能使用+添加元素)
        result$plots$upset <- p_upset
        
        # 显示图形
        print(p_upset)
        
        # 保存图形
        if(save_plots) {
          png(file.path(output_dir, "missing_upset.png"), width = 800, height = 600)
          print(p_upset)
          dev.off()
        }
      }, error = function(e) {
        cat("创建缺失组合图时出错:", e$message, "\n")
      })
    }
  }
  
  #===========================================
  # 3. 缺失值相关性矩阵
  #===========================================
  
  # 创建缺失指示矩阵 (0=存在, 1=缺失)
  miss_indicator <- as.data.frame(missing_matrix) * 1
  
  # 计算缺失指示变量之间的相关性
  miss_cor <- cor(miss_indicator, use = "pairwise.complete.obs")
  
  # 将相关矩阵添加到结果
  result$miss_cor <- miss_cor
  
  # 绘制相关性矩阵
  if(create_plots) {
    tryCatch({
      # 使用corrplot包绘制相关性矩阵
      corrplot::corrplot(miss_cor, 
                         method = "color",      # 颜色方块
                         type = "upper",        # 上三角矩阵
                         diag = FALSE,          # 不显示对角线
                         tl.col = "black",      # 文本颜色
                         tl.srt = 45,           # 文本角度
                         title = "缺失值相关性矩阵",
                         mar = c(0, 0, 2, 0),   # 调整边距
                         addCoef.col = "black", # 添加相关系数
                         number.cex = 0.7,      # 相关系数文字大小
                         cl.ratio = 0.15)       # 色标比例
      
      # 保存图形
      if(save_plots) {
        png(file.path(output_dir, "missing_correlation.png"), width = 800, height = 700)
        corrplot::corrplot(miss_cor, 
                           method = "color", type = "upper", diag = FALSE,
                           tl.col = "black", tl.srt = 45, 
                           title = "缺失值相关性矩阵",
                           mar = c(0, 0, 2, 0), 
                           addCoef.col = "black", number.cex = 0.7, 
                           cl.ratio = 0.15)
        dev.off()
      }
    }, error = function(e) {
      cat("创建相关性矩阵图形时出错:", e$message, "\n")
    })
  }
  
  # 寻找高相关的缺失模式对
  high_miss_cor <- which(abs(miss_cor) > high_cor_threshold & miss_cor != 1, arr.ind = TRUE)
  if(nrow(high_miss_cor) > 0) {
    high_miss_pairs <- data.frame(
      Variable1 = rownames(miss_cor)[high_miss_cor[,1]],
      Variable2 = colnames(miss_cor)[high_miss_cor[,2]],
      Correlation = miss_cor[high_miss_cor]
    )
    # 去除重复对（因为相关矩阵是对称的）
    high_miss_pairs <- high_miss_pairs[!duplicated(t(apply(high_miss_pairs[,1:2], 1, sort))), ]
    # 按相关性绝对值排序
    high_miss_pairs <- high_miss_pairs[order(-abs(high_miss_pairs$Correlation)), ]
    
    # 添加到结果
    result$high_miss_pairs <- high_miss_pairs
    
    cat("\n高相关的缺失模式对 (|r| >", high_cor_threshold, "):\n")
    print(high_miss_pairs)
  } else {
    cat("\n未发现高相关的缺失模式对 (|r| >", high_cor_threshold, ")\n")
  }
  
  #===========================================
  # 4. 变量缺失与其他变量关系分析 (MAR检测)
  #===========================================
  
  # 选择有缺失值的变量进行分析
  vars_with_missing <- names(which(var_missing > 0))
  
  # 存储t检验结果
  t_test_results <- list()
  
  if(length(vars_with_missing) > 0) {
    cat("\n=== 变量缺失与其他变量的关系分析 ===\n")
    
    # 选择数值型变量进行检验
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    
    for(miss_var in vars_with_missing) {
      cat("\n缺失变量:", miss_var, "\n")
      
      # 创建缺失指示变量
      miss_indicator <- is.na(data[[miss_var]])
      
      # 只在存在缺失的情况下进行分析
      if(sum(miss_indicator) > 0 && sum(miss_indicator) < nrow(data)) {
        
        # 对其他变量进行t检验
        var_test_results <- data.frame(
          Variable = character(),
          p_value = numeric(),
          stringsAsFactors = FALSE
        )
        
        for(num_var in setdiff(names(data), miss_var)) {
          # 跳过自身变量
          if(num_var == miss_var) next
          
          # 处理数值型和非数值型变量
          if(is.numeric(data[[num_var]])) {
            # 跳过有缺失值的行
            if(sum(is.na(data[[num_var]])) > 0) {
              test_data <- data[!is.na(data[[num_var]]), ]
              test_indicator <- is.na(test_data[[miss_var]])
            } else {
              test_data <- data
              test_indicator <- miss_indicator
            }
            
            # 只有当两组都有数据时才进行检验
            if(sum(test_indicator) > 0 && sum(!test_indicator) > 0) {
              # t.test可能会因为数据问题报错，使用tryCatch处理
              result_t <- tryCatch({
                t_test <- t.test(test_data[[num_var]] ~ test_indicator)
                t_test$p.value
              }, error = function(e) {
                NA
              })
              
              var_test_results <- rbind(var_test_results, data.frame(
                Variable = num_var,
                p_value = result_t,
                stringsAsFactors = FALSE
              ))
            }
          } else if(is.factor(data[[num_var]]) || is.character(data[[num_var]])) {
            # 对分类变量使用卡方检验
            # 跳过有缺失值的行
            if(sum(is.na(data[[num_var]])) > 0) {
              test_data <- data[!is.na(data[[num_var]]), ]
              test_indicator <- is.na(test_data[[miss_var]])
            } else {
              test_data <- data
              test_indicator <- miss_indicator
            }
            
            # 尝试进行卡方检验
            result_chi <- tryCatch({
              # 创建列联表
              tab <- table(test_data[[num_var]], test_indicator)
              # 确保表格不是空的
              if(sum(tab) > 0 && min(dim(tab)) > 1) {
                # 进行卡方检验
                chi_test <- chisq.test(tab, simulate.p.value = TRUE)
                chi_test$p.value
              } else {
                NA
              }
            }, error = function(e) {
              NA
            })
            
            if(!is.na(result_chi)) {
              var_test_results <- rbind(var_test_results, data.frame(
                Variable = num_var,
                p_value = result_chi,
                stringsAsFactors = FALSE
              ))
            }
          }
        }
        
        # 显示显著的关系
        if(nrow(var_test_results) > 0) {
          # 排序并添加显著性标记
          var_test_results <- var_test_results %>%
            dplyr::arrange(p_value) %>%
            dplyr::mutate(Significant = ifelse(p_value < sig_level, "*", ""))
          
          # 添加到结果列表
          t_test_results[[miss_var]] <- var_test_results
          
          cat("变量值与缺失状态相关分析 (t检验/卡方检验):\n")
          print(var_test_results)
          
          # 显示MAR的证据
          sig_vars <- var_test_results$Variable[var_test_results$p_value < sig_level]
          if(length(sig_vars) > 0) {
            cat("\n可能的MAR证据：", miss_var, "的缺失与以下变量显著相关：\n")
            cat(paste(sig_vars, collapse = ", "), "\n")
          } else {
            cat("\n未发现", miss_var, "的缺失与其他变量显著相关，可能符合MCAR\n")
          }
        } else {
          cat("无法进行测试，可能数据不足\n")
        }
      } else {
        cat("无缺失值或全部缺失，无法分析\n")
      }
    }
  }
  
  # 将t检验结果添加到结果列表
  result$t_test_results <- t_test_results
  
  #===========================================
  # 5. Little's MCAR测试 (如果naniar包可用)
  #===========================================
  
  if(has_naniar) {
    tryCatch({
      mcar_result <- naniar::mcar_test(data)
      result$mcar_test <- mcar_result
      
      cat("\n=== Little's MCAR检验 ===\n")
      print(mcar_result)
      
      cat("\n解释：\n")
      if(mcar_result$p.value > sig_level) {
        cat("p值 =", round(mcar_result$p.value, 4), ">", sig_level, "，未拒绝MCAR假设\n")
        cat("证据支持数据符合完全随机缺失(MCAR)机制\n")
      } else {
        cat("p值 =", round(mcar_result$p.value, 4), "<", sig_level, "，拒绝MCAR假设\n")
        cat("证据表明数据可能符合随机缺失(MAR)或非随机缺失(MNAR)机制\n")
      }
    }, error = function(e) {
      cat("\nLittle's MCAR检验失败，可能因为数据结构或缺失模式：\n")
      cat(e$message, "\n")
    })
  }
  
  #===========================================
  # 6. 缺失机制推断与处理建议
  #===========================================
  
  cat("\n=== 缺失机制推断与处理建议 ===\n")
  
  # 基于各种分析结果推断缺失机制
  
  # a. 基于Little's MCAR测试
  if(has_naniar && exists("mcar_result", where = result)) {
    if(result$mcar_test$p.value > sig_level) {
      result$conclusion$mcar_test <- "MCAR"
      cat("- 基于Little's MCAR检验，数据可能符合MCAR机制\n")
      cat("- 推荐处理方法：简单删除或基本插补方法(均值/中位数)\n")
    } else {
      result$conclusion$mcar_test <- "MAR_or_MNAR"
      cat("- 基于Little's MCAR检验，数据可能符合MAR或MNAR机制\n")
      
      # 检查是否有MAR的证据
      mar_evidence <- FALSE
      
      # 遍历所有t检验结果，查找显著相关
      for(var_name in names(t_test_results)) {
        if(!is.null(t_test_results[[var_name]])) {
          if(any(t_test_results[[var_name]]$p_value < sig_level, na.rm = TRUE)) {
            mar_evidence <- TRUE
            break
          }
        }
      }
      
      if(mar_evidence) {
        result$conclusion$t_tests <- "MAR"
        cat("- 存在证据支持MAR机制（缺失与其他观测变量相关）\n")
        cat("- 推荐处理方法：多重插补(MI)、最大似然估计(ML)、预测平均匹配(PMM)\n")
      } else {
        result$conclusion$t_tests <- "Unclear"
        cat("- 无法明确区分MAR和MNAR，建议考虑两种可能性\n")
        cat("- 推荐处理方法：进行敏感性分析，比较不同缺失处理方法的结果\n")
        cat("- 对于可能的MNAR，考虑收集更多信息或使用选择模型\n")
      }
    }
  } else {
    # b. 基于相关性矩阵和t检验的推断
    mar_evidence <- FALSE
    
    # 遍历所有t检验结果，查找显著相关
    for(var_name in names(t_test_results)) {
      if(!is.null(t_test_results[[var_name]])) {
        if(any(t_test_results[[var_name]]$p_value < sig_level, na.rm = TRUE)) {
          mar_evidence <- TRUE
          break
        }
      }
    }
    
    if(mar_evidence || (exists("high_miss_pairs", where = result) && nrow(result$high_miss_pairs) > 0)) {
      result$conclusion$overall <- "MAR"
      cat("- 发现变量间缺失存在相关性，可能符合MAR机制\n")
      cat("- 推荐处理方法：多重插补(MI)或基于模型的方法\n")
    } else {
      result$conclusion$overall <- "MCAR"
      cat("- 未发现明显的缺失模式相关性，可能符合MCAR机制\n")
      cat("- 推荐处理方法：如缺失比例低，可使用简单删除或基本插补\n")
    }
  }
  
  # c. 针对每个变量的机制判断
  result$var_mechanism <- list()
  for(var in vars_with_missing) {
    if(var %in% names(t_test_results) && !is.null(t_test_results[[var]])) {
      if(any(t_test_results[[var]]$p_value < sig_level, na.rm = TRUE)) {
        result$var_mechanism[[var]] <- "MAR"
      } else {
        result$var_mechanism[[var]] <- "MCAR"
      }
    }
  }
  
  # 额外注意事项
  cat("\n额外注意事项：\n")
  cat("- 考虑缺失比例：高缺失率(>20%)的变量可能需要特殊处理\n")
  cat("- 考虑数据类型：分类变量和连续变量应使用不同的插补方法\n")
  cat("- 考虑理论重要性：分析目标相关变量的缺失应格外谨慎处理\n")
  
  return(result)
}

#' 缺失值简单处理函数
#'
#' 根据分析结果对数据进行缺失值处理，支持多种处理方法
#'
#' @param data 数据框，包含需要处理的数据
#' @param method 处理方法，可以是"deletion"(删除)、"mean"(均值)、"median"(中位数)、"mode"(众数)
#' @param vars 需要处理的变量，默认为NULL表示所有变量
#' @param threshold 缺失比例阈值，缺失比例超过此值的行或列会被删除，默认为0.5
#'
#' @return 处理后的数据框
#' @export
#'
#' @examples
#' # 使用完整删除
#' data_complete <- handle_missing(my_data, method = "deletion")
#' 
#' # 使用均值插补
#' data_imputed <- handle_missing(my_data, method = "mean", vars = c("x1", "x2"))
handle_missing <- function(data, method = c("deletion", "mean", "median", "mode"), 
                           vars = NULL, threshold = 0.5) {
  
  method <- match.arg(method)
  
  # 如果未指定变量，则处理所有变量
  if(is.null(vars)) {
    vars <- names(data)
  }
  
  # 确保变量存在于数据中
  vars <- vars[vars %in% names(data)]
  
  # 如果没有需要处理的变量，直接返回原始数据
  if(length(vars) == 0) {
    warning("没有找到需要处理的变量")
    return(data)
  }
  
  # 复制数据以避免修改原始数据
  result_data <- data
  
  if(method == "deletion") {
    # 完整删除
    result_data <- data[complete.cases(data[, vars]), ]
    cat("删除缺失值:\n")
    cat("- 原始行数:", nrow(data), "\n")
    cat("- 处理后行数:", nrow(result_data), "\n")
    cat("- 删除的行数:", nrow(data) - nrow(result_data), "\n")
  } else {
    # 插补
    for(var in vars) {
      if(sum(is.na(data[[var]])) > 0) {
        if(is.numeric(data[[var]])) {
          # 数值型变量
          if(method == "mean") {
            # 均值插补
            result_data[[var]][is.na(result_data[[var]])] <- mean(data[[var]], na.rm = TRUE)
            cat("变量", var, "使用均值插补 (", 
                round(mean(data[[var]], na.rm = TRUE), 2), ")\n")
          } else if(method == "median") {
            # 中位数插补
            result_data[[var]][is.na(result_data[[var]])] <- median(data[[var]], na.rm = TRUE)
            cat("变量", var, "使用中位数插补 (", 
                round(median(data[[var]], na.rm = TRUE), 2), ")\n")
          }
        } else if(is.factor(data[[var]]) || is.character(data[[var]])) {
          # 分类变量
          if(method == "mode") {
            # 众数插补
            tab <- table(data[[var]], useNA = "no")
            mode_val <- names(tab)[which.max(tab)]
            result_data[[var]][is.na(result_data[[var]])] <- mode_val
            cat("变量", var, "使用众数插补 (", mode_val, ")\n")
          }
        }
      }
    }
  }
  
  return(result_data)
}

# 将此文件保存为R脚本，例如"missing_mechanism_analysis.R"
# 然后可以使用source("missing_mechanism_analysis.R")加载函数

# 使用示例
if(FALSE) {  # 这部分代码不会被执行，仅作为示例
  # 读取数据
  data <- read.csv("your_data.csv")
  
  # 分析缺失机制
  missing_analysis <- analyze_missing_mechanism(data)
  
  # 根据分析结果处理缺失值
  # 例如，对于符合MCAR的变量使用删除法
  mcar_vars <- names(missing_analysis$var_mechanism)[
    sapply(missing_analysis$var_mechanism, function(x) x == "MCAR")]
  
  data_clean <- handle_missing(data, method = "deletion", vars = mcar_vars)
  
  # 对于符合MAR的变量，可能需要使用更复杂的方法（如多重插补）
  # 这需要其他包如mice
}