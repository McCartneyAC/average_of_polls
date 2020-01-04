


is_extant <-function(x) any(!is.na(x))
is_numeric<-function(x) any(is.numeric(x))

# Data import
use <- function(name) {
  csv <- ".csv"
  xlsx <- ".xlsx"
  dta <- ".dta"
  sav <- ".sav"
  if (grepl(csv, name)) {
    readr::read_csv(name)
  } else if (grepl(xlsx, name)) {
    readxl::read_xlsx(name)
  } else if (grepl(dta, name)) {
    haven::read_dta(name)
  } else if (grepl(sav, name)) {
    haven::read_spss(name)
  } else {
    stop("unknown data type.")
  }
}

gg_added_var <- function(partial, extended, se = TRUE) {
  # In a multiple regression, the added variable plot for a predictor X, say,
  # is the plot showing the residual of Y against all predictors except X against the
  # residual of X on all predictors except X, of course.
  # Adapted from Steven Pollack
  # https://github.com/stevenpollack/stat151a/blob/master/From_Lab/Feb-26-2014.R
  require(ggplot2)
  partial_residuals <- resid(partial)
  full_residuals <- resid(extended)
  if (se) {
    avPlot <- ggplot(
      data = data.frame(x = partial_residuals, y = full_residuals),
      aes(x = partial_residuals, y = full_residuals)
    ) +
      geom_point() +
      stat_smooth(method = "lm")
  } else {
    avPlot <- ggplot(
      data = data.frame(x = partial_residuals, y = full_residuals),
      aes(x = partial_residuals, y = full_residuals)
    ) +
      geom_point() +
      stat_smooth(method = "lm", se = FALSE)
  }
  return(avPlot)
}


sjp_corr <- function (data, title = NULL, axis.labels = NULL, sort.corr = TRUE, 
                      decimals = 3, na.deletion = c("listwise", "pairwise"), 
                      corr.method = c("pearson", "spearman", "kendall"), 
                      geom.colors = "RdBu", wrap.title = 50, wrap.labels = 20, 
                      show.legend = FALSE, legend.title = NULL, show.values = TRUE, 
                      show.p = TRUE, p.numeric = FALSE) 
{
  opt <- getOption("p_zero")
  if (is.null(opt) || opt == FALSE) {
    p_zero <- ""
  }
  else {
    p_zero <- "0"
  }
  na.deletion <- match.arg(na.deletion)
  corr.method <- match.arg(corr.method)
  if (is.null(axis.labels) && is.data.frame(data)) {
    axis.labels <- unname(sjlabelled::get_label(data, def.value = colnames(data)))
  }
  if (is.brewer.pal(geom.colors[1])) {
    geom.colors <- (scales::brewer_pal(palette = geom.colors[1]))(5)
  }
  else if (geom.colors[1] == "gs") {
    geom.colors <- (scales::grey_pal())(5)
  }
  if (any(class(data) == "matrix")) {
    corr <- data
    cpvalues <- NULL
  }
  else {
    if (na.deletion == "listwise") {
      data <- stats::na.omit(data)
      corr <- stats::cor(data, method = corr.method)
    }
    else {
      corr <- stats::cor(data, method = corr.method, use = "pairwise.complete.obs")
    }
    computePValues <- function(df) {
      cp <- c()
      for (i in seq_len(ncol(df))) {
        pv <- c()
        for (j in seq_len(ncol(df))) {
          test <- suppressWarnings(stats::cor.test(df[[i]], 
                                                   df[[j]], alternative = "two.sided", 
                                                   method = corr.method))
          pv <- c(pv, round(test$p.value, 4))
        }
        cp <- rbind(cp, pv)
      }
      return(cp)
    }
    cpvalues <- computePValues(data)
  }
  if (is.null(axis.labels)) 
    axis.labels <- row.names(corr)
  if (!is.null(title)) 
    title <- sjmisc::word_wrap(title, wrap.title)
  if (!is.null(axis.labels)) 
    axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)
  if (sort.corr) {
    neword <- order(corr[1, ])
    orderedCorr <- corr[neword, neword]
    axis.labels <- axis.labels[neword]
    if (!is.null(cpvalues)) 
      cpvalues <- cpvalues[neword, neword]
  }
  else {
    cl <- ncol(corr)
    orderedCorr <- corr[cl:1, cl:1]
    axis.labels <- rev(axis.labels)
    if (!is.null(cpvalues)) 
      cpvalues <- cpvalues[cl:1, cl:1]
  }
  yo <- c()
  for (i in seq_len(nrow(corr))) {
    yo <- c(yo, rep(i, nrow(corr)))
  }
  orderedCorr <- tidyr::gather(data.frame(orderedCorr), "var", 
                               "value", !!seq_len(ncol(orderedCorr)), factor_key = TRUE)
  if (!is.null(cpvalues)) 
    cpvalues <- tidyr::gather(data.frame(cpvalues), "var", 
                              "value", !!seq_len(ncol(cpvalues)), factor_key = TRUE)
  orderedCorr <- cbind(orderedCorr, ordx = seq_len(nrow(corr)), 
                       ordy = yo)
  if (!is.null(cpvalues)) {
    if (!p.numeric) {
      cpv <- sapply(cpvalues$value, get_p_stars)
    }
    else {
      cpv <- sapply(cpvalues$value, function(x) {
        if (x < 0.001) 
          x <- sprintf("\n(< %s.001)", p_zero)
        else x <- sub("0", p_zero, sprintf("\n(%.*f)", 
                                           decimals, x))
      })
    }
  }
  else {
    cpv <- ""
  }
  orderedCorr$ps <- cpv
  if (!show.values) {
    orderedCorr$val.labels <- ""
  }
  else {
    if (show.p) {
      orderedCorr$val.labels <- sprintf("%.*f%s", 
                                        decimals, orderedCorr$value, orderedCorr$ps)
    }
    else {
      orderedCorr$val.labels <- sprintf("%.*f", decimals, 
                                        orderedCorr$value)
    }
  }
  orderedCorr$val.labels[orderedCorr$ordx >= orderedCorr$ordy] <- NA
  orderedCorr$ordx <- as.factor(orderedCorr$ordx)
  orderedCorr$ordy <- as.factor(orderedCorr$ordy)
  message(sprintf("Computing correlation using %s-method with %s-deletion...", 
                  corr.method, na.deletion))
  corrPlot <- ggplot(orderedCorr, 
                     aes_string(x = "ordx", 
                                y = "ordy", 
                                fill = "value", 
                                label = "val.labels")) + 
    geom_tile(size = 0, colour = "black") + 
    scale_x_discrete(labels = axis.labels, 
                     breaks = seq_len(length(axis.labels))) +
    scale_y_discrete(labels = axis.labels, 
                     breaks = seq_len(length(axis.labels))) + 
    scale_fill_gradientn(colours = geom.colors, 
                         limits = c(-1, 1)) + 
    geom_text(size = 4, colour = "black", family = "xkcd") + 
    labs(title = title, x = NULL, y = NULL)
  
  if (show.legend) 
    corrPlot <- corrPlot + guides(fill = legend.title)
  else corrPlot <- corrPlot + guides(fill = "none")
  corrPlot
}

is.brewer.pal <- function(pal) {
  bp.seq <- c("BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd", "RdPu",
              "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "Blues", "Greens", "Greys",
              "Oranges", "Purples", "Reds")
  bp.div <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
              "RdYlGn", "Spectral")
  bp.qul <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
              "Set2", "Set3")
  bp <- c(bp.seq, bp.div, bp.qul)
  pal %in% bp
}
get_p_stars <- function(pval, thresholds = NULL) {
  
  if (is.null(thresholds)) thresholds <- c(.05, .01, .001)
  
  dplyr::case_when(
    is.na(pval) ~ "",
    pval < thresholds[3] ~ "***",
    pval < thresholds[2] ~ "**",
    pval < thresholds[1] ~ "*",
    TRUE ~ ""
  )
}