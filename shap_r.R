# Função SHAP -------------------------------------------------------------

shap <- function(model,data,var_start=1,var_n=10,
                 list_var=NULL,subsample=0.01,plot=F,special_na=c()) {
  
  # require("SHAPforxgboost")
  
  data <- as.data.frame(data)
  
  for (i in special_na) {
    data <- data %>% mutate_all(~na_if(.,i))
  }
  
  data <- as.matrix(data)
  
  shap_values <- shap.values(xgb_model = model, 
                             X_train = data)
  
  shap <- shap_values$mean_shap_score
  
  var_n <- min(var_n,length(shap))
  
  if (is.null(list_var)) {
    list_var <- names(shap)[var_start:var_n]
  }
  
  if (nrow(data)<1e+03) {
    subsample=1
  }
  
  std1 <- function(x) {
    return((x - min(x, na.rm = T))/(max(x, na.rm = T) - 
                                      min(x, na.rm = T)))
  }
  
  shap_score_sub <- setDT(shap_values$shap_score)[,names(shap_values$mean_shap_score)[names(shap_values$mean_shap_score)%in%list_var], 
                                                  with = F]
  shap_score_long <- melt.data.table(shap_score_sub, measure.vars = colnames(shap_score_sub))
  fv_sub <- as.data.table(data)[,names(shap_values$mean_shap_score)[names(shap_values$mean_shap_score)%in%list_var], 
                                with = F]
  fv_sub_long <- melt.data.table(fv_sub, measure.vars = colnames(fv_sub))
  fv_sub_long[, `:=`(stdfvalue, std1(value)), by = "variable"]
  names(fv_sub_long) <- c("variable", "rfvalue", "stdfvalue")
  shap_long2 <- cbind(shap_score_long, fv_sub_long[, c("rfvalue", 
                                                       "stdfvalue")])
  shap_long2[, `:=`(mean_value, mean(abs(value))), by = variable]
  setkey(shap_long2, variable)
  
  var_importance = data.frame(var=names(shap_values$mean_shap_score), 
                              importance=shap_values$mean_shap_score,
                              row.names = NULL)
  
  var_importance = var_importance[1:var_n,]
  
  g1 <- ggplot(var_importance, aes(x=reorder(var,importance), y=importance)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    theme_light() + 
    theme(axis.title.y=element_blank()) 
  
  if (!plot) {
    return(list(shap_values=shap,
                importance_plot=g1))
    break()
  }
  
  g2 <- shap.plot.summary(shap_long2, dilute = (1/subsample)) 
  
  # Gráfico 2
  dev.off()
  win.metafile()
  dev.control('enable')
  par(bg = 'white')
  xgb.plot.shap(data = data,
                model = model, 
                features = names(shap_values$mean_shap_score)[names(shap_values$mean_shap_score)%in%list_var],
                n_col = 2,
                plot_loess = T,
                subsample = subsample
  )
  
  g3 <- recordPlot()
  dev.off()
  
  return(list(shap_values=shap,
              importance_plot=g1,
              g1=g2,
              g2=g3))
}


shap_3 <- function(model,data,var_start=1,var_n=10,
                   list_var=NULL,subsample=0.01,plot=F,special_na=c(),
                   shap_values=NULL) {
  
  # require("SHAPforxgboost")
  if(length(special_na)>0) {
    data <- as.data.frame(data)
    
    for (i in special_na) {
      data <- data %>% mutate_all(~na_if(.,i))
    }
  }
  
  data <- as.matrix(data)
  
  if(is.null(shap_values)) {
    
    shap_values <- shap.values(xgb_model = model, 
                               X_train = data)
  }
  
  shap <- shap_values$mean_shap_score
  
  var_n <- min(var_n,length(shap))
  
  if (is.null(list_var)) {
    list_var <- names(shap)[var_start:var_n]
  }
  
  if (nrow(data)<1e+03) {
    subsample=1
  }
  
  shap_long2 <- shap.prep(shap_contrib = shap_values$shap_score %>% 
                            select(one_of(list_var)), 
                          X_train = data[,list_var,drop=FALSE])
  
  var_importance = data.frame(var=names(shap[list_var]), 
                              importance=shap[list_var],
                              row.names = NULL)
  
  var_importance = var_importance[order(-var_importance$importance),]
  var_importance = var_importance[var_start:var_n,]
  list_var = list_var[list_var%in%c(as.character(var_importance$var))]
  
  g1 <- ggplot(var_importance, aes(x=var, y=importance)) + 
    geom_bar(stat = "identity") + 
    coord_flip() +
    theme_light() + 
    theme(axis.title.y=element_blank()) 
  
  if (!plot) {
    return(list(shap_values=shap,
                importance_plot=g1))
    break()
  }
  
  g2 <- shap.plot.summary(shap_long2, dilute = (1/subsample)) 
  
  #Gráfico 2
  
  p <- list()
  for (i in 1:length(list_var)) {
    p[[i]] <- shap.plot.dependence(data_long = shap_long2,
                                   x=list_var[i],size0=0.2) + ylab('SHAP')
  }
  
  if (!is.null(dev.list())) { 
    dev.off()
  }
  
  win.metafile()
  dev.control('enable')
  par(bg = 'white')
  g3 <- do.call(grid.arrange,p)
  g3 <- recordPlot()
  dev.off()
  
  return(list(shap_values=shap,
              importance_plot=g1,
              g1=g2,
              g2=g3))
}



#Exemplo de uso
data("iris")
X1 = as.matrix(iris[,-5])
mod1 = xgboost::xgboost(
  data = X1, label = iris$Species, gamma = 0, eta = 1,
  lambda = 0,nrounds = 1, verbose = FALSE)

shap_ = shap(mod1,X1
             ,list_var = c('Sepal.Width','Sepal.Length'),plot=T
)

shap_$g2


# Função iml --------------------------------------------------------------

shap_local <- function(model,data,target,x.interest,top_n=20,
                       shapley=NULL,list_var=NULL) {
  
  if (!is.null(shapley)) {
    shapley$explain(x.interest = x.interest)
    
    tb = shapley$results %>% arrange(-abs(phi))
    if (!is.null(list_var)) {
      tb = tb %>% filter(feature%in%c(list_var))
    }
    tb = head(tb,top_n)
    
    g = ggplot(tb,aes(x=reorder(feature.value,phi),y=phi,
                      fill=phi<0)) + 
      geom_bar(stat='identity') + coord_flip() +
      theme(legend.position = 'none') + xlab(NULL) + 
      ggtitle(paste0('Actual prediction: ', 
                     round(shapley$y.hat.interest,2),
                     '\n Average prediction:',
                     round(shapley$y.hat.average,2))) +
      theme_minimal() +
      guides(fill = FALSE)
    
    
    return(list(tab=shapley$results,
                plot=g,
                shapley=shapley))
    
    break()
  }
  
  nas <- names(which(colMeans(is.na(data))==1))
  
  if (length(nas)>0) {
    data = data %>% mutate_at(.vars = nas,
                              .funs = list(~ ifelse(is.na(.),-9999,.)))
  } 
  
  predictor = Predictor$new(model, data = data, y = target, 
                            predict.fun = function(model, newdata) {
                              newData_x = xgb.DMatrix(data.matrix(newdata), missing = NA)
                              results<-predict(model, newData_x)
                              return(results)
                            })
  
  shapley = Shapley$new(predictor, x.interest = x.interest 
                        #, sample.size = 100
  )
  tb = shapley$results %>% arrange(-abs(phi))
  if (!is.null(list_var)) {
    tb = tb %>% filter(feature%in%c(list_var))
  }
  tb = head(tb,top_n)
  
  g = ggplot(tb,aes(x=reorder(feature.value,phi),y=phi,
                    fill=phi<0)) + 
    geom_bar(stat='identity') + coord_flip() +
    theme(legend.position = 'none') + xlab(NULL) + 
    ggtitle(paste0('Actual prediction: ', 
                   round(shapley$y.hat.interest,2),
                   '\n Average prediction:',
                   round(shapley$y.hat.average,2))) +
    theme_minimal() +
    guides(fill = FALSE)
  
  
  return(list(tab=shapley$results,
              plot=g,
              shapley=shapley))
  
}

#Exemplo de uso
# Pior e melhor score 1 a 3 meses

dados_ = dados
dados_$pred <- pred

setDT(dados_,keep.rownames = TRUE)

dados_ %>% 
  arrange(pred) %>% head(1) %>% select(pred,rn) # 179075 e 99742

local2=shap_local(shapley = local$shapley,
                  x.interest = X[179075,])



# Outras funções ----------------------------------------------------------

AUC <- function(pred,depvar){
  # require("ROCR")
  p   <- prediction(as.numeric(pred),depvar)
  auc <- performance(p,"auc")
  auc <- unlist(slot(auc, "y.values"))
  return(auc)
}

ROC_auc <- function(pred, actual, pred2=NULL, actual2=NULL,
                    titulo_1=NULL,titulo_2=NULL,
                    color_1='red',color_2='blue',legend.cex=0.8) {
  # require("ROCR")
  metrics <- ROCR::prediction(as.numeric(pred), actual)
  
  perf <- performance(metrics, measure="tpr",
                      x.measure="fpr")
  
  auc <- performance(metrics, measure="auc")
  
  if (!is.null(pred2) & !is.null(actual2)) {
    
    metrics2 <- ROCR::prediction(as.numeric(pred2), actual2)
    
    perf2 <- performance(metrics2, measure="tpr",
                         x.measure="fpr")
    
    au2c <- performance(metrics2, measure="auc")
    
    p = plot(perf,col=color_1) + abline(a=0,b=1)
    p = par(new=T)
    p = plot(perf2,col=color_2)
    if (!is.null(titulo_1) & !is.null(titulo_2)) {
      p = legend("bottomright", inset=.02,legend=c(paste0(titulo_1,': ',round(auc@y.values[[1]],3)*100,'%'),
                                                   paste0(titulo_2,': ',round(au2c@y.values[[1]],3)*100,'%')),
                 col=c(color_1, color_2),lty=1:1,cex=legend.cex,pt.cex = 1)
      
      # p = title(main=paste0(titulo_1,': ',round(auc@y.values[[1]],3)*100,'% \n',
      #                       titulo_2,': ',round(au2c@y.values[[1]],3)*100,'%'))
    }
    
    return(
      p
    )
    
    
  }
  
  return(plot(perf, main = paste0("AUC: ", round(auc@y.values[[1]],3)*100,'%')) + abline(a=0,b=1))
  
}


format_data <- function(data,id_resp=c(),remove=c()) {
  
  data <- data %>% mutate_if(is.integer, as.numeric)
  info_numeric <- data %>%
    select(-one_of(id_resp,remove)) %>%
    select_if(is.numeric)
  
  fea_names <- names(info_numeric)
  
  mat <- data.matrix(info_numeric)
  
  removed <- setdiff(names(data),fea_names)
  
  return(list(mat=mat, names=fea_names, removed=removed))
  
}

modelo <- function(train,test,resposta,params=params = list(booster="gbtree",objective="binary:logistic"),
                   id_resp=c(),remove=c(),seed=234,nrounds=100) {
  
  dtrain <- train
  dtest <- test
  
  # Labels
  dtrain <- dtrain %>% filter(!is.na(!!as.symbol(resposta)))
  dtest <- dtest %>% filter(!is.na(!!as.symbol(resposta)))
  ltrain <- dtrain %>% select(a=one_of(resposta)) %>%
    pull(a)
  ltest <- dtest %>% select(a=one_of(resposta)) %>%
    pull(a)
  
  t = format_data(dtrain,id_resp,remove)
  
  dtrain <- xgb.DMatrix(data = t$mat,
                        label=data.matrix(ltrain))
  
  dtest <- xgb.DMatrix(data = format_data(dtest,id_resp,remove)$mat,
                       label=data.matrix(ltest))
  
  watchlist <- list(train=dtrain, test=dtest)
  
  set.seed(seed)
  xgbtrainModel <- xgb.train(data=dtrain,
                             params=params,
                             watchlist=watchlist,
                             nrounds=nrounds,
                             early_stopping_rounds=100)
  
  xgbPred_test = predict(xgbtrainModel,dtest)
  xgbPred_train = predict(xgbtrainModel,dtrain)
  
  KS_test = KS(xgbPred_test,ltest)
  AUC_test = AUC(xgbPred_test,ltest)
  
  KS_train = KS(xgbPred_train,ltrain)
  AUC_train = AUC(xgbPred_train,ltrain)
  
  return(list(model=xgbtrainModel,
              pred=xgbPred_test,
              real=ltest,
              X_mat=t$mat,
              dtrain=dtrain,
              dtest=dtest,
              names=t$names,
              ks=list(train=KS_train, test=KS_test),
              auc=list(train=AUC_train,test=AUC_test)))
  
}

aa=modelo(train=head(dados,30),
          test=tail(dados,30),
          resposta = "m6ever60",
          id_resp = c('if_nr_cliente','ano_mes','m12ever90'),
          remove=c('m6over60'))

