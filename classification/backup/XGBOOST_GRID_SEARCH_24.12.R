
max_depths = seq(5,9,1)
etas = seq(0.05,0.25,0.1)
gammas = c(0.01,0.05,0.1)
min_child_weights = c(1,5,25)
max_delta_steps = c(1,5,25)

max_auc = 0

for(max_depth in max_depths){
  for(eta in etas){
    for(gamma in gammas){
      for(min_child_weight in min_child_weights){
        for(max_delta_step in max_delta_steps){
          
          print(paste(max_depth,eta,gamma,min_child_weight,max_delta_step))
          
          params = list(
            max_depth = max_depth, eta = eta, gamma = gamma, min_child_weight = min_child_weight,
            max_delta_step = max_delta_step, objective = "binary:logistic", eval_metric = "auc"
          )
          
          set.seed(372532)
          model = xgb.cv(params, train_xgb, nround = 300, nfold = 5, early_stopping_rounds=5)
          
          mean_auc = as.numeric(model$evaluation_log[model$niter-5,4])
          print(mean_auc)
          
          if(mean_auc > max_auc){
            max_auc = mean_auc
            
            best_params = list(
              max_depth = max_depth, eta = eta, gamma = gamma, min_child_weight = min_child_weight,
              max_delta_step = max_delta_step, objective = "binary:logistic", eval_metric = "auc"
            )
            
            best_iter = model$niter
          }
        }
      }
    }
  }
}

best_iter
max_auc
best_params

