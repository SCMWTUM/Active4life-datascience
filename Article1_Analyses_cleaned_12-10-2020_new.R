library(foreign)
library(haven) 
library(Rgraphviz)
library(bnlearn)
library(caTools)
library(Hmisc)
library(tibble)
library(plyr)
library(gRain) 
library(RBGL)
library("mice")
set.seed(5) 

# Read complete preprocessed dataset including concept variables 
DataMaartje_C = read.csv("..:/...../....../...../....csv") # fill in path to dataset csv file

# function to include edge from-to into blacklist matrix bl 
createbl = function(bl, from, to)
{
  if(missing(bl)==1)
  {
    bl = expand.grid(from, to) 
  } 
  else
  {
    temporary = expand.grid(from,to) 
    names(temporary) = names(bl) 
    bl = rbind(bl, temporary)
  }  
  return(bl)
}

# discretisize specific variable in dataframe according to range (option) manually creating categories meaningful in field, and labelling missing value category in case this method is applied    
discretebn = function(dataframe, variable, option, missingcategory)
{
  if(option == 1) 
  {
    dataframe[[variable]] = as.factor(dataframe[[variable]])
    if(missingcategory == 1)
    {
      dataframe[[variable]] = mapvalues(dataframe[[variable]], from = c("-1"), to = c("missing")) 
    }
  }
  if(option == 2) 
  {
    if(missingcategory == 1) 
      {
        dataframe[[variable]] = cut(dataframe[[variable]], c(-Inf,0,2,3,4,Inf), right = FALSE) 
        levels(dataframe[[variable]])[1] = "missing" 
      }
    if(missingcategory == 0)
    {
      dataframe[[variable]] = cut(dataframe[[variable]], c(-Inf,2,3,4,Inf), right = FALSE)
    }
  }
  if(option == 3) 
  {
    if(missingcategory == 1)
    {
      dataframe[[variable]] = cut(dataframe[[variable]], c(-Inf,0,4,7,Inf), right = FALSE)
      levels(dataframe[[variable]])[1] = "missing"
    }
    if(missingcategory == 0)
    {
      dataframe[[variable]] = cut(dataframe[[variable]], c(-Inf,4,7,Inf), right = FALSE)
    }
  }
  if(option == 4) 
  {
    if(missingcategory == 1)
    {
      dataframe[[variable]] = cut(dataframe[[variable]], c(-Inf,0,210,420,Inf), right = FALSE) 
      levels(dataframe[[variable]])[1] = "missing"
    }
    if(missingcategory == 0)
    {
      dataframe[[variable]] = cut(dataframe[[variable]], c(-Inf,210,420,Inf), right = FALSE)
    }
  }
  if(option == 5) 
  {
    if(missingcategory == 1)
    {
      dataframe[[variable]] = cut(dataframe[[variable]], c(-Inf,0,2,3,Inf), right = FALSE)
      levels(dataframe[[variable]])[1] = "missing"
    }
    if(missingcategory == 0)
    {
      dataframe[[variable]] = cut(dataframe[[variable]], c(-Inf,2,3,Inf), right = FALSE)
    }
  }
  return(dataframe)
}

# Selection of concept variables to be included in analyses 
DataPhase2 = as.data.frame(DataMaartje_C[c("C_conditionEnvironment_T0", "C_conditionIntervention_T0", "C_stage_T0", "C_stage_T1", "C_stage_T2", "C_SQUASH_moderateintensive_T0", "C_SQUASH_moderateintensive_T1", "C_SQUASH_moderateintensive_T2", "C_SQUASH_moderateintensive_T3", "C_reference",   "C_selfefficacy_T0", "C_selfefficacy_T1", "C_attitudepros_T0", "C_attitudepros_T1", "C_attitudecons_T0", "C_attitudecons_T1", "C_intrinsicmotivation_T0", "C_intrinsicmotivation_T1", "C_intention_T0", "C_intention_T1", "C_intention_T2", "C_intention_T3", "C_commitment_T0", "C_commitment_T1", "C_commitment_T2", "C_strategicplanning_T0", "C_strategicplanning_T1", "C_strategicplanning_T2", "C_strategicplanning_T3", "C_actionplanning_T0", "C_actionplanning_T1", "C_actionplanning_T2", "C_copingplanning_T0", "C_copingplanning_T1", "C_copingplanning_T2", "C_habit_T0", "C_habit_T2", "C_habit_T3", "C_socialmodelling_T0", "C_socialmodelling_T1", "C_socialmodelling_T2", "C_socialsupport_T0", "C_socialsupport_T1")])
 
# Subsets of included variables according to time slots, stage excluded 
varT0_P2_EXCLSTAGE = c("C_conditionEnvironment_T0", "C_conditionIntervention_T0", "C_SQUASH_moderateintensive_T0",  "C_selfefficacy_T0", "C_attitudepros_T0", "C_attitudecons_T0", "C_intrinsicmotivation_T0", "C_intention_T0", "C_commitment_T0", "C_strategicplanning_T0", "C_actionplanning_T0", "C_copingplanning_T0", "C_habit_T0", "C_socialmodelling_T0", "C_socialsupport_T0") 
varT1_P2_EXCLSTAGE = c( "C_SQUASH_moderateintensive_T1",  "C_selfefficacy_T1", "C_attitudepros_T1", "C_attitudecons_T1", "C_intrinsicmotivation_T1", "C_intention_T1",  "C_strategicplanning_T1", "C_actionplanning_T1", "C_copingplanning_T1", "C_socialmodelling_T1", "C_socialsupport_T1", "C_commitment_T1")   
varT2_P2_EXCLSTAGE = c( "C_SQUASH_moderateintensive_T2", "C_intention_T2", "C_commitment_T2", "C_strategicplanning_T2", "C_actionplanning_T2", "C_copingplanning_T2", "C_habit_T2", "C_socialmodelling_T2") 
varT3_P2_EXCLSTAGE = c("C_SQUASH_moderateintensive_T3",   "C_intention_T3", "C_strategicplanning_T3", "C_habit_T3") 

# Eventually (depending on method to handle missing data values) impute missing data/NA values and discretiseze entire dataset /  selection of variables 
preprocessinganddiscretisationgeneral = function(dataset, method) 
{
  if(method == 1)
  {
    for(i in 1:ncol(dataset))
    {
      dataset[,i][is.na(dataset[,i]) == 1] = -1 # assign missing values to separate category
    }
  }
  if(method == 3) 
  {
    for(i in 1:ncol(dataset))
    {
      dataset[,i][is.na(dataset[,i]) == 1] = mean(dataset[,i],na.rm = TRUE) # mean imputation
    }
  }
  if(method == 4)
  {
    missing = 0 # in case of multiple imputation no separate missing data category
  }
  if(method == 1){missing = 1}
  if(method == 2){missing = 0} 
  if(method == 3){missing = 0} 
  dataset = discretebn(dataset, "C_conditionEnvironment_T0", 1, missing)
  dataset = discretebn(dataset, "C_conditionIntervention_T0", 1, missing)
  #dataset = discretebn(dataset, "C_stage_T0", 1, missing) #stage excluded
  #dataset = discretebn(dataset, "C_stage_T1", 1, missing) #stage excluded
  #dataset = discretebn(dataset, "C_stage_T2", 1, missing) #stage excluded
  dataset = discretebn(dataset, "C_SQUASH_moderateintensive_T0", 4, missing)
  dataset = discretebn(dataset, "C_SQUASH_moderateintensive_T1", 4, missing)
  dataset = discretebn(dataset, "C_SQUASH_moderateintensive_T2", 4, missing)
  dataset = discretebn(dataset, "C_SQUASH_moderateintensive_T3", 4, missing)
  dataset = discretebn(dataset, "C_selfefficacy_T0", 2, missing)
  dataset = discretebn(dataset, "C_selfefficacy_T1", 2, missing)
  dataset = discretebn(dataset, "C_attitudepros_T0", 2, missing)
  dataset = discretebn(dataset, "C_attitudepros_T1", 2, missing)
  dataset = discretebn(dataset, "C_attitudecons_T0", 2, missing)
  dataset = discretebn(dataset, "C_attitudecons_T1", 2, missing)
  dataset = discretebn(dataset, "C_intrinsicmotivation_T0", 2, missing)
  dataset = discretebn(dataset, "C_intrinsicmotivation_T1", 2, missing)
  dataset = discretebn(dataset, "C_intention_T0", 3, missing)
  dataset = discretebn(dataset, "C_intention_T1", 3, missing)
  dataset = discretebn(dataset, "C_intention_T2", 3, missing)
  dataset = discretebn(dataset, "C_intention_T3", 3, missing)
  dataset = discretebn(dataset, "C_commitment_T0", 2, missing)
  dataset = discretebn(dataset, "C_commitment_T1", 2, missing)
  dataset = discretebn(dataset, "C_commitment_T2", 2, missing)
  dataset = discretebn(dataset, "C_strategicplanning_T0", 2, missing)
  dataset = discretebn(dataset, "C_strategicplanning_T1", 2, missing)
  dataset = discretebn(dataset, "C_strategicplanning_T2", 2, missing)
  dataset = discretebn(dataset, "C_strategicplanning_T3", 2, missing)
  dataset = discretebn(dataset, "C_actionplanning_T0", 2, missing)
  dataset = discretebn(dataset, "C_actionplanning_T1", 2, missing)
  dataset = discretebn(dataset, "C_actionplanning_T2", 2, missing)
  dataset = discretebn(dataset, "C_copingplanning_T0", 2, missing)
  dataset = discretebn(dataset, "C_copingplanning_T1", 2, missing)
  dataset = discretebn(dataset, "C_copingplanning_T2", 2, missing)
  dataset = discretebn(dataset, "C_habit_T0", 2, missing)
  dataset = discretebn(dataset, "C_habit_T2", 2, missing)
  dataset = discretebn(dataset, "C_habit_T3", 2, missing)
  dataset = discretebn(dataset, "C_socialmodelling_T0", 2, missing)
  dataset = discretebn(dataset, "C_socialmodelling_T1", 2, missing)
  dataset = discretebn(dataset, "C_socialmodelling_T2", 2, missing)
  dataset = discretebn(dataset, "C_socialsupport_T0", 5, missing)
  dataset = discretebn(dataset, "C_socialsupport_T1", 5, missing)
  return(dataset)
} 

# For continuous case, no discretisation, only imputing missing data eventually (case of missing value category not included, since not applied)  
preprocessingcontinue = function(dataset, method)
{
  if(method == 3) 
  {
    for(i in 1:ncol(dataset))
    {
      dataset[,i][is.na(dataset[,i]) == 1] = mean(dataset[,i],na.rm = TRUE)
    }
  }
  dataset[1:ncol(dataset)] <- lapply(dataset[1:ncol(dataset)], as.numeric) 
  return(dataset)
}

# Create blacklist for time dimension (stage excluded)
blacklistPhase2_exclstage = data.frame(matrix(ncol = 2, nrow = 0)) 
names(blacklistPhase2_exclstage) = c("from", "to") 
blacklistPhase2_exclstage = createbl(blacklistPhase2_exclstage, varT1_P2_EXCLSTAGE, varT0_P2_EXCLSTAGE)
blacklistPhase2_exclstage = createbl(blacklistPhase2_exclstage, varT2_P2_EXCLSTAGE, varT0_P2_EXCLSTAGE) 
blacklistPhase2_exclstage = createbl(blacklistPhase2_exclstage, varT2_P2_EXCLSTAGE, varT1_P2_EXCLSTAGE)
blacklistPhase2_exclstage = createbl(blacklistPhase2_exclstage, varT3_P2_EXCLSTAGE, varT0_P2_EXCLSTAGE)
blacklistPhase2_exclstage = createbl(blacklistPhase2_exclstage, varT3_P2_EXCLSTAGE, varT1_P2_EXCLSTAGE)
blacklistPhase2_exclstage = createbl(blacklistPhase2_exclstage, varT3_P2_EXCLSTAGE, varT2_P2_EXCLSTAGE)

# criterion ; 1-BIC, 2-BDE, 3-AIC
# method ; 1-missing category, 2-structural EM, 3-mean imputation, 4-multiple imputation
# algorithm ; 1-HC, 2-TABU

# Learn Bayesian network structure for given data, blacklist, network score, method to handle missing data and structure learning algorithm, not changed for method == 4 because this was outperformed by SEM
bnstructurealternatives = function(dataset, blacklistdataset, criterion, method, algorithm) 
{
  if(criterion == 1 & (method == 1|method == 3) & algorithm == 1) 
  {
    return(hc(dataset,  blacklist = blacklistdataset))
  }
  if(criterion == 2 & (method == 1|method == 3) & algorithm == 1) 
  {
    return(hc(dataset,  blacklist = blacklistdataset, score = "bde")) 
  }
  if(criterion == 1 & method == 2 & algorithm == 1)  
  {
    return(structural.em(dataset, maximize = "hc", maximize.args = list(blacklist = blacklistdataset), return.all = TRUE)) 
  }
  if(criterion == 2 & method == 2 & algorithm == 1) 
  {
    return(structural.em(dataset, maximize = "hc", maximize.args = list(blacklist = blacklistdataset, score = "bde"), return.all = TRUE))
  }
  if(criterion == 1 & (method == 1|method == 3) & algorithm == 2) 
  {
    return(tabu(dataset,  blacklist = blacklistdataset))
  }
  if(criterion == 2 & (method == 1|method == 3) & algorithm == 2) 
  {
    return(tabu(dataset,  blacklist = blacklistdataset, score = "bde")) 
  }
  if(criterion == 1 & method == 2 & algorithm == 2)  
  {
    return(structural.em(dataset, maximize = "tabu", maximize.args = list(blacklist = blacklistdataset), return.all = TRUE))
  }
  if(criterion == 2 & method == 2 & algorithm == 2) 
  {
    return(structural.em(dataset, maximize = "tabu", maximize.args = list(blacklist = blacklistdataset, score = "bde"), return.all = TRUE))
  }
  
  if(criterion == 3 & (method == 1|method == 3) & algorithm == 1)
  {
    return(hc(dataset,  blacklist = blacklistdataset, score = "aic")) 
  }
  if(criterion == 3 & method == 2 & algorithm == 1)
  {
    return(structural.em(dataset, maximize = "hc", maximize.args = list(blacklist = blacklistdataset, score = "aic"), return.all = TRUE)) 
  }
  if(criterion == 3 & (method == 1|method == 3) & algorithm == 2)
  {
    return(tabu(dataset,  blacklist = blacklistdataset, score = "aic")) 
  }
  if(criterion == 3 & method == 2 & algorithm == 2)
  {
    return(structural.em(dataset, maximize = "tabu", maximize.args = list(blacklist = blacklistdataset, score = "aic"), return.all = TRUE))
  }
}

# 1 run 10-fold cross validation
bn.cv.handlemissings = function(dataset, blacklistdataset, criterion, method, algorithm)
{ 
  datasetprepr = preprocessinganddiscretisationgeneral(dataset, method)
  folds = split(datasetprepr, sample(1:nrow(datasetprepr), 10, replace = FALSE)) 
  loglikelihoodfolds = as.data.frame(matrix(ncol = 1, nrow = 10)) #initialisation of loglikelihood for testing on each of the 10 folds
  for(i in 1:10) 
  {
    rm(trainingsset)
    rm(testset)
    # definition of trainings and test sets
    for(j in 1:10)
    {
      if(i == j)
      {
        testset = folds[[i]]
      }
      if(i != j)
      {
        if(exists("trainingsset") == FALSE)
        {trainingsset = folds[[i]]}
        if(exists("trainingsset") == TRUE)
        {trainingsset = rbind(trainingsset, folds[[i]])}
      }
    }
    
    # learn bn structure and parameters on training data
    if(criterion == 1 & (method == 1|method == 3) & algorithm == 1) 
    {
      bn = hc(trainingsset, blacklist = blacklistdataset) 
    } 
    if(criterion == 2 & (method == 1|method == 3) & algorithm == 1) 
    {
      bn = hc(trainingsset,  blacklist = blacklistdataset, score = "bde")
    }
    if(criterion == 1 & method == 2 & algorithm == 1)
    {
      bn = structural.em(trainingsset, maximize = "hc", maximize.args = list(blacklist = blacklistdataset))
    }
    if(criterion == 2 & method == 2 & algorithm == 1)
    {
      bn = structural.em(trainingsset, maximize = "hc", maximize.args = list(blacklist = blacklistdataset, score = "bde"))
    }
    if(criterion == 1 & (method == 1|method == 3) & algorithm == 2)
    {
      bn = tabu(trainingsset,  blacklist = blacklistdataset)
    }
    if(criterion == 2 & (method == 1|method == 3) & algorithm == 2)
    {
      bn = tabu(trainingsset,  blacklist = blacklistdataset, score = "bde")
    }
    if(criterion == 1 & method == 2 & algorithm == 2)
    {
      bn = structural.em(trainingsset, maximize = "tabu", maximize.args = list(blacklist = blacklistdataset))
    }
    if(criterion == 2 & method == 2 & algorithm == 2)
    {
      bn = structural.em(trainingsset, maximize = "tabu", maximize.args = list(blacklist = blacklistdataset, score = "bde"))
    }
    
    if(criterion == 3 & (method == 1|method == 3) & algorithm == 1)
    {
      bn = hc(trainingsset,  blacklist = blacklistdataset, score = "aic")
    }
    if(criterion == 3 & method == 2 & algorithm == 1)
    {
      bn = structural.em(trainingsset, maximize = "hc", maximize.args = list(blacklist = blacklistdataset, score = "aic"), return.all = TRUE)
    }
    if(criterion == 3 & (method == 1|method == 3) & algorithm == 2)
    {
      bn = tabu(trainingsset,  blacklist = blacklistdataset, score = "aic")
    }
    if(criterion == 3 & method == 2 & algorithm == 2)
    {
      bn = structural.em(trainingsset, maximize = "tabu", maximize.args = list(blacklist = blacklistdataset, score = "aic"), return.all = TRUE)
    }
    if(criterion == 1 & method == 4 & algorithm == 2) # method = 4 multiple imputation
    {
      rm(impute)
      rm(imputedset1)
      rm(imputedset2)
      rm(imputedset3)
      rm(imputedset4)
      rm(imputedset5)
      rm(imputedset6)
      rm(imputedset7)
      rm(imputedset8)
      rm(imputedset9)
      rm(imputedset10)
      rm(imputedset11)
      rm(imputedset12)
      rm(imputedset13)
      rm(imputedset14)
      rm(imputedset15)
      rm(imputedset16)
      rm(imputedset17)
      rm(imputedset18)
      rm(imputedset19)
      rm(imputedset20)
      rm(BNimputed1)
      rm(BNimputed2)
      rm(BNimputed3)
      rm(BNimputed4)
      rm(BNimputed5)
      rm(BNimputed6)
      rm(BNimputed7)
      rm(BNimputed8)
      rm(BNimputed9)
      rm(BNimputed10)
      rm(BNimputed11)
      rm(BNimputed12)
      rm(BNimputed13)
      rm(BNimputed14)
      rm(BNimputed15)
      rm(BNimputed16)
      rm(BNimputed17)
      rm(BNimputed18)
      rm(BNimputed19)
      rm(BNimputed20)
      impute = mice(trainingsset, m = 20, method = "cart") # m = 20 imputed datasets created (for training) 
      imputedset1 = complete(impute, action = 1) 
      imputedset2 = complete(impute, action = 2)
      imputedset3 = complete(impute, action = 3)
      imputedset4 = complete(impute, action = 4)
      imputedset5 = complete(impute, action = 5)
      imputedset6 = complete(impute, action = 6)
      imputedset7 = complete(impute, action = 7)
      imputedset8 = complete(impute, action = 8)
      imputedset9 = complete(impute, action = 9)
      imputedset10 = complete(impute, action = 10)
      imputedset11 = complete(impute, action = 11)
      imputedset12 = complete(impute, action = 12)
      imputedset13 = complete(impute, action = 13)
      imputedset14 = complete(impute, action = 14)
      imputedset15 = complete(impute, action = 15)
      imputedset16 = complete(impute, action = 16)
      imputedset17 = complete(impute, action = 17)
      imputedset18 = complete(impute, action = 18)
      imputedset19 = complete(impute, action = 19)
      imputedset20 = complete(impute, action = 20)
      BNimputed1 = tabu(imputedset1,  blacklist = blacklistdataset) # learn structure using Tabu algoritme and BIC criterion
      BNimputed2 = tabu(imputedset2,  blacklist = blacklistdataset)
      BNimputed3 = tabu(imputedset3,  blacklist = blacklistdataset)
      BNimputed4 = tabu(imputedset4,  blacklist = blacklistdataset)
      BNimputed5 = tabu(imputedset5,  blacklist = blacklistdataset)
      BNimputed6 = tabu(imputedset6,  blacklist = blacklistdataset)
      BNimputed7 = tabu(imputedset7,  blacklist = blacklistdataset)
      BNimputed8 = tabu(imputedset8,  blacklist = blacklistdataset)
      BNimputed9 = tabu(imputedset9,  blacklist = blacklistdataset)
      BNimputed10 = tabu(imputedset10,  blacklist = blacklistdataset)
      BNimputed11 = tabu(imputedset11,  blacklist = blacklistdataset)
      BNimputed12 = tabu(imputedset12,  blacklist = blacklistdataset)
      BNimputed13 = tabu(imputedset13,  blacklist = blacklistdataset)
      BNimputed14 = tabu(imputedset14,  blacklist = blacklistdataset)
      BNimputed15 = tabu(imputedset15,  blacklist = blacklistdataset)
      BNimputed16 = tabu(imputedset16,  blacklist = blacklistdataset)
      BNimputed17 = tabu(imputedset17,  blacklist = blacklistdataset)
      BNimputed18 = tabu(imputedset18,  blacklist = blacklistdataset)
      BNimputed19 = tabu(imputedset19,  blacklist = blacklistdataset)
      BNimputed20 = tabu(imputedset20,  blacklist = blacklistdataset)
      stability = custom.strength(list(BNimputed1, BNimputed2, BNimputed3, BNimputed4, BNimputed5, BNimputed6, BNimputed7, BNimputed8, BNimputed9, BNimputed10, BNimputed11, BNimputed12, BNimputed13, BNimputed14, BNimputed15, BNimputed16, BNimputed17, BNimputed18, BNimputed19, BNimputed20), nodes = nodes(BNimputed1))
      bn = averaged.network(stability)
      bn = cextend(bn , strict = FALSE) # to find DAG equivalent network, because of undirected edges in averaged network 
      # if there are still undirected edges, direction is determined randomly 
      while(nrow(undirected.arcs(bn)) != 0)
      {  
        rm(arci)
        arci = vector(mode = "character", length = 2)
        arci[1] = undirected.arcs(bn)[1,1]
        arci[2] = undirected.arcs(bn)[1,2]
        rm(averagedBN1to10testcycles)
        rm(random)
        random = sample(1:2, 1)
        if(random == 1)
        {
          fromi = 1
          toi = 2
        }
        if(random == 2)
        {
          fromi = 2
          toi = 1
        }
        averagedBN1to10testcycles = try(set.arc(bn, from = arci[fromi], to = arci[toi]))
        if(class(averagedBN1to10testcycles) == "try-error")
        {
          rm(averagedBN1to10testcycles)
          averagedBN1to10testcycles = try(set.arc(bn, from = arci[toi], to = arci[fromi]))
          if(averagedBN1to10testcycles == "try-error")
          {
            bn = drop.edge(bn, from = arci[fromi], to = arci[toi])
          }
          if(averagedBN1to10testcycles != "try-error")
          {
            bn = averagedBN1to10testcycles
          }
        }
        if(class(averagedBN1to10testcycles) != "try-error")
        {
          bn = averagedBN1to10testcycles
        }
      }
    }
    bnfit = as.grain(bn.fit(bn, trainingsset, method = "bayes", iss = 1)) # parameter learning using bayesian method with imaginary sample size 1
    
    # evaluate performance of algorithm (and parameters) on test data 
    for(record in 1:nrow(testset))   
    {
      rm(nodestestset)
      rm(statestestset)
      for(variable in 1:ncol(testset)) 
      {
        if(is.na(testset[record,variable]) == 0 & testset[record,variable] != "missing") #evidence only if variable value is known (not na or recalculated to -1)    
            {
            if(exists("nodestestset") == TRUE)
            {
              nodestestset = c(nodestestset, variable.names(testset[variable])) #evidence is list of variable values observed in record 
            }
            if(exists("statestestset") == TRUE)
            {
              statestestset = c(statestestset, as.character(testset[record,variable])) 
            }
            if(exists("nodestestset") == FALSE) #initialisation
            {
              nodestestset = variable.names(testset[variable])
            }
            if(exists("statestestset") == FALSE)
            {
              statestestset = as.character(testset[record,variable])
            }
          }
        } 
        Evidencerecord = setEvidence(bnfit, nodes = nodestestset, states = statestestset) # evidence record values in testset in model from training data 
        retractEvidence(bnfit, nodes = nodestestset)
        LogLikelihoodrecord = log(pEvidence(Evidencerecord)) #probability of observing evidence under the model
        if(exists("LogLikelihooddataset") == FALSE){LogLikelihooddataset = LogLikelihoodrecord} #initialisation
        if(exists("LogLikelihooddataset") == TRUE){LogLikelihooddataset = LogLikelihooddataset + LogLikelihoodrecord} #prob of observing combination of records in dataset (testset) 
      }
      loglikelihoodfolds[i,1] = LogLikelihooddataset #update loglikelihood in vector for evaluating all k = 10 testsets/folds
      rm(LogLikelihooddataset)
    }
  return(loglikelihoodfolds[,1])
}

# Confidence interval 95% for standard normal distribution
calculateCI = function(observationsvector)  
{
  lowerbound = mean(as.vector(observationsvector)) - 1.960 * (sd(as.vector(observationsvector))/sqrt(nrow(as.data.frame(observationsvector))))
  upperbound = mean(as.vector(observationsvector)) + 1.960 * (sd(as.vector(observationsvector))/sqrt(nrow(as.data.frame(observationsvector))))
  CI = c(lowerbound, " - ", upperbound) 
  return(CI)
}

# Run cross validation for selection of parameter settings (TABU algorithm, BIC network score, structural EM vs mean imputation method vs multiple imputation) and calculate CI for mean log likelihood. Note stage (and reference) excluded from analyses
DataPhase2temp = DataPhase2[,c(1:2,6:9,11:43)] 
cv.tabu.bic.na.exlstage = bn.cv.handlemissings(dataset = DataPhase2temp, blacklistdataset = blacklistPhase2_exclstage, criterion = 1, method =  2, algorithm = 2)
CI.cv.tabu.bic.na.exlstage = calculateCI(cv.tabu.bic.na.exlstage)
cv.tabu.bic.na.exlstage = mean(cv.tabu.bic.na.exlstage)
DataPhase2temp = DataPhase2[,c(1:2,6:9,11:43)]
cv.tabu.bic.meanimp.exlstage = bn.cv.handlemissings(dataset = DataPhase2temp, blacklistdataset = blacklistPhase2_exclstage, criterion = 1, method = 3, algorithm = 2)
CI.cv.tabu.bic.meanimp.exlstage = calculateCI(cv.tabu.bic.meanimp.exlstage)
cv.tabu.bic.meanimp.exlstage = mean(cv.tabu.bic.meanimp.exlstage)
DataPhase2temp = DataPhase2[,c(1:2,6:9,11:43)] 
cv.tabu.bic.MI.exlstage = bn.cv.handlemissings(dataset = DataPhase2temp, blacklistdataset = blacklistPhase2_exclstage, criterion = 1, method =  4, algorithm = 2) # BIC, Tabu, Multiple Imputation
CI.cv.tabu.bic.MI.exlstage = calculateCI(cv.tabu.bic.MI.exlstage)
cv.tabu.bic.MI.exlstage = mean(cv.tabu.bic.MI.exlstage) 

# Learn Bayesian network model from continuous data based on selected parameter settings from cross validation
DataPhase2temp = DataPhase2[,c(1:2,6:9,11:43)] # concept variable selection excl stage and reference 
DataPhase2temp = preprocessingcontinue(DataPhase2temp, 2)
BNPhase2 = bnstructurealternatives(DataPhase2temp, blacklistPhase2_exclstage, 1, 2, 2) # Bayesian network structure learnt by TABU using BIC score and structural EM, blacklist for time dimension
par(cex=3500)
graphviz.plot(BNPhase2$dag, shape = "ellipse")

# Model averaging analysis
stability_dynamicbnstructure = boot.strength(BNPhase2$imputed, R=20, algorithm = "tabu", algorithm.args = list(blacklist = blacklistPhase2_exclstage)) # Bootstrap to assess strengths of BN arcs/directions, using 20 replicates, based on selected dataset where NA imputed by expected value final run structural em 
plot(stability_dynamicbnstructure)
averaged.dynamicbn = averaged.network(stability_dynamicbnstructure) # Build network with only significant arcs from bootstrap analysis
# attr(stability_dynamicbnstructure, "threshold") 
graphviz.compare(BNPhase2$dag, averaged.dynamicbn, shape = "ellipse")  
compare(BNPhase2$dag, averaged.dynamicbn, arcs = TRUE)
# strength.plot(averaged.dynamicbn, stability_dynamicbnstructure, shape = "ellipse")
averaged.dynamicbn.fit = bn.fit(averaged.dynamicbn, DataPhase2temp) # Learning maximum likelihood parameter estimates for averaged continuous model 
# averaged.dynamicbn.fit$C_intrinsicmotivation_T1 

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------