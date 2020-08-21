require(varhandle)
require(stringi)
require(stringr)
require(clipr)
require(rlang)
require(haven)
require(dplyr)
require(nnet)
require(ROCit)

# Allows to make complexe univar and multivar analyses from a single *dataframe*, plus predictions and ROC curves
# For now only works for dependent variables (being predicted) as binary
# Dependencies as above (!)

# Steps in general :
#   i) parse DF to generate a DF of variables with 
#      1) varname → variable name
#      2) vartype (named vtype_naked in makeMultivarMasterDF) → type of var (bin, dbl, fact)
#      2) <whatever> → [optional] columns to put '#' or nothing to select or not those variables later thru grep
#      3) factref → for factors, which one is reference
#      4) factopt → for factors, all factors that existed (only useful for user, won't be parsed by multivar fct)
#   ii) user can save it as CSV and make modifications, then reimport that masterDF
#   iii) run multivar analysis with various options, returning
#      1) multivar analysis
#      2) [optional] univar analyses
#      3) prediction from multivar analysis
#      4) ROC info

# makeMultivarMasterDF
#   args
#     db : the dataframe
#     emptyCols [optional] : a list of cols to add to the returned dataframe (retval) which will be empty
#   returns
#     a dataframe :
#       varname : all var names in db
#       vtype_naked : the var type corresponding (dbl, bin, fact) → WARNING when passed thru multivar in 'mv.vars' DF, expected DF col name is 'vartype' (not vtype_naked)
#         the name is different for ppl to add different columns to the 'naked type' → will be explained later but when parsing mv.vars$vartype, multivar removes the type if
#         preceded by '#'. Hence if one analysis excludes some variables, and another analysis another group, this info can be saved in the masterDF in different cols.
#       <cols from emptyCols arg> : cols same length but with ""
#       factref : default factor of reference for factor vars
#       factopt : lists all factors for user to be easier to modify the masterDF

# multivar
#   args
#     mv.vars : vt (from makeMultiVarMasterDF) DF with any vars but minimally
#       varname : the name of var (such as data DF provided in mv.ds below)
#       vartype : the vartype corresponding (bin, dbl, fact) as *characters* ex. 'dbl'; to avoid this var being considered, add # ie '#dbl' won't be considered
#       factref : for vartype 'fact' which factor to consider as reference
#     mv.ds : the dataframe of the main dataset
#     mv.tv : vector of same length as any col of mv.ds to identify which belongs to training (t) vs validation (v) dataset; TRUE is for training
#       note1 : to have a random sample of size k, use 'as.logical(rbinom(len(<the dataset>), 1, k))', ex 'as.logical(rbinom(len(mydataset), 1, 0.7))' for 70% of training, randomly
#       note2 : if don't care, just put random thing (if only 0s or 1s might have a bug)
#     mv.out : the outcome (binary, as logical), needs to be same length as all cols in mv.ds
#     mv.digits : rounding digits
#     mv.remove_at : if predictor variables ('varname') are missing, at what threshold of missingness (0-1) they should be removed; if to be always kept put 1
#     mv.univartoo : if to return univar analysis for all variables in vartype that do not start with '#'
#       note1 : might increase runtime significantly
#     mv.onlypredict : if only to predict, not to do usual regression
#   returns
#     a dataframe :
#       glm.univar : a DF with all varnames, the estimate, pvalue and LCI/UCI estimates
#         note1 : won't keep all details of univar analysis
#       glm.results.forprediction : result of the GLM, but only from the training set
#       glm.results : result of the GLM for all data (training + validation)
#       glm.predict : the applied glm.results.forpredicton to the validation set (NAs will be returned for data in the prediction set)
#       roc : a ROCit object from the prediction, with many elements in list, but most interesting
#         AUC : the AUC
#         FPR and TPR : (false pos rate and true pos rate)
#           note1 : to plot it, 'plot(<multivar obj>$roc$FPR, <multivar obj>$roc$TPR)


# Generic len function to make it easier
len <- function(argl){
  return(length(argl))
}


makeMultivarMasterDF <- function( db, emptyCols = c('') ) {
  # Prepare a CSV file with varnames and vartypes to tell the script what to analyze
  # Find vartype from variables, main reason <typeof(<arg>)> is no good is that factors are returned as integers with that
  
  vartypes = NULL
  factref = NULL
  factopt = NULL
  
  # go thru vars with their names
  for(i in 1:len(db$pre[1,])){
    # find if it is a factor
    isfactor = FALSE
    if(typeof(db$pre[,i]) == "integer"){
      if(is.factor(db$pre[,i])){ isfactor = TRUE } 
    } 
    
    # if it is a factor then add the default factor of reference and list all factors
    if(isfactor){
      vartypes = c(vartypes, "factor")
      factref = c(factref, levels(db$pre[,i])[1])
      factopt = c(factopt, paste("'",paste(levels(db$pre[,i]), collapse="','"),"'",sep=""))
    } else {
      vartypes = c(vartypes, typeof(db$pre[,i]))
      factref = c(factref, "")
      factopt = c(factopt, "")
    }
  }
  
  # prepare the retval DF to be returned
  retval = 
    data.frame(  
      varname = colnames(db$pre),  
      vtype_naked = renametype(vartypes)
    )
  
  # add the empty cols as specified
  for ( emptyCol in emptyCols ) {
    retval[emptyCol] = rep("", len(vartypes))
  }
  
  # add factref/factopt (after creating DF just to show them that way)
  retval$factref = factref
  retval$factopt = factopt
  
  
  return ( retval )
}




# main multivar function, depends on univar below
multivar = function(
  mv.vars, # dataframe of variables + types
  mv.ds, # the training dataset excluding outcome variable
  mv.tv, # the vector of TRAINING vs VALIDATION set (training = TRUE, validation = FALSE)
  mv.out, # outcome vector (binary) --> for NAs, patients will be excluded (useful for subanalysis eg recurrent fallers TRUE, fallers FALSE, non-fallers NA)
  mv.digits, # digits
  mv.remove_at, # percentage (0-1) at which missing variables are removed
  mv.univartoo, # no univariate regression too
  mv.onlypredict = FALSE # if only to do the prediction (and not the multivar lin regression)
){
  
  
  if(length(mv.ds[,1]) == length(mv.out)){  
    
    mv.ds = mv.ds[!is.na(mv.out), ]
    mv.out = mv.out[!is.na(mv.out)]
    
    if(missing(mv.univartoo)){
      mv.univartoo = FALSE
    }
    if(missing(mv.digits)){
      mv.digits = 4
    }
    if(missing(mv.remove_at)){
      mv.remove_at = 1
    } else {
      if(is.na(mv.remove_at)){mv.remove_at=1}
      if(mv.remove_at<=0){mv.remove_at=1}
    }
    
    # Multivariate model
    # Variable names
    mv.vars$varname = unfactor(factor(mv.vars$varname))
    mv.vars$newvartype = gsub("#","",mv.vars$vartype)
    
    # Univariate analysis results
    mv.vars.df = data.frame(
      mv.outcome = mv.out
    )
    
    options(na.action='na.exclude')
    
    for(i in 1:len(mv.vars$varname)){
      
      this.var = get(mv.vars$varname[i], mv.ds)
      if(is.factor(this.var)){this.var = unfactor(this.var)}
      this.var[is.na(this.var)] = 0
      this.ref = as.character(mv.vars$factref[i])
      if(mv.vars$newvartype[i] == "bin"){
        mv.vars.df = cbind(
          mv.vars.df,
          setNames(data.frame(as.logical(this.var)),mv.vars$varname[i])
        )
      }
      if(mv.vars$newvartype[i] == "dbl"){
        mv.vars.df = cbind(
          mv.vars.df,
          setNames(data.frame(as.double(this.var)),mv.vars$varname[i])
        )
      }
      if(mv.vars$newvartype[i] == "fact"){
        mv.vars.df = cbind(
          mv.vars.df,
          setNames(data.frame(relevel(factor(this.var), ref = this.ref)),mv.vars$varname[i])
        )
      }
      
      
    }
    
    mv.vars.dfpf = mv.vars.df
    
    #mv.vars.df = gee.factor(mv.vars.df)
    
    mv.vars.dfa =  mv.vars.df[,c("mv.outcome", mv.vars$varname[grep("#",mv.vars$vartype,invert=TRUE)])]
    
    if(!mv.onlypredict){
      mv.glm.results =    glm(
        formula = formula(mv.vars.dfa),
        data = mv.vars.dfa,
        binomial(link = "logit")
      )
    } else {
      mv.glm.results = NULL
    }
    
    mv.glm.results.forprediction =    glm(
      formula = formula(mv.vars.dfa),
      data = mv.vars.dfa[mv.tv,],
      binomial(link = "logit")
    )
    
    mv.glm.predict.raw = 
      predict(
        mv.glm.results.forprediction,
        mv.vars.dfa[!mv.tv,],
        na.action = "na.exclude"
      )
    
    mv.glm.predict = rep(NA, length(mv.out))
    mv.glm.predict[as.integer(names(mv.glm.predict.raw))] = round(exp(mv.glm.predict.raw), digits = mv.digits)
    
    mv.glm.univar = NULL
    if(mv.univartoo){
      mv.glm.univar = univar(
        uv.vars = mv.vars[grep("#",mv.vars$vartype,invert=TRUE),],
        uv.ds = mv.vars.dfa[,-1],
        uv.out = mv.out,
        uv.digits = mv.digits
      )
    }
    
    return(
      list(
        
        glm.univar = mv.glm.univar,
        
        glm.results.forprediction = mv.glm.results.forprediction,
        
        glm.results = mv.glm.results,
        
        glm.predict = mv.glm.predict,
        
        roc =   rocit(
          score = mv.glm.predict[!is.na(mv.glm.predict)],
          class = mv.out[!is.na(mv.glm.predict)]
        )
        
      )
    )
    
  } else {
    
    return(0)
    
  }
  
}





univar = function(
  uv.vars, # dataframe of variables + types
  uv.ds, # the dataset excluding outcome variable
  uv.out, # outcome vector (binary) --> for NAs, patients will be excluded (useful for subanalysis eg recurrent fallers TRUE, fallers FALSE, non-fallers NA)
  uv.digits # digits
){
  # Variable names
  if(is.factor(uv.vars$varnames)){uv.vars$varname = unfactor(uv.vars$varname)}
  
  # Univariate analysis results
  unires = NULL
  
  options(na.action='na.exclude')
  
  # Go thru variables one at the time
  for(i in 1:length(uv.vars$varname)){
    # Look at type
    if(uv.vars$vartype[i]=="dbl"){
      # If double then linear reagression so GLM with gaussian/identity
      # get(varname, clc) is equivalent to clc$varname
      # then we get the summary into this.glm object in order to later extract coefficients
      this.glm =
        glm(
          uv.out ~ get(uv.vars$varname[i],uv.ds),
          #family = gaussian(link = "identity")
          family = binomial(link = "logit")
        )
      
      
      # This out is the small data frame which will be binded to unires
      # need to put first row with 0s as otherwise with only one covariate R does not make a data.frame just a vector
      # and later cuases problem
      # and we round to 3 digits
      this.out = data.frame(
        rbind(
          c(0,0,0,0),
          round(c(summary(this.glm)$coefficients[2,c(1,4)], confint(this.glm)[2,]), digits=3)
        )
      )
      
      # Rename the rows with varnames (is longer in the bin/fact section as might be > 1 variable)
      # first is "" b/c of the c(0,0,0,0) line necessity
      rownames(this.out) = c("",uv.vars$varname[i])
      
      # Then bind the var name to the column so when exported it is present
      # and a column with var type
      # and obviously data itself
      this.out = cbind(
        c("",paste(uv.vars$varname[i],"= <x>")),
        c("","dbl"),
        this.out
      )
      
    } else {
      # Here same but for bin or fact
      # GLM converts factors to individual bin variables
      
      # Looks if it is bin or fact and then applies logical or factor to it
      # the data for this variable is stored in this.data
      this.data = data.frame(factor(get(uv.vars$varname[i],uv.ds)),as.logical(get(uv.vars$varname[i],uv.ds)))[,as.integer(1+(uv.vars$vartype[i]=="bin"))]
      
      # Apply GLM with bin/logit to this.data and extract summary in same object as above for doubles/lin regression
      this.glm =
        glm(
          uv.out ~ this.data,
          family = binomial(link = "logit")
        )
      
      
      # Same as for dbl, but here we bind the first (empty) line, the second line and possibly other lines if 
      # it is a factor with 3 or more answer options
      # len(this.glm$coefficients[,1]) --> the number of coefficients apart from intercept
      if((len(summary(this.glm)$coefficients[,1])) == 2){
        inside.this.out = c(
          summary(this.glm)$coefficients[2:(len(summary(this.glm)$coefficients[,1])),c(1,4)], 
          confint(this.glm)[2:(len(summary(this.glm)$coefficients[,1])),]
        )
      } else {
        
        inside.this.out = cbind(
          summary(this.glm)$coefficients[2:(len(summary(this.glm)$coefficients[,1])),c(1,4)], 
          confint(this.glm)[2:(len(summary(this.glm)$coefficients[,1])),]
        )
      }
      
      
      this.out = data.frame(
        rbind(
          c(0,0,0,0),
          round(
            inside.this.out,
            digits=3
          )
        )
      )
      
      # Row names from variable name + the level (when 3 or more options) : 'fish' + ' = ' + '2' where maybe 2 is a trout
      rownames(this.out) = c("",paste(uv.vars$varname[i],levels(factor(this.data))[-1],sep=" = "))
      
      # Mini-object for this variable adding the col with the variable names and another with type
      # first line of cbind is quite long : it removes the name of variable for all factors apart first and pads it with spaces so it looks better
      this.out = cbind(
        c("",rownames(this.out)[2],paste(paste(rep(" ",str_length(rownames(this.out)[2])-(str_length(str_split(rownames(this.out)[2]," = ")[[1]][2])+3)),collapse=""),"=",matrix(unlist(str_split(rownames(this.out[-1,])," = ")),nrow=2)[2,-1]))[1:len(rownames(this.out))],
        c("",as.character(uv.vars$vartype[i]), rep("", len(rownames(this.out))-2)),
        this.out
      )
      
    }
    
    # Regardless of data type for this variable, here we add columns with the estimate and its 95% CI and rearrange columns
    #this.out = cbind(this.out[,c(1,2)],round(this.out[,3] + this.out[,4]*qnorm(0.025), digits=3), round(this.out[,3] + this.out[,4]*qnorm(0.5), digits=3), round(this.out[,3] + this.out[,4]*qnorm(0.975), digits=3), this.out[,-c(1,2,3)])
    
    #this.out[,7]=""
    #this.out[,8]=""
    
    # Once these modifications are done, we give proper names to the columns for the mini-object (data frame) summarizing
    # results for this univariate analysis on a sigle variable
    #colnames(this.out) = c("var","type","est", "p_value", "LCI_est", "UCI_est", "empty", "empty")
    colnames(this.out) = c("var","type","est", "p_value", "LCI_est", "UCI_est")
    
    
    
    if(uv.vars$vartype[i]=="fact"){
      this.out[,1] = paste(this.out[,1],"||",sep="")
    }
    
    
    # And we bind the results for this variable to the main results (unires)
    # [-1, ] to remove the first line c(0,0,0,0) we had put initially to create the data frame which was necessary otherwise
    # the df is converted into a vector
    unires = 
      rbind(
        unires,
        this.out[-1,]
      )
    
    
  }
  
  # Now that the first col of the unires dataframe is the name of the vars, just remove it in the df itself otherwise
  # heavy when visualizing in console
  rownames(unires) = 1:(length(unires$var))
  
  # Make p values too low (0 b/c of round) look better
  unires$p_value[unires$p_value==0]="<0.001"
  
  # For columns 3 to 5 ie estimate and their + and - 95% CI, we need to apply a inv logit fct
  # easier to do it here because when computing the + and - for CI, need to use exp AFTER the estimate +/- sd
  # ie use exp(estimate +/- sd) and not exp(estimate) + exp(sd)
  #for(i in 3:5){unires[!(unires$type %in% "dbl"),i] = exp(unires[!(unires$type %in% "dbl"),i])}
  
  
  #unires[,3:5] = round(exp(unires[,3:5]), digits = uv.digits)
  
  #return(unires)
  ures = rbind(
    unires[StrRight(as.character(unires[,1]),2)!="||",],
    unires[StrRight(as.character(unires[,1]),2)=="||",]
  )
  ures[,1] = sub("||","",ures[,1],fixed=TRUE)
  return(ures)
}