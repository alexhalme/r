require(varhandle)
require(stringi)
require(stringr)
require(clipr)
require(DescTools)
require(gee)
require(geepack)
require(ICC)
require(ICCbin)
require(lme4)  
require(rlang)
require(haven)
require(dplyr)
require(nnet)
require(ROCit)


smartUnfactor <- function(obj) {
  if (is.factor(obj)) {
    return(unfactor(obj))
  } else {
    return(obj)
  }
}


# 'Table 1' builder
table1builder <- function(var, varlabel = "", dig=3){
  # provide var as a list (dataframe works too) with each member being a different subgroup for a given variable
  # so for a variable, send :
  #function(
  #  var = list(
  #    group1 = db1$age,
  #    group2 = db2$age,
  #    group3 = db3$age
  #  )
  #)
  
  # If only a vector, put it in a list to be parsable
  if(typeof(var)!="list"){
    var = list(var)
  }
  
  # Removes the label from output if not provided
  if(varlabel == ""){
    retval = NULL 
  } else {
    retval = varlabel
  }
  # If values are true/false
  if(typeof(var[[1]])=="logical"){
    # Add a buffer to fit with factors (see later) as factors need an extra column
    retval = c(retval,"")
    # Go thru input list (each list element is for a group)
    for(i in 1:len(var)){
      # Get it without NAs
      varnona = var[[i]][!is.na(var[[i]])]
      # Append the retval row with total of TRUEs and %
      retval = c(
        retval, 
        paste(as.character(sum(varnona))," (",as.character(round(100*sum(varnona)/len(varnona),digits=dig)),"%)",sep="")      
      )
    }
  }
  if((typeof(var[[1]])=="integer") | (typeof(var[[1]])=="double")){
    # Checks if it is a factor
    if(is.factor(unlist(var))){
      # If a factor, then first line is retval (NULL if no varlabel) then the first level
      retline = c(retval, levels(unlist(var))[1])
      # Retval reinitialized b/c done by row here, and rows (retline) append to retval
      retval = NULL
      # Go thru all levels
      for(level in levels(unlist(var))){
        # First level : was already done prior, so no need for it
        # Checks also if varlabel is "" or not because if yes needs to add an additional "" column to lineup
        if(level != levels(unlist(var))[1]){
          retline = list(c("", level),level)[[as.integer(varlabel == "") + 1]]
        }
        # For this level, goes thru all groups
        for(i in 1:len(var)){
          # Remove NAs
          varnona = var[[i]][!is.na(var[[i]])]
          # Same as logical, but appending to this line
          retline = c(
            retline, 
            paste(as.character(sum(unfactor(varnona) == level))," (",as.character(round(100*sum(unfactor(varnona) == level)/len(unfactor(varnona) == level),digits=dig)),"%)",sep="")      
          )
        }
        # All done for this level for all groups so can append the line to retval
        retval = rbind(
          retval,
          retline
        )
      }
    } else {
      # Case where it is a 'true' integer (or a double)
      # Need to add buffer to align with potential factors (same as for logical above)
      retval = c(retval,"")
      # Goes thru groups
      for(i in 1:len(var)){
        # Removes NAs
        varnona = var[[i]][!is.na(var[[i]])]
        # Mean and SD appended
        retval = c(
          retval, 
          paste(as.character(round(mean(varnona),digits=dig)),as.character(round(sd(varnona),digits=dig)),sep=" ± ")
        )
      }
    }
  }
  # Remove row names otherwise would be nothing or 'retline'
  rownames(retval) <- NULL
  # Return element which is a vector or a matrix
  return(retval)
}

# Wrapper for dataset for table1builder
# Provide a dataset with each member being a variable <ds = data.frame(var1 = <var1_content>, var2 = <var2_content>, ...)>
# and an <igr> sorting vector of length of data.frame which will be factored (assigns to which group each observation is from)
table1builder_wrapper <- function(ds, igr=NULL, dig = 2){
  # If ds not a list them make it
  if(typeof(var)!="list"){
    var = list(var)
  }
  # If no igr provided then all from same group
  if(typeof(igr)=="NULL"){
    gr = factor(rep(1,len(ds[,1])))
    igr = rep(1,len(ds[,1]))
  } else {
    # Twice to make sure the factors are integers and numerically ordered
    gr = factor(as.integer(factor(igr)))
  }
  # Empty retval
  retval = NULL
  # Go thru each variable
  for(i in 1:len(ds)){
    # Input list to be put thru table1builder
    inl = NULL
    # Go thru all groups/levels of gr possible
    for(j in 1:len(levels(gr))){
      # For this group (j), get all observations matching (ie of lines in that group) for ith variable of the ds dataset
      inl[[j]] = ds[gr == j,i]
    }
    # Then inl is a list of vectors with each group being a vector (for a given variable)
    # rbind the output of tablebuilder to retval
    # Also, table1builder gets colnames(ds)[i] as argument to name the col
    retval = rbind(
      retval,
      table1builder(inl,colnames(ds)[i],dig=dig)
    )
  }
  # Compute n
  line_n = c("","")
  for(i in 1:len(levels(factor(igr)))){
    line_n = c(line_n, paste("n",as.character(sum(as.character(igr) == as.character(levels(factor(igr))[i]))),sep=" = "))
  }
  # Add coltitles
  retval = rbind(
    c("","",levels(factor(igr))),
    line_n,
    rep("",len(line_n)),
    retval
  )
  # Remove row names otherwise would be nothing or 'retline'
  rownames(retval) <- NULL
  # Return element which is a vector or a matrix
  return(retval)
}

# Clip a table
clipdf <- function(dfo, tcol = FALSE, trow = FALSE, dig = 3){
  dfo = as.data.frame(dfo)
  # round and make sure non numeric cols don't get rounded
  df = data.frame(NOTHING = rep(NA, len(dfo[,1])))
  for ( col in colnames(dfo) ){
    if( typeof(dfo[[col]]) %in% c('double', 'integer') && ! is.factor(dfo[[col]]) ){
      df = cbind(df, round(dfo[col], digits=dig))
    } else {
      df = cbind(df, dfo[col])
    }
  }
  df = df[!colnames(df) %in% c('NOTHING')]
  
  base = paste(
    apply(
      list(df,cbind(.rn=rownames(df),df))[[(as.integer(trow) + 1)]],
      1,
      function(x) paste(x,collapse='\t')
    ),
    collapse="\n"
  )
  
  if(tcol){
    write_clip(
      paste(
        list(paste(colnames(df), collapse="\t"),paste("<rn>",paste(colnames(df),collapse="\t"), sep="\t"))[as.integer(trow) + 1],
        base,
        sep="\n"
      )
    )
  } else {
    write_clip(base)
  }
}


# function to simplify output of ICC for bin data, provide the binary endpoint (c.y) and the cluster numbers (c.id)
calc.icc.bin <- function(c.y,c.cid){
  
  c.cid = as.integer(factor(c.cid))
  
  # make sure that c.y is same length as c.cid and no NAs otherwise obviously mismatch and won't work, if wrong will return -1
  if(length(c.y[!is.na(c.y)])==length(c.cid[!is.na(c.cid)])){
    
    # a bit complicated but this creates a data frame removing all clusters with only 1 participant
    # merges c.y and c.cid in a frame, then makes a histogram (table) and adds the values of that table (0 and 1)
    # so that sum is # of participant for that cluster
    # then keeps cluster IDs where that sum is 2 or more with the "which"
    # then remove duplicates through a factor/unfactor/levels method
    # then applies this vector of cluster IDs with > 1 participant to the initial dataframe to keep only the ones we want
    # and that data frame is put thru the iccbin function
    # the reason we remove n = 0 or n=1 clusters is otherwise iccbin won't work (and most ICC in bin situations won't either)
    data.icc.bin = data=data.frame(cid=c.cid,y=c.y)[c.cid %in% as.integer(levels(factor(as.character(which(table(data.frame(cid=c.cid,y=c.y))[,1]+table(data.frame(cid=c.cid,y=c.y))[,2]>1))))),]
    raw.icc.bin = iccbin(cid=cid,y=y,data=data.icc.bin)
    
    # here we return the raw output of the iccbin function
    # plus the medican ICC and median ICC LCI and UCI
    return(list(raw = raw.icc.bin,
                median.icc = median(as.double(unfactor(raw.icc.bin$estimates[,"ICC"]))[!is.na(as.double(unfactor(raw.icc.bin$estimates[,"ICC"])))]),
                median.icc.lci = median(as.double(raw.icc.bin$ci$LowerCI[!is.na(raw.icc.bin$ci$LowerCI)])),
                median.icc.uci = median(as.double(raw.icc.bin$ci$UpperCI[!is.na(raw.icc.bin$ci$UpperCI)]))
    )
    )
    
    
    
  } else {
    return(-1)
  }
  
}

# from the output of the geeglm function for BINARY values, this function returns the interesting stuff
# provides the raw coeffiencts (not so useful)
# but also the ORs which are exp(coefficient)
# and the p value
# for the raw coef and ORs, 3 values are returned : [,1] is the LCI, [,2] is point estimate/MLE/expected value and [,3] is UCI
coefglm <- function(glm.o, covar.o){
  if(missing(covar.o)){
    covar.names = "gr"
  } else {
    covar.names = c("gr", colnames(covar.o))
  }
  
  out.names = NULL
  for(i in 1:length(covar.names)){
    out.names[[covar.names[i]]] = list(raw = summary(glm.o)$coefficients[covar.names[i],"Estimate"]+summary(glm.o)$coefficients[covar.names[i],"Std.err"]*c(qnorm(0.025),qnorm(0.5),qnorm(0.975)),
                                       or = exp(summary(glm.o)$coefficients[covar.names[i],"Estimate"]+summary(glm.o)$coefficients[covar.names[i],"Std.err"]*c(qnorm(0.025),qnorm(0.5),qnorm(0.975))),
                                       p = summary(glm.o)$coefficients[covar.names[i],"Pr(>|W|)"]
    )
  }
  
  
  return(out.names)
  
}  


gee.nor <- function(v, y, cl, gr, round.digits, covar){
  
  if(missing(round.digits)){
    round.digits = 2
  }
  
  # y : vector of length sum(dsv) with the values
  
  # This removes cluster numbers for excluded records (puts NA) otherwise functions 
  # have a vector of clusters with cluster numbers with 0 records and they tend to 
  # output weird stuff
  rcl = cl[v]
  rcl = as.integer(factor(rcl))
  
  # Data frame for covariates
  gee.data = data.frame(
    y = y,
    gr = gr
  )
  
  if(!missing(covar)){
    gee.data = cbind(
      gee.data,
      covar
    )
  }
  
  # We return a list with the output from the GLM/GEE, the extracted coefficients with the coefglm fct, a data frame with basic data 
  # analysis w/o GEE analysis, a parametric test with these analysis (not accounting for clustering) and finally a object with ICCs for 
  # the dataset 
  return(
    list(
      # Applies a GEE for normal data : y[v] is the normally distributed data keeping only the data when v = TRUE and gr is the group
      # id is the cluster refactored as above
      # rest is kind of standard
      geeglm =
        geeglm(formula = formula(gee.data[v,]), #y[v] ~ gr[v],
               data = gee.data[v,],
               id = rcl,
               family = gaussian(link="identity"),
               corstr = "exchangeable"
        ),
      
      # Here same as geeglm --> function outputs the coefficients
      coefglm =
        coefglm(
          geeglm(formula = formula(gee.data[v,]), #y[v] ~ gr[v],
                 data = gee.data[v,],
                 id = rcl,
                 family = gaussian(link="identity"),
                 corstr = "exchangeable"
          ),
          covar
        ),
      
      link = "identity (always)",
      
      # For reader to see easy things gives mean/sd/difference between groups w/o cluster analysis
      simple.description =
        data.frame(
          mean.ctrl = round(mean(y[v & gr==0]), round.digits),
          mean.intv = round(mean(y[v & gr==1]), round.digits),
          sd.ctrl = round(sd(y[v & gr==0]), round.digits),
          sd.intv = round(sd(y[v & gr==1]), round.digits),
          d = round(mean(y[v & gr==1])-mean(y[v & gr==0]), round.digits)
        ),
      
      # Same as above but t.testing it w/o cluster analysis
      simple.ttest=
        t.test(y[v & gr==1],y[v & gr==0]),
      
      # Computes ICCs
      ICCs=
        data.frame(ICC=round(ICCest(as.factor(rcl),y[v],alpha=0.05)$ICC,3),
                   LowerCI=round(ICCest(as.factor(rcl),y[v],alpha=0.05)$LowerCI,3),
                   UpperCI=round(ICCest(as.factor(rcl),y[v],alpha=0.05)$UpperCI,3)
        )
      
    )
  )
  
}


gee.bin <- function(n, z, cl, gr, round.digits, covar){
  # Covar is a data frame where we must be completely sure that no variables are identical, optional
  
  
  if(missing(round.digits)){
    round.digits = 4
  }
  
  # n : vector of baseline length max(iid) of TRUE/FALSE
  # z : vector of outcome +ve 
  
  # This removes cluster numbers for excluded records
  rcl = cl
  rcl[!n] = NA
  rcl = as.integer(factor(rcl))
  
  
  # Here to chan
  rcl.icc = as.integer(factor(cl[n]))
  
  
  # Data frame for covariates
  gee.data = data.frame(
    z = z,
    gr = gr
  )
  
  if(!missing(covar)){
    gee.data = cbind(
      gee.data,
      covar
    )
  }
  
  
  
  # We return a list with the output from the GLM/GEE, the extracted coefficients with the coefglm fct, a data frame with basic data 
  # analysis w/o GEE analysis, a parametric test with these analysis (not accounting for clustering) and finally a object with ICCs for 
  # the dataset 
  return(
    list(
      # Applies a GEE for normal data : z[n] is the binary data (0/1) keeping only the data when n = TRUE and gr is the group
      # id is the cluster refactored as above
      # rest is kind of standard
      geeglm=
        geeglm(formula = formula(gee.data[n,]), #z[n] ~ gr[n],
               data = gee.data[n,],
               id = as.integer(factor(rcl[n])),
               family = binomial(link=c("logit", "identity")[as.integer(missing(covar)) + 1]),
               corstr = "exchangeable"
        ),
      
      # Here same as geeglm --> function outputs the coefficients
      coefglm=
        coefglm(
          geeglm(formula = formula(gee.data[n,]), #z[n] ~ gr[n],
                 data = gee.data[n,],
                 id = as.integer(factor(rcl[n])),
                 family = binomial(link=c("logit", "identity")[as.integer(missing(covar)) + 1]),
                 corstr = "exchangeable"
          ),
          covar
        ),
      
      # Specify link --> ideally identity but GLM with identity not working if covariates present
      link = c("logit (because of presence of covariates)", "identity (no covariates)")[as.integer(missing(covar)) + 1],
      
      # For reader to see easy things gives absolute numbers between groups + risk difference w/o cluster analysis
      simple.description =
        data.frame(
          z.ctrl = sum(z[n & gr %in% 0]),
          z.intv = sum(z[n & gr %in% 1]),
          n.ctrl = sum(n & gr %in% 0),
          n.intv = sum(n & gr %in% 1),
          
          r.ctrl = round((sum(z[n & gr %in% 0]))/(sum(n & gr %in% 0)), round.digits),
          r.intv = round((sum(z[n & gr %in% 1]))/(sum(n & gr %in% 1)), round.digits),
          
          d =round(((sum(z[n & gr %in% 1]))/(sum(n & gr %in% 1)))-((sum(z[n & gr %in% 0]))/(sum(n & gr %in% 0))), round.digits)
        ),
      
      # Same as above but prop.testing it w/o cluster analysis
      simple.proptest =
        prop.test(c(sum(z[n & gr %in% 0]),sum(z[n & gr %in% 1])),c(sum(n & gr %in% 0),sum(n & gr %in% 1))),
      
      # Computes ICCs
      ICCs = "..."
      #calc.icc.bin(z[n],rcl[n])
    )
  )
}







# This function takes a dataframe of covariates and (1) for factors appends dummy variables for each factor then removes the factor and (2) does nothing for the other variables
gee.factor <- function(covardf){
  
  #current.na.action <- options('na.action')
  options(na.action='na.pass')
  
  l.f = NULL
  for(i in 1:length(covardf)){
    if(is.factor(covardf[,i])){l.f = rbind(l.f, i)}
    if(is.logical(covardf[,i])){covardf[,i] = as.integer(covardf[,i])}
  }
  
  for(i in l.f){
    tempmat = model.matrix( ~ covardf[,i] -1)
    
    for(j in 1:length(colnames(tempmat))){
      colnames(tempmat)[j] <- paste(
        colnames(covardf)[i],
        str_split(colnames(tempmat),"]")[[j]][2],
        sep=""
      )
      
      tempmat[,j] = as.logical(tempmat[,j])
    }
    
    covardf = cbind(covardf, tempmat)
  }
  
  #options(na.action=current.na.action)
  options(na.action='na.exclude')
  
  return(covardf[, -l.f])
  
  
}


# Function to return only the col names
gee.factor.colnames <- function(covardf, remove.first){
  
  if(missing(remove.first)){
    remove.first = FALSE
  }
  
  out.colnames = NULL
  
  l.f = NULL
  for(i in 1:length(covardf)){
    if(is.factor(covardf[,i])){l.f = rbind(l.f, i)}
    if(is.logical(covardf[,i])){covardf[,i] = as.integer(covardf[,i])}
  }
  
  for(i in 1:length(covardf)){
    
    if(i %in% l.f){
      tempmat = model.matrix( ~ covardf[,i] -1)
      
      if(remove.first){tempmat = tempmat[ ,2:(length(colnames(tempmat)))]}
      
      for(j in 1:length(colnames(tempmat))){
        colnames(tempmat)[j] <- paste(
          colnames(covardf)[i],
          str_split(colnames(tempmat),"]")[[j]][2],
          sep=""
        )
      }
      
      out.colnames = c(out.colnames, colnames(tempmat))
    } else {
      out.colnames = c(out.colnames, colnames(covardf)[i])
    }
    
    
  }
  
  return(out.colnames)
}





len <- function(var_len){
  return(length(var_len))
}

tables <- function(table_var){
  return(table(table_var, exclude=NULL))
}

setdiffs <- function(vec1, vec2){
  return(sort(c(setdiff(vec1, vec2), setdiff(vec2, vec1))))
}

comparins <- function(vec1, vec2, vdigits){
  if(!missing(vdigits)){
    vec1[!is.na(vec1)] = round(vec1[!is.na(vec1)], vdigits)
    vec2[!is.na(vec2)] = round(vec2[!is.na(vec2)], vdigits)
    
  }
  
  
  
  df_to_mat <- function(dfm, dfm.digits){
    if(missing(dfm.digits)){dfm.digits = 4}
    
    zero.str = paste("0.",paste(rep("0",dfm.digits),sep="",collapse=""),sep="",collapse="")
    dfm.nb.mat = matrix(as.character(round(dfm, digits= dfm.digits)),nrow=length(dfm[,1]))
    dfm.nb.mat[!grepl("\\.",dfm.nb.mat)] = zero.str
    
    return(
      dfm.retval =
        rbind(
          c(
            "",
            colnames(dfm)
          ),
          cbind(
            rownames(dfm), 
            dfm.nb.mat
            #round(matrix(dfm,nrow=length(dfm[,1])), digits= dfm.digits)
          )
        )
    )
    
  }
  
  rbool <- function(n, p){
    return(runif(n,0,1)<p)
  }
  
  
  if((length(vec1)!=length(vec2)) | (typeof(vec1)!=typeof(vec2))){
    return(NA)
  } else {
    vecout = logical(length(vec1))*NA
    vecout[!is.na(vec1) & !is.na(vec2)] = as.logical(vec1[!is.na(vec1) & !is.na(vec2)] != vec2[!is.na(vec1) & !is.na(vec2)])
    vecout[is.na(vec1) & !is.na(vec2)] = TRUE
    vecout[!is.na(vec1) & is.na(vec2)] = TRUE
    vecout[is.na(vec1) & is.na(vec2)] = FALSE
    
    return(which(as.logical(vecout)))
  }
  
}

# second argument <len_inhead> optional (then defaults at 10)
heads <- function(inhead, len_inhead){
  if(missing(len_inhead)){len_inhead = 10}
  
  if(typeof(inhead)=="list"){
    return(inhead[1:len_inhead, ])
  } else {
    return(inhead[1:len_inhead])
  }
}

glm.apply <- function(glm.object, glm.data, glm.link, glm.digits){
  # Goal of function is to apply a dataset and to glm object regression to see result
  # difficult as sometimes some vars don't match so we remove them here and do with what we have
  
  # Get summary to extract coeffecients
  glm.object = summary(glm.object)
  
  # Get the intercept for further use then it is out of the way otherwise the object has coefficients 2:... and data frame from 1:...
  glm.intercept = glm.object$coefficients[1,]
  
  # Order all in same order (alphabetic)
  # Here this also removes the first line (ie intercept)
  glm.object$coefficients =
    rbind(
      glm.object$coefficients[2:len(glm.object$coefficients[,1]),][order(rownames(glm.object$coefficients[2:len(glm.object$coefficients[,1]),])),]
    )
  
  # Order dataframe
  glm.data = glm.data[,order(colnames(glm.data))]
  
  
  # Figure out which are missing ie which are in object and not data and concatenate it with the vice versa
  # we use rownames and colnames
  missing.vars = c(
    rownames(glm.object$coefficients)[!(rownames(glm.object$coefficients) %in% colnames(glm.data))],
    colnames(glm.data)[!(colnames(glm.data) %in% rownames(glm.object$coefficients))]
  )
  
  # Then clean the objects from missing vars
  glm.object$coefficients = glm.object$coefficients[!(rownames(glm.object$coefficients) %in% missing.vars), ]
  glm.data = glm.data[,!(colnames(glm.data) %in% missing.vars)]
  
  # Create a matrix with the data as this is much easier to process with the later apply fct
  mat.data = matrix(
    unlist(
      glm.data[1:(length(glm.data[,1])), 1:(length(glm.data[1,]))]
    ),
    nrow = length(glm.data[,1])
  )
  # Remove NAs and put 0 otherwise results will be full of NAs
  mat.data[is.na(mat.data)] = 0
  
  # Creat a matrix with the coefficients too (although will only use one vector)
  mat.coef = matrix(
    unlist(
      glm.object$coefficients[1:(length(glm.object$coefficients[,1])), 1:(length(glm.object$coefficients[1,]))]
    ),
    nrow = length(glm.object$coefficients[,1])
  )
  
  # Apply coefs to the dataframe matrix
  # and add the intercept
  glm.apply_results =
    apply(data.frame(1:length(mat.data[,1])),
          1,
          function(x) sum(mat.data[x,]*mat.coef[,1])
    ) + glm.intercept[1]
  
  # Apply link function if relevant - if nothing assumes by default it is identity
  if(!missing(glm.link)){
    if(glm.link == "logit"){
      glm.apply_results = exp(glm.apply_results) 
    }
  }
  
  # For rounding
  if(missing(glm.digits)){
    glm.digits = 4
  } 
  
  return(round(glm.apply_results, digits=glm.digits))
  
  
}

# Inventory of relevant variables

# Function to take a vector of integers and return a logical vectors with TRUE, FALSE or NA
# int_vector is the vector of integers to analyze
# means_yes is the integer value (or vector of thereof) meaning the values should be returned as TRUE
# means_no is the same as means_yes but for FALSE
# all integer values that are either NA or NOT included in c(means_yes, means_no) are then returned as NA
easy_bin <-function(int_vector, means_yes, means_no){
  
  bool_vector = as.logical(rep(NA, length(int_vector)))
  
  bool_vector[int_vector %in% means_yes] = TRUE
  bool_vector[int_vector %in% means_no] = FALSE
  
  return(bool_vector)
  
}


# Function to take a vector of integers and return the same vector, but with identified numbers as NAs
# int_vector is the vector of integers to analyze
# means_na is the integer value (or vector of therof) meanin the value is NA
# means_not_na is the integer value (or vector of therof) meaning the value is NOT NA and all other values will be NAed
# only provide either means_na OR means_not_na
easy_int <- function(int_vector, means_na, means_not_na){
  
  if(missing(means_na)==missing(means_not_na)){
    return(NA)
  } else {
    
    new_int_vector = int_vector
    
    if(missing(means_na)){
      new_int_vector[!(int_vector %in% means_not_na)] = NA
    } else {
      new_int_vector[int_vector %in% means_na] = NA
    }
    
  }
  
  return(new_int_vector)
}





