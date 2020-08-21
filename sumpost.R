pcs = round(t(matrix(c(0,-1.31872,-3.02396,-5.56461,-8.37399,NA,-7.23216,-3.45555,0,NA,NA,NA,-6.24397,-2.73557,0,NA,NA,NA,-4.61617,0,NA,NA,NA,NA,-5.51747,0,NA,NA,NA,NA,3.04365,0,NA,NA,NA,NA,2.32091,0,NA,NA,NA,NA,0,-3.80130,-6.50522,-8.38063,-11.25544,NA,0,0.66514,1.36689,2.37241,2.90426,3.46638,0,-0.42251,-1.14387,-1.61850,-2.02168,-2.44706,4.61446,3.41593,2.34247,1.28044,0.41188,0,-0.33682,-0.94342,-0.18043,0.11038,0,NA),nrow=6,ncol=12)),digits=2)
mcs = round(t(matrix(c(0,-0.06064,0.03482,-0.16891,-1.71175,NA,3.93115,1.86840,0,NA,NA,NA,2.68282,1.43103,0,NA,NA,NA,1.44060,0,NA,NA,NA,NA,1.66968,0,NA,NA,NA,NA,-6.82672,0,NA,NA,NA,NA,-5.69921,0,NA,NA,NA,NA,0,0.90384,1.49384,1.76691,1.48619,NA,0,-1.94949,-4.09842,-6.31121,-7.92717,-10.19085,0,-0.92057,-1.65178,-3.29805,-4.88962,-6.02409,-16.15395,-10.77911,-8.09914,-4.59055,-1.95934,0,-6.29724,-8.26066,-5.63286,-3.13896,0,NA),nrow=6,ncol=12)),digits=2)
d6 = round(t(matrix(c(0,0,-0.045,NA,NA,0,-0.063,-0.063,-0.063,NA,0,0,-0.042,-0.077,-0.137,0,-0.078,-0.078,-0.078,-0.106,0,-0.059,-0.059,-0.113,-0.134,0,-0.063,-0.066,-0.081,-0.093),nrow=5,ncol=6)),digits=3)


# This function formats the dataset : it switches order of some variables such that all are ordered where a larger number is worse
# also for whatever reason, in the dataset, question 12 has 6 answers although in PCS/MCS, there should be only 5, so we remove "a good bit of time" answer
# for yes/no questions we put a regular scale [1,2] instead of [0,1]
# and for all questions, if there is an answer over its maximum number of possible answers, then we get "NA"; for Q1, range is 1-5, but there might be a 6 (I don't answer) or 999 (no answer) which are coded as NA
# The dataset format should be the following :
# ENTRY_ID(integer/long identifier)|AGE(integer, no NA)|SF_Q1(integer)|SF_Q2(integer)|...|SF_Q12(integer)
# of note, SF_QY should be integers and not NAs (NA can be coded in original dataset as 999), they will be transformed here in NA if appropriate
# Finally, it computes the SF-6D transformed numbers in $d6
db_format <- function(db_format_list){
  repl = length(db_format_list$ENTRY_ID); na_repl = rep(NA, repl)
  # this is just an empty vector of length of the other vectors of the dataset
  
  # Create 6D matrix
  db_format_list$d6 = NULL
  db_format_list$d6 = rbind(db_format_list$d6,array(0,dim=c(length(db_format_list$ENTRY_ID),6)))
  
  
  # First, make that higher number means worse
  db_format_list$SF_Q2[db_format_list$SF_Q2<4] = 4 - db_format_list$SF_Q2[db_format_list$SF_Q2<4]
  db_format_list$SF_Q3[db_format_list$SF_Q3<4] = 4 - db_format_list$SF_Q3[db_format_list$SF_Q3<4]
  db_format_list$SF_Q9 = db_format_list$SF_Q9
  db_format_list$SF_Q10 = db_format_list$SF_Q10
  db_format_list$SF_Q11[db_format_list$SF_Q11<7] = 7 - db_format_list$SF_Q11[db_format_list$SF_Q11<7]
  db_format_list$SF_Q12[db_format_list$SF_Q12<7] = 7 - db_format_list$SF_Q12[db_format_list$SF_Q12<7]
  
  # Second, in Q12, remove the "a good bit of time" category and replace by "some"
  db_format_list$SF_Q12[(db_format_list$SF_Q12>3)&(db_format_list$SF_Q12<7)]=db_format_list$SF_Q12[(db_format_list$SF_Q12>3)&(db_format_list$SF_Q12<7)]-1
  
  # Third, change scale for Q4-Q7 to (1,2) instead of (0,1)
  db_format_list$SF_Q4 = db_format_list$SF_Q4+1
  db_format_list$SF_Q5 = db_format_list$SF_Q5+1
  db_format_list$SF_Q6 = db_format_list$SF_Q6+1
  db_format_list$SF_Q7 = db_format_list$SF_Q7+1
  
  # Fourth, fill in d6 matrix 
  # d6[,1] or PF is Q2 w/o transformation 
  db_format_list$d6[,1] <- db_format_list$SF_Q2
  
  # d6[,2] is for  particular : combo of Q5 and Q6
  # keeping the value of Q5 (1,2,3,4,1000) and multiplying by 5 the value of Q6 (5,10,15,20,5000)
  # gives 25 different possibilities : 6, 7, 8, 9, 11, 12, 13, 14, 16, 17, 18, 19, 21, 22, 23, 24, 1005, 1010, 1015, 1020, 5001, 5002, 5003, 5004, 6000
  # so 6 is like (1,1), 7 is like (2,1), 11 like (1,2) and 12 like (2,2) for points 1-2-3-4; if after these transformations
  # its bigger than this max point of 4 then it has to be NAed (see lower)
  db_format_list$d6[,2] <- db_format_list$SF_Q5 + (5*db_format_list$SF_Q6)
  db_format_list$d6[db_format_list$d6[,2]==6,2] <- 1
  db_format_list$d6[db_format_list$d6[,2]==7,2] <- 2
  db_format_list$d6[db_format_list$d6[,2]==11,2] <- 3
  db_format_list$d6[db_format_list$d6[,2]==12,2] <- 4
  
  # d6[,3] or PA is Q8 w/o transformation 
  db_format_list$d6[,3] <- db_format_list$SF_Q8
  
  # d6[,4] and d6[,5] or VI and MH are like Q10 and Q11 but with transformation : valid scores from 4 to 6 must be 
  # decreased by 1 (like in Q12 for MCS/PCS, where "a good bit of time" is removed)
  db_format_list$d6[,4] <- db_format_list$SF_Q10
  db_format_list$d6[,5] <- db_format_list$SF_Q11
  db_format_list$d6[((db_format_list$d6[,4]>3)&(db_format_list$d6[,4]<7)),4]=db_format_list$d6[((db_format_list$d6[,4]>3)&(db_format_list$d6[,4]<7)),4]-1
  db_format_list$d6[((db_format_list$d6[,5]>3)&(db_format_list$d6[,5]<7)),5]=db_format_list$d6[((db_format_list$d6[,5]>3)&(db_format_list$d6[,5]<7)),5]-1
  
  # d6[,6] or PA is Q12 w/o transformation 
  db_format_list$d6[,6] <- db_format_list$SF_Q12
  
  
  
  # Put NA for NA
  db_format_list$AGE[db_format_list$AGE==999] = mean(db_format_list$AGE[!is.na(db_format_list$AGE)]);db_format_list$AGE=as.integer(db_format_list$AGE)
  db_format_list$SF_Q1[db_format_list$SF_Q1>5] = NA
  db_format_list$SF_Q2[db_format_list$SF_Q2>3] = NA
  db_format_list$SF_Q3[db_format_list$SF_Q3>3] = NA
  db_format_list$SF_Q4[db_format_list$SF_Q4>2] = NA
  db_format_list$SF_Q5[db_format_list$SF_Q5>2] = NA
  db_format_list$SF_Q6[db_format_list$SF_Q6>2] = NA
  db_format_list$SF_Q7[db_format_list$SF_Q7>2] = NA
  db_format_list$SF_Q8[db_format_list$SF_Q8>5] = NA
  db_format_list$SF_Q9[db_format_list$SF_Q9>6] = NA
  db_format_list$SF_Q10[db_format_list$SF_Q10>6] = NA
  db_format_list$SF_Q11[db_format_list$SF_Q11>6] = NA
  db_format_list$SF_Q12[db_format_list$SF_Q12>5] = NA
  
  db_format_list$d6[db_format_list$d6[,1]>3,1] <- NA
  db_format_list$d6[db_format_list$d6[,2]>4,2] <- NA
  db_format_list$d6[db_format_list$d6[,3]>5,3] <- NA
  db_format_list$d6[db_format_list$d6[,4]>5,4] <- NA
  db_format_list$d6[db_format_list$d6[,5]>5,5] <- NA
  db_format_list$d6[db_format_list$d6[,6]>5,6] <- NA
  
  return(db_format_list)
}


# Number of NAs : this function calculates the number of NAs in SF_Q answers and in XCS_Q answers
# for each SF_Q answer that is NA, $nas_SF12 is incremented of 1
# for each XCS_Q (PCS_Q vs MCS_Q) answer that is NA, $nas_XCS is incremented of 1
# they should indeed be identical for a "true" dataset, but in imputation datasets, they might be differ during computations
no_of_nas <- function(n_o_n){
  repl = length(n_o_n$ENTRY_ID); na_repl = rep(NA, repl)
  # empty vector of length of other vectors of that object to create same-length new vectors
  
  n_o_n$nas_SF12 = rep(0, repl)
  n_o_n$nas_SF12[is.na(n_o_n$SF_Q1)] = n_o_n$nas_SF12[is.na(n_o_n$SF_Q1)] + 1
  n_o_n$nas_SF12[is.na(n_o_n$SF_Q2)] = n_o_n$nas_SF12[is.na(n_o_n$SF_Q2)] + 1
  n_o_n$nas_SF12[is.na(n_o_n$SF_Q3)] = n_o_n$nas_SF12[is.na(n_o_n$SF_Q3)] + 1
  n_o_n$nas_SF12[is.na(n_o_n$SF_Q4)] = n_o_n$nas_SF12[is.na(n_o_n$SF_Q4)] + 1
  n_o_n$nas_SF12[is.na(n_o_n$SF_Q5)] = n_o_n$nas_SF12[is.na(n_o_n$SF_Q5)] + 1
  n_o_n$nas_SF12[is.na(n_o_n$SF_Q6)] = n_o_n$nas_SF12[is.na(n_o_n$SF_Q6)] + 1
  n_o_n$nas_SF12[is.na(n_o_n$SF_Q7)] = n_o_n$nas_SF12[is.na(n_o_n$SF_Q7)] + 1
  n_o_n$nas_SF12[is.na(n_o_n$SF_Q8)] = n_o_n$nas_SF12[is.na(n_o_n$SF_Q8)] + 1
  n_o_n$nas_SF12[is.na(n_o_n$SF_Q9)] = n_o_n$nas_SF12[is.na(n_o_n$SF_Q9)] + 1
  n_o_n$nas_SF12[is.na(n_o_n$SF_Q10)] = n_o_n$nas_SF12[is.na(n_o_n$SF_Q10)] + 1
  n_o_n$nas_SF12[is.na(n_o_n$SF_Q11)] = n_o_n$nas_SF12[is.na(n_o_n$SF_Q11)] + 1
  n_o_n$nas_SF12[is.na(n_o_n$SF_Q12)] = n_o_n$nas_SF12[is.na(n_o_n$SF_Q12)] + 1
  
  n_o_n$nas_PCS = rep(0, repl);n_o_n$nas_MCS = rep(0, repl)
  n_o_n$nas_PCS[is.na(n_o_n$PCS_Q1)] = n_o_n$nas_PCS[is.na(n_o_n$PCS_Q1)] + 1
  n_o_n$nas_PCS[is.na(n_o_n$PCS_Q2)] = n_o_n$nas_PCS[is.na(n_o_n$PCS_Q2)] + 1
  n_o_n$nas_PCS[is.na(n_o_n$PCS_Q3)] = n_o_n$nas_PCS[is.na(n_o_n$PCS_Q3)] + 1
  n_o_n$nas_PCS[is.na(n_o_n$PCS_Q4)] = n_o_n$nas_PCS[is.na(n_o_n$PCS_Q4)] + 1
  n_o_n$nas_PCS[is.na(n_o_n$PCS_Q5)] = n_o_n$nas_PCS[is.na(n_o_n$PCS_Q5)] + 1
  n_o_n$nas_PCS[is.na(n_o_n$PCS_Q6)] = n_o_n$nas_PCS[is.na(n_o_n$PCS_Q6)] + 1
  n_o_n$nas_PCS[is.na(n_o_n$PCS_Q7)] = n_o_n$nas_PCS[is.na(n_o_n$PCS_Q7)] + 1
  n_o_n$nas_PCS[is.na(n_o_n$PCS_Q8)] = n_o_n$nas_PCS[is.na(n_o_n$PCS_Q8)] + 1
  n_o_n$nas_PCS[is.na(n_o_n$PCS_Q9)] = n_o_n$nas_PCS[is.na(n_o_n$PCS_Q9)] + 1
  n_o_n$nas_PCS[is.na(n_o_n$PCS_Q10)] = n_o_n$nas_PCS[is.na(n_o_n$PCS_Q10)] + 1
  n_o_n$nas_PCS[is.na(n_o_n$PCS_Q11)] = n_o_n$nas_PCS[is.na(n_o_n$PCS_Q11)] + 1
  n_o_n$nas_PCS[is.na(n_o_n$PCS_Q12)] = n_o_n$nas_PCS[is.na(n_o_n$PCS_Q12)] + 1
  n_o_n$nas_MCS = n_o_n$nas_PCS
  
  
  n_o_n$nas_sf6d = rep(0, repl);n_o_n$nas_s6d = rep(0, repl)
  for(w in 1:6){n_o_n$nas_sf6d[is.na(n_o_n$d6[,w])] = n_o_n$nas_sf6d[is.na(n_o_n$d6[,w])] + 1}
  for(w in 1:6){n_o_n$nas_s6d[is.na(n_o_n$d6c[,w])] = n_o_n$nas_s6d[is.na(n_o_n$d6c[,w])] + 1}
  
  return(n_o_n)
}




# Computation : computes PCS and MCS scores for a given data set
# options :   - compute_singles : replaces all subscores ($XCS_QY) with NA then assigns them either a number based on $SF_QY score or NA if that $SF_QY is NA
#             - compute_tot : replaces $XCS_tot and $XCS_tot_with_na with NA, then computes the total for PCS and MCS ($XCS_tot) which will be NA if one of the $XCS_Q is NA; the $XCS_tot_with_na is a score computed for all available and will not be NA (unless all 12 items are NA)
#             - reset_all : since compute_singles only resets single subscores ($XCS_QY) and compute_tot replaces only total scores, this option allows to reset all if only one of compute_singles or compute_tot is used
compute_cs <- function(c_cs,compute_singles,compute_tot,reset_all){
  repl = length(c_cs$ENTRY_ID); na_repl = rep(NA, repl)
  # empty vector of length of other vectors to create new vectors
  
  # this fcts resets all to NA (XCS_tot and XCS_QY) 
  if(reset_all == TRUE){
    c_cs$PCS_tot=na_repl;c_cs$PCS_Q1=na_repl;c_cs$PCS_Q2=na_repl;c_cs$PCS_Q3=na_repl;c_cs$PCS_Q4=na_repl;c_cs$PCS_Q5=na_repl;c_cs$PCS_Q6=na_repl;c_cs$PCS_Q7=na_repl;c_cs$PCS_Q8=na_repl;c_cs$PCS_Q9=na_repl;c_cs$PCS_Q10=na_repl;c_cs$PCS_Q11=na_repl;c_cs$PCS_Q12=na_repl
    c_cs$MCS_tot=na_repl;c_cs$MCS_Q1=na_repl;c_cs$MCS_Q2=na_repl;c_cs$MCS_Q3=na_repl;c_cs$MCS_Q4=na_repl;c_cs$MCS_Q5=na_repl;c_cs$MCS_Q6=na_repl;c_cs$MCS_Q7=na_repl;c_cs$MCS_Q8=na_repl;c_cs$MCS_Q9=na_repl;c_cs$MCS_Q10=na_repl;c_cs$MCS_Q11=na_repl;c_cs$MCS_Q12=na_repl
    c_cs$d6_tot=na_repl;c_cs$d6_tot_nomax=na_repl;c_cs$d6c = NULL;c_cs$d6c = rbind(c_cs$d6c,array(0,dim=c(length(c_cs$ENTRY_ID),6)));c_cs$d6c[c_cs$d6c==0] = NA
  }
  
  # resets then computes individual results (XCS_QY) from the pre-defined PCS/MCS matrices
  if(compute_singles == TRUE){
    c_cs$PCS_Q1=na_repl;c_cs$PCS_Q2=na_repl;c_cs$PCS_Q3=na_repl;c_cs$PCS_Q4=na_repl;c_cs$PCS_Q5=na_repl;c_cs$PCS_Q6=na_repl;c_cs$PCS_Q7=na_repl;c_cs$PCS_Q8=na_repl;c_cs$PCS_Q9=na_repl;c_cs$PCS_Q10=na_repl;c_cs$PCS_Q11=na_repl;c_cs$PCS_Q12=na_repl;c_cs$PCS_tot_with_na=na_repl
    c_cs$PCS_Q1[!is.na(c_cs$SF_Q1)]=pcs[1,c_cs$SF_Q1[!is.na(c_cs$SF_Q1)]]
    c_cs$PCS_Q2[!is.na(c_cs$SF_Q2)]=pcs[2,c_cs$SF_Q2[!is.na(c_cs$SF_Q2)]]
    c_cs$PCS_Q3[!is.na(c_cs$SF_Q3)]=pcs[3,c_cs$SF_Q3[!is.na(c_cs$SF_Q3)]]
    c_cs$PCS_Q4[!is.na(c_cs$SF_Q4)]=pcs[4,c_cs$SF_Q4[!is.na(c_cs$SF_Q4)]]
    c_cs$PCS_Q5[!is.na(c_cs$SF_Q5)]=pcs[5,c_cs$SF_Q5[!is.na(c_cs$SF_Q5)]]
    c_cs$PCS_Q6[!is.na(c_cs$SF_Q6)]=pcs[6,c_cs$SF_Q6[!is.na(c_cs$SF_Q6)]]
    c_cs$PCS_Q7[!is.na(c_cs$SF_Q7)]=pcs[7,c_cs$SF_Q7[!is.na(c_cs$SF_Q7)]]
    c_cs$PCS_Q8[!is.na(c_cs$SF_Q8)]=pcs[8,c_cs$SF_Q8[!is.na(c_cs$SF_Q8)]]
    c_cs$PCS_Q9[!is.na(c_cs$SF_Q9)]=pcs[9,c_cs$SF_Q9[!is.na(c_cs$SF_Q9)]]
    c_cs$PCS_Q10[!is.na(c_cs$SF_Q10)]=pcs[10,c_cs$SF_Q10[!is.na(c_cs$SF_Q10)]]
    c_cs$PCS_Q11[!is.na(c_cs$SF_Q11)]=pcs[11,c_cs$SF_Q11[!is.na(c_cs$SF_Q11)]]
    c_cs$PCS_Q12[!is.na(c_cs$SF_Q12)]=pcs[12,c_cs$SF_Q12[!is.na(c_cs$SF_Q12)]]
    
    c_cs$MCS_Q1=na_repl;c_cs$MCS_Q2=na_repl;c_cs$MCS_Q3=na_repl;c_cs$MCS_Q4=na_repl;c_cs$MCS_Q5=na_repl;c_cs$MCS_Q6=na_repl;c_cs$MCS_Q7=na_repl;c_cs$MCS_Q8=na_repl;c_cs$MCS_Q9=na_repl;c_cs$MCS_Q10=na_repl;c_cs$MCS_Q11=na_repl;c_cs$MCS_Q12=na_repl;c_cs$MCS_tot_with_na=na_repl
    c_cs$MCS_Q1[!is.na(c_cs$SF_Q1)]=mcs[1,c_cs$SF_Q1[!is.na(c_cs$SF_Q1)]]
    c_cs$MCS_Q2[!is.na(c_cs$SF_Q2)]=mcs[2,c_cs$SF_Q2[!is.na(c_cs$SF_Q2)]]
    c_cs$MCS_Q3[!is.na(c_cs$SF_Q3)]=mcs[3,c_cs$SF_Q3[!is.na(c_cs$SF_Q3)]]
    c_cs$MCS_Q4[!is.na(c_cs$SF_Q4)]=mcs[4,c_cs$SF_Q4[!is.na(c_cs$SF_Q4)]]
    c_cs$MCS_Q5[!is.na(c_cs$SF_Q5)]=mcs[5,c_cs$SF_Q5[!is.na(c_cs$SF_Q5)]]
    c_cs$MCS_Q6[!is.na(c_cs$SF_Q6)]=mcs[6,c_cs$SF_Q6[!is.na(c_cs$SF_Q6)]]
    c_cs$MCS_Q7[!is.na(c_cs$SF_Q7)]=mcs[7,c_cs$SF_Q7[!is.na(c_cs$SF_Q7)]]
    c_cs$MCS_Q8[!is.na(c_cs$SF_Q8)]=mcs[8,c_cs$SF_Q8[!is.na(c_cs$SF_Q8)]]
    c_cs$MCS_Q9[!is.na(c_cs$SF_Q9)]=mcs[9,c_cs$SF_Q9[!is.na(c_cs$SF_Q9)]]
    c_cs$MCS_Q10[!is.na(c_cs$SF_Q10)]=mcs[10,c_cs$SF_Q10[!is.na(c_cs$SF_Q10)]]
    c_cs$MCS_Q11[!is.na(c_cs$SF_Q11)]=mcs[11,c_cs$SF_Q11[!is.na(c_cs$SF_Q11)]]
    c_cs$MCS_Q12[!is.na(c_cs$SF_Q12)]=mcs[12,c_cs$SF_Q12[!is.na(c_cs$SF_Q12)]]
    
    c_cs$d6c = NULL;c_cs$d6c = rbind(c_cs$d6c,array(0,dim=c(length(c_cs$ENTRY_ID),6)));c_cs$d6c[c_cs$d6c==0] = NA
    for(w in 1:6){c_cs$d6c[!is.na(c_cs$d6[,w]),w]=d6[w,c_cs$d6[!is.na(c_cs$d6[,w]),w]]}
  }
  
  # This resets then computes the total vectors
  if(compute_tot == TRUE){
    # Reset
    c_cs$PCS_tot=na_repl;c_cs$PCS_tot_with_na=na_repl
    c_cs$MCS_tot=na_repl;c_cs$MCS_tot_with_na=na_repl
    c_cs$d6_tot=na_repl;c_cs$d6_tot_nomax=na_repl;c_cs$d6_tot_with_na=na_repl
    
    # $XCS_tot is easy : the initial score, then adding each of the individual $XCS_QY score; if one is NA, then the $XCS_tot automatically is returned NA (that is when no_of_nas is not 0)
    
    c_cs = no_of_nas(c_cs)
    
    c_cs$PCS_tot[c_cs$nas_PCS==0]= round(56.57706,digits=2) + c_cs$PCS_Q1[c_cs$nas_PCS==0] + c_cs$PCS_Q2[c_cs$nas_PCS==0] + c_cs$PCS_Q3[c_cs$nas_PCS==0] + c_cs$PCS_Q4[c_cs$nas_PCS==0] + c_cs$PCS_Q5[c_cs$nas_PCS==0] + c_cs$PCS_Q6[c_cs$nas_PCS==0] + c_cs$PCS_Q7[c_cs$nas_PCS==0] + c_cs$PCS_Q8[c_cs$nas_PCS==0] + c_cs$PCS_Q9[c_cs$nas_PCS==0] + c_cs$PCS_Q10[c_cs$nas_PCS==0] + c_cs$PCS_Q11[c_cs$nas_PCS==0] + c_cs$PCS_Q12[c_cs$nas_PCS==0]
    c_cs$MCS_tot[c_cs$nas_MCS==0]= round(60.7675781,digits=2) + c_cs$MCS_Q1[c_cs$nas_MCS==0] + c_cs$MCS_Q2[c_cs$nas_MCS==0] + c_cs$MCS_Q3[c_cs$nas_MCS==0] + c_cs$MCS_Q4[c_cs$nas_MCS==0] + c_cs$MCS_Q5[c_cs$nas_MCS==0] + c_cs$MCS_Q6[c_cs$nas_MCS==0] + c_cs$MCS_Q7[c_cs$nas_MCS==0] + c_cs$MCS_Q8[c_cs$nas_MCS==0] + c_cs$MCS_Q9[c_cs$nas_MCS==0] + c_cs$MCS_Q10[c_cs$nas_MCS==0] + c_cs$MCS_Q11[c_cs$nas_MCS==0] + c_cs$MCS_Q12[c_cs$nas_MCS==0]
    c_cs$d6_tot[c_cs$nas_sf6d==0]= 1 + c_cs$d6c[c_cs$nas_sf6d==0,1] + c_cs$d6c[c_cs$nas_sf6d==0,2] + c_cs$d6c[c_cs$nas_sf6d==0,3] + c_cs$d6c[c_cs$nas_sf6d==0,4] + c_cs$d6c[c_cs$nas_sf6d==0,5] + c_cs$d6c[c_cs$nas_sf6d==0,6]
    #c_cs$d6_tot_nomax[c_cs$nas_s6d==0]= 1 + c_cs$d6c[c_cs$nas_sf6d==0,1] + c_cs$d6c[c_cs$nas_sf6d==0,2] + c_cs$d6c[c_cs$nas_sf6d==0,3] + c_cs$d6c[c_cs$nas_sf6d==0,4] + c_cs$d6c[c_cs$nas_sf6d==0,5] + c_cs$d6c[c_cs$nas_sf6d==0,6]
    
    for(w in 1:length(c_cs$AGE)){
      if((max6d(c_cs$d6[w,])==1)&&(c_cs$nas_sf6d[w]==0)){
        c_cs$d6_tot[w] = c_cs$d6_tot[w] - 0.077
      }
    }
    
    # Compute regardless of NAs : initially the vector ($XCS_tot_with_na) is set to the initial value (57 vs 61 for PCS, MCS, respectively)
    # then for each subscore $XCS_QY, if it is not NA, then the $XCS_tot_with_na is incremented for that score
    for(i in 1:length(c_cs$PCS_tot_with_na)){
      c_cs$PCS_tot_with_na[i] = round(56.57706,digits=2)
      if(!is.na(c_cs$PCS_Q1[i])){c_cs$PCS_tot_with_na[i]=c_cs$PCS_tot_with_na[i]+c_cs$PCS_Q1[i]}
      if(!is.na(c_cs$PCS_Q2[i])){c_cs$PCS_tot_with_na[i]=c_cs$PCS_tot_with_na[i]+c_cs$PCS_Q2[i]}
      if(!is.na(c_cs$PCS_Q3[i])){c_cs$PCS_tot_with_na[i]=c_cs$PCS_tot_with_na[i]+c_cs$PCS_Q3[i]}
      if(!is.na(c_cs$PCS_Q4[i])){c_cs$PCS_tot_with_na[i]=c_cs$PCS_tot_with_na[i]+c_cs$PCS_Q4[i]}
      if(!is.na(c_cs$PCS_Q5[i])){c_cs$PCS_tot_with_na[i]=c_cs$PCS_tot_with_na[i]+c_cs$PCS_Q5[i]}
      if(!is.na(c_cs$PCS_Q6[i])){c_cs$PCS_tot_with_na[i]=c_cs$PCS_tot_with_na[i]+c_cs$PCS_Q6[i]}
      if(!is.na(c_cs$PCS_Q7[i])){c_cs$PCS_tot_with_na[i]=c_cs$PCS_tot_with_na[i]+c_cs$PCS_Q7[i]}
      if(!is.na(c_cs$PCS_Q8[i])){c_cs$PCS_tot_with_na[i]=c_cs$PCS_tot_with_na[i]+c_cs$PCS_Q8[i]}
      if(!is.na(c_cs$PCS_Q9[i])){c_cs$PCS_tot_with_na[i]=c_cs$PCS_tot_with_na[i]+c_cs$PCS_Q9[i]}
      if(!is.na(c_cs$PCS_Q10[i])){c_cs$PCS_tot_with_na[i]=c_cs$PCS_tot_with_na[i]+c_cs$PCS_Q10[i]}
      if(!is.na(c_cs$PCS_Q11[i])){c_cs$PCS_tot_with_na[i]=c_cs$PCS_tot_with_na[i]+c_cs$PCS_Q11[i]}
      if(!is.na(c_cs$PCS_Q12[i])){c_cs$PCS_tot_with_na[i]=c_cs$PCS_tot_with_na[i]+c_cs$PCS_Q12[i]}
      
      c_cs$MCS_tot_with_na[i] = round(60.7675781,digits=2)
      if(!is.na(c_cs$MCS_Q1[i])){c_cs$MCS_tot_with_na[i]=c_cs$MCS_tot_with_na[i]+c_cs$MCS_Q1[i]}
      if(!is.na(c_cs$MCS_Q2[i])){c_cs$MCS_tot_with_na[i]=c_cs$MCS_tot_with_na[i]+c_cs$MCS_Q2[i]}
      if(!is.na(c_cs$MCS_Q3[i])){c_cs$MCS_tot_with_na[i]=c_cs$MCS_tot_with_na[i]+c_cs$MCS_Q3[i]}
      if(!is.na(c_cs$MCS_Q4[i])){c_cs$MCS_tot_with_na[i]=c_cs$MCS_tot_with_na[i]+c_cs$MCS_Q4[i]}
      if(!is.na(c_cs$MCS_Q5[i])){c_cs$MCS_tot_with_na[i]=c_cs$MCS_tot_with_na[i]+c_cs$MCS_Q5[i]}
      if(!is.na(c_cs$MCS_Q6[i])){c_cs$MCS_tot_with_na[i]=c_cs$MCS_tot_with_na[i]+c_cs$MCS_Q6[i]}
      if(!is.na(c_cs$MCS_Q7[i])){c_cs$MCS_tot_with_na[i]=c_cs$MCS_tot_with_na[i]+c_cs$MCS_Q7[i]}
      if(!is.na(c_cs$MCS_Q8[i])){c_cs$MCS_tot_with_na[i]=c_cs$MCS_tot_with_na[i]+c_cs$MCS_Q8[i]}
      if(!is.na(c_cs$MCS_Q9[i])){c_cs$MCS_tot_with_na[i]=c_cs$MCS_tot_with_na[i]+c_cs$MCS_Q9[i]}
      if(!is.na(c_cs$MCS_Q10[i])){c_cs$MCS_tot_with_na[i]=c_cs$MCS_tot_with_na[i]+c_cs$MCS_Q10[i]}
      if(!is.na(c_cs$MCS_Q11[i])){c_cs$MCS_tot_with_na[i]=c_cs$MCS_tot_with_na[i]+c_cs$MCS_Q11[i]}
      if(!is.na(c_cs$MCS_Q12[i])){c_cs$MCS_tot_with_na[i]=c_cs$MCS_tot_with_na[i]+c_cs$MCS_Q12[i]}
      
      c_cs$d6_tot_with_na[i] = 1
      for(w in 1:6){
        if(!is.na(c_cs$d6c[i,w])){c_cs$d6_tot_with_na[i]=c_cs$d6_tot_with_na[i]+c_cs$d6c[i,w]}
      }
      if(max6d(c_cs$d6[i,])==1){
        c_cs$d6_tot_with_na[i] = c_cs$d6_tot_with_na[i] - 0.077
      }
    }
  }
  return(c_cs)
}

max6d <- function(max6dv){
  max6db = FALSE
  maxv = c(3,4,5,5,5,5)
  for(zz in 1:6){
    if(!is.na(max6dv[zz])){
      if(max6dv[zz]==maxv[zz]){
        max6db=TRUE
      }
    }
  }
  if(max6db==TRUE){return(1)}
  if(max6db==FALSE){return(0)}
}      

# Small function where a variable (max 1 index) is imputed and what is outputed is a series of strings for all of these variables that are NA
# option vnameyn is set to 0 if the name inputed is the same to be returned, otherwise that name is changed by vname
# say we have datalist$a which is a 1 dimension array datalist$a[1:12] = c(NA,1,2,1,1,NA,NA,1,NA,1,2,1)
# then pNA(datalist$a,1,"a") would return only the NA indices : c("a[1]","a[6]","a[7]","a[9]")
# this is usefull for a large dataset where most values are known but some aren't that is put into JAGS in order to only monitor stochastic variables
# which reduces considerably the CODA file size while not destroying useful information (it doesn't monitor known, non stochastic, variables)
pNA <- function(pNA_var,vnameyn,vname){
  if(length(pNA_var[is.na(pNA_var)])==0){
    return()
  } else {
    if(vnameyn==1){return(gsub(" ","",paste(deparse(substitute(pNA_var)),"[",as.character(which(pNA_var %in% NA)),"]")))}
    else{return(gsub(" ","",paste(vname,"[",as.character(which(pNA_var %in% NA)),"]")))}
  }
}

# This function creates a vector V of length len such as sum(V) = 1; it serves as a fake probability vector for categorial data 
iprior <- function(len){
  i.p <- rep(0,length(len))
  i.pr <- i.p
  for(zz in 1:len){
    i.p[zz] <- abs(rnorm(1,50,50))
  }
  for(zz in 1:len){
    i.pr[zz] <- i.p[zz] / sum(i.p)
  }
  return(i.pr)
}

# Same as pNA function actually
pNA2 <- function(pNA_var,vnameyn,vname){
  if(length(pNA_var[is.na(pNA_var)])==0){
    return()
  } else {
    if(vnameyn==1){return(gsub(" ","",paste(deparse(substitute(pNA_var)),"[",as.character(which(pNA_var %in% NA)),"]")))}
    else{return(gsub(" ","",paste(vname,as.character(which(pNA_var %in% NA)),"]")))}
  }
}

# Here is were we extract the parameters that were found with MCMC simulation through JAGS to reuse them later for actually computing missing data
# This function computes the posterior summary of ONE given parameter eg q[2,10] - it is much faster than running summary(csa_rjo.mcmc)
# comes from DDABE-utils.R file from John Krushke; I only amended it to remove some of the returned info (too many details)
# and to return a string (the name of the variable)
sumPost = function( name_var, paramSampleVec , 
                    compVal=NULL , ROPE=NULL , credMass=0.95 ) {
  meanParam = mean( paramSampleVec )
  medianParam = median( paramSampleVec )
  dres = density( paramSampleVec )
  modeParam = dres$x[which.max(dres$y)]
  mcmcEffSz = round( effectiveSize( paramSampleVec ) , 1 )
  names(mcmcEffSz) = NULL
  hdiLim = HDIofMCMC( paramSampleVec , credMass=credMass )
  if ( !is.null(compVal) ) {
    pcgtCompVal = ( 100 * sum( paramSampleVec > compVal ) 
                    / length( paramSampleVec ) )
  } else {
    compVal=NA
    pcgtCompVal=NA
  }
  if ( !is.null(ROPE) ) {
    pcltRope = ( 100 * sum( paramSampleVec < ROPE[1] ) 
                 / length( paramSampleVec ) )
    pcgtRope = ( 100 * sum( paramSampleVec > ROPE[2] ) 
                 / length( paramSampleVec ) )
    pcinRope = 100-(pcltRope+pcgtRope)
  } else { 
    ROPE = c(NA,NA)
    pcltRope=NA 
    pcgtRope=NA 
    pcinRope=NA 
  }  
  return( c( NameOfVar=name_var, Mean=meanParam , Median=medianParam , Mode=modeParam , 
             HDImass=credMass , HDIlow=hdiLim[1] , HDIhigh=hdiLim[2]  ) )
}

HDIofMCMC = function( sampleVec , credMass=0.95 ) {
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   sampleVec
  #     is a vector of representative values from a probability distribution.
  #   credMass
  #     is a scalar between 0 and 1, indicating the mass within the credible
  #     interval that is to be estimated.
  # Value:
  #   HDIlim is a vector containing the limits of the HDI
  sortedPts = sort( sampleVec )
  ciIdxInc = ceiling( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}