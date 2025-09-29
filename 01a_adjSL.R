# Preparation #######

library(Boruta); library(caret); library(dplyr); library(MASS); library(partykit); library(pdp); library(randomForest); library(tree)

## Load Data ##########
x <- read.delim(
  file="01a_adj.txt",
  stringsAsFactors=TRUE,
  dec=','
)

## Persistence ##########
# positive values show that the previous comparison was *analytic* and negative values show that it was *synthetic*
# the closer to +/-1 the closer the previous comparison

x$PERSIST_DIST[x$PERSIST_DIST=="none"] <- NA
x$PERSIST_DIST <- as.numeric(as.character(x$PERSIST_DIST))
x$PERSISTENCE <- -(1-(x$PERSIST_DIST/1000))
x$PERSISTENCE[is.na(x$PERSISTENCE)] <- 0
x$PERSISTENCE[x$PERSIST_DIST>1000] <- 0
x$PERSISTENCE[x$PERSIST_COMP=='analytic'] <- -x$PERSISTENCE[x$PERSIST_COMP=='analytic']

# PERSISTENCE now only includes those cases within 1,000 characters preceding the current adjective
# adjust the PERSIST_FORM variable accordingly to only include adjectives preceding within 1,000 characters
x$PERSIST_FORM.1000 <- x$PERSIST_FORM
x$PERSIST_FORM.1000[x$PERSIST_DIST > 1000] <- 'none'

## Rhythm ############
# Add the rhythm score of the analytic and the synthetic option, respectively.
x$RHY_A <- ifelse(x$COMPARISON=='analytic', x$RHY_SCORE_POS, x$RHY_SCORE_ALT_POS)
x$RHY_S <- ifelse(x$COMPARISON=='synthetic', x$RHY_SCORE_POS, x$RHY_SCORE_ALT_POS)

#  If positive, the analytic pattern more closely adheres to the rhythmic alternation. If negative, the synthetic pattern adheres more closely to the rhythmic alternation.
x$RHY_DIFF <- x$RHY_S-x$RHY_A

## Segment ############
# Add the segment score of the analytic and the synthetic comparison, respectively.
x$SEG_A <- ifelse(x$COMPARISON=='analytic', x$SEG_SCORE, x$SEG_SCORE_ALT)
x$SEG_S <- ifelse(x$COMPARISON=='synthetic', x$SEG_SCORE, x$SEG_SCORE_ALT)

# If positive, the analytic pattern more closely adheres to the segment alternation. If negative, the synthetic pattern adheres more closely to the segment alternation.
x$SEG_DIFF <- x$SEG_S-x$SEG_A


# Summary of the data
summary(x)


# Data Exploration ##########
## COMPL ########
tree(x$COMPARISON ~ x$COMPL)
# conflate to *to-infinitive* (i) and *prepositional phrase* (p) vs. *no complement* (n) and *than-phrase* (t) (see Mondorf (2014) for why *than-phrases* are not complements)
x$COMPL.confl <- x$COMPL
levels(x$COMPL.confl) <- c('y', 'n', 'y', 'n')
prop.table(table(x$COMPL.confl))


## LEXDIV ###########
hist(x$LEXDIV)
# one pretty low value
boxplot(x$LEXDIV)$stats[1,1]
# winsorize to lowest value in boxplot that is not an outlier (-43.82754)
x$LEXDIV.win <- x$LEXDIV
x$LEXDIV.win[x$LEXDIV.win < -43.82754] <- - 43.82754

hist(x$LEXDIV.win)


## READABILITY #########
hist(x$READABILITY)
# log it
x$READABILITY.log <- log2(abs(x$READABILITY)) * sign(x$READABILITY)
hist(x$READABILITY.log)

## SYNT_FUN ########
prop.table(table(x$SYNT_FUN))
tree(x$COMPARISON ~ x$SYNT_FUN)
# conflate to *p* and *pn* vs. *a* and *n*
x$SYNT_FUN.confl <- x$SYNT_FUN
levels(x$SYNT_FUN.confl) <- c('a', 'a', 'p', 'p')
prop.table(table(x$SYNT_FUN.confl))

## WORD_COUNT #########
hist(x$WORD_COUNT)
# log it
x$WORD_COUNT.log <- log2(x$WORD_COUNT)
hist(x$WORD_COUNT.log)

## ZIPF_FREQ #######
hist(x$ZIPF_FREQ)
# Boxcox transformation
b <- boxcox(lm(x$ZIPF_FREQ ~ 1))
(lambda <- b$x[which.max(b$y)])
x$ZIPF_FREQ.trans <- (x$ZIPF_FREQ^lambda - 1)/lambda
hist(x$ZIPF_FREQ.trans)

# Manually add the interaction terms ########
## Categorical Variables ########
x$VARIETYxADVMOD <- x$VARIETY:x$ADVMOD
x$VARIETYxCOMPL.confl <- x$VARIETY:x$COMPL.confl
x$VARIETYxFORM <- x$VARIETY:x$FORM
x$VARIETYxNEWSPAPER <- x$VARIETY:x$NEWSPAPER
x$VARIETYxPERSIST_FORM.1000 <- x$VARIETY:x$PERSIST_FORM.1000
x$VARIETYxSTRESS_LAST_SYLL <- x$VARIETY:x$STRESS_LAST_SYLL
x$VARIETYxSYNT_FUN.confl <- x$VARIETY:x$SYNT_FUN.confl

## Numeric Variables #########
# ADJ_LEN
plot(ctree(COMPARISON ~ ADJ_LEN, data=x)) # 4 levels
x$ADJ_LEN.cat <- cut(x$ADJ_LEN, c(-Inf, 4, 5, 7, Inf), labels=c('(0,4]', '(4,5]', '(5,7]', '(7,13]'))

# DPNOFREQ
plot(ctree(COMPARISON ~ DPNOFREQ, data=x)) # 3 levels
x$DPNOFREQ.cat <- cut(x$DPNOFREQ, c(-Inf, 0.017, 0.162, Inf), labels=c('[0,0.017]', '(0.017,0.162]', '(0.162,1]'))

# FINAL_SEGMENT
plot(ctree(COMPARISON ~ FINAL_SEGMENT, data=x)) # 5 levels
x$FINAL_SEGMENT.cat <- cut(x$FINAL_SEGMENT, c(-Inf, 0, 0.333, 0.625, 0.667, Inf), labels=c('0', '(0,0.333]', '(0.333,0.625]', '(0.625,0.667]', '(0.667,1]'))

# LEXDIV.win
plot(ctree(COMPARISON ~ LEXDIV.win, data=x)) # No splits

# PERSISTENCE
plot(ctree(COMPARISON ~ PERSISTENCE, data=x)) # 2 levels
x$PERSISTENCE.cat <- cut(x$PERSISTENCE, c(-Inf, 0.587, Inf), labels=c('[-1,0.587]', '(0.587,1]'))

# READABILITY.log
plot(ctree(COMPARISON ~ READABILITY.log, data=x)) # No splits

# RHY_DIFF
plot(ctree(COMPARISON ~ RHY_DIFF, data=x)) # 4 levels
x$RHY_DIFF.cat <- cut(x$RHY_DIFF, c(-Inf, -0.33, -0.25, -0.054, Inf), labels=c('[-1,-0.33]', '(-0.33,-0.25]', '(-0.25,-0.054]', '(0.054,1]'))

# RHY_A
plot(ctree(COMPARISON ~ RHY_A, data=x)) # 4 levels
x$RHY_A.cat <- cut(x$RHY_A, c(-Inf, 0.179, 0.225, 0.33, Inf), labels=c('[0,0.179]', '(0.179,0.225]', '(0.225,0.33]', '(0.33,1]'))

# RHY_S
plot(ctree(COMPARISON ~ RHY_S, data=x)) # 2 levels
x$RHY_S.cat <- cut(x$RHY_S, c(-Inf, 0.217, Inf), labels=c('[0,0.217]', '(0.217,1]'))

# SEG_DIFF
plot(ctree(COMPARISON ~ SEG_DIFF, data=x)) # No splits

# SEG_A
plot(ctree(COMPARISON ~ SEG_A, data=x)) # No splits

# SEG_S
plot(ctree(COMPARISON ~ SEG_S, data=x)) # 2 levels
x$SEG_S.cat <- cut(x$SEG_S, c(-Inf, 0.327,  Inf), labels=c('[0,0.327]', '(0.327,1]'))

# WORD_COUNT.log
plot(ctree(COMPARISON ~ WORD_COUNT.log, data=x)) # No splits

# ZIPF_FREQ.trans
plot(ctree(COMPARISON ~ ZIPF_FREQ.trans, data=x)) # 4 levels
x$ZIPF_FREQ.cat <- cut(x$ZIPF_FREQ.trans, c(-Inf, 13.293, 14.756, 14.942,  Inf), labels=c('[-0.5,13.293]', '(13.293,14.756]', '(14.756,14.942]', '(14.942,24]'))

x$VARIETYxADJ_LEN.cat <- x$VARIETY:x$ADJ_LEN.cat
x$VARIETYxDPNOFREQ.cat <- x$VARIETY:x$DPNOFREQ.cat
x$VARIETYxFINAL_SEGMENT.cat <- x$VARIETY:x$FINAL_SEGMENT.cat
x$VARIETYxPERSISTENCE.cat <- x$VARIETY:x$PERSISTENCE.cat
x$VARIETYxRHY_DIFF.cat <- x$VARIETY:x$RHY_DIFF.cat
x$VARIETYxRHY_A.cat <- x$VARIETY:x$RHY_A.cat
x$VARIETYxRHY_S.cat <- x$VARIETY:x$RHY_S.cat
x$VARIETYxSEG_S.cat <- x$VARIETY:x$SEG_S.cat
x$VARIETYxZIPF_FREQ.cat <- x$VARIETY:x$ZIPF_FREQ.cat

# Model Preparation #######
# Compute the two baselines: 73.8% is the one to beat
c("baseline 1"=baseline.1 <- max(prop.table(table(x$COMPARISON))),
  "baseline 2"=baseline.2 <- sum(prop.table(table(x$COMPARISON))^2))

# Variable Selection by Boruta
set.seed(sum(utf8ToInt("All the Young Dudes")))
predictors <- Boruta(COMPARISON ~ ADJ_LEN + ADVMOD + COMPL.confl + DPNOFREQ + FINAL_SEGMENT + FORM +  LEXDIV.win + NEWSPAPER + PERSIST_FORM.1000 + PERSISTENCE + READABILITY.log + RHY_DIFF + RHY_A + RHY_S + SEG_DIFF + SEG_A + SEG_S + STRESS_LAST_SYLL + SYNT_FUN.confl + VARIETY + WORD_COUNT.log + ZIPF_FREQ.trans + VARIETYxADJ_LEN.cat + VARIETYxDPNOFREQ.cat + VARIETYxFINAL_SEGMENT.cat +  VARIETYxPERSISTENCE.cat + VARIETYxRHY_DIFF.cat + VARIETYxRHY_A.cat + VARIETYxRHY_S.cat + VARIETYxSEG_S.cat + VARIETYxZIPF_FREQ.cat + VARIETYxADVMOD + VARIETYxCOMPL.confl + VARIETYxFORM + VARIETYxNEWSPAPER + VARIETYxPERSIST_FORM.1000 + VARIETYxSTRESS_LAST_SYLL + VARIETYxSYNT_FUN.confl, data=x, maxRuns=200)

attStats(predictors)

# Hyperparameter tweaking
collector <- matrix(rep(NA, 60), ncol=10, dimnames=list(   
  NTREE=ntree.vals <- c(500, 1000, 1500, 2000, 2500, 3000),  
  MTRY=mtry.vals <- 1:10)) 
for(k in seq(ntree.vals)){       
  for(j in seq(mtry.vals)){  
    seedy <- set.seed(sum(utf8ToInt("All the Young Dudes")))  
    collector[k,j] <- randomForest(   
      COMPARISON ~           
        ADJ_LEN + DPNOFREQ + FINAL_SEGMENT + LEXDIV.win + PERSISTENCE + READABILITY.log + 
        RHY_DIFF + RHY_A + SEG_DIFF + SEG_A + SEG_S + STRESS_LAST_SYLL + SYNT_FUN.confl + 
        WORD_COUNT.log + ZIPF_FREQ.trans + VARIETYxADJ_LEN.cat + VARIETYxDPNOFREQ.cat + 
        VARIETYxFINAL_SEGMENT.cat + VARIETYxRHY_DIFF.cat + VARIETYxRHY_A.cat + VARIETYxRHY_S.cat + 
        VARIETYxSEG_S.cat + VARIETYxZIPF_FREQ.cat + VARIETYxADVMOD + VARIETYxCOMPL.confl +
        VARIETYxPERSIST_FORM.1000 + VARIETYxSTRESS_LAST_SYLL + VARIETYxSYNT_FUN.confl, 
      data=x,    
      ntree=ntree.vals[k], mtry=mtry.vals[j],
      importance=TRUE)$err.rate[ntree.vals[k], "OOB"] 
  }
}
mtry <- which(t(collector)==min(t(collector)), arr.ind=TRUE)[1,1] 
ntree <- which(t(collector)==min(t(collector)), arr.ind=TRUE)[1,2]*500 


# The Model ##########
set.seed(sum(utf8ToInt("All the Young Dudes")))
(rf.1 <- randomForest(COMPARISON ~ 
                        ADJ_LEN + DPNOFREQ + FINAL_SEGMENT + LEXDIV.win + PERSISTENCE + READABILITY.log + 
                        RHY_DIFF + RHY_A + SEG_DIFF + SEG_A + SEG_S + STRESS_LAST_SYLL + SYNT_FUN.confl + 
                        WORD_COUNT.log + ZIPF_FREQ.trans + VARIETYxADJ_LEN.cat + VARIETYxDPNOFREQ.cat + 
                        VARIETYxFINAL_SEGMENT.cat + VARIETYxRHY_DIFF.cat + VARIETYxRHY_A.cat + VARIETYxRHY_S.cat + 
                        VARIETYxSEG_S.cat + VARIETYxZIPF_FREQ.cat + VARIETYxADVMOD + VARIETYxCOMPL.confl + 
                        VARIETYxPERSIST_FORM.1000 + VARIETYxSTRESS_LAST_SYLL + VARIETYxSYNT_FUN.confl, 
                      data=x,
                      ntree=500,
                      mtry=5,
                      importance=TRUE))

# Logloss
x$PREDS.NUM.rf1 <- predict(
  rf.1, 
  type="prob")[,"synthetic"]
x$PREDS.CAT.rf1 <- predict(rf.1)
x$PREDS.NUM.rf1.obs <- ifelse(  # the probability with wich the observed variant was chosen
  x$COMPARISON=="synthetic",
  x$PREDS.NUM.rf1,
  1-x$PREDS.NUM.rf1)
logloss <- mean(-log(x$PREDS.NUM.rf1.obs)) # 0.084

# Confusion Matrix
confusionMatrix(x$PREDS.CAT.rf1, x$COMPARISON)

# Variable Importance
(varimps <- rf.1$importance)[,3:4]

# Plots ##########
## Variable Importance #######
dotchart(sort(varimps[,1]), pch=4, xlab='Mean Decrease in Accuracy', main='Variable Importance Plot')

## The Most Important Variables ########
# ADJ_LEN
pd.cas <- partial(    # make pd.c contain partial dependence scores
  object=rf.1,        # from this forest
  pred.var="ADJ_LEN", # for this predictor
  which.class=2,      # for the 2nd level of the response
  train=x,
  prob=TRUE)
tab.cas <- prop.table(table(x$ADJ_LEN))
plot(
  main="Partial dep. of COMPARISON on ADJ_LEN",
  type="b", pch=16,
  xlab="Adjective Length in Characters",
  ylab=substitute(paste('Probability of ', italic('synthetic'), ' comparison')),
  ylim=c(0,1),
  x=pd.cas$ADJ_LEN,
  y=pd.cas$yhat,
  cex=1+tab.cas*10
)
abline(h=sum(x$COMPARISON=='synthetic')/nrow(x), lty=2)
lines(lowess(pd.cas$yhat ~ pd.cas$ADJ_LEN), lwd=6, col="#BCBCBCB0")

# STRESS_LAST_SYLL
pd.corpustrigger <- partial(object=rf.1,
                             pred.var='STRESS_LAST_SYLL',
                             which.class=2, train=x,
                             prob=TRUE)
b <- matrix(data=pd.corpustrigger$yhat, nrow=2)
rownames(b) <- levels(x$STRESS_LAST_SYLL)
plot(x=0, ylim=c(0.5,1), xlim=c(0,1), xaxt='n', bty='n', pch='', 
     xlab='Stress on the last syllable of the adjective lemma',
     ylab=substitute(paste('Probability of ', italic('synthetic'), 'comparison')),
     main='Partial dep. of COMPARISON on STRESS_LAST_SYLL',
     cex.main=1.5);grid()
axis(1, at=0:1, labels=c('no', 'yes'))
abline(h=sum(x$COMPARISON=='synthetic')/nrow(x), lty=2)
points(x=0:1, y=b[,1], pch=16, col=alpha('#004F86', alpha=0.7), cex=3)

# VARIETYxZIPF_FREQ.cat
pd.corpustrigger <- partial(object=rf.1,
                             pred.var='VARIETYxZIPF_FREQ.cat',
                             which.class=2, train=x,
                             prob=TRUE)
b <- matrix(data=pd.corpustrigger$yhat, nrow=4)
rownames(b) <- levels(x$ZIPF_FREQ.cat)
colnames(b) <- levels(x$VARIETY)
plot(x=0, ylim=c(0.5,1), xlim=c(-0.2,3.2), xaxt='n', bty='n', pch='', 
     xlab='Frequency of the Adjective Lemma',
     ylab=substitute(paste('Probability of ', italic('synthetic'), ' comparison')),
     main='Partial dep. of COMPARISON on VARIETYxZIPF_FREQ.cat',
     cex.main=1);grid()
axis(1, at=0:3, labels=c("[0,13.293]", levels(x$ZIPF_FREQ.cat)[2:4]))
abline(h=sum(x$COMPARISON=='synthetic')/nrow(x), lty=2)
points(x=c(0.05, 1.05, 2.05, 3.05), y=b[,2], pch=16, col=alpha('#004F86', alpha=0.7), cex=3)
points(x=c(-0.05, 0.95, 1.95, 2.95), y=b[,1], pch=16, col=alpha('#7BC8FF', alpha=0.7), cex=3)
legend('top', legend=c('BrE', 'SLE'), fill=c('#7BC8FF', '#004F86'), ncol=2, xjust=0.5, yjust=0.5)

# VARIETYxFINAL_SEGMENT.cat
pd.corpustrigger <- partial(object=rf.1,
                             pred.var='VARIETYxFINAL_SEGMENT.cat',
                             which.class=2, train=x,
                             prob=TRUE)
b <- matrix(data=pd.corpustrigger$yhat, nrow=5)
rownames(b) <- levels(x$FINAL_SEGMENT.cat)
colnames(b) <- levels(x$VARIETY)
plot(x=0, ylim=c(0.5,1), xlim=c(-0.2,4.2), xaxt='n', bty='n', pch='', 
     xlab='Similarity of the Final Segment to the Synthetic Ending',
     ylab=substitute(paste('Probability of ', italic('synthetic'), ' comparison')),
     main='Partial dep. of COMPARISON on VARIETYxFINAL_SEGMENT.cat',
     cex.main=1);grid()
axis(1, at=0:4, labels=levels(x$FINAL_SEGMENT.cat))
abline(h=sum(x$COMPARISON=='synthetic')/nrow(x), lty=2)
points(x=c(0.05,1.05,2.05,3.05,4.05), y=b[,2], pch=16, col=alpha('#004F86', alpha=0.7), cex=3)
points(x=c(-0.05,0.95,1.95,2.95,3.95), y=b[,1], pch=16, col=alpha('#7BC8FF', alpha=0.7), cex=3)
legend('top', legend=c('BrE', 'SLE'), fill=c('#7BC8FF', '#004F86'), ncol=2, xjust=0.5, yjust=0.5)

# sessionInfo()
# R version 4.3.0 (2023-04-21 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 26100)
# 
# Matrix products: default
# 
# 
# locale:
#   [1] LC_COLLATE=German_Germany.utf8 
# [2] LC_CTYPE=German_Germany.utf8   
# [3] LC_MONETARY=German_Germany.utf8
# [4] LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.utf8    
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
#   [1] grid      stats    
# [3] graphics  grDevices
# [5] utils     datasets 
# [7] methods   base     
# 
# other attached packages:
#   [1] tree_1.0-43         
# [2] randomForest_4.7-1.1
# [3] pdp_0.8.1           
# [4] partykit_1.2-20     
# [5] mvtnorm_1.2-2       
# [6] libcoin_1.0-9       
# [7] MASS_7.3-58.4       
# [8] dplyr_1.1.2         
# [9] caret_6.0-94        
# [10] lattice_0.21-8      
# [11] ggplot2_3.5.0       
# [12] Boruta_8.0.0        
# 
# loaded via a namespace (and not attached):
#   [1] gtable_0.3.3        
# [2] recipes_1.0.6       
# [3] vctrs_0.6.3         
# [4] tools_4.3.0         
# [5] generics_0.1.3      
# [6] stats4_4.3.0        
# [7] parallel_4.3.0      
# [8] tibble_3.2.1        
# [9] fansi_1.0.4         
# [10] pkgconfig_2.0.3     
# [11] ModelMetrics_1.2.2.2
# [12] Matrix_1.5-4        
# [13] data.table_1.14.8   
# [14] lifecycle_1.0.3     
# [15] compiler_4.3.0      
# [16] stringr_1.5.0       
# [17] munsell_0.5.0       
# [18] codetools_0.2-19    
# [19] class_7.3-21        
# [20] prodlim_2023.03.31  
# [21] Formula_1.2-5       
# [22] pillar_1.9.0        
# [23] gower_1.0.1         
# [24] iterators_1.0.14    
# [25] rpart_4.1.19        
# [26] foreach_1.5.2       
# [27] nlme_3.1-162        
# [28] parallelly_1.36.0   
# [29] lava_1.7.2.1        
# [30] tidyselect_1.2.0    
# [31] digest_0.6.31       
# [32] inum_1.0-5          
# [33] stringi_1.7.12      
# [34] future_1.33.0       
# [35] reshape2_1.4.4      
# [36] purrr_1.0.1         
# [37] listenv_0.9.0       
# [38] splines_4.3.0       
# [39] colorspace_2.1-0    
# [40] cli_3.6.1           
# [41] magrittr_2.0.3      
# [42] survival_3.5-5      
# [43] utf8_1.2.3          
# [44] future.apply_1.11.0 
# [45] withr_2.5.0         
# [46] scales_1.3.0        
# [47] lubridate_1.9.2     
# [48] timechange_0.2.0    
# [49] globals_0.16.2      
# [50] nnet_7.3-18         
# [51] timeDate_4022.108   
# [52] hardhat_1.3.0       
# [53] rlang_1.1.1         
# [54] Rcpp_1.0.10         
# [55] glue_1.6.2          
# [56] pROC_1.18.4         
# [57] ipred_0.9-14        
# [58] rstudioapi_0.14     
# [59] R6_2.5.1            
# [60] plyr_1.8.8 




