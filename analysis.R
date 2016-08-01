setwd('/home/olivia/split-apply-combine')

#this loads in the data, each test has been saved separately
df <- rbind(read.csv ("CatLearn1.csv"), read.csv ("CatLearn1B.csv"),
            read.csv ("CatLearn2.csv"), read.csv ("CatLearn2B.csv"),
            read.csv ("CatLearn1_training.csv"), read.csv ("CatLearn1B_training.csv"),
            read.csv ("CatLearn2_training.csv"), read.csv ("CatLearn2B_training.csv"))

#id is participant id
#cue is Woman or Man
#q is category Q
#r is category R
df <- data.frame(id = df$ID, cue = df$VIS.CUE_L.STM, q = df$VIS.TA.ISFXT, r = df$VIS.TB.ISFXT, timestamp = df$Timestamp)

#get rid of not a values
df[is.na(df)] <- 0

#make sure cue is character
df$cue<-as.character(df$cue)

#correctly name the two cues
df$cue[df$cue=='lady']<-'Woman'
df$cue[df$cue=='']<-'Man'

#set q and r to 1 for -1s 
#-1 is when eye coords are in AOI but not fixating
df$q[df$q == -1] <- 1
df$r[df$r == -1] <- 1

#* Trial starts with blank screen
#* "Look, Look" sound file started
#* 250 ms from start of trial (not waiting until sound done), cue appear
#* 250 ms, cue made 500x500 pixels
#* 250 ms, cue back to 300x300 pixels
#* 250 ms, cue made 500x500 px
#* 250 ms, cue made 300x300 px
#* 250 ms, target appears  (if train trial, central above;  if test, left/right above)
#* 250 ms, if label condition play "look, a dax" file, otherwise stay silent
#do not wait until end of sound file to start next delay timer
#* 2500 ms delay, then end trial.
#so we care only about data from 3500 onwards, hence drop the res
df <- df[!df$timestamp < 3500, ]
#now dump timestamp as it's useless for a t-test
df$timestamp <- NULL

#count up the fixations per condition to calculate proportins
df <- as.data.frame(table(df))
#rename Freq to fixations to make it clear when looking at the df
colnames(df)[which(names(df) == "Freq")] <- "fixations"

#get rid of times when both AOIs are fixated (impossible)
df <- df[!(df$q == 1 & df$r == 1), ]
#and get rid of times when baby is looking at something else (we don't care)
df <- df[!(df$q == 0 & df$r == 0), ]

#now we calculate the proprtion of fixations that are for each cue
library(plyr)     
df<-ddply(df,.(id, cue),transform,prop=fixations/sum(fixations))

#we want to label correct and incorrect fixations, which are dependant on what has been cued
df$correct <- 0   
#so we write a function to apply to our df
func <- function(fix_type, q, r, cue){
  fix_type <- 0 
  if(q==1 & cue == 'Woman' | r==1 & cue == 'Man') { #if we are looking at category q and it is correct given the cue
     fix_type <- 1 #1 means correct
  } else {
    fix_type <- 0
  }
}
#we apply this function to the correct column in order to populate it with 1s for correct fixations 
df$correct <- mapply(func, df$correct,  df$q,  df$r, df$cue)

#now we can calcuate prop correct and incorrent using the prop column
Proportion_Correct <- df[df$correct == 1,]$prop
Proportion_Incorrect <- df[df$correct == 0,]$prop

#and so perform a paired t-test
print(t.test(Proportion_Correct, Proportion_Incorrect,paired=TRUE, alt="greater"))

#and also create boxplots to visualise the data
boxplot(Proportion_Correct, Proportion_Incorrect,
        data=ToothGrowth,
        notch=TRUE, 
        col=(c("darkgreen","lightblue")),
#        main="Two Categories",
        names=c("Correct", "Incorrect"),
        ylab = 'Proportion Looking at AOIs',
        xlab = 'Fixations')
library(ggplot2)
df$correct <- as.factor(df$correct)

p <- ggplot(df, aes(x=df$correct, y=df$prop)) + geom_violin()
p
# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(df, aes(x=df$correct, y=df$prop)) + 
  geom_violin(trim=FALSE)

# violin plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# violin plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))