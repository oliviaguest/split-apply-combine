setwd('/home/olivia/split-apply-combine')

#this loads in the data, each test has been saved separately, based on the sequence of stimuli shown (this is pretty bad practice, but t'was not I)
raw_df <- rbind(
            read.csv ("CatLearn1.csv"), read.csv ("CatLearn1B.csv"),
            read.csv ("CatLearn1_training.csv"), read.csv ("CatLearn1B_training.csv")
            )

#id is participant id
#cue is Woman or Man
#a is category A, previously Q
#b is category b, previously R
#we just loaded in the files with sequence 0
df_0 <- data.frame(id = raw_df$ID, cue = raw_df$VIS.CUE_L.STM, a = raw_df$VIS.TA.ISFXT, b = raw_df$VIS.TB.ISFXT, timestamp = raw_df$Timestamp, sequence =rep(0, length(raw_df$ID)))

#this loads in the data, each test has been saved separately
raw_df <- rbind(
              read.csv ("CatLearn2.csv"), read.csv ("CatLearn2B.csv"),
              read.csv ("CatLearn2_training.csv"), read.csv ("CatLearn2B_training.csv")
              )

#and now we do the same for those with sequence 1
df_1 <- data.frame(id = raw_df$ID, cue = raw_df$VIS.CUE_L.STM, a = raw_df$VIS.TA.ISFXT, b = raw_df$VIS.TB.ISFXT, timestamp = raw_df$Timestamp, sequence =rep(1, length(raw_df$ID)))

#now merge them both since they can be told apart by df$sequence
df <- merge(x = df_0, y = df_1, all = TRUE)

#get rid of not a values
df[is.na(df)] <- 0

#make sure cue is character
df$cue<-as.character(df$cue)

#correctly name the two cues
df$cue[df$cue=='lady']<-'Woman'
df$cue[df$cue=='']<-'Man'

#set a and b to 1 for -1s 
#-1 is when eye coords are in AOI but not fixating
df$a[df$a == -1] <- 1
df$b[df$b == -1] <- 1

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

#count up the fixations per condition to calculate proportions
df <- as.data.frame(table(df))

#rename Freq to fixations to make it clear when looking at the df
colnames(df)[which(names(df) == "Freq")] <- "fixations"

#get rid of times when both AOIs are fixated (impossible)
df <- df[!(df$a == 1 & df$b == 1), ]
#and get rid of times when baby is looking at something else (we don't care)
df <- df[!(df$a == 0 & df$b == 0), ]

#now we calculate the proprtion of fixations that are for each cue
library(plyr)     
df<-ddply(df,.(id, cue),transform,prop=fixations/sum(fixations))

#we want to label correct and incorrect fixations, which are dependant on what has been cued
df$correct <- 0   
#so we write a function to apply to our df
func <- function(fix_type, a, b, cue, sequence){
  fix_type <- 0 
  if (sequence == 0) {
    if(a==1 & cue == 'Man' | b==1 & cue == 'Woman') { #if we are looking at sequence 0 then category A is cued by 'Man', and B is cued by 'Woman'
       fix_type <- 1 #1 means correct
    } else {
      fix_type <- 0
    }
  } else if (sequence == 1) {
    if(a==1 & cue == 'Woman' | b==1 & cue == 'Man') { #if sequence 1 then category A is cued by 'Woman', and B is cued by 'Man'
      fix_type <- 1 #1 means correct
    } else {
      fix_type <- 0
    }
  }
  
}
#get rid of not a values
df[is.na(df)] <- 0
#we apply this function to the correct column in order to populate it with 1s for correct fixations 
df$correct <- mapply(func, df$correct,  df$a,  df$b, df$cue, df$sequence)

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

#now to make some violin plots
library(ggplot2)
df$correct <- as.factor(df$correct)

p = ggplot(df, aes(x=df$correct, y=df$prop)) + geom_violin()
p
# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
p <- ggplot(df, aes(x=df$correct, y=df$prop)) + 
  geom_violin(trim=FALSE)
# violin plot with mean points
p + stat_summary(fun.y=mean, geom="point", shape=23, size=2)
# violin plot with median points
p + stat_summary(fun.y=median, geom="point", size=2, color="red")
# violin plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# violin plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.1))
p + geom_boxplot(width=0.1)
p <- p + stat_summary(fun.data=mean_sdl, mult=1, 
                 geom="pointrange", color="red")
p <-  p + geom_jitter(shape=16, position=position_jitter(0.1))
p <- p  +  scale_x_discrete(name = "Fixations", limits = c('0', '1'), labels=c("0" = "Incorrect", "1" = "Correct"))
p <- p + scale_y_continuous(name = "Proportion Looking at AOIs")
p
