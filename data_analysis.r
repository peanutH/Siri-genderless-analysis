## import data

data = read.csv("tetris.csv")

head(data)

## seperate the visual and audio data

data$condition = factor(data$condition)

audio = subset(data$score, data$condition == "auditory")

visual = subset(data$score, data$condition == "visual")


## check the mean, max, min, sd, median, IQR

### check for overall data

mean(data$score); sd(data$score); max(data$score); min(data$score); median(data$score); IQR(data$score)

### check for audio data

mean(audio); sd(audio); max(audio); min(audio); median(audio); IQR(audio)

### check for visual data

mean(visual); sd(visual); max(visual); min(visual); median(visual); IQR(visual)

ul = sd(audio) + mean(audio)

ul

max(audio)

## Check outliers

which(data$score > 100)

which(data$score < 0)

### audio upper (3*sd + mean) and lower (mean - 3*sd ) limit 

audio_ul = mean(audio) + sd(audio)

audio_ll = mean(audio) - sd(audio) 

which(data$score > audio_ul, data$condition == "auditory")

which(data$score < audio_ll, data$condition == "auditory")

###visual upper (3*sd + mean) and lower (mean - 3*sd ) limit

visual_ul = mean(visual) + 3*sd(visual)

visual_ul = mean(visual) - 3*sd(visual)

which(data$score > visual_ul, data$condition == "visual")

which(data$score < visual_ul, data$condition == "visual")

## plot the data

boxplot (data$score, main="Boxplot of Tetris Scores", ylim= c(0,100), ylab = "Score")

boxplot (data$score~tetris$condition, main="Boxplot of Tetris Scores", ylim= c(0,100), xlab = "Condition", ylab = "Score")

hist (audio, xlab="Tetris Scores", main = "Tetris Scores of Different Interruptions", xlim = c(0,100), breaks=seq(0,100,5), col = rgb(1,.5,0,1/3))

hist (visual, breaks = seq(0,100,5), col=rgb(0,1,0,1/3), add=TRUE)

legend("topleft",c("Audio","Visual"),fill=c(rgb(1,.5,0,1/3),rgb(0,1,0,1/3)))

## Shapiro test

shapiro.test (audio)

shapiro.test (visual)

install.packages ('car')

library('car')

leveneTest(data$score, data$condition)

## t test

t.test (audio, visual, paired = FALSE)

## plot result

data.mean = c(mean(audio), mean(visual))

data.sd = c(sd(audio), sd(visual))

names(data.mean) = c("Audio", "Visual")

se.bar = function(x, y, sds, n, upper=sds/sqrt(n), lower=upper, length=0.1,...)
    
{
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)}
    
br = barplot (data.mean, main = "Graph of Condition Means", xlab= "Interruption Condition", ylab="Tetris Score", ylim=c(0,100),col=c(col=rgb(1,.5,0,1),rgb(0,1,0,1)))

se.bar(br,data.mean, data.sd, 16)


