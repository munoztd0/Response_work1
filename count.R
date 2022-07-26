---
title: "Data wrangling workshop I for Neuroscience Master students"
author: "David Munoz Tord"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:   
  html_document:     
    toc: true
    toc_float: true
    number_sections: false
  pdf_document:
    extra_dependencies: ["float"]
---

```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)
if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}
pacman::p_load(reticulate, R.matlab, sessioninfo) #reticulate for python and R.matlab for... Matlab
use_virtualenv(virtualenv = "r-reticulate")
``` 

# Data wrangling workshop for Neuroscience Master students - Part I

## 1) Make you code reproducible (AKA work on other machines) <br> 

### For python:
```{python, eval=F}
import os
print(os.getcwd().strip('"\''))
```

### For R:

Setup

```{r, message=F}
## # Repro!
if(!require(pacman)) {
  install.packages("pacman")
  install.packages("devtools")
  library(pacman)
}
pacman::p_load(rstudioapi, tidyverse,ggthemes, corrplot, moments, afex, viridis, Rmisc, glmnet, caret, MASS) #moments -> skewness #afex -> lmer #viridis -> color palette # Rmisc -> summarySEwithin

devtools::source_gist("2a1bb0133ff568cbe28d", filename = "geom_flat_violin.R") # to download small scripts (AKA "gists") 

options(scipen = 666, warn=-1, contrasts=c("contr.sum","contr.poly")) #this is general options for ouputs
```

Load data
```{r, message=F}
#homepath = dirname(rstudioapi::getActiveDocumentContext()$path) # or like this # 
homepath = getwd()
#data <- read_csv("data/data.csv") # or
data <- read_csv(paste(homepath, "data/data.csv", sep="/"))
```

### For Matlab/Octave:
```{octave, eval=F}
homepath = pwd;
disp(homepath)
```



### Optional:




#### For Python (do that within your terminal when you finished your scripts):
It will automatically save module requirements in a ./requirements.txt file
```{bash eval=FALSE}
$ pip install pipreqs # install pipreqs on your machine
$ pipreqs .  #run that in the directory where you have you script
```
Then someone can run "pip install -r requirements.txt" before running your script



## 2) Rearrange the data <br> 


```{r}
data$intervention = as.factor(data$intervention)
data$id = as.factor(data$id)
nums1 <- unlist(lapply(data, is.numeric)) #find numeric variables

df_plot = data %>% pivot_longer(
  cols = names(data[ , nums1]),
  names_to = c("var", "session"), 
  names_pattern = "var(.*)_(.*)",
  values_to = "measure"
)

df_plot$session = as.factor(df_plot$session)
df_plot$var = ordered(as.numeric(df_plot$var)) #order for neat plots

df = df_plot %>% pivot_wider(names_from = var, values_from = measure)
nums <- unlist(lapply(df, is.numeric)) #find numeric variables
```

## 3) Plot the data <br> 

```{r, warning=F}
#plot densities
dens_plot = ggplot(df_plot)+
  geom_density(aes( x=measure))+
  facet_wrap(~var, scales = "free")+
  labs(title = "Density per variable")+
  theme(axis.title = element_text()) + 
  ylab("Density") + xlab('Value'); dens_plot

#pairs(~., data = df[ , nums], main = "Scatterplot Matrix of variables") #default solution
corrplot::corrplot(cor(df[ , nums], use="pairwise.complete.obs"), type="lower") #using corrplot

#box plot
box_plot = ggplot(df_plot)+
  geom_boxplot(aes(x=intervention, y=measure))+
  scale_x_discrete(labels=c("Placebo", "Control")) + 
  facet_wrap(~var, scales = "free")+
  labs(title = "Boxplot by group")+
  ggthemes::theme_fivethirtyeight()+
  theme(axis.title = element_text()) + 
  ylab("predictors") + xlab(''); box_plot


```

### Scaling

```{r}
df_scaled = df; df_scaled[nums] <- lapply(df_scaled[nums], scale) #standardize

pivot_re <- function(data, nums) {x = data %>% pivot_longer(
  cols = names(data[ , nums]),
  names_to = "var", 
  values_to = "measure")
  x$var = ordered(as.numeric(x$var)) #order for neat plots
  return(x)} # #create this function because we gonna need it

df_scaled_plot = pivot_re(df_scaled, nums)

df_scaled_plot$var = ordered(as.numeric(df_scaled_plot$var)) #order for neat plots

# we use %+% to change the dataset of a ggplot object!
box_plot_scaled = box_plot %+% df_scaled_plot +   labs(title = "Boxplot by group (standardized)")+  ylab("standardized predictors") ; box_plot_scaled

#if you ever want/need to remove outlier based uniquely on the sd here is a little helper
remove_out <- function(dataraw, datasd, sd, nums){

  df_dict <- data.frame(variable = names(datasd[nums]), out_low = rep(-sd,length(names(datasd[nums]))),  out_high = rep(sd,length(names(datasd[nums]))))

  for (var in df_dict$variable) {
    dataraw[[var]] [datasd[[var]] < df_dict[df_dict$variable == var, ]$out_low | datasd[[var]] > df_dict[df_dict$variable == var, ]$out_high] <- NaN}
  return(dataraw)
}

#for example
#df_out = remove_out(df,df_scaled, 3, nums)

```

### Optional:

Try to "clean" the dataset (e.g. remove outliers, log transform, etc..) as best as you think to be able to analyze the data based on the observations you did on the plots. Then maybe try to do an 2x2 anova (whatch out for the repeated measurment) on a problematic variable before and after you cleaned/transformed it. 

<br><br>

### Bi-variate outliers ?
```{r}
# create list of variable names
col_names = 1:14%>% str_c("var", .); diff_names = as.character(1:14)

#function to create difference scores
diff_column <- function(data, col_names) {
  i = 0
  for (col in col_names) {
    i = i+1
  data[,str_c(i)] = data[,str_c(col, "_ses2")] - data[,str_c(col, "_ses1")]}
  return(data)
}

df_diff = diff_column(data, col_names)

df_diff = df_diff[c("id", "intervention",diff_names)] # keep only columns of interest

df_diff[diff_names] = scale(df_diff[diff_names]) #scale

df_out = remove_out(df_diff,df_diff, 5, diff_names)

df_out[diff_names] = scale(df_out[diff_names], scale = F ) #center at 0

df_out = na.omit(df_out)

df_diff_plot <- pivot_re(df_out, diff_names)

#box plot
box_plot_diff= box_plot %+% df_diff_plot +   labs(title = "Boxplot by group (diff score Z)")+  ylab("standardized diff scores") ; box_plot_diff

#dens plot
dens_plot_diff= dens_plot %+% df_diff_plot + geom_density(aes(x=measure, fill = intervention)) + xlab("standardized diff scores") ; dens_plot_diff

```

### Skewness
Check skewness ! Transform: log ? sqrt? log1p?
```{r, warning=F}
lapply(na.omit(df[diff_names]), moments::skewness) #check

df_sqrt = df; df_sqrt[diff_names] <- lapply(df_sqrt[diff_names], sqrt); df_sqrt_plot = pivot_re(df_sqrt, diff_names) #create sqrt df
df_log = df; df_log[diff_names] <- lapply(df_log[diff_names], log); df_log_plot = pivot_re(df_log, diff_names) #create log df
df_log1p = df; df_log1p[diff_names] <- lapply(df_log1p[diff_names], log1p); df_log1p_plot = pivot_re(df_log1p, diff_names) #create log1p df

#Square root (handles non negative data and zeroes)
dens_sqrt_plot = dens_plot_diff %+% df_sqrt_plot + labs(title = "Density per variable (square root transformed)"); dens_sqrt_plot
lapply(na.omit(df_sqrt[diff_names]), moments::skewness) # calculate skewness of the data

#Log (handles non negative data and but NOT zeroes)
dens_log_plot = dens_plot_diff %+% df_log_plot + labs(title = "Density per variable (log transformed)"); dens_log_plot
lapply(na.omit(df_log[diff_names]), moments::skewness) # here we see NA apparearing, why ? because we introduced -inf ( log(0) = -inf ) !

#Log1p (handles non negative data AND  zeroes)
dens_log1p_plot = dens_plot_diff %+% df_log1p_plot + labs(title = "Density per variable (log + 1 transformed)"); dens_log1p_plot
lapply(na.omit(df_log1p[diff_names]), moments::skewness) # here we see NO NA apparearing, why ? because ( log(0+1) = 0 ) !

#zero-inflated -> BAD
```

### Modeling
```{r, warning=F, message =F}
## Create a formula for a model with a large number of variables:
fmla <- as.formula(paste(" `1` ~ intervention + ", paste0(sprintf("`%s`",  diff_names[c(-1,-2)]), collapse= "+"), " + Error(id)"))

#this really just to demonstrate the influence of outliers

#with huge outliers
model1 = aov_car(fmla, data= na.omit(df_diff), factorize = F, anova_table = list(correction = "GG", es = "pes"))
nice(model1, MSE=F); emmeans::ref_grid(model1) #triple check everything is centered at 0

#without huge outliers
model = aov_car(fmla, data= na.omit(df_out), factorize = F, anova_table = list(correction = "GG", es = "pes"))
nice(model, MSE=F); emmeans::ref_grid(model) #triple check everything is centered at 0

devtools::source_gist("383aa93ffa161665c0dca1103ef73d9d", filename = "effect_CI.R") # to download  pes_ci

pes_ci(fmla, data= na.omit(df_out), factorize = F)
```

### Plot interaction (with class)
```{r}
#this is just stuff to make the formating nice
averaged_theme <- theme_bw(base_size = 24, base_family = "Helvetica")+
    theme(strip.text.x = element_text(size = 24, face = "bold"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"),
          legend.position=c(.9,.9),
          legend.title  = element_text(size = 12),
          legend.text  = element_text(size = 10),
          legend.key.size = unit(0.2, "cm"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_line(size=.2, color="lightgrey") ,
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(size = 22),
          axis.title.y = element_text(size = 22),
          axis.line = element_line(size = 0.5),
          panel.border = element_blank())
  
pal = viridis::viridis(n=3) # specialy conceived for colorblindness  
  

# AVERAGED EFFECT 
dfH <- summarySEwithin(df,
                       measurevar = "1",
                       betweenvars = "intervention",
                       withinvars = "session", 
                       idvar = "id", na.rm = T)

dfH$cond <- ifelse(dfH$session == "ses1", -0.25, 0.25)
df$cond <- ifelse(df$session == "ses1", -0.25, 0.25)
set.seed(666); df <- df %>% mutate(condjit = jitter(as.numeric(cond), 0.3),
                                  grouping = interaction(id, cond))

labels = c("0"="Placebo", "1"="Treatment")

plt <- ggplot(df, aes(x = cond, y = `1`, fill = session, color = session)) +
  geom_point(data = dfH, alpha = 0.5) +
  geom_line(aes(x = condjit, group = id, y = `1`), alpha = .3, size = 0.5, color = 'gray') +
  geom_flat_violin(scale = "count", trim = FALSE, alpha = .2, aes(fill = session, color = NA))+
  geom_point(aes(x = condjit), alpha = .3,) +
  geom_crossbar(data = dfH, aes(y = `1`, ymin=`1`-ci, ymax=`1`+ci,), width = 0.2 , alpha = 0.1)+
  ylab('Var 1') +
  xlab('') +
  #scale_y_continuous(expand = c(0, 0), breaks = c(seq.int(0,100, by = 20)), limits = c(-0.5,100.5)) + # choose limits
  scale_x_continuous(labels=c("Pre", "Post"),breaks = c(-.25,.25), limits = c(-.5,.5)) +
  scale_fill_manual(values=c("ses1"= pal[1], "ses2"=pal[2]), guide = 'none') +
  scale_color_manual(values=c("ses1"=pal[1], "ses2"=pal[2]), guide = 'none') +
  theme_bw() + facet_wrap(~intervention, labeller=labeller(intervention =labels)) + averaged_theme ; plt


#if you want to save the figures
pdf('figures/inter.pdf')
print(plt)
dev.off()

```

<br><br>

## 4) Feature selection (optional but at least try to think about it)


Let's say you want to test for the significance of the intervention variable (placebo VS treatment) on var1.
You also want to control for baseline difference between groups (session pre-post) and other nuissance covariates (e.g. age, weight, number of cigarette smoked per week, etc..). Here you have many "nuissance" variables (var2 all the way to var20) and not that much observations... 

One thing you could do is enter ALL of them into an ANCOVA, but that's a bit dubious because you are artifically decreasing your degrees of freedom (and increasing the chances of multi-colinearity). We are not gonna enter too much into the details about that right here but try to think why we generally wouldn't want that.

Another thing one might do is to enter all these variables and then removing the ones that are not "statiscally significant" and then rerun the analysis with only the ones left. Again that is definitely not the best way to go for because you doing multiple tests (and also some variables that were significant before might still be once you removed others). 
 
So one way to circumvent these issues is to first implement a proper variable selection method (not based on p-values). You can achieve this via several methods. One way to do that could be to fit different models (with and without certain variables) and compare them with a model selection criterion (AIC, BIC, SIC, loo, R², etc..) to only keep the "best model". 

<br><br>
Alternatively you could also select only variables that explain a certain thershold of variance (i.e. how useful is this variable to predict an outcome, here "is the individual in the placebo or treatment group?"). Finally, you can also create composite variables (component) that reflects most of the variance from those variables (however try to think about what is the drawback of this last method if you use it).



This is a rather complex and technical exercise (as well as time and computationally consuming) but I think it's definitely primordial for scientists to know how to select variables (feature) from dataset.

PS: No need to do the ANCOVA, the purpose of this exercise is really about wich variables you would retain in the final model and there is defintely no "right or wrong" answers.

<br><br>

### ML
Using penalized (lasso) maximum likelihood for variable selection (cv.glmnet), i.e. finding the minimum lambda (regularization parameter) to infer the best number of features to use in order to simplify the model and avoid  overfitting.\
Using that because: \
- Handles the problem of correlated inputs
- Perform better than stepwise selection
```{r var_sel, warning=F, cache=T, message=F}

#1) with Random Forests
sizes = seq(sqrt(ncol(df_out[-1]))*.5, ncol(df_out[-1]), by = 5)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

rfeResults = rfe(x = df_out[c(-1, -2)], y = df_out$intervention, sizes=sizes, rfeControl=control)
predictors(rfeResults)
plot(rfeResults, type=c("g", "o"))


#2) with Stepwise
full.model = glm(intervention ~., data = df_out[-1], family = binomial)
step.model <- full.model %>% MASS::stepAIC(trace = FALSE)
coef(step.model)


#3) with Lasso
mat<-model.matrix(intervention~.,data=df_out[-1]); mat=mat[,-1] # remove intercept

set.seed(123);
mat<-model.matrix(intervention~.,data=df_out[-1]); mat=mat[,-1] # remove intercept
foldid <- sample(rep(seq(3), length.out = nrow(df_out[-1]))) # to be reproducible
fit<-glmnet::cv.glmnet(x=mat,y=df_out[-1]$intervention,family ="binomial",type.measure='mse',alpha=1) #, foldid = foldid, nfolds = 3, standardize = FALSE)
plot(fit)
c<-coef(fit,s='lambda.min',exact=TRUE); inds<-which(c!=0) #include only variable lambda coeficients bigger than 0
variables<-row.names(c)[inds]; variables<-variables[!variables %in% '(Intercept)']; variables #extract those variables

df_diff_plot <- pivot_re(df_diff, c("1", "3", "14"))

#plot
box_plot %+% df_diff_plot + labs(title = 'Variables selected', y="Difference scores")
dens_log_plot %+% df_diff_plot + labs(title = 'Variables selected', y="Difference scores")

```


## 5) Preparing behavioral data for fMRI analysis

Now we are entering the realm of task-based functional MRI. We will defintely go more into details in a future workshop but let's just go through a bit of data wrangling anyone confronted to fMRI will have to go through.

Let's start by loading the events associated to an fMRI acquisition. Here (in data/sub-control100/data.mat) is just an example of a typical file that you will have to extract from: the onsets (AKA when does the stuff you showed to your participant appeared), the durations (how long did it appear) and the behavioral (also generally called modulators) data associated to your task (e.g. reaction times or liking ratings for each condition). You WILL need these regressors to model you fMRI data later on.

The thing is.. these files are generally (at least here in Geneva) generated for each participantin a .mat format. So that's whyI gave it to you hee like you probably get it in your lab too. If you don't feel concerned by that and your lab and moved on to python as its presentation software for fMRI, good for you and you might skip this. Otherwise this task might be quite instructive for you. So let's dive in.


This files contain a matlab "structure array" which simply put is "a data type that groups related data using data containers called 'fields'. Each field can contain any type of data". This might be confusing at first (trust me .. it is) but so let's go step by step. This is a really simple design were we have only 3 odor conditions and the "start" of each trial.

### For Matlab/Octave
```{octave, comment=""}
load('data/sub-control100/data.mat')  %loading data

disp('Each structure has two fields named:'); disp(fieldnames(durations2))

%to access the data in a field you must use dot notation (e.g. structName.fieldName)

disp(['The "start" field contains ', num2str(length(durations2.start)), ' observattions'])
disp('The "odor" field has 3 nested substructures named'); disp(fieldnames(durations2.odor))
disp(['Each of the 3 nested substructure in the "odor" field contains ', num2str(length(durations2.odor.reward)), ' observattions'])
```

Now what we would like for most MRI softwares is a "3 column timming files" with respectively the onset time (in seconds); the duration (in seconds); the relative magnitude of each stimulus (set to 1 for none). I put examples of what it should look like in the /data folder.

So basically the task will be to recreate that type of files for sub-01 and make you code easilly scale to more subjects (here is just duplicated the sub-01 into more subjects to test your code).


Of course you are not forced to use matlab/octave here to achieve that.
<br><br>


### For python:
```{python}
import scipy.io
mat = scipy.io.loadmat('data/sub-control100/data.mat')
dict.keys(mat) #its read as a dictionnary so this gonna get ugly but stay with me
#mat["modulators2"]["start"][0] #this gets you an array within an array !not good enough!  
mat["modulators2"]["start"][0][0][0:2] #this actually get you the array (but remove the [0:2])
#mat["modulators2"]["odor"]["control"][0][0] #this doesn't work!
mat["modulators2"]["odor"][0][0]["control"][0][0][0:2] #you have to get you dictionnary from the array first ! (but remove the [0:2])
```
<br><br>


### For R:
```{r}
library(R.matlab)

subject = dir("data", pattern="sub-*", all.files=T,
    full.names=T)

data = tibble()

for (s in subject) {
  
  mat <- readMat(paste(s, "/data.mat", sep=""))
  
  #start
  onset = mat$onsets2[[2]]
  duration = mat$durations2[[2]]
  modulator = mat$modulators2[[1]] #watchout it's reversed here !
  start = cbind(onset, duration, modulator)
  write.table(start, paste(s, "/start.txt", sep=""), row.names = F, sep="\t", col.names = F)
  
  
  #odors
  ##reward
  onset =  mat$onsets2[[1]][[1]]
  duration = mat$durations2[[1]][[1]]
  modulator = as_tibble(mat$modulators2[[2]][[1]]) #watchout it's reversed here !
  odor_rew = cbind(onset, duration, modulator)
  write.table(odor_rew, paste(s, "/odor_rew.txt", sep=""), row.names = F, sep="\t", col.names = F)
  
  modulator$condition = "reward"; mod_rew = cbind(modulator, onset); 
  
  ##neutral
  onset =  mat$onsets2[[1]][[2]]
  duration = mat$durations2[[1]][[2]]
  modulator = as_tibble(mat$modulators2[[2]][[2]]) #watchout it's reversed here !
  odor_neu = cbind(onset, duration, modulator)
  write.table(odor_neu, paste(s, "/odor_neu.txt", sep=""), row.names = F, sep="\t", col.names = F)
  
  modulator$condition = "neutral"; mod_neu = cbind(modulator, onset); 
  
  ##control
  onsets_odor_con =  mat$onsets2[[1]][[3]]
  durations_odor_con = mat$durations2[[1]][[3]]
  modulator_odor_con = as_tibble(mat$modulators2[[2]][[3]]) #watchout it's reversed here !
  odor_con = cbind(onset, duration, modulator)
  write.table(odor_con, paste(s, "/odor_con.txt", sep=""), row.names = F, sep="\t", col.names = F)
  
  modulator$condition = "control"; mod_con = cbind(modulator, onset); 
  
  #behavioral
  df = rbind (mod_rew, mod_neu, mod_con);
  df = df[order(df$onset),] #order by onset
  df$trial = 1:54 #trial nuumber
  df$id = str_sub(s,-3,-1) #extract last 3 charcaters of the string
  df$group = str_sub(s,-10,-4) 
  
  data = rbind(data,df)
}


```
### Optional:

Try to create a dataset that includes each subjects behavioral dataset appended together with a column with their ID (numbers following control or treatment), a column with their group (control or treatment), a column with the condition name (reward, control or neutral) and 1 column with their behavioral data (from the modulators2.odor field) and a column with the number of the trial (1-54). Have extra caution to put the behavioral data back into the right order (you can get that by sorting them by their  onsets !). Hang in there..
<br><br>

## 6) The end

<br><br>
There you are, you finished this list of exercise. Good job! You deserve a pat on the shoulder and a good cup (jar?) of coffee.

I hope you learned things that will be useful for your research/career and let me know whta you liked/disliked.



<br>
<br><br><br><br>

