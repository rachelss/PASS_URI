library(googleVis)
library(tidyverse)

############################################
year <- '2012'
start_term <- '2129'  # change terms to include/exclude enrollment prior to expected freshman fall; comment out to include all terms
filter_summer <- TRUE  #use FALSE to include summer enrollment
filter_NENR <- TRUE   #use FALSE to include NENR values
var_of_interest <- 'SEX'  #use either SEX or ETHN
############################################

filename_pass <- paste0('PASS_Fall ',year,' course enrl history thru 2176 term.csv')
filename_passcareer <- paste0('PASS_Fall ',year,' careerterm data thru 2176 term.csv')

pass <- read.csv(filename_pass, stringsAsFactors = FALSE)
pass_career1 <- read.csv(filename_passcareer, stringsAsFactors = FALSE)
pass_career1 <- pass_career1 %>% filter(!is.na(SID))

if (filter_summer == TRUE){
  pass_career1 <- pass_career1 %>% 
    filter(TERM %% 10 !=6 | TERM == max(pass_career1$TERM))
}

pass_race <- pass %>% select(SID,ETHN) %>% distinct(SID,ETHN)
pass_sex <- pass %>% select(SID,SEX) %>% distinct(SID,SEX)

pass_career <- left_join(pass_career1, pass_race, by='SID')
pass_career <- left_join(pass_career, pass_sex, by='SID')

if(var_of_interest =='ETHN'){ pass_career <- filter(pass_career,ETHN!='NORPT') }

#over terms
#origin = STATUS
#dest = STATUS2

#if WTHDRW_REASON
#elif DEGR_PLAN1
#else ENRL_STAT

majors <- table(pass_career$DEGR_PLAN1)[-1]

pass_career$STATUS <- ifelse(is.na(pass_career$GRAD_TERM), 
                             pass_career$ENRL_STAT, 
                             pass_career$DEGR_PLAN1)
pass_career$STATUS2 <- ifelse(pass_career$WTHDRW_CODE == 'WDR',
                              pass_career$WTHDRW_CODE,
                              pass_career$STATUS)
pass_career$STATUS3 <- ifelse(pass_career$STATUS2 == 'ENRL',
                              pass_career[[eval(var_of_interest)]],
                              pass_career$STATUS2)

pass_terms <- select(pass_career,SID,TERM,STATUS3) %>% 
  distinct(SID,TERM,STATUS3) %>% spread(TERM,STATUS3)

terms <-colnames(pass_terms)[-1]

df <- data.frame(SID=character(),
                 START=character(),
                 END=character()
)

# show majors for subsequent years rather than disappearing / NERL
for (i in 1:(ncol(pass_terms)-1)){
  for (j in 1:nrow(pass_terms)){
    if (pass_terms[j,i] %in% c(names(majors),'GRAD')){
      #pass_terms[j,i+1] <- pass_terms[j,i]
      pass_terms[j,i+1] <- 'GRAD'
      
    }
  }
}

pass_terms[is.na(pass_terms)] <- 'NENR'

#filter for later start term
if (exists("start_term")){
  start_term_index <- which(colnames(pass_terms)==start_term)
  last_term <- colnames(pass_terms)[length(colnames(pass_terms))]
  pass_terms <- select(pass_terms,SID,start_term_index:length(colnames(pass_terms)))
  terms <-colnames(pass_terms)[-1]
}

for (i in 1:(length(terms)-1)){
  c1<-terms[i]
  c2<-terms[i+1]
  t<-select(pass_terms,"SID",c1,c2)
#  t[,2] <- ifelse(t[,2] == 'ENRL', c1, t[,2])
#  t[,3] <- ifelse(t[,3] == 'ENRL', c2, t[,3])
  t[,2] <- paste0(t[,2],'_',c1)
  t[,3] <- paste0(t[,3],'_',c2)
  colnames(t) <- c('SID','START','END')
#  t<-filter(t, START !='NENR')
  df<-rbind(df,t)
}

df2 <- df %>% group_by(START,END) %>% summarise(count=n())
df2$count <- ifelse(grepl('NENR', df2$START) & !grepl('WDR', df2$END),0,df2$count)
df2$count <- ifelse(grepl('WDR', df2$START),0,df2$count)

if (filter_NENR == TRUE){
  df2$count <- ifelse(grepl('NENR', df2$END) & !grepl(start_term, df2$START),0,df2$count)
}

p<-gvisSankey(df2, from="START", to="END", weight = "count",
     options=list(height=730, width=1000,
                  sankey = "{iterations: 0 }"
                  )
                )
plot(p)
