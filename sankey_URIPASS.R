library(googleVis)
library(tidyverse)

pass <- read.csv('PASS_Fall 2014 course enrl history thru 2176 term.csv')
pass_career <- read.csv('PASS_Fall 2014 careerterm data thru 2176 term.csv',
                        stringsAsFactors = FALSE)
pass_career <- pass_career[1:364,]

#over terms
#origin = STATUS
#dest = STATUS2

#if WTHDRW_REASON
#elif DEGR_PLAN1
#else ENRL_STAT
pass_career$STATUS <- ifelse(is.na(pass_career$GRAD_TERM), 
                             pass_career$ENRL_STAT, 
                             pass_career$DEGR_PLAN1)
pass_career$STATUS2 <- ifelse(pass_career$WTHDRW_CODE == 'WDR',
                              pass_career$WTHDRW_CODE,
                              pass_career$STATUS)

pass_terms <- select(pass_career,SID,TERM,STATUS2) %>% spread(TERM,STATUS2)
pass_terms[is.na(pass_terms)] <- 'NENR'

terms <-colnames(pass_terms)[-1]

df <- data.frame(SID=character(),
                 START=character(),
                 END=character()
                 )

for (i in 1:(length(terms)-1)){
  c1<-terms[i]
  c2<-terms[i+1]
  t<-select(pass_terms,"SID",c1,c2)
#  t[,2] <- ifelse(t[,2] == 'ENRL', c1, t[,2])
#  t[,3] <- ifelse(t[,3] == 'ENRL', c2, t[,3])
  t[,2] <- paste0(t[,2],c1)
  t[,3] <- paste0(t[,3],c2)
  colnames(t) <- c('SID','START','END')
#  t<-filter(t, START !='NENR')
  df<-rbind(df,t)
}

df2 <- df %>% group_by(START,END) %>% summarise(count=n())
df2$count <- ifelse(grepl('NENR', df2$START) & !grepl('WDR', df2$END),0,df2$count)
df2$count <- ifelse(grepl('WDR', df2$START),0,df2$count)

plot(gvisSankey(df2, from="START", to="END", weight = "count",
     options=list(height=730, width=1000,
                  sankey = "{ node: { colors: ['green',
                                                'red',
                                                'green',
                                                'red', 
                                                'green',
                                                'green',
                                                'red',
                                                'green',
                                                'green',
                                                'green',
                                                'red',
                                                'purple',
                                                'green',
                                                'red',
                                                'green','red',
                                                'green','red',
                                                'green','red',
                                                'green','purple',
                        'green','blue',
                  'blue','blue','green','red',
                                                'red','green','purple','purple',
                  'red'] },
                              iterations: 0
                            }"
                  )
                )
     )
