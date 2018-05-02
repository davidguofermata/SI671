library(data.table)


cen_dat=fread('/Users/davidguo/SI671/project/cc-est2016-alldata.csv')
fl=fread('/Users/davidguo/SI671/project/FL-clean.csv')

fl[,stop_by_off_date:=.N,by=list(stop_date, officer_id)]
fl[,stop_by_off_date:=.N,by=list(stop_date, officer_id)]

library(stringr)
fl$hour=""
fl$hour=substring(fl$stoptime, 1,2)
mod_fit <- glm(out_bin ~ officer_id,  data=ct_train, family="binomial")

ggplot(data=fl) + 
  stat_count(mapping = aes(x=day, y=..prop.., group=1))+ 
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Proportion of Stop Outcomes in FL") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=fl, aes(x=as.Date(stop_date))) + geom_freqpoly(aes(color=stop_outcome),size=1,alpha=I(0.5))
  ggtitle("Proportion of Stop Outcomes in FL") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

fl_sub=fl[,c("officer_id","stop_chron","stop_outcome")]
names(fl_sub)=c("id","time","event")
flseq=seqecreate(fl_sub[complete.cases(fl_sub),])

fl$stop_date=as.Date(fl$stop_date)
fl=fl[which(fl$stop_date>"2010-12-31"),]

fl[fl$stop_outcome=="",]=NA
fl=fl[!is.na(fl$stop_outcome),]
fl$out_bin=NA
fl$out_bin=as.factor(fl$out_bin)
fl[which(fl$stop_outcome=="Warning" | fl$stop_outcome=="Faulty Equipment Notice"),]$out_bin="Light"
fl[which(fl$stop_outcome=="Citation" |
           fl$stop_outcome=="Misdemanor Arrest" | 
           fl$stop_outcome=="Felony Arrest"),]$out_bin="Severe"
fl$officer_race=as.factor(fl$officer_race)
fl$driver_race=as.factor(fl$driver_race)
fl$officer_rank=as.factor(fl$officer_rank)
fl$officer_gender=as.factor(fl$officer_gender)

fl=fl[!is.na(fl$out_bin),]
write.csv(fl, "fl_sub.csv")
set.seed(32423)
subind=sample(seq(1:4563349), size=.75*4563349)
fl_train=fl[subind,]
fl_test=fl[-subind,]

 set.seed(4535948)
 ind=sample(seq(1:5369820), size=.75*5369820)
 ind=sample(seq(1:100000), size=.75*100000)
 fl_train=fl_sub[ind,]
 fl_test=fl_sub[-ind,]

temp$n_stops=nchar(temp$seq)

out_ran=sample(c("Light","Severe"), size=dim(fl_test)[1], replace=TRUE, prob=c(0.5,0.5))
pred_ran=mean(fl_test$out_bin==out_ran, na.rm=TRUE)

out_ran_wt=sample(c("Light","Severe"), size=dim(fl_test)[1], replace=TRUE, prob=c(0.238,0.762))
pred_ran=mean(fl_test$out_bin==out_ran_wt, na.rm=TRUE)

mod_fit = glm(data=fl_train, out_bin~officer_race+driver_race, family="binomial")
mod_pred=ifelse(predict(mod_fit, fl_test, type="response") > 0.5, "Severe", "Light")
mean(fl_test$out_bin==mod_pred, na.rm=TRUE)

mod_fit2 = glm(data=fl_train, out_bin~officer_race+driver_race+driver_age, family="binomial")
mod_pred2=ifelse(predict(mod_fit2, fl_test, type="response") > 0.5, "Severe", "Light")
mean(fl_test$out_bin==mod_pred2, na.rm=TRUE)

mod_fit = glm(data=fl_train, out_bin~as.factor(violation), family="binomial")
mod_pred=ifelse(predict(mod_fit, fl_test, type="response") > 0.5, "Severe", "Light")
mean(fl_test$out_bin==mod_pred, na.rm=TRUE)


library(chron)
fl$stop_date=as.character(fl$stop_date)
fl$stop_sec=paste(fl$stop_time,":00", sep="")
fl$stop_chron=chron(dates=fl$stop_date, times=fl$stop_sec,
                        format=c(dates="y-m-d", times="h:m:s"))
fl$stop_chron=trunc(fl$stop_chron, units="hours")
fl_seq=fl[,c("stop_chron","officer_id","stop_outcome", "driver_race")]

fl_seq$abr=NA
fl_seq$rabr=NA
fl_seq[which(fl_seq$stop_outcome=="Citation"),]$abr="C"
fl_seq[which(fl_seq$stop_outcome=="Warning"),]$abr="W"
fl_seq[which(fl_seq$stop_outcome=="Faulty Equipment Notice"),]$abr="E"
fl_seq[which(fl_seq$stop_outcome=="Felony Arrest"),]$abr="F"

fl_seq[which(fl_seq$driver_race=="Asian"),]$rabr="A"
fl_seq[which(fl_seq$driver_race=="Black"),]$rabr="B"
fl_seq[which(fl_seq$driver_race=="Hispanic"),]$rabr="H"
fl_seq[which(fl_seq$driver_race=="Other"),]$rabr="O"
fl_seq[which(fl_seq$driver_race=="White"),]$rabr="W"

fl_seq$abr=as.factor(fl_seq$abr)
fl_seq$rabr=as.factor(fl_seq$rabr)

library(data.table)
temp=fl_seq[order(officer_id,stop_chron)]
temp[,seq:=paste(abr, collapse=''),by=c("officer_id")]
temp[,rseq:=paste(rabr, collapse=''),by=c("officer_id")]
temp=unique(temp[,c("officer_id","seq","rseq")])

temp=temp[which((nchar(temp$seq)>1 | nchar(temp$rseq)>1) & (nchar(temp$rseq)<10000) | nchar(temp$seq)<10000),]


# library(markovchain)
# seq_m=tempp[1]
# transmat=markovchainListFit(temppp)


library(stringr)
tempp=str_split(temp$seq,"")
temppr=str_split(temp$rseq,"")
library(plyr)
temppp=ldply(tempp,rbind)#data.frame(t(ldply(tempp,rbind)))
tempppr=ldply(temppr,rbind)

library(seqHMM)
tempseq=seqdef(temppp, 1:ncol(temppp))
temprseq=seqdef(tempppr, 1:ncol(tempppr))

rownames(tempseq)=paste("P",rownames(tempseq),sep="")

set.seed(23429389)
ind=sample(nrow(temprseq))
temprseq_shuf=temprseq[ind,]
folds <- cut(seq(1,nrow(temprseq_shuf)),breaks=8,labels=FALSE)

testIndexes <- which(folds==1,arr.ind=TRUE)
testData <- temprseq_shuf[testIndexes, ]
trainData <- temprseq_shuf[-testIndexes, ]

testData=tempp[ind[testIndexes]]

#write.csv(tempseq, "seq_fl.csv")
#write.csv(testData, "/Users/davidguo/SI671/project/testseq.csv")
trainhmm2r=build_hmm(trainData, n_states=2)
trainhmm3r=build_hmm(trainData, n_states=3)
trainhmm4r=build_hmm(trainData, n_states=4)
trainhmm5r=build_hmm(trainData, n_states=5)

trainhmm2

library(markovchain)
#trainfb=forward_backward(trainhmm)
hmmfit2r=fit_model(trainhmm2r, control_em=list(maxeval=20, print_level=2), 
                 control_global=list(maxeval=20),
                 control_local=list(maxeval=20),threads=2)
hmmfit3r=fit_model(trainhmm3r, control_em=list(maxeval=20, print_level=2), 
                    control_global=list(maxeval=20),
                    control_local=list(maxeval=20),threads=2)
hmmfit4r=fit_model(trainhmm4r, control_em=list(maxeval=20, print_level=2), 
                   control_global=list(maxeval=20),
                   control_local=list(maxeval=20),threads=2)
hmmfit5r=fit_model(trainhmm5r, control_em=list(maxeval=20, print_level=2), 
                   control_global=list(maxeval=20),
                   control_local=list(maxeval=20),threads=2)

set.seed(23389)
ind=sample(nrow(tempseq))
tempseq_shuf=tempseq[ind,]
folds <- cut(seq(1,nrow(tempseq_shuf)),breaks=8,labels=FALSE)

testIndexes <- which(folds==1,arr.ind=TRUE)
testData <- tempseq_shuf[testIndexes, ]
trainData <- tempseq_shuf[-testIndexes, ]

trainhmm2=build_hmm(trainData, n_states=2)
trainhmm3=build_hmm(trainData, n_states=3)
trainhmm4=build_hmm(trainData, n_states=4)
trainhmm5=build_hmm(trainData, n_states=5)
trainhmm6=build_hmm(trainData, n_states=6)
trainhmm10=build_hmm(trainData, n_states=10)

hmmfit2=fit_model(trainhmm2, control_em=list(maxeval=20, print_level=2), 
                  control_global=list(maxeval=20),
                  control_local=list(maxeval=20),threads=2)
hmmfit3=fit_model(trainhmm3, control_em=list(maxeval=20, print_level=2), 
                  control_global=list(maxeval=20),
                  control_local=list(maxeval=20),threads=2)
hmmfit4=fit_model(trainhmm4, control_em=list(maxeval=20, print_level=2), 
                  control_global=list(maxeval=20),
                  control_local=list(maxeval=20),threads=2)
hmmfit5=fit_model(trainhmm5, control_em=list(maxeval=20, print_level=2), 
                   control_global=list(maxeval=20),
                   control_local=list(maxeval=20),threads=2)
hmmfit6=fit_model(trainhmm6, control_em=list(maxeval=20, print_level=2), 
                  control_global=list(maxeval=20),
                  control_local=list(maxeval=20),threads=2)
hmmfit10=fit_model(trainhmm10, control_em=list(maxeval=20, print_level=2), 
                  control_global=list(maxeval=20),
                  control_local=list(maxeval=20),threads=2)

fl=subset(fl,select=-c(officer_rank_cl))


install.packages("Rglpk")
install.packages("CRF")


simseq=simulate_hmm(1000, 
                    initial_probs = trainhmm4$initial_probs, 
                    transition_probs = trainhmm4$transition_probs, 
                    emission_probs = trainhmm4$emission_probs, 
                    sequence_length = 1000) #sum(testData["P2581",]!="%"))
randhmm=simulate_hmm(1000, initial_probs = c(0.2, 0.2, 0.2, 0.2, 0.2) ,
                     transition_probs = matrix(0.2,5,5),
                     emission_probs = matrix(.25,5,4),
                     sequence_length=1000)

plot(simseq$observations, border=NA)
plot(randhmm$observations)
plot(simseq$observations,idxs=0, sortv="from.start", border=NA, space=0)
plot(randhmm$observations,idxs=0, sortv="from.start", border=NA, space=0)

library(HMM)
phmm2=initHMM(States=c("1","2"), Symbols=c("C","E","F","W"), startProbs=trainhmm2$initial_probs,
             transProbs=trainhmm2$transition_probs, emissionProbs = trainhmm2$emission_probs)
phmm3=initHMM(States=c("1","2","3"), Symbols=c("C","E","F","W"), startProbs=trainhmm3$initial_probs,
              transProbs=trainhmm3$transition_probs, emissionProbs = trainhmm3$emission_probs)
phmm4=initHMM(States=c("1","2","3","4"), Symbols=c("C","E","F","W"), startProbs=trainhmm4$initial_probs,
              transProbs=trainhmm4$transition_probs, emissionProbs = trainhmm4$emission_probs)
phmm5=initHMM(States=c("1","2","3","4","5"), Symbols=c("C","E","F","W"), startProbs=trainhmm5$initial_probs,
              transProbs=trainhmm5$transition_probs, emissionProbs = trainhmm5$emission_probs)

phmm2f=exp(forward(phmm2,testData[[1]][-2083]))

library(lubridate)

fl$day=wday(as.Date(fl$stop_date),label=TRUE)

library(chron)
# fl_sub$stop_sec=paste(fl_sub$stop_time,":00", sep="")
# fl_sub$stop_chron=chron(dates=fl_sub$stop_date, times=fl_sub$stop_sec,
#         format=c(dates="y-m-d", times="h:m:s"))
# fl_sub$stop_chron=trunc(fl_sub$stop_chron, units="hours")
# fl_sub_seq=fl_sub[,c("stop_chron","officer_id","stop_outcome")]
# 
# fl_sub_seq$abr=NA
# fl_sub_seq[which(fl_sub_seq$stop_outcome=="Citation"),]$abr="C"
# fl_sub_seq[which(fl_sub_seq$stop_outcome=="Warning"),]$abr="W"
# fl_sub_seq[which(fl_sub_seq$stop_outcome=="Faulty Equipment Notice"),]$abr="E"
# fl_sub_seq[which(fl_sub_seq$stop_outcome=="Felony Arrest"),]$abr="F"
# library(data.table)
# temp=fl_sub_seq[order(officer_id,stop_chron)]
# temp[,seq:=paste(abr, collapse=''),by=c("officer_id")]
# temp=unique(temp[,c("officer_id","seq")])
# fl_sub_seq$stop_outcome=as.character(fl_sub_seq$stop_outcome)

library(reshape2)
temp=dcast(fl_sub_seq,officer_id~stop_chron, value.var = "stop_outcome")
seq_sub=seqdef(temp, labels=c("Citation", "Warning", "Faulty Equipment Notice", "Felony Arrest"))


ggplot(data=tx) + 
  stat_count(mapping = aes(x=stop_outcome, y=..prop.., group=1))+ 
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Proportion of Stop Outcomes in TX")

# library(ggplot2)
# library(GGally)
# ggparcoord(ct, columns=c("weekday", "stop_by_off_dayofweek"), mapping=aes(color=officer_id))
# #ggplot(data=ct, aes(x=violation_raw)) + geom_bar() + coord_flip()

library(dplyr)
ct_wk_race= ct %>%
  group_by(weekday, driver_race) %>%
  mutate(mn_stop_race_dayofweek=mean(stop_by_race_dayofweek)) %>%
  #distinct(race, mn_stop_dayofweek)

ct_wk=ct %>% select(stop_date, weekday, officer_id, stop_by_off_day, stop_by_off_dayofweek) %>%
      distinct(stop_date, weekday, officer_id, .keep_all=TRUE) %>%
      group_by(weekday) %>%
      mutate(mn_stop_dayofweek=mean(stop_by_off_dayofweek)) %>%
      distinct(weekday, mn_stop_dayofweek)

library(ggplot2)

ggplot(aes(x=stop_date, y=stop_by_off_day, group=officer_id),
       data=subset(ct, officer_id=unique(ct$officer_id)[1]))+geom_line(size=0.05)
       
ggplot(aes(x=weekday, y=stop_by_off_dayofweek, col=officer_id, group=officer_id),
       data=ct)+ geom_line(size=0.4)  + theme(legend.position="none")

library(dplyr)
#ct_prop=ct %>% count(officer_id, driver_race) %>% mutate(tot=sum(n))
  #prop.table(addmargins(),1
    
      temp=table(ct[, c("officer_id", "driver_race")])
      ct_tab=temp#/rowSums(temp)
      ct_all=as.vector(addmargins(temp,2)[,6]/3574097)
      ct_pop_race=c(135565,362296,479087,NA,2772410)
      ct_tab_adj=ct_tab
      for (i in 1:5){
        ct_tab_adj[,i]=ct_tab[,i]/ct_pop_race[i]
      }
      for(i in 1:length(ct_all)){
       ct_tab_adj[i,]=ct_tab_adj[i,]/ct_all[i]
      }
ct_tab_adj[,-4]
edge_ct=as.matrix(ct_tab_adj)
net_ct= graph_from_incidence_matrix(edge_ct)

cluster_label_prop(net_ct)
deg.dist <- degree_distribution(net_ct, cumulative=T, mode="all")
plot(1-deg.dist)

plot(net_ct,vertex.label=NA, vertex.color=ct$county_name, 
     edge.width=0.5, edge.curved=0.1,
     layout=layout_with_kk(net_ct)
     )
  
      
#install.packages("acs")
library(acs)
#library(census)
#getCensus("Connecticut")

library(blscrapeR)
library(ggplot2)


df <- get_bls_county(stateName = "Connecticut")
df$unemployed_rate=as.numeric(df$unemployed_rate)
map_bls(map_data=df, fill_rate = "unemployed_rate", projection = "lambert",
        stateName = c("Connecticut"))

#md=fread('/Users/davidguo/SI671/project/MD-clean.csv')
#baltimore=fread('/Users/davidguo/SI671/project/BPD_arrests.csv')
md2_1=fread('/Users/davidguo/SI671/project/MD_Traffic_Stops_through_20161003/md_stops/Export Worksheet-Table 1.csv')
md2_4=fread('/Users/davidguo/SI671/project/MD_Traffic_Stops_through_20161003/md_stops/MD-2016-Table 1.csv')
md2_2=fread('/Users/davidguo/SI671/project/MD_Traffic_Stops_through_20161003/md_stops/Sheet1-Table 1.csv')
md2_3=fread('/Users/davidguo/SI671/project/MD_Traffic_Stops_through_20161003/md_stops/Sheet2-Table 1.csv')
names(md2_2)=names(md2_1)
names(md2_3)=names(md2_1)
names(md2_4)=names(md2_1)
md2=rbind(md2_1,md2_2,md2_3,md2_4)
remove(md2_1,md2_2,md2_3,md2_4)
md2[md2$ETHNICITY=="W`" | md2$ETHNICITY=="W"]$ETHNICITY="WHITE"
md2[md2$ETHNICITY=="BLK"]$ETHNICITY="BLACK"
md2[md2$ETHNICITY=="" | md2$ETHNICITY=="UNKNOWN"]$ETHNICITY=NA
md2[md2$ETHNICITY=="f"]$ETHNICITY="F" #hispanic
md2[md2$ETHNICITY=="m"]$ETHNICITY="M" #black
#hiq maybe his?

md2=md2[-which((md2$ETHNICITY=="F" | md2$ETHNICITY=="M" | md2$ETHNICITY=="hiq"))]
md2$ETHNICITY=as.factor(md2$ETHNICITY)
md2$OFFICERID=as.factor(md2$OFFICERID)
edge_md=as.matrix(table(md2[, c("OFFICERID", "ETHNICITY")]))
net= graph_from_incidence_matrix(edge_md)

#Choose your favorite algorithm to find communities.  The algorithm below is great for large networks but only works with undirected graphs
c_g <- fastgreedy.community(net)

#Collapse the graph by communities.  This insight is due to this post http://stackoverflow.com/questions/35000554/collapsing-graph-by-clusters-in-igraph/35000823#35000823

res_g <- simplify(contract(net, membership(c_g)))
plot(res_g,vertex.label=NA)

#install.packages(xlsx)
#install.packages("SpatioTemporal")
#install.packages("ape")
#install.packages("igraph")

library(igraph)
