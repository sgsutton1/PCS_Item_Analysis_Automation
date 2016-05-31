ptm <- proc.time()


library(grid)
library(gridBase)
library(gridExtra)
library(dplyr)
library(psychometric)
library(CTT)


setwd("C:/Users/suttonst/desktop/Automated Item Analysis/dichotomy_scores")


items = read.csv(file="final_items_v1.csv", head=T)

#i = read.csv(file="all_items2.csv",head=T)

#items = testerrrrrrr

setwd("C:/Users/suttonst/desktop/Automated Item Analysis/score_summary/")

sums = read.csv(file="allscores.csv",head=T)


infor = read.csv(file="C:/Users/suttonst/desktop/Automated Item Analysis/test_info.csv")

infor = infor[,c(1,8)]

colnames(infor)[1] = "test_id"

sums = inner_join(sums,infor,by=c("test_id" = "test_id"))

#alg2_ids = c(3007532)

#ELA3MODB = 3007530



for (s in unique(sums$test_id)){

#for (s in ELA3MODB){
  
  
  
t1 = subset(sums,sums$test_id == s)
t1 = droplevels(t1)

t1$test_name = gsub("/","-",t1$test_name)

x = summary(t1$score)

tab1 = data.frame(TestName = as.character(t1$test_name[1]),
                 Date = Sys.Date())


tab2 = data.frame(Items=as.integer(t1[1,12]),
                  Students = as.integer(nrow(t1)),
                  Min_Score = as.numeric(x[[1]]),
                  Q1 = as.numeric(x[[2]]),
                  Mean = as.numeric(x[[4]]),
                  Median = as.numeric(x[[3]]),
                  Q3 = as.numeric(x[[5]]),
                  Max_Score = as.numeric(x[[6]]),
                  StdDev = as.numeric(round(sd(t1$score)),digits=1),
                  Alpha = as.numeric(0))


tab2 = tab2[1,]


setwd("C:/Users/suttonst/desktop/Automated Item Analysis/dichotomy_scores/")

fname = paste(t1$test_name[1],".csv",sep='')



if(fname %in% dir() == TRUE){

ia_dat = read.csv(file=fname,head=T)

ia_dat = ia_dat[,3:length(ia_dat)]


rel_if_d = reliability(ia_dat,itemal=TRUE)


tab3 = item.exam(ia_dat,discrim=T)

tab3[is.na(tab3)] <- 0

tab3 = tab3[,3:5]

tab3$AlphaIfDel = rel_if_d$alphaIfDeleted

tab3 = round(tab3,digits=3)


tab2$Alpha = round(rel_if_d$alpha,digits=3)

keys = read.csv(file="C:/Users/suttonst/desktop/Automated Item Analysis/test_keys.csv")

t1key = droplevels(subset(keys,keys$test_id == s))
 
t1key = t1key[order(t1key$test_question_id),]

tab3$correct_answer = t1key$correct_answer
tab3$item_id = t1key$test_question_id



item_holder = droplevels(subset(items,items$test_id == s))
item_holder = item_holder[order(item_holder$test_question_id),]

item_out_df = data.frame(Omit=as.numeric(),A=as.numeric(),B=as.numeric(),C=as.numeric(),D=as.numeric())

count=1

new_p_holder = data.frame(Pval = as.numeric())


for (i in unique(item_holder$test_question_id)){
  
  
  

  
  idf = subset(item_holder, item_holder$test_question_id == i)
  freq_tab = t(as.matrix((table(idf$student_answer))))
  
  freq_tab = freq_tab[freq_tab[,1:length(freq_tab)] > 0,drop=F]
  
  #freq_tab = freq_tab[,c(" ","a","b","c","d")]
  
  
  keyer = subset(t1key,t1key$test_question_id == i)
  
  #if (keyer$points_possible > 1){
    
  new_pval = sum(idf$points_earned)/(keyer$points_possible * tab2$Students)
  new_p_holder[count,1] = new_pval
  
  #}
  
  
  freqs = round((freq_tab/nrow(idf)*100),digits=1)
  freqs = freqs[1:5]
  
  
  item_out_df[count,] = freqs
  
    
    count = count + 1
}

item_out_df$ids = (unique(item_holder$test_question_id)) 
final_tab3 = inner_join(tab3,item_out_df, by=c("item_id" = "ids"))

final_tab3[,1:3] = round(final_tab3[,1:3], digits =2)

final_tab3$Difficulty = new_p_holder$Pval
#tab3$C1 = freqs


tab4 = data.frame(ItemDifficulty = as.character(c("Easy (Higher than 70%)","Moderate (40% to 70%)","Hard (Less than 40%)")),
  PercentItems = as.numeric(c(0,0,0)),
  ItemDiscrimination = as.character(c("Good (Higher than 0.3)","Acceptable (0.2 to 0.3)", "Needs Review (Less than 0.2)")),
  PercentItems = as.numeric(c(0,0,0)))

tab4[1,2] = (sum(final_tab3$Difficulty > 0.699)/nrow(final_tab3)*100)
tab4[2,2] = (sum(final_tab3$Difficulty > 0.399 & final_tab3$Difficulty < 0.7)/nrow(final_tab3)*100)
tab4[3,2] = (sum(final_tab3$Difficulty < 0.4)/nrow(final_tab3)*100)


tab4[1,4] = (sum(final_tab3$Item.Tot.woi > 0.299)/nrow(final_tab3)*100)
tab4[2,4] = (sum(final_tab3$Item.Tot.woi > 0.199 & final_tab3$Item.Tot.woi < 0.3)/nrow(final_tab3)*100)
tab4[3,4] = (sum(final_tab3$Item.Tot.woi < 0.2)/nrow(final_tab3)*100)


layout(matrix(c(1,1,2,3)),2,2)

final_tab3 = final_tab3[,c(-6)]

final_tab3[ final_tab3$Difficulty == "NaN" , 1:4] <- "-"

if(t1key$correct_answer[1] == " "){
  final_tab3 = final_tab3[,1:4]
 }else{ 
  final_tab3 = final_tab3
}

length(final_tab3)

final_tab3 = final_tab3[,c(2,1,3:length(final_tab3))]

final_tab3$Difficulty[final_tab3$Difficulty > 1.0] <- "N/A"
final_tab3$Discrimination[final_tab3$Discrimination > 1.0] <- "N/A"

# f1 = tableGrob(tab1,rows = NULL)
# f2 = tableGrob(tab2,rows=NULL)
# f3 = tableGrob(final_tab3)
# f4 = tableGrob(tab4,rows=NULL)
# grid.arrange(f1,f2,f4,f3,layout_matrix = cbind(c(1,2,3,4)))
# grid.arrange(f1,f2,f4,f3,ncol=1,nrow=4,heights = unit(c(3.5,2.2,5.5,4),"cm"))
# marrangeGrob(f1,f2,f4,f3,hist_g,nrow=6,ncol=2)
# 
# tabs = cbind(tab1,tab2,tab4)
# tabs2 = list(tab1,final_tab3)




pdf_n2 = paste(tab1[[1]],"_tables.csv",sep='')
pdf(file=pdf_n2)

table_fname = paste(tab1[[1]],"_tables.csv",sep='')

setwd("C:/Users/suttonst/desktop/Automated Item Analysis/tables/")

write.table(tab1,table_fname,row.names=F,sep=",")
write.table(tab2,table_fname,row.names=F,sep=",",append=TRUE)
write.table(tab4,table_fname,row.names=F,sep=",",append=TRUE)
write.table(final_tab3,table_fname,row.names=F,sep=",",append=TRUE)

dev.off()

par(mar=c())
pdf_n = paste(tab1[[1]],"_hist.pdf",sep='')
pdf(file=pdf_n)
hist_g = hist(t1$score,
              main = tab1[[1]],
              xlab="Percent Score",
              
              breaks=5,
              right=T)
dev.off()
}}

proc.time() - ptm
