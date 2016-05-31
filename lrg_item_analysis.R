library(dplyr)

setwd("C:/Users/suttonst/desktop/Automated Item Analysis/dichotomy_scores")


items = read.csv(file="final_items_v1.csv", head=T)

items = items[,c(2,3,5,7,11)]

#it = inner_join(items,d,by=c("test_id" = "test_id"))
#items = it

colnames(items)[2] = "test_id"



items = items[order(items$test_id,items$test_question_id,items$student_code),]


items$test_name = gsub("/","-",items$test_name)

#items$test_id = as.factor(items$test_id)

rm(outdf,tdf,tdf_i,tdf_join,tdf_final,count1,count2,count3,filename,i,n_items,tname,unique_items,x,cname)

u = unique(items$test_question_id)

count1 = 1
count2 = 2
count3 = 3

gr1_modb = 3007532

for (i in unique(items$test_id)){
#for (i in gr1_modb){
  tdf = subset(items,items$test_id == i)
  tdf = droplevels(tdf)
  
  n_items = length(unique(tdf$test_question_id))
  
  outdf = data.frame("student_code" = unique(tdf$student_code)
                     )
  outdf[1:nrow(outdf),2] = as.character(tdf[1,5])
  colnames(outdf)[2] = "test_name"
  unique_items = unique(tdf$test_question_id)    
  if(length(unique_items) > 4){
  for(x in unique_items){
    
    
    
    tdf_i = subset(tdf,tdf$test_question_id == x)
    tdf_i = droplevels(tdf_i)
    cname = paste("Q",count1,sep='')
    
    colnames(tdf_i)[4] = cname
    tdf_i = tdf_i[,c(1,4)]  
    
    if(count1 == 1){  
      tdf_join = left_join(outdf,tdf_i,by= c("student_code" = "student_code"))
    } else if (count1 == 2) {
      tdf_final = left_join(tdf_join,tdf_i,by=c("student_code" = "student_code"))
    } else {
      tdf_final = left_join(tdf_final,tdf_i,by=c("student_code" = "student_code"))
    }
    
    
    #outdf[,count2] = tdf_i$points_earned
    
    count2 = (count2 + 1)
    count1 = count1 + 1
  }
  
  
  
  count1 = 1
  count2 = 2
  
  tname = as.character(tdf[1,5])
  
  filename = paste(tname,".csv",sep='')
  
  write.csv(tdf_final,file=filename,row.names = F)
 } else{}
  
  
}




##tcase = subset(items,items$test_id == 3007478)
##tcase = droplevels(tcase)
##length(unique(tcase$test_question_id))
