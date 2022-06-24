X11(type="Xlib")
source(file="game.r")
# library(tuneR)
library(beepr)
# setWavPlayer('/usr/bin/afplay')
Snake<-setRefClass("Snake",contains="Game",
   methods=list(
     initialize = function(name,width,height,debug) {
       callSuper(name,width,height,debug)
       name<<-"Snake Game"
     },
     init = function(){
       callSuper()
       # paused用来记录当前是否为从暂停中恢复的状态，若是，则在stage0的
       # 初始化阶段，导入暂停前的棋盘
       if (paused) {
         e<<-log_e
         m<<-log_m
       }else{
         # 否则的话，则按照正常初始化方式来进行处理
         e$step<<-1/width
         e$dir<<-e$lastd<<-'up'
         e$head<<-c(2,2)
         e$lastx<<-e$lasty<<-2
         e$tail<<-data.frame(x=c(),y=c())
         
         e$col_furit<<-2
         e$col_head<<-4
         e$col_tail<<-8
         e$col_path<<-0
         e$col_barrier<<-1
         
         # 全局变量，用来记录障碍物的坐标
         obst_x<<-round(height/2)
         obst_y<<-round(width/2)
         # 障碍物每次移动的步数
         obst_step<<-1
         # 连续吃到水果的次数，每满三次重新计数
         cnt<<-0
       }
     },
     lose=function(){
       if(length(which(e$head<1))>0 | length(which(e$head>width))>0){
         # 音效
         beep(5)
         
         fail("Out of ledge.")
         return(NULL)
       }
       if(m[e$head[1],e$head[2]]==e$col_tail){
         # 音效
         beep(5)
         
         fail("head hit tail.")
         return(NULL)
       }
       # 判别是否因撞到障碍物而游戏结束
       # 获取蛇头和蛇神所在各自的坐标
       idx<-which(m>0)
       px<- (ifelse(idx%%width==0,width,idx%%width)-1)/width+e$step/2
       py<- (ceiling(idx/height)-1)/height+e$step/2
       pxy<-data.frame(x=px,y=py,col=m[idx])
       # 获取障碍物的坐标
       x<-(obst_y-0.5)/width
       y<-1-(obst_x-0.5)/height
       # 提取出蛇身和蛇头的点坐标
       temp<-pxy[pxy$col>2,]
       # 在有蛇身的情况下，移动过程中会额外多记录一个历史移动点位
       # 由于需要考虑和障碍物擦肩而过的情况，因此在计算是否碰撞障碍物时
       # 需要去除最后一个历史点位
       if (nrow(temp)>1) {
         temp<-temp[1:(nrow(temp)-1),]
       }
       # 获取点的坐标集合
       temp<-temp[temp$x==x,]
       temp<-temp[temp$y==y,]
       # 若障碍物的坐标和蛇头蛇身有重合，fail
       if (nrow(temp)>0){
         # 音效
         beep(5)
         
         fail("head/tail hit obstruction.")
         return(NULL)
       }
     },
     furit=function(){
       if(length(index(e$col_furit))<=0){
         idx<-sample(index(e$col_path),1)
         fx<-ifelse(idx%%width==0,10,idx%%width)
         fy<-ceiling(idx/height)
         m[fx,fy]<<-e$col_furit
         if(debug){
           print(paste("furit idx",idx))
           print(paste("furit axis:",fx,fy))
         }
       }
     },
     # 障碍物的移动函数
     obst=function(){
       # 若触碰边界，则移动的step反向
       if (obst_y==width||obst_y==1) {
         obst_step<<--obst_step
       }
       # 移动
       obst_y<<-obst_y+obst_step
     },
     head=function(){
       e$lastx<<-e$head[1]
       e$lasty<<-e$head[2]
       if(e$dir=='up') e$head[2]<<-e$head[2]+1
       if(e$dir=='down') e$head[2]<<-e$head[2]-1
       if(e$dir=='left') e$head[1]<<-e$head[1]-1
       if(e$dir=='right') e$head[1]<<-e$head[1]+1
     },
     body=function(){
       if(isFail) return(NULL)
       m[e$lastx,e$lasty]<<-e$col_path
       m[e$head[1],e$head[2]]<<-e$col_head
       if(length(index(e$col_furit))<=0){
         # 记录连续吃到水果的次数
         cnt<<-cnt+1
         # 吃到水果时播放声音
         beep(2)
         # 若满三次，则额外多增加一格，并重新计数
         if (cnt==3) {
           e$tail<<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty))
           cnt<<-0
         }
         e$tail<<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty))
       }
       if(nrow(e$tail)>0) {
         e$tail<<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty))
         m[e$tail[1,]$x,e$tail[1,]$y]<<-e$col_path
         e$tail<<-e$tail[-1,]
         m[e$lastx,e$lasty]<<-e$col_tail
       }
       if(debug){
         print(paste("snake idx",index(e$col_head)))
         print(paste("snake axis:",e$head[1],e$head[2]))
       }
     },
     drawTable=function(){
       if(isFail) return(NULL)
       plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
       if(debug){
         abline(h=seq(0,1,e$step),col="gray60")
         abline(v=seq(0,1,e$step),col="gray60")
         df<-data.frame(x=rep(seq(0,0.95,e$step),width),y=rep(seq(0,0.95,e$step),each=height),lab=seq(1,width*height))
         text(df$x+e$step/2,df$y+e$step/2,label=df$lab)
       }
     },
     drawMatrix=function(){
       if(isFail) return(NULL)
       idx<-which(m>0)
       px<- (ifelse(idx%%width==0,width,idx%%width)-1)/width+e$step/2
       py<- (ceiling(idx/height)-1)/height+e$step/2
       pxy<-data.frame(x=px,y=py,col=m[idx])
       points(pxy$x,pxy$y,col=pxy$col,pch=15,cex=4.4)
       # 将障碍物进行可视化
       points((obst_y-0.5)/width,1-(obst_x-0.5)/height,pch=15,cex=4.4,col="black")
       # 增加右上角的分数显示
       text(0.85, 0.975, paste("Current Score:", nrow(e$tail)), font=10)
     },
     stage1=function(){
       callSuper()
       furit()
       # 若刚从暂停状态中恢复，则不进行任何操作
       if (!paused) {
         obst()
         head()
         lose()
         body()
       }
       # 从暂停刚恢复状态中变回正常状态
       paused<<-FALSE
       drawTable()
       drawMatrix()
     },
     stage0=function(){
       callSuper()
       plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
       text(0.5,0.7,label=name,cex=5)
       text(0.5,0.4,label="Any keyboard to start",cex=2,col=4)
       text(0.5,0.3,label="Up,Down,Left,Rigth to control direction",cex=2,col=2)
       text(0.2,0.05,label="Author:DanZhang",cex=1)
       text(0.5,0.05,label="http://blog.fens.me",cex=1)
     },
     stage2=function(){
       callSuper()
       info<-paste("Congratulations! You have eat",nrow(e$tail),"fruits!")
       print(info)
       plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
       text(0.5,0.7,label="Game Over",cex=5)
       text(0.5,0.4,label="Space to restart, q to quit.",cex=2,col=4)
       text(0.5,0.3,label=info,cex=2,col=2)
       text(0.2,0.05,label="Author:DanZhang",cex=1)
       text(0.5,0.05,label="http://blog.fens.me",cex=1)
     },
     # 新增stage3，用来表示暂停状态
     stage3=function(){
       callSuper()
       info<-paste("Paused!")
       print(info)
       plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
       text(0.5,0.7,label="Game Paused",cex=5)
       text(0.5,0.4,label="Press p to continue.",cex=2,col=4)
       text(0.5,0.3,label=info,cex=2,col=2)
       text(0.2,0.05,label="Author:DanZhang",cex=1)
       text(0.5,0.05,label="http://blog.fens.me",cex=1)
     },
     keydown=function(K){
       callSuper(K)
       if(stage==1){
         if(K == "q") stage2()
         # 若暂停，则保存棋盘，转换为暂停状态
         else if(K == "p"){
           log_e<<-e
           log_m<<-m
           paused<<-TRUE
           stage3()
         }
         else {
           if(tolower(K) %in% c("up","down","left","right")){
             e$lastd<<-e$dir
             e$dir<<-tolower(K)
             stage1()
           }
         }
         return(NULL)
       }
       # 在暂停状态中继续按p可以恢复正常运行的状态
       if(stage==3){
         if(K == "p"){
           stage1()
         }
       }
       return(NULL)
     }
   )
)

snake<-function(){
  game<-Snake$new()
  game$initFields(debug=FALSE)
  game$run()
}

snake()