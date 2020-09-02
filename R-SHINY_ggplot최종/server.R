library(shiny)
library(devtools)
library(ggplot2)
library(dplyr)
library(readr)
library(extrafont)
library(showtext)
library(rsconnect)
library(tidyverse)
library(shinythemes)

data<-read_rds("data.rds")

shinyServer(function (input, output) {
    output$plot <-renderPlot({
        ggplot(data=data, aes(x = lbls, y = Freq)) + 
            geom_bar(stat="identity", fill='#5CBED2') +
            geom_text(data=data, aes(label=percent),hjust=-0.2,size=5, check_overlap=T)+coord_flip() +
            labs(x="kinds of sul")+ggtitle("소비자가 선호하는 술")+
            theme(plot.title = element_text(size = 20), axis.title = element_text(size = 20),
                  axis.text.x=element_text(size = 20),axis.text.y =element_text(size = 20))},height = 700,width = 850)
    
    kta <-read.csv("sulcsv1.csv",header=TRUE, fileEncoding = "euc-kr")
    kta_most<-subset(kta, select=c("most_pkta", "menu"))
    kta_most<-na.omit(kta_most)
    kta_most$menu[kta_most$menu==1]<-"볶음류"
    kta_most$menu[kta_most$menu==2]<-"구이류"
    kta_most$menu[kta_most$menu==3]<-"해산물류"
    kta_most$menu[kta_most$menu==4]<-"탕/찌개류"
    kta_most$menu[kta_most$menu==5]<-"과일류"
    kta_most$menu[kta_most$menu==6]<-"마른안주류"
    kta_most$menu[kta_most$menu==7]<-"치즈류"
    kta_most$menu[kta_most$menu==8]<-"튀김류"
    kta_most$menu[kta_most$menu==9]<-"과자류"
    kta_most$menu[kta_most$menu==10]<-"무침류"
    kta_most$menu[kta_most$menu==11]<-"전류"
    kta_most$menu[kta_most$menu==12]<-"김치류"
    
    output$plot1<-renderPlot({
        if(input$list == "장수막걸리"){
            kta_most_장수<-subset(kta_most, most_pkta==1)
            kta_most_장수graph<-prop.table(table(kta_most_장수))*100
            barplot(kta_most_장수graph, main="장수막걸리", xlab="안주",ylab="퍼센트", ylim=c(0,40), col= "#6E8DAB")
        }
        else if(input$list == "지평막걸리"){
            kta_most_지평<-subset(kta_most, most_pkta==2)
            kta_most_지평graph<-prop.table(table(kta_most_지평))*100
            barplot(kta_most_지평graph, main="지평막걸리", xlab="안주",ylab="퍼센트", ylim=c(0,40), col="#6E8DAB")
        }
        else if(input$list == "느린마을막걸리"){
            kta_most_느린마을<-subset(kta_most, most_pkta==3)
            kta_most_느린마을graph<-prop.table(table(kta_most_느린마을))*100
            barplot(kta_most_느린마을graph, main="느린마을막걸리", xlab="안주",ylab="퍼센트", ylim=c(0,40), col="#6E8DAB") 
        }
        else if(input$list == "서울의밤"){
            kta_most_서울의밤<-subset(kta_most, most_pkta==4)
            kta_most_서울의밤graph<-prop.table(table(kta_most_서울의밤))*100
            barplot(kta_most_서울의밤graph, main="서울의 밤", xlab="안주",ylab="퍼센트", ylim=c(0,30), col="#6E8DAB")
        }
        else if(input$list == "화요"){
            kta_most_화요<-subset(kta_most, most_pkta==5)
            kta_most_화요graph<-prop.table(table(kta_most_화요))*100
            barplot(kta_most_화요graph, main="화요", xlab="안주",ylab="퍼센트", ylim=c(0,40), col="#6E8DAB")
        }
        else{
            kta_most_복분자<-subset(kta_most, most_pkta==10)
            kta_most_복분자graph<-prop.table(table(kta_most_복분자))*100
            barplot(kta_most_복분자graph, main="복분자", xlab="안주",ylab="퍼센트", ylim=c(0,30), col="#6E8DAB")
        }
    }
    ,height = 700,width =850 
    )
    
    kta_most1<-subset(kta, select=c("most_pkta", "menu"))
    kta_most1<-na.omit(kta_most1)
    kta_most1$most_pkta[kta_most1$most_pkta==1]<-"장수막걸리"
    kta_most1$most_pkta[kta_most1$most_pkta==2]<-"지평막걸리"
    kta_most1$most_pkta[kta_most1$most_pkta==3]<-"느린마을막걸리"
    kta_most1$most_pkta[kta_most1$most_pkta==4]<-"서울의밤"
    kta_most1$most_pkta[kta_most1$most_pkta==5]<-"화요"
    kta_most1$most_pkta[kta_most1$most_pkta==6]<-"일품진로"
    kta_most1$most_pkta[kta_most1$most_pkta==7]<-"백화수복"
    kta_most1$most_pkta[kta_most1$most_pkta==8]<-"경주법주화랑"
    kta_most1$most_pkta[kta_most1$most_pkta==9]<-"예담"
    kta_most1$most_pkta[kta_most1$most_pkta==10]<-"복분자"
    kta_most1$most_pkta[kta_most1$most_pkta==11]<-"알밤막걸리"
    kta_most1$menu[kta_most1$menu==1]<-"볶음류"
    kta_most1$menu[kta_most1$menu==2]<-"구이류"
    kta_most1$menu[kta_most1$menu==3]<-"해산물류"
    kta_most1$menu[kta_most1$menu==4]<-"탕/찌개류"
    kta_most1$menu[kta_most1$menu==5]<-"과일류"
    kta_most1$menu[kta_most1$menu==6]<-"마른안주류"
    kta_most1$menu[kta_most1$menu==7]<-"치즈류"
    kta_most1$menu[kta_most1$menu==8]<-"튀김류"
    kta_most1$menu[kta_most1$menu==9]<-"과자류"
    kta_most1$menu[kta_most1$menu==10]<-"무침류"
    kta_most1$menu[kta_most1$menu==11]<-"전류"
    kta_most1$menu[kta_most1$menu==12]<-"김치류"
    
     output$plot2<-renderPlot({
        if(input$snacklist == "볶음류"){
            kta_most_볶음류<-subset(kta_most1, menu=="볶음류")
            table1<-table(kta_most_볶음류)
            kta_most_볶d<-data.frame(table1)
            kta_most_볶d<-kta_most_볶d[order(desc(kta_most_볶d$Freq))[1:5],]
            kta_most_볶d<-subset(kta_most_볶d, select = -menu)
            ggplot(kta_most_볶d, aes(most_pkta, Freq))+geom_bar(stat="identity",width = 0.5, fill="#5CBED2")+xlab("술종류")+ggtitle("볶음류")+
                theme(plot.title = element_text(hjust = 0.5, size=25), axis.title = element_text(hjust=0.6, size = 20),
                      axis.text.x=element_text(size = 15),axis.text.y =element_text(size = 15))
        }
        else if(input$snacklist == "구이류"){
            kta_most_구이류<-subset(kta_most1, menu=="구이류")
            table2<-table(kta_most_구이류)
            kta_most_구d<-data.frame(table2)
            kta_most_구d<-kta_most_구d[order(desc(kta_most_구d$Freq))[1:5],]
            kta_most_구d<-subset(kta_most_구d, select = -menu)
            ggplot(kta_most_구d, aes(most_pkta, Freq))+geom_bar(stat="identity", fill="#5CBED2",width = 0.5)+xlab("술종류")+ggtitle("구이류")+
                theme(plot.title = element_text(hjust = 0.5, size=25), axis.title = element_text(hjust=0.6, size = 20),
                      axis.text.x=element_text(size = 15),axis.text.y =element_text(size = 15))
        }
        else if(input$snacklist == "해산물류"){
            kta_most_해산물류<-subset(kta_most1, menu=="해산물류")
            table3<-table(kta_most_해산물류)
            kta_most_해d<-data.frame(table3)
            kta_most_해d<-kta_most_해d[order(desc(kta_most_해d$Freq))[1:5],]
            kta_most_해d<-subset(kta_most_해d, select = -menu)
            ggplot(kta_most_해d, aes(most_pkta, Freq))+geom_bar(stat="identity", fill="#5CBED2",width = 0.5)+xlab("술종류")+ggtitle("해산물류")+
                theme(plot.title = element_text(hjust = 0.5, size=25), axis.title = element_text(hjust=0.6, size = 20),
                      axis.text.x=element_text(size = 15),axis.text.y =element_text(size = 15))
        }
        else if(input$snacklist == "탕/찌개류"){
            kta_most_찌개류<-subset(kta_most1, menu=="탕/찌개류")
            table4<-table(kta_most_찌개류)
            kta_most_탕d<-data.frame(table4)
            kta_most_탕d<-kta_most_탕d[order(desc(kta_most_탕d$Freq))[1:5],]
            kta_most_탕d<-subset(kta_most_탕d, select = -menu)
            kta_most_탕d<-kta_most_탕d[-4,]
            ggplot(kta_most_탕d, aes(most_pkta, Freq))+geom_bar(stat="identity", fill="#5CBED2",width = 0.5)+xlab("술종류")+ggtitle("탕/찌개류")+
                theme(plot.title = element_text(hjust = 0.5, size=25), axis.title = element_text(hjust=0.6, size = 20),
                      axis.text.x=element_text(size = 15),axis.text.y =element_text(size = 15))
        }
        else if(input$snacklist =="과일류"){
            kta_most_과일류<-subset(kta_most1, menu=="과일류")
            table5<-table(kta_most_과일류)
            kta_most_과d<-data.frame(table5)
            kta_most_과d<-kta_most_과d[order(desc(kta_most_과d$Freq))[1:5],]
            kta_most_과d<-subset(kta_most_과d, select = -menu)
            kta_most_과d<-kta_most_과d[-4,]
            ggplot(kta_most_과d, aes(most_pkta, Freq))+geom_bar(stat="identity", fill="#5CBED2",width = 0.5)+xlab("술종류")+ggtitle("과일류")+
                theme(plot.title = element_text(hjust = 0.5, size=25), axis.title = element_text(hjust=0.6, size = 20),
                      axis.text.x=element_text(size = 15),axis.text.y =element_text(size = 15))
        }
        else if(input$snacklist == "마른안주류"){
            kta_most_마른안주류<-subset(kta_most1, menu=="마른안주류")
            table6<-table(kta_most_마른안주류)
            kta_most_마d<-data.frame(table6)
            kta_most_마d<-kta_most_마d[order(desc(kta_most_마d$Freq))[1:5],]
            kta_most_마d<-subset(kta_most_마d, select = -menu)
            ggplot(kta_most_마d, aes(most_pkta, Freq))+geom_bar(stat="identity", fill="#5CBED2",width = 0.5)+xlab("술종류")+ggtitle("마른안주류")+
                theme(plot.title = element_text(hjust = 0.5, size=25), axis.title = element_text(hjust=0.6, size = 20),
                      axis.text.x=element_text(size = 15),axis.text.y =element_text(size = 15))
        }
        else if(input$snacklist == "튀김류"){
            kta_most_튀김류<-subset(kta_most1, menu=="튀김류")
            table7<-table(kta_most_튀김류)
            kta_most_튀d<-data.frame(table7)
            kta_most_튀d<-kta_most_튀d[order(desc(kta_most_튀d$Freq))[1:5],]
            kta_most_튀d<-subset(kta_most_튀d, select = -menu)
            ggplot(kta_most_튀d, aes(most_pkta, Freq))+geom_bar(stat="identity", fill="#5CBED2",width = 0.5)+xlab("술종류")+ggtitle("튀김류")+
                theme(plot.title = element_text(hjust = 0.5, size=25), axis.title = element_text(hjust=0.6, size = 20),
                      axis.text.x=element_text(size = 15),axis.text.y =element_text(size = 15))
        }
        else {
            kta_most_전류<-subset(kta_most1, menu=="전류")
            table8<-table(kta_most_전류)
            kta_most_전d<-data.frame(table8)
            kta_most_전d<-kta_most_전d[order(desc(kta_most_전d$Freq))[1:5],]
            kta_most_전d<-subset(kta_most_전d, select = -menu)
            kta_most_전d<-kta_most_전d[-5,]
            ggplot(kta_most_전d, aes(most_pkta, Freq))+geom_bar(stat="identity", fill="#5CBED2",width = 0.5)+xlab("술종류")+ggtitle("전류")+
                theme(plot.title = element_text(hjust = 0.5, size=25), axis.title = element_text(hjust=0.6, size = 20),
                      axis.text.x=element_text(size = 15),axis.text.y =element_text(size = 15))
            
        }
        }
        ,height = 700,width =850 
        )
})

