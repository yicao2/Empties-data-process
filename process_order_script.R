###### Script for process orders file ##########
###Set working directory ####
## The directory have all the raw data file of orders ##
directory<-getwd()
setwd(directory)

## Read raw data files ####
read.rawdata<-function(file_names,newfile){
  num.file<-length(file_names)
  data.all<-NULL
  for(i in 1:num.file){
    temp<-read.csv(file_names[i],header=T,comment.char = "#")
    temp<-as.data.frame(temp)
    data<-data.frame(Plant=temp$Plant,Shipment.Number=temp$Shipment.Number,Delivery.Date=temp$Delivery.Date,Material.Number=temp$Material.Number,Qty.delivered.in.base.UOM.=temp$Qty.delivered.in.base.UOM.,Qty.returned.in.base.UOM.=temp$Qty.returned.in.base.UOM.)
    data.all<-rbind(data.all,data)
  }
   data.all$Delivery.Date<-as.Date(data.all$Delivery.Date,"%m/%d/%y")
   range(data.all$Delivery.Date)
   end<-max(data.all$Delivery.Date)

   ## read new updated orders file
   newdata <- read.csv(newfile, header=T,comment.char="#")
   newdata$Delivery.Date<-as.Date(newdata$Delivery.Date,"%m/%d/%y")
   newdata<-newdata[!is.na(newdata$Delivery.Date),]
   range(CRF.Kontich$Delivery.Date)
   new_data<-subset(newdata,Delivery.Date>end)
   new_data<-new_data[,names(new_data) %in% c("Shipment.Number","Delivery.Date","Plant","Qty.delivered.in.base.UOM.","Qty.returned.in.base.UOM.","Material.Number")]

   total.data<-rbind(data.all,new_data)
   print("check missing value")
   print(which(is.na(total.data$Delivery.Date)))
   print("time range")
   print(range(total.data$Delivery.Date))
   return(total.data)
}
## Process raw data file ##
library(BBmisc)
bottle<-function(number.id,DLH_order_all){
  id<-which(DLH_order_all$Material.Number==number.id)
  data<-DLH_order_all[id,]
  Date<-NULL
  qty.del<-qty.del.sale<-qty.emp<-NULL
  plant<-NULL
  content.emp<-list()
  ship.num<-unique(data$Shipment.Number)
  for(i in 1:length(ship.num)){
    id.ship<-which(data$Shipment.Number==ship.num[i])
    Date<-rbind(Date,as.character(data$Delivery.Date[id.ship])[1])
    plant<-rbind(plant,unique(as.character(data$Plant[id.ship])))
    qty.del<-rbind(qty.del,sum(data$Qty.delivered.in.base.UOM.[id.ship]))
    qty.del.sale<-rbind(qty.del.sale,sum(data$Qty.delivered.in.base.UOM.[id.ship]))
    qty.emp<-rbind(qty.emp,sum(data$Qty.returned.in.base.UOM.[id.ship]))
    content.emp[[i]]<-data$Material.Number[id.ship]
  }
  bt.summary<-data.frame(date=Date,ship.num=ship.num,plant=plant,qty.emp=qty.emp,qty.del=qty.del)
  return(bt.summary)
}
data.plant<-function(plant03,DLH_order_all){
  Date<-NULL
  qty.del<-qty.del.sale<-qty.emp<-NULL
  ship.num<-unique(plant03$Shipment.Number)
  for(i in 1:length(ship.num)){
    id.ship<-which(DLH_order_all$Shipment.Number==ship.num[i])
    Date<-rbind(Date,as.character(DLH_order_all$Delivery.Date[id.ship])[1])
    qty.del<-rbind(qty.del,sum(DLH_order_all$Qty.delivered.in.base.UOM.[id.ship]))
    qty.emp<-rbind(qty.emp,sum(DLH_order_all$Qty.returned.in.base.UOM.[id.ship]))
  }
  result<-data.frame(date=Date,qty.emp=qty.emp,qty.del=qty.del)
}
form.data<-function(data.plant03){
  data.plant03$date<-as.Date(data.plant03$date)
  data.plant03<-sortByCol(data.plant03,col="date")

  data.plant03$trip<-array()
  data.plant03$trip[which(data.plant03$qty.emp!=0 & data.plant03$qty.del!=0)]<-"round trip"
  data.plant03$trip[which(data.plant03$qty.emp!=0 & data.plant03$qty.del==0)]<-"one-way trip(emp)"
  data.plant03$trip[which(data.plant03$qty.emp==0 & data.plant03$qty.del!=0)]<-"one-way trip(del)"
  data.plant03$trip[which(data.plant03$qty.emp==0 & data.plant03$qty.del==0)]<-"no.del"
  id.date<-unique(data.plant03$date)

  date<-NULL
  qty.emp<-NULL
  qty.del<-NULL
  ship.num<-NULL
  ship.rd<-NULL
  ship.1emp<-NULL
  ship.1del<-NULL
  for(i in 1:length(id.date)){
    date<-rbind(date,as.character(data.plant03$date[which(data.plant03$date==id.date[i])])[1])
    qty.emp<-rbind(qty.emp,sum(data.plant03$qty.emp[which(data.plant03$date==id.date[i])]))
    qty.del<-rbind(qty.del,sum(data.plant03$qty.del[which(data.plant03$date==id.date[i])]))
    ship.num<-rbind(ship.num,length(which(data.plant03$date==id.date[i])))
    ship.rd<-rbind(ship.rd,sum(data.plant03$trip[which(data.plant03$date==id.date[i])]=="round trip"))
    ship.1emp<-rbind(ship.1emp,sum(data.plant03$trip[which(data.plant03$date==id.date[i])]=="one-way trip(emp)"))
    ship.1del<-rbind(ship.1del,sum(data.plant03$trip[which(data.plant03$date==id.date[i])]=="one-way trip(del)"))
  }
  date<-as.Date(date)
  qty.emp<-round(qty.emp/70)
  qty.del<-round(qty.del/70)
  result<-data.frame(date=date,qty.emp=qty.emp,qty.del=qty.del,ship.num=ship.num,ship.rd=ship.rd,ship.1emp=ship.1emp,ship.1del=ship.1del)
  return(result)
}
process.rawdata<-function(total.data){
  total.data<-sortByCol(total.data,col="Delivery.Date")
  Date<-NULL
  qty.del<-qty.del.sale<-qty.emp<-NULL
  plant<-NULL
  content.emp<-list()
  ship.num<-unique(total.data$Shipment.Number)
  for(i in 1:length(ship.num)){
    id.ship<-which(total.data$Shipment.Number==ship.num[i])
    Date<-rbind(Date,as.character(total.data$Delivery.Date[id.ship])[1])
    plant<-rbind(plant,unique(as.character(total.data$Plant[id.ship])))
    qty.del<-rbind(qty.del,sum(total.data$Qty.delivered.in.base.UOM.[id.ship]))
    qty.del.sale<-rbind(qty.del.sale,sum(total.data$Qty.delivered.in.base.UOM.[id.ship]))
    qty.emp<-rbind(qty.emp,sum(total.data$Qty.returned.in.base.UOM.[id.ship]))
    content.emp[[i]]<-total.data$Material.Number[id.ship]
  }
    content.num<-lapply(content.emp,unique)
    order_summary<-data.frame(date=Date,plant=plant,qty.emp=qty.emp,qty.del=qty.del,content=summary(content.num)[,1])
    order_summary$ship.num<-ship.num
    order_summary$shipment<-array()
    order_summary$shipment[which(order_summary$qty.emp!=0 & order_summary$qty.del!=0)]<-"round trip"
    order_summary$shipment[-which(order_summary$qty.emp!=0 & order_summary$qty.del!=0)]<-"one-way trip"
    ## Current one-way, round way trip situation ##
    table(order_summary$plant,order_summary$shipment)
    product<-unique(total.data$Material.Number)
    bottle.id<-NULL
    for(i in 1:length(product)){
      temp<-bottle(product[i],total.data)
      if(sum(temp$qty.emp)!=0)
      {bottle.id<-rbind(bottle.id,product[i])}
      else next
    }
    bottles<-subset(total.data,Material.Number %in% bottle.id)
    id.date<-unique(bottles$Delivery.Date)

    plant02<-subset(bottles,Plant=="BE02")
    plant03<-subset(bottles,Plant=="BE03")
    plant04<-subset(bottles,Plant=="BE04")

    data.plant02<-data.plant(plant02,total.data)
    data.plant03<-data.plant(plant03,total.data)
    data.plant04<-data.plant(plant04,total.data)

    Data.plant03<-form.data(data.plant03)
    Data.plant02<-form.data(data.plant02)
    Data.plant04<-form.data(data.plant04)

    print("check missing value")
    print(which(is.na(Data.plant02)))
    print(which(is.na(Data.plant03)))
    print(which(is.na(Data.plant04)))

    write.csv(Data.plant02,file="order_plant02.csv")
    write.csv(Data.plant03,file="order_plant03.csv")
    write.csv(Data.plant04,file="order_plant04.csv")

    save(Data.plant02,Data.plant03,Data.plant04,file="orderfiles.Rdata")

}



##Take CRF as an example ##
file_names<-c("CRF Kontich H1 2014.csv","CRF Kontich H2 2014.csv","CRF Kontich H1 2015.csv","CRF Kontich H2 2015.csv")
newfile<-"CRF Kontich.csv"
CRFK_ord_all<-read.rawdata(file_names,newfile)
process.rawdata(CRFK_ord_all)



