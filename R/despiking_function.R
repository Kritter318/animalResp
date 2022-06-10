# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

despike<-function(data,variable,baseline,bl_location,bl_window=50,window1=15,window2=10){
  my_data<-data.frame(CO2=get(paste(variable),data))
  my_data$row<-1:length(my_data[,1])
  `rownames<-`(my_data, my_data$row)
  end<-end(my_data$row)[1]
  data_rows<-my_data$row
  oscillations<-c()
  ####### stop function if bl_location isn't correct
  if (!(bl_location == "above" | bl_location =="below")){
    stop("Error: bl_location not appropriate value")
  }

  for (a in data_rows[1]:data_rows[end(data_rows)[1]]){
    ###### adjust baseline min/max depending on where baseline is compared to the data
    if (bl_location == "above" ){
      bl<-max(as.numeric(baseline))+as.numeric(bl_window)}
    if(bl_location == "below"){
      bl<-(min(as.numeric(baseline))-as.numeric(bl_window))}

    subCO2<-c(my_data$CO2[a],my_data$CO2[a+1:9])
    tavg<-mean(subCO2[-1])
    tsub_sd<-sd(subCO2[-1])
    tsub_var<-var(subCO2[-1])/2
    if (tsub_var > 15){
      tsub_min<-((tavg-tsub_var)-window1)
      tsub_max<-((tavg+tsub_var)+window1)
    }else{
      tsub_min<-((tavg-tsub_sd)-window1)
      tsub_max<-((tavg+tsub_sd)+window1)
    }



    if(a == (end-9)){
      break
    }
    if(bl_location == "above"){
      if (my_data$`CO2`[a] > bl){
        oscillations<-append(oscillations,my_data$row[a])
        next
      }
    }
    if(bl_location == "below"){
      if (my_data$`CO2`[a] < bl){
        oscillations<-append(oscillations,my_data$row[a])
        next
      }
    }
    if (my_data$`CO2`[a] < tsub_min){
      oscillations<-append(oscillations,my_data$row[a])
      next
    }
    if (my_data$`CO2`[a] > tsub_max){
      oscillations<-append(oscillations,my_data$row[a])
      next
    }

  }


  oscillations

  #plot_name<-paste(data_deposit_dir,"/",ID2,"_oscillations1.jpeg",sep="")


  x<- c(my_data$row)
  y<- c(my_data$CO2)
  #jpeg(filename=plot_name, width = 1000, height = 800)
  #par(mfrow=c(2,1),mfcol=c(2,1))
  plot1<-plot(x,y, col="black", pch=16, xlab="Row", ylab=variable, main=paste(variable, 'by Row widow1 =',window1),ylim=c((min(y)),max(y)))
  plot1<-plot1+points(y=my_data$CO2[my_data$row %in% oscillations],x=my_data$row[my_data$row %in% oscillations], col="red")
  print(plot1)



  ##########################################################################################################
  ############################# oscillations run through again to deal with really bad files ################
  if(length(oscillations)>0){
    my_data2<-my_data[-c(oscillations),]
    data_rows<-my_data2$row
    `rownames<-`(my_data2, my_data2$row)
    end<-end(my_data2$row)[1]
  }else{
    my_data2<-my_data
  }


  oscillations2<-c()
  for (a in end[1]:data_rows[1]){
    subCO2<-c(my_data2$CO2[a],my_data2$CO2[a-1:9])
    tavg<-mean(subCO2[-1])
    tsub_sd<-sd(subCO2[-1])
    tsub_var<-var(subCO2[-1])/2
    if (tsub_var > 10){
      tsub_min<-((tavg-tsub_var)-window2)
      tsub_max<-((tavg+tsub_var)+window2)
    }else{
      tsub_min<-((tavg-tsub_sd)-window2)
      tsub_max<-((tavg+tsub_sd)+window2)
    }


    if(a == (9)){
      break
    }
    if (my_data2$`CO2`[a] < tsub_min){
      oscillations2<-append(oscillations2,my_data2$row[a])
      next
    }
    if (my_data2$`CO2`[a] > tsub_max){
      oscillations2<-append(oscillations2,my_data2$row[a])
      next
    }

  }

  oscillations2
  ##### combine all oscillations

  all_oscillations<-c(oscillations,oscillations2)


  #plot_name2<-paste(plot_loc,"/",ID2,"_oscillations2.jpg",sep="")
  #jpeg(filename = plot_name2, height=800, width=1000)
  #par(mfrow=c(2,1))
  x<- c(my_data2$row)
  y<- c(my_data2$CO2)

  plot3<-plot(x,y, col="black", pch=16, xlab="Row", ylab=variable, main=paste(variable,'by Row window2 =',window2),ylim=c((min(y)),max(y)))
  plot3<-plot3+points(y=my_data2$CO2[my_data2$row %in% oscillations2],x=my_data2$row[my_data2$row %in% oscillations2], col="red")
  print(plot3)


  return(all_oscillations)
}
