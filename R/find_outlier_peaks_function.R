# data=my_data
# variable="Photocell"
# lag=60
# z_threshold = 8
# noisy=FALSE
# manual_bkgd="none"
find_outlier_peaks<-function(data,variable,lag,z_threshold=3,manual_bkgd="none",noisy=FALSE){
  data<-as.numeric(get(variable,data))
  result<-data.frame("i"=c(seq(1:(lag))),"data"=data[1:(lag)],"signal"=c(rep(0,(lag))),"z"=c(rep(0,(lag))),"bkgd_pos"=c(rep(0,(lag))))
  test<-t.test(data[1:lag],data[((end(data)[1])-lag):end(data)[1]])
  if(noisy == FALSE){
    if(manual_bkgd[1] == "none"){
      if (test[["p.value"]]<= 0.05){
        manual_bkgd<-c(1:lag,((end(data)[1])-lag):end(data)[1])
      }
    }
  }
  
  
  for(i in (lag+1):(length(data))){
    if (is.na(data[i])){
      i_result<-data.frame("i"=(i),"data"=data[i],"signal"=(0),"z"=z,"bkgd_pos"=background_pos)
      result<-rbind(result,i_result)
      next
    }
    if (manual_bkgd[1] == "none"){
      if(sum(abs(result$signal[(i-(lag)):(i-1)]))==0){
        background<-summary(c(data[(i-lag):(i-1)]))
        background_pos<-paste((i-lag),":",i,sep="")
        std<-sd(data[(i-lag):(i-1)],na.rm=TRUE)
      }
    }else{
      background<-summary(c(data[manual_bkgd]))
      end_background<-length(manual_bkgd)
      background_pos<-paste((manual_bkgd[1]),":",manual_bkgd[end_background],sep="")
      std<-sd(data[manual_bkgd],na.rm = TRUE)
    }
    
    z<-((data[i]-as.numeric(paste(background[4])))/(std))
    if (z<(-(z_threshold))){
      i_result<-data.frame("i"=(i),"data"=data[i],"signal"=(-0.5),"z"=z,"bkgd_pos"=background_pos)
      result<-rbind(result,i_result)
    }
    if(z==0){
      i_result<-data.frame("i"=(i),"data"=data[i],"signal"=0,"z"=z,"bkgd_pos"=background_pos)
      result<-rbind(result,i_result)
    }
    if(z>(-(z_threshold))&z<z_threshold){
      i_result<-data.frame("i"=(i),"data"=data[i],"signal"=0,"z"=z,"bkgd_pos"=background_pos)
      result<-rbind(result,i_result)
    } 
    if(z>z_threshold){
      i_result<-data.frame("i"=(i),"data"=data[i],"signal"=0.5,"z"=z,"bkgd_pos"=background_pos)
      result<-rbind(result,i_result)
    }
    
  }
  
  #i=2
  if (noisy == TRUE){
    result_2<-data.frame("i"=c(rev((length(data)-(lag-1)):(length(data)))),"data"=data[length(data):(length(data)-(lag-1))],"signal"=c(rep(0,(lag))),"z"=c(rep(0,(lag))),"bkgd_pos"=c(rep(0,(lag))))
    for(i in (length(data)-(lag)):1){
      if (manual_bkgd[1] == "none"){
        if(sum(abs(result_2$signal[match((i+(lag)),result_2$i):match((i+1),result_2$i)]))==0){
          background<-summary(c(data[(i+lag):(i+1)]))
          background_pos<-paste((i+lag),":",i,sep="")
          std<-sd(data[(i+lag):(i+1)],na.rm=TRUE)
        }
      }else{
        background<-summary(c(data[manual_bkgd]))
        end_background<-length(manual_bkgd)
        background_pos<-paste((manual_bkgd[1]),":",manual_bkgd[end_background],sep="")
        std<-sd(data[manual_bkgd],na.rm=TRUE)
      }
      
      z<-((data[i]-as.numeric(paste(background[4])))/(std))
      if (z<(-(z_threshold))){
        i_result_2<-data.frame("i"=(i),"data"=data[i],"signal"=(-0.5),"z"=z,"bkgd_pos"=background_pos)
        result_2<-rbind(result_2,i_result_2)
      }
      if(z==0){
        i_result_2<-data.frame("i"=(i),"data"=data[i],"signal"=0,"z"=z,"bkgd_pos"=background_pos)
        result_2<-rbind(result_2,i_result_2)
      }
      if(z>(-(z_threshold))&z<z_threshold){
        i_result_2<-data.frame("i"=(i),"data"=data[i],"signal"=0,"z"=z,"bkgd_pos"=background_pos)
        result_2<-rbind(result_2,i_result_2)
      } 
      if(z>z_threshold){
        i_result_2<-data.frame("i"=(i),"data"=data[i],"signal"=0.5,"z"=z,"bkgd_pos"=background_pos)
        result_2<-rbind(result_2,i_result_2)
      }
      
    }
    
  }
  
  
  
  i=103
  
  if(noisy == FALSE){
    peaks<-data.frame("peak"=c(),"start"=c(),"end"=c(),"signal"=c())
    for(i in 2:length(result$i)){
      peak_num<-length(peaks$peak)+1
      
      if (result$signal[i]==0.5&result$signal[(i-1)]==0){
        peaks_1<-data.frame("peak"=c(0),"start"=c(0),"end"=c(0),"signal"=c(0))
        peaks_1$peak<-peak_num
        peaks_1$start<-result$i[i]
        peaks_1$signal<-result$signal[i]
      }
      if (result$signal[i]==(-0.5)&result$signal[(i-1)]==0){
        peaks_1<-data.frame("peak"=c(0),"start"=c(0),"end"=c(0))
        peaks_1$peak<-peak_num
        peaks_1$start<-result$i[i]
        peaks_1$signal<-result$signal[i]
      }
      if(i== length(result$i)){
        if(result$signal[i]==0.5){
          peaks_1$end<-NA
          peaks<-rbind(peaks,peaks_1)
          break
        }
        if(result$signal[i]==(-0.5)){
          peaks_1$end<-NA
          peaks<-rbind(peaks,peaks_1)
          next
        }
        break
      }
      
      if(result$signal[i]==0.5& result$signal[(i+1)]==0){
        peaks_1$end<-result$i[i]
        peaks<-rbind(peaks,peaks_1)
        next
      }
      if(result$signal[i]==(-0.5)& result$signal[(i+1)]==0){
        peaks_1$end<-result$i[i]
        peaks<-rbind(peaks,peaks_1)
        next
      }
      
    }
    plot1<-plot(data)
    plot1<-plot1+lines((y=result$signal+as.numeric(paste(background[4]))),x=result$i, col="red")
    #plot1<-labs(title=)
  } 
  #################################################################################################################
  
  if(noisy == TRUE){
    peaks<-data.frame("peak"=c(),"start"=c(),"end"=c(),"signal"=c(),"direction"=c())
    ############################################
    ###### run loop forward on result
    ############################################
    for(i in 2:length(result$i)){
      peak_num<-length(peaks$peak)+1
      if (result$signal[i]==0.5&result$signal[(i-1)]==0){
        peaks_1<-data.frame("peak"=c(0),"start"=c(0),"end"=c(0),"signal"=c(0),"direction"=c(0))
        peaks_1$peak<-peak_num
        peaks_1$start<-result$i[i]
        peaks_1$signal<-result$signal[i]
        peaks_1$direction<-"forward"
      }
      if (result$signal[i]==(-0.5)&result$signal[(i-1)]==0){
        peaks_1<-data.frame("peak"=c(0),"start"=c(0),"end"=c(0),"direction"=c(0))
        peaks_1$peak<-peak_num
        peaks_1$start<-result$i[i]
        peaks_1$signal<-result$signal[i]
        peaks_1$direction<-"forward"
      }
      if(i== length(result$i)){
        if(result$signal[i]==0.5){
          peaks_1$end<-result$i[i]
          peaks<-rbind(peaks,peaks_1)
          break
        }
        if(result$signal[i]==(-0.5)){
          peaks_1$end<-result$i[i]
          peaks<-rbind(peaks,peaks_1)
          next
        }
        break
      }
      
      if(result$signal[i]==0.5& result$signal[(i+1)]==0){
        peaks_1$end<-result$i[i]
        peaks<-rbind(peaks,peaks_1)
        next
      }
      if(result$signal[i]==(-0.5)& result$signal[(i+1)]==0){
        peaks_1$end<-result$i[i]
        peaks<-rbind(peaks,peaks_1)
        next
      }
    }
    
    ###########################################
    ### run loop backwards on result_2
    ###########################################
    
    
    for(i in length(result_2$i):1){
      peak_num<-length(peaks$peak)+1
      if(i== length(result_2$i)){
        if(result_2$signal[i]==0.5){
          peaks_1<-data.frame("peak"=c(0),"start"=c(0),"end"=c(0),"signal"=c(0),"direction"=c(0))
          peaks_1$peak<-peak_num
          peaks_1$start<-result_2$i[i]
          peaks_1$direction<-"backward"
          peaks_1$signal<-result_2$signal[i]
          next
        }
        if(result_2$signal[i]==(-0.5)){
          peaks_1<-data.frame("peak"=c(0),"start"=c(0),"end"=c(0),"signal"=c(0),"direction"=c(0))
          peaks_1$peak<-peak_num
          peaks_1$start<-result_2$i[i]
          peaks_1$direction<-"backward"
          peaks_1$signal<-result_2$signal[i]
          next
        }
      }
      if (result_2$signal[i]==0.5&result_2$signal[(i+1)]==0){
        peaks_1<-data.frame("peak"=c(0),"start"=c(0),"end"=c(0),"signal"=c(0),"direction"=c(0))
        peaks_1$peak<-peak_num
        peaks_1$start<-result_2$i[match(i,result_2$i)]
        peaks_1$signal<-result_2$signal[match(i,result_2$i)]
        peaks_1$direction<-"backward"
      }
      if (result_2$signal[i]==(-0.5)&result_2$signal[(i+1)]==0){
        peaks_1<-data.frame("peak"=c(0),"start"=c(0),"end"=c(0),"direction"=c(0))
        peaks_1$peak<-peak_num
        peaks_1$start<-result_2$i[i]
        peaks_1$signal<-result_2$signal[i]
        peaks_1$direction<-"backward"
      }
      if(i== 1){
        if(result_2$signal[i]==0.5){
          peaks_1$end<-result_2$i[i]
          peaks<-rbind(peaks,peaks_1)
          break
        }
        if(result_2$signal[i]==(-0.5)){
          peaks_1$end<-result_2$i[i]
          peaks<-rbind(peaks,peaks_1)
          next
        }
        break
      }
      
      if(result_2$signal[i]==0.5& result_2$signal[i-1]==0){
        peaks_1$end<-result_2$i[i]
        peaks<-rbind(peaks,peaks_1)
        next
      }
      if(result_2$signal[i]==(-0.5)& result_2$signal[i-1]==0){
        peaks_1$end<-result_2$i[i]
        peaks<-rbind(peaks,peaks_1)
        next
      }
    }
    plot1<-plot(data)
    plot1<-plot1+lines(x=result$i,y=(result$signal+as.numeric(paste(background[4]))),col="red")
    plot1<-plot1+lines((y=result_2$signal+as.numeric(paste(background[4]))),x=result_2$i, col="blue")
    
  }
  
  print(plot1)
  return(peaks)
}