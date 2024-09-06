library(arrow)
library(data.table)
library(tidyverse)

dat <- arrow::open_dataset(
  sources="/workspace/Projects/ILEC/VBT/Data/ilecdata"
)

 


dat2 <- dat %>%
  filter(Observation_Year >= 2013
         & Observation_Year <= 2017 
         & Issue_Age >= 18
         & Duration <= 25) %>%
  group_by(Number_of_Pfd_Classes,
           Preferred_Class,
           Smoker_Status,
           Face_Amount_Band,
           Observation_Year,
           Duration,
           Issue_Age,
           Insurance_Plan,
           SOA_Antp_Lvl_TP,
           Issue_Year,
           Sex) %>%
  summarize(amount_2015vbt=sum(ExpDth_Amt_VBT2015),
            amount_actual=sum(Death_Claim_Amount),
            policy_2015vbt=sum(ExpDth_Cnt_VBT2015),
            policy_actual=sum(Death_Count)
            ) %>%
  collect() %>%
  as.data.table()

dat2[,`:=`(Number_of_Pfd_Classes=as.integer(as.character(Number_of_Pfd_Classes)),
           Preferred_Class=as.integer(as.character(Preferred_Class)),
           Smoker_Status=as.character(Smoker_Status))]

dat2[,`:=`(Number_of_Pfd_Classes=nafill(Number_of_Pfd_Classes,fill=1),
           Preferred_Class=nafill(Preferred_Class,fill=1))]
     
dat2[,`:=`(uw=paste(
  substr(Smoker_Status,1,1),
  Number_of_Pfd_Classes,
  Preferred_Class,
  sep="/"
))]

dat2[,
     dur_band1:=cut(Duration,
                    breaks=c(0,1,2,3,5,10,15,20,25),
                    labels=c("01","02","03","04-05","06-10","11-15","16-20","21-25"))]

ia_breaks <- c(17,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,99)

dat2[,
     ia_band1:=cut(
       Issue_Age,
       breaks=ia_breaks,
       labels=paste((ia_breaks+1)[1:(length(ia_breaks)-1)],ia_breaks[2:length(ia_breaks)],sep="-")
     )]


dat2[,
     ltp:=factor(
       as.character(SOA_Antp_Lvl_TP),
       levels=c("5 yr anticipated","10 yr anticipated","15 yr anticipated","20 yr anticipated","25 yr anticipated",
                "30 yr anticipated","N/A (Not Term)","Unknown","Not Level Term" ),
       labels=c(" 5 yr", "10 yr","15 yr","20 yr","25 yr","30 yr","Not Level Term","Unknown","Not Level Term")
       )]

dat2[,
     iy_band1:=cut(Issue_Year,
                   breaks=c(1900,1989,1999,2009,2019),
                   labels=c("1900-1989","1990-1999","2000-2009","2010+"))]


dat3 <- dat2[,
             .(amount_2015vbt=sum(amount_2015vbt),
               amount_actual=sum(amount_actual),
               policy_2015vbt=sum(policy_2015vbt),
               policy_actual=sum(policy_actual)),
             by=.(uw,face_amount_band=Face_Amount_Band,observation_year=Observation_Year,
                  dur_band1, ia_band1,
                  gender=Sex, insurance_plan=Insurance_Plan,
                  ltp, iy_band1)][amount_2015vbt>0]

arrow::write_parquet(x=dat3,
     sink="ilec13_17_framework.parquet")
