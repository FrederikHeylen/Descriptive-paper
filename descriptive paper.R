###########################################################################################
###########################################################################################
##############################-~_Descriptive Paper-~-######################################
###########################################################################################
#### Evelien Willems      #################################################################
#### Frederik Heylen ######################################################################
###########################################################################################
###########################################################################################

#_____________________________________Load packages_______________________________________#
{
  library("Hmisc")
  library("effects")
  library("RcmdrMisc")
  library("interplot")
}
#________________________________________Load data________________________________________#
{#Survey + advisory council + website coding + the hierachical coding
data <- read.csv2("totalsurvey.csv")

#only the survey data + website coding
cigDF1 <- read.csv2("C:/Users/Fheylen/Dropbox/Website codering/Advocacy Patterns/surveymotherfile1.csv")
cigDF2 <- read.csv2("C:/Users/Fheylen/Dropbox/Website codering/Advocacy Patterns/17052017 webclean.csv")

names(cigDF1)[c(3)] <- c("ID")

cigDF <- merge(cigDF1,cigDF2 , by="ID")

rm(cigDF1, cigDF2)}
#________________________________________Functions________________________________________#
{recoder <- function(df, a, y)
{
  x <- grep(a, names(data), value = TRUE)
  
  for(i in x) {
    z <- car::recode(df[[i]], y)
    df <- cbind(df, z)
    names(df)[names(df) == 'z'] <- gsub("b", "", paste(i, "r", sep=""))
    print(gsub("b", "", paste(i, "r", sep="")))
    print(table(df[[gsub("b", "", paste(i, "r", sep=""))]]))
  }
  df
}}
#________________________________________Recode __________________________________________#

#q73 members origin region, this is based on additional coding done during the survey process (we needed to identify the language). 
#It is also possibe to do this based solely on the survey data (but might be less accurate), See code below.
{
  cigDF <- within(cigDF, {
    q73b_01r <- Recode(q73b_01, '-9998=NA')
  })
  cigDF <- within(cigDF, {
    q73b_02r <- Recode(q73b_02, '-9998=NA')
  })
  cigDF <- within(cigDF, {
    q73b_03r <- Recode(q73b_03, '-9998=NA')
  })
  cigDF <- within(cigDF, {
    q73b_04r <- Recode(q73b_04, '-9998=NA')
  })
  cigDF <- within(cigDF, {
    q73b_05r <- Recode(q73b_05, '-9998=NA')
  })
  cigDF <- within(cigDF, {
    q73b_06r <- Recode(q73b_06, '-9998=NA')
  })
  
  cigDF <- within(cigDF, {
    q73b_07r <- Recode(q73b_07, '-9998=NA')
  })
  cigDF$q73r <- ifelse(cigDF$Language == "Nederlandstalig", 1,
                       ifelse(cigDF$Language == "Franstalig", 2, 
                              ifelse(cigDF$Language== "Tweetalig", 3,
                                     ifelse(cigDF$Language == "Tweetalig Franstalig", 3,
                                            ifelse(cigDF$Language == "Tweetalig Nederlandstalig", 3,  
                                                   ifelse(cigDF$q73b_04r==1&cigDF$q73b_01r!=1&cigDF$q73b_02r!=1,1,
                                                          ifelse((cigDF$q73b_01r==1|cigDF$q73b_02r==1)&(cigDF$q73b_04r!=1&cigDF$q73b_06r!=1) | (cigDF$q73b_05r == 1 & cigDF$q73b_04r != 1  & cigDF$q73b_06r != 1) ,2,
                                                                 ifelse((cigDF$q73b_06r==1|(cigDF$q73b_04r==1&cigDF$q73b_01r==1))|(cigDF$q73b_04r==1&cigDF$q73b_02r==1),3,
                                                                        NA))))))))
  
  
  cigDF <- within(cigDF, {
    q73r <- Recode(q73r, '1=2; 2=3; 3=1')
  })
  
  #1 = Federal
  #2= FLemish
  #3 = Walloon
}

    #q73 members origin region (1 = Flanders, 2 = Wallonia, 3 = Federal)
    {
    #   cigDF <- within(cigDF, {
    #     q73b_01r <- Recode(q73b_01, '-9998=NA')
    #   })
    #   cigDF <- within(cigDF, {
    #     q73b_02r <- Recode(q73b_02, '-9998=NA')
    #   })
    #   cigDF <- within(cigDF, {
    #     q73b_03r <- Recode(q73b_03, '-9998=NA')
    #   })
    #   cigDF <- within(cigDF, {
    #     q73b_04r <- Recode(q73b_04, '-9998=NA')
    #   })
    #   cigDF <- within(cigDF, {
    #     q73b_05r <- Recode(q73b_05, '-9998=NA')
    #   })
    #   cigDF <- within(cigDF, {
    #     q73b_06r <- Recode(q73b_06, '-9998=NA')
    #   })
    #   
    #   cigDF$q73r <- ifelse(cigDF$q73b_04r==1&cigDF$q73b_01r!=1&cigDF$q73b_02r!=1,2,
    #                        ifelse((cigDF$q73b_01r==1|cigDF$q73b_02r==1)&(cigDF$q73b_04r!=1&cigDF$q73b_06r!=1),3,
    #                               ifelse((cigDF$q73b_06r==1|(cigDF$q73b_04r==1&cigDF$q73b_01r==1))|(cigDF$q73b_04r==1&cigDF$q73b_02r==1),1,
    #                                      NA)))
    }


#group coding:type
{
  cigDF$q534 <- ifelse((cigDF$OS2==26&cigDF$OS1!=98) | (cigDF$OS3==26&cigDF$OS1!=98),1, 0)
  
  cigDF$q535 <- ifelse((cigDF$OS2==98&cigDF$OS1!=26) | (cigDF$OS3==98),1, 0)
  
  cigDF$q560 <- cigDF$OS1
  cigDF$q561 <- cigDF$OS2
  cigDF$q562 <- cigDF$OS3
  
  cigDF$q561 <- as.character(cigDF$q561)
  cigDF$q561[(cigDF$OS2==26&cigDF$OS1!=98) | (cigDF$OS2==98&cigDF$OS1!=26)] <- NA
  cigDF$q561 <- as.numeric (cigDF$q561)
  
  cigDF$q562 <- as.character(cigDF$q562)
  cigDF$q562[(cigDF$OS3==26) | (cigDF$OS3==98)] <- NA
  cigDF$q562 <- as.numeric(cigDF$q562)
  cigDF$type <- ifelse(cigDF$q560 == 10 | cigDF$q560 == 50, 1,
                            ifelse(cigDF$q560 == 21 | cigDF$q560 == 61, 2,
                                   ifelse(cigDF$q560 == 22 | cigDF$q560 == 62 , 3,
                                          ifelse(cigDF$q560 == 23 | cigDF$q560 == 63 , 4,
                                                 ifelse(cigDF$q560 == 24 | cigDF$q560==64 , 5,
                                                        ifelse(cigDF$Cause == 1 |cigDF$q560 == 66, 5,
                                                               ifelse(cigDF$Identity == 1 , 4,
                                                                      ifelse(cigDF$q560==25 | cigDF$q560 == 65, 6,
                                                                             ifelse(cigDF$q560==30 | cigDF$q560 == 40, 7, NA)))))))))
  #1 = business
  #2 = professionals
  #3 = labour
  #4 = identity
  #5 = cause
  #6 = leisure
  #7 = institutions & PA
  }

#q02 age of the organization (log. transformed)
{
  cigDF$q02r <- Recode (cigDF$q02, '-9998= NA ; 1234 = NA ;0=NA')
  
}

#number of staff 
{
  cigDF$q21_01rec <- ifelse(cigDF$q21_01 == -9998 & (cigDF$q21_02 != -9998 |  cigDF$q21_03 != -9998 | cigDF$q21_04 != -9998) , 0, cigDF$q21_01)
  cigDF$q21_02rec <- ifelse(cigDF$q21_02 == -9998 & (cigDF$q21_01 != -9998 |  cigDF$q21_03 != -9998 | cigDF$q21_04 != -9998) , 0, cigDF$q21_02)
  cigDF$q21_03rec <- ifelse(cigDF$q21_03 == -9998 & (cigDF$q21_02 != -9998 |  cigDF$q21_01 != -9998 | cigDF$q21_04 != -9998) , 0, cigDF$q21_03)
  cigDF$q21_04rec <- ifelse(cigDF$q21_04 == -9998 & (cigDF$q21_02 != -9998 |  cigDF$q21_03 != -9998 | cigDF$q21_01 != -9998) , 0, cigDF$q21_04)
  
  
  cigDF <- within(cigDF, {
    q21_01r <- Recode(q21_01rec, '-9998=NA')
  })
  
}

#resources
{
  cigDF <- within (cigDF,{
    q08r <- Recode(q08, ' -9998 = NA; 9=NA'
    )}) 
  
}

#policydomains
{ cigDF <- within (cigDF,{
  q16_01r <- Recode(q16_01, ' -9998 = NA'
  )}) 
  cigDF<- within(cigDF,{
    q16_02r <- Recode(q16_02, ' -9998 = NA'
    )})
  cigDF<- within(cigDF,{
    q16_03r <- Recode(q16_03, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{
    q16_04r <- Recode(q16_04, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{
    q16_05r <- Recode(q16_05, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{
    q16_06r <- Recode(q16_06, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{
    q16_07r <- Recode(q16_07, ' -9998 = NA'
    )})
  cigDF<- within(cigDF,{
    q16_08r <- Recode(q16_08, '-9998 = NA'
    )})
  cigDF <- within(cigDF,{ 
    q16_09r <- Recode(q16_09, ' -9998 = NA'
    )})
  cigDF<- within(cigDF,{ 
    q16_10r <- Recode(q16_10, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{ 
    q16_11r <- Recode(q16_11, ' -9998 = NA'
    )})
  cigDF<- within(cigDF,{
    q16_12r <- Recode(q16_12, ' -9998 = NA'
    )})
  cigDF<- within(cigDF,{
    q16_13r <- Recode(q16_13, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{
    q16_14r <- Recode(q16_14, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{
    q16_15r <- Recode(q16_15, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{ 
    q16_16r <- Recode(q16_16, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{
    q16_17r <- Recode(q16_17, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{
    q16_18r <- Recode(q16_18, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{
    q16_19r <- Recode(q16_19, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{
    q16_20r <- Recode(q16_20, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{
    q16_21r <- Recode(q16_21, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{
    q16_22r <- Recode(q16_22, ' -9998 = NA'
    )})
  cigDF <- within(cigDF,{
    q16_23r <- Recode(q16b_23, ' -9998 = NA'
    )})


cigDF$q16_25r <- 0 # q16_25 = sport

cigDF$q16_25r <- ifelse( cigDF$ID == 1665 | cigDF$ID == 187 | cigDF$ID == 2313 | cigDF$ID ==  2714 | cigDF$ID == 245
                               | cigDF$ID == 2169 | cigDF$ID == 1421 | cigDF$ID == 2225 | cigDF$ID == 175  | cigDF$ID == 2754
                               | cigDF$ID == 2238 | cigDF$ID == 2259 | cigDF$ID == 2264 | cigDF$ID == 2308 | cigDF$ID == 2321
                               | cigDF$ID == 2231 | cigDF$ID == 2168 | cigDF$ID == 2265 | cigDF$ID == 2278 | cigDF$ID == 2317
                               | cigDF$ID == 2206 | cigDF$ID == 2260 | cigDF$ID == 2760 | cigDF$ID == 2251 | cigDF$ID == 2732
                               | cigDF$ID == 2147 | cigDF$ID == 2267 | cigDF$ID == 2306 | cigDF$ID == 2172 | cigDF$ID == 2281
                               | cigDF$ID == 2304 | cigDF$ID == 2761 | cigDF$ID == 2171 | cigDF$ID == 2198 | cigDF$ID == 2309
                               | cigDF$ID == 2315 | cigDF$ID == 2248 | cigDF$ID == 2221 | cigDF$ID == 1803 | cigDF$ID == 2208
                               | cigDF$ID == 2287 | cigDF$ID == 2240 | cigDF$ID == 2200 | cigDF$ID == 2252 | cigDF$ID == 2275
                               | cigDF$ID == 2189 | cigDF$ID == 2192 | cigDF$ID == 2320 | cigDF$ID == 2255 | cigDF$ID == 2153
                               | cigDF$ID == 2145 | cigDF$ID == 2680 | cigDF$ID == 2152 | cigDF$ID == 2296 | cigDF$ID == 25
                               | cigDF$ID == 2272 | cigDF$ID == 2245 | cigDF$ID == 2242 | cigDF$ID == 2314 | cigDF$ID == 2230
                               | cigDF$ID == 2190, 1, cigDF$q16_25r )

cigDF$q16_26r <- 0 #q16_26 = Jeunesse 

cigDF$q16_26r <- ifelse( cigDF$ID == 1584 | cigDF$ID == 1724  | cigDF$ID == 1450 | cigDF$ID == 1338 | cigDF$ID == 1910
                               | cigDF$ID == 1959 | cigDF$ID == 1309 | cigDF$ID ==1391 | cigDF$ID == 1946 | cigDF$ID ==  1626
                               | cigDF$ID == 1537 | cigDF$ID == 2061 | cigDF$ID == 1874 | cigDF$ID == 1329| cigDF$ID ==  1909
                               | cigDF$ID == 1451 | cigDF$ID == 1608 | cigDF$ID == 1383 | cigDF$ID ==1461 | cigDF$ID ==  1968
                               | cigDF$ID == 1636 | cigDF$ID == 2496 , 1, cigDF$q16_26r )


cigDF$q16_27r <- 0 #16_27 = manufacturing/services 

cigDF$q16_27r <- ifelse(  cigDF$ID == 2594 | cigDF$ID == 2042 | cigDF$ID == 1288 | cigDF$ID == 1178 | cigDF$ID == 2352
                                | cigDF$ID == 2462 | cigDF$ID == 2473 | cigDF$ID == 2620 | cigDF$ID == 2572 | cigDF$ID == 308
                                | cigDF$ID == 597 | cigDF$ID == 2345 | cigDF$ID == 447 | cigDF$ID == 668 | cigDF$ID == 2556
                                | cigDF$ID == 172 | cigDF$ID == 1000 | cigDF$ID == 1278 | cigDF$ID == 2421 | cigDF$ID == 2126
                                | cigDF$ID == 838 | cigDF$ID == 1108  | cigDF$ID == 1152 | cigDF$ID == 2578 | cigDF$ID == 2363
                                | cigDF$ID == 2057 | cigDF$ID == 2377 | cigDF$ID == 557 | cigDF$ID == 927 | cigDF$ID == 2523
                                | cigDF$ID == 2511 | cigDF$ID == 2382 | cigDF$ID == 1186 | cigDF$ID == 1749 | cigDF$ID == 813
                                | cigDF$ID == 566 | cigDF$ID == 2507 | cigDF$ID == 1873 | cigDF$ID == 2322 | cigDF$ID == 843
                                | cigDF$ID == 2353 | cigDF$ID == 2440 | cigDF$ID == 1994 | cigDF$ID == 2372 , 1, cigDF$q16_27r )

cigDF$rights <- ifelse(cigDF$q16_12r == 1 | cigDF$q16_19r == 1, 1, 0) # one category for rights
cigDF$fordef <- ifelse(cigDF$q16_14r == 1 | cigDF$q16_15r == 1, 1, 0) # one category for rights
cigDF$EU <- ifelse(cigDF$q16_16r == 1 | cigDF$q16_18r == 1, 1, 0) # one category for EU policy

}

#political contacts
{
  #Federaal
  {
    cigDF$q33_01r<- ifelse(cigDF$q33_01 + cigDF$q33_02 + cigDF$q33_03 + cigDF$q33_04 + cigDF$q33_05 + cigDF$q33_06 + cigDF$q33b_07 + cigDF$q33b_08 == -79984, NA,
                           ifelse (cigDF$q33_01 == -9998, 0 , 
                                   ifelse(cigDF$q33_01 == 1, 0, 
                                          ifelse(cigDF$q33_01 ==2, 1, 
                                                 ifelse(cigDF$q33_01 ==3,  2, 
                                                        ifelse(cigDF$q33_01 ==4, 3,
                                                               ifelse(cigDF$q33_01 ==5, 4, NA))))))) 
    
    cigDF$q33_02r<- ifelse(cigDF$q33_01 + cigDF$q33_02 + cigDF$q33_03 + cigDF$q33_04 + cigDF$q33_05 + cigDF$q33_06 + cigDF$q33b_07 + cigDF$q33b_08 == -79984, NA,
                           ifelse (cigDF$q33_02 == -9998, 0 , 
                                   ifelse(cigDF$q33_02 == 1, 0, 
                                          ifelse(cigDF$q33_02 ==2, 1, 
                                                 ifelse(cigDF$q33_02 ==3,  2, 
                                                        ifelse(cigDF$q33_02 ==4, 3,
                                                               ifelse(cigDF$q33_02 ==5, 4, NA))))))) 
    
    cigDF$q33_03r<- ifelse(cigDF$q33_01 + cigDF$q33_02 + cigDF$q33_03 + cigDF$q33_04 + cigDF$q33_05 + cigDF$q33_06 + cigDF$q33b_07 + cigDF$q33b_08 == -79984, NA,
                           ifelse (cigDF$q33_03 == -9998, 0 , 
                                   ifelse(cigDF$q33_03 == 1, 0, 
                                          ifelse(cigDF$q33_03 ==2, 1, 
                                                 ifelse(cigDF$q33_03 ==3,  2, 
                                                        ifelse(cigDF$q33_03 ==4, 3,
                                                               ifelse(cigDF$q33_03 ==5, 4, NA)))))))
    
    cigDF$q33_04r<- ifelse(cigDF$q33_01 + cigDF$q33_02 + cigDF$q33_03 + cigDF$q33_04 + cigDF$q33_05 + cigDF$q33_06 + cigDF$q33b_07 + cigDF$q33b_08 == -79984, NA,
                           ifelse (cigDF$q33_04 == -9998, 0 , 
                                   ifelse(cigDF$q33_04 == 1, 0, 
                                          ifelse(cigDF$q33_04 ==2, 1, 
                                                 ifelse(cigDF$q33_04 ==3,  2, 
                                                        ifelse(cigDF$q33_04 ==4, 3,
                                                               ifelse(cigDF$q33_04 ==5, 4, NA))))))) 
    
    cigDF$q33_05r<- ifelse(cigDF$q33_01 + cigDF$q33_02 + cigDF$q33_03 + cigDF$q33_04 + cigDF$q33_05 + cigDF$q33_06 + cigDF$q33b_07 + cigDF$q33b_08 == -79984, NA,
                           ifelse (cigDF$q33_05 == -9998, 0 , 
                                   ifelse(cigDF$q33_05 == 1, 0, 
                                          ifelse(cigDF$q33_05 ==2, 1, 
                                                 ifelse(cigDF$q33_05 ==3,  2, 
                                                        ifelse(cigDF$q33_05 ==4, 3,
                                                               ifelse(cigDF$q33_05 ==5, 4, NA))))))) 
    
    cigDF$q33_06r<- ifelse(cigDF$q33_01 + cigDF$q33_02 + cigDF$q33_03 + cigDF$q33_04 + cigDF$q33_05 + cigDF$q33_06 + cigDF$q33b_07 + cigDF$q33b_08 == -79984, NA,
                           ifelse (cigDF$q33_06 == -9998, 0 , 
                                   ifelse(cigDF$q33_06 == 1, 0, 
                                          ifelse(cigDF$q33_06 ==2, 1, 
                                                 ifelse(cigDF$q33_06 ==3,  2, 
                                                        ifelse(cigDF$q33_06 ==4, 3,
                                                               ifelse(cigDF$q33_06 ==5, 4, NA))))))) 
    
    cigDF$q33_07r<- ifelse(cigDF$q33_01 + cigDF$q33_02 + cigDF$q33_03 + cigDF$q33_04 + cigDF$q33_05 + cigDF$q33_06 + cigDF$q33b_07 + cigDF$q33b_08 == -79984, NA,
                           ifelse (cigDF$q33b_07 == -9998, 0 , 
                                   ifelse(cigDF$q33b_07 == 1, 0, 
                                          ifelse(cigDF$q33b_07 ==2, 1, 
                                                 ifelse(cigDF$q33b_07 ==3,  2, 
                                                        ifelse(cigDF$q33b_07 ==4, 3,
                                                               ifelse(cigDF$q33b_07 ==5, 4, NA))))))) 
    
    cigDF$q33_08r<- ifelse(cigDF$q33_01 + cigDF$q33_02 + cigDF$q33_03 + cigDF$q33_04 + cigDF$q33_05 + cigDF$q33_06 + cigDF$q33b_07 + cigDF$q33b_08 == -79984, NA,
                           ifelse (cigDF$q33b_08 == -9998, 0 , 
                                   ifelse(cigDF$q33b_08 == 1, 0, 
                                          ifelse(cigDF$q33b_08 ==2, 1, 
                                                 ifelse(cigDF$q33b_08 ==3,  2, 
                                                        ifelse(cigDF$q33b_08 ==4, 3,
                                                               ifelse(cigDF$q33b_08 ==5, 4, NA))))))) 
  }
  #descriptives Federal
  { # executive 
    
    cigDF$q33_01rr <- ifelse( !is.na(cigDF$q33_01r), cigDF$q33_01r,
                              ifelse(cigDF$federalfinal == 0 & is.na(cigDF$q33_01r), 0, NA))
    
    cigDF$q33_04rr <- ifelse( !is.na(cigDF$q33_04r), cigDF$q33_04r,
                              ifelse(cigDF$federalfinal == 0 & is.na(cigDF$q33_04r), 0, NA))
    
    cigDF$q33_07rr <- ifelse( !is.na(cigDF$q33_07r), cigDF$q33_07r,
                              ifelse(cigDF$federalfinal == 0 & is.na(cigDF$q33_07r), 0, NA))
    
    cigDF$q33_08rr <- ifelse( !is.na(cigDF$q33_08r), cigDF$q33_08r,
                              ifelse(cigDF$federalfinal == 0 & is.na(cigDF$q33_08r), 0, NA))
    
    a <- as.data.frame(prop.table(table(cigDF$q33_01rr)))
    b <- as.data.frame(prop.table(table(cigDF$q33_04rr)))
    c <- as.data.frame(prop.table(table(cigDF$q33_07rr)))
    d <- as.data.frame(prop.table(table(cigDF$q33_08rr)))
    
    A <- (a[1,2] + b[1,2] + c[1,2] + d[1,2])/4
    B <- (a[2,2] + b[2,2] + c[2,2] + d[2,2])/4
    C <- (a[3,2] + b[3,2] + c[3,2] + d[3,2])/4
    D <- (a[4,2] + b[4,2] + c[4,2] + d[4,2])/4
    E <- (a[5,2] + b[5,2] + c[5,2] + d[5,2])/4
    
    #mean contact to the executive
    fedexec <-  c(A,B,C,D,E)
    fedexec
    as.data.frame(fedexec)
    
    
    #bureaucracy
    
    cigDF$q33_05rr <- ifelse( !is.na(cigDF$q33_05r), cigDF$q33_05r,
                              ifelse(cigDF$federalfinal == 0 & is.na(cigDF$q33_05r), 0, NA))
    cigDF$q33_06rr <- ifelse( !is.na(cigDF$q33_06r), cigDF$q33_06r,
                              ifelse(cigDF$federalfinal == 0 & is.na(cigDF$q33_06r), 0, NA))
    
    a <- as.data.frame(prop.table(table(cigDF$q33_05rr)))
    b <- as.data.frame(prop.table(table(cigDF$q33_06rr)))
    
    A <- (a[1,2] + b[1,2])/2
    B <- (a[2,2] + b[2,2])/2
    C <- (a[3,2] + b[3,2])/2
    D <- (a[4,2] + b[4,2])/2
    E <- (a[5,2] + b[5,2])/2
    
    fedbur <-  c(A,B,C,D,E)
    fedbur
    
    #Parliament
    
    cigDF$q33_02rr <- ifelse( !is.na(cigDF$q33_02r), cigDF$q33_02r,
                              ifelse(cigDF$federalfinal == 0 & is.na(cigDF$q33_02r), 0, NA))
    cigDF$q33_03rr <- ifelse( !is.na(cigDF$q33_03r), cigDF$q33_03r,
                              ifelse(cigDF$federalfinal == 0 & is.na(cigDF$q33_03r), 0, NA))
    
    
    a <- as.data.frame(prop.table(table(cigDF$q33_02rr)))
    b <- as.data.frame(prop.table(table(cigDF$q33_03rr)))
    
    A <- (a[1,2] + b[1,2])/2
    B <- (a[2,2] + b[2,2])/2
    C <- (a[3,2] + b[3,2])/2
    D <- (a[4,2] + b[4,2])/2
    E <- (a[5,2] + b[5,2])/2
    
    fedpar <-  c(A,B,C,D,E)
    fedpar
  }
  #Waals gewest
  {
    cigDF$q100_01r<- ifelse(cigDF$q100b_01 + cigDF$q100b_02 + cigDF$q100b_03 + cigDF$q100b_04 + cigDF$q100b_05 + cigDF$q100b_06 == -59988, NA,
                            ifelse (cigDF$q100b_01 == -9998, 0 , 
                                    ifelse(cigDF$q100b_01 == 1, 0, 
                                           ifelse(cigDF$q100b_01 ==2, 1, 
                                                  ifelse(cigDF$q100b_01 ==3,  2, 
                                                         ifelse(cigDF$q100b_01 ==4, 3,
                                                                ifelse(cigDF$q100b_01 ==5, 4, NA))))))) 
    
    cigDF$q100_02r<- ifelse(cigDF$q100b_01 + cigDF$q100b_02 + cigDF$q100b_03 + cigDF$q100b_04 + cigDF$q100b_05 + cigDF$q100b_06 == -59988, NA,
                            ifelse (cigDF$q100b_02 == -9998, 0 , 
                                    ifelse(cigDF$q100b_02 == 1, 0, 
                                           ifelse(cigDF$q100b_02 ==2, 1, 
                                                  ifelse(cigDF$q100b_02 ==3,  2, 
                                                         ifelse(cigDF$q100b_02 ==4, 3,
                                                                ifelse(cigDF$q100b_02 ==5, 4, NA)))))))
    
    cigDF$q100_03r<- ifelse(cigDF$q100b_01 + cigDF$q100b_02 + cigDF$q100b_03 + cigDF$q100b_04 + cigDF$q100b_05 + cigDF$q100b_06 == -59988, NA,
                            ifelse (cigDF$q100b_03 == -9998, 0 , 
                                    ifelse(cigDF$q100b_03 == 1, 0, 
                                           ifelse(cigDF$q100b_03 ==2, 1, 
                                                  ifelse(cigDF$q100b_03 ==3,  2, 
                                                         ifelse(cigDF$q100b_03 ==4, 3,
                                                                ifelse(cigDF$q100b_03 ==5, 4, NA)))))))
    
    cigDF$q100_04r<- ifelse(cigDF$q100b_01 + cigDF$q100b_02 + cigDF$q100b_03 + cigDF$q100b_04 + cigDF$q100b_05 + cigDF$q100b_06 == -59988, NA,
                            ifelse (cigDF$q100b_04 == -9998, 0 , 
                                    ifelse(cigDF$q100b_04 == 1, 0, 
                                           ifelse(cigDF$q100b_04 ==2, 1, 
                                                  ifelse(cigDF$q100b_04 ==3,  2, 
                                                         ifelse(cigDF$q100b_04 ==4, 3,
                                                                ifelse(cigDF$q100b_04 ==5, 4, NA)))))))
    
    cigDF$q100_05r<- ifelse(cigDF$q100b_01 + cigDF$q100b_02 + cigDF$q100b_03 + cigDF$q100b_04 + cigDF$q100b_05 + cigDF$q100b_06 == -59988, NA,
                            ifelse (cigDF$q100b_05 == -9998, 0 , 
                                    ifelse(cigDF$q100b_05 == 1, 0, 
                                           ifelse(cigDF$q100b_05 ==2, 1, 
                                                  ifelse(cigDF$q100b_05 ==3,  2, 
                                                         ifelse(cigDF$q100b_05 ==4, 3,
                                                                ifelse(cigDF$q100b_05 ==5, 4, NA)))))))
    
    cigDF$q100_06r<- ifelse(cigDF$q100b_01 + cigDF$q100b_02 + cigDF$q100b_03 + cigDF$q100b_04 + cigDF$q100b_05 + cigDF$q100b_06 == -59988, NA,
                            ifelse (cigDF$q100b_06 == -9998, 0 , 
                                    ifelse(cigDF$q100b_06 == 1, 0, 
                                           ifelse(cigDF$q100b_06 ==2, 1, 
                                                  ifelse(cigDF$q100b_06 ==3,  2, 
                                                         ifelse(cigDF$q100b_06 ==4, 3,
                                                                ifelse(cigDF$q100b_06 ==5, 4, NA)))))))
    
    
    
    cigDF$q101_01r<- ifelse(cigDF$q101b_01 + cigDF$q101b_02 + cigDF$q101b_03 + cigDF$q101b_04 + cigDF$q101b_05 + cigDF$q101b_06 == -59988, NA,
                            ifelse (cigDF$q101b_01 == -9998, 0 , 
                                    ifelse(cigDF$q101b_01 == 1, 0, 
                                           ifelse(cigDF$q101b_01 ==2, 1, 
                                                  ifelse(cigDF$q101b_01 ==3,  2, 
                                                         ifelse(cigDF$q101b_01 ==4, 3,
                                                                ifelse(cigDF$q101b_01 ==5, 4, NA))))))) 
    
    cigDF$q101_02r<- ifelse(cigDF$q101b_01 + cigDF$q101b_02 + cigDF$q101b_03 + cigDF$q101b_04 + cigDF$q101b_05 + cigDF$q101b_06 == -59988, NA,
                            ifelse (cigDF$q101b_02 == -9998, 0 , 
                                    ifelse(cigDF$q101b_02 == 1, 0, 
                                           ifelse(cigDF$q101b_02 ==2, 1, 
                                                  ifelse(cigDF$q101b_02 ==3,  2, 
                                                         ifelse(cigDF$q101b_02 ==4, 3,
                                                                ifelse(cigDF$q101b_02 ==5, 4, NA)))))))
    
    cigDF$q101_03r<- ifelse(cigDF$q101b_01 + cigDF$q101b_02 + cigDF$q101b_03 + cigDF$q101b_04 + cigDF$q101b_05 + cigDF$q101b_06 == -59988, NA,
                            ifelse (cigDF$q101b_03 == -9998, 0 , 
                                    ifelse(cigDF$q101b_03 == 1, 0, 
                                           ifelse(cigDF$q101b_03 ==2, 1, 
                                                  ifelse(cigDF$q101b_03 ==3,  2, 
                                                         ifelse(cigDF$q101b_03 ==4, 3,
                                                                ifelse(cigDF$q101b_03 ==5, 4, NA)))))))
    
    cigDF$q101_04r<- ifelse(cigDF$q101b_01 + cigDF$q101b_02 + cigDF$q101b_03 + cigDF$q101b_04 + cigDF$q101b_05 + cigDF$q101b_06 == -59988, NA,
                            ifelse (cigDF$q101b_04 == -9998, 0 , 
                                    ifelse(cigDF$q101b_04 == 1, 0, 
                                           ifelse(cigDF$q101b_04 ==2, 1, 
                                                  ifelse(cigDF$q101b_04 ==3,  2, 
                                                         ifelse(cigDF$q101b_04 ==4, 3,
                                                                ifelse(cigDF$q101b_04 ==5, 4, NA)))))))
    
    cigDF$q101_05r<- ifelse(cigDF$q101b_01 + cigDF$q101b_02 + cigDF$q101b_03 + cigDF$q101b_04 + cigDF$q101b_05 + cigDF$q101b_06 == -59988, NA,
                            ifelse (cigDF$q101b_05 == -9998, 0 , 
                                    ifelse(cigDF$q101b_05 == 1, 0, 
                                           ifelse(cigDF$q101b_05 ==2, 1, 
                                                  ifelse(cigDF$q101b_05 ==3,  2, 
                                                         ifelse(cigDF$q101b_05 ==4, 3,
                                                                ifelse(cigDF$q101b_05 ==5, 4, NA)))))))
    
    cigDF$q101_06r<- ifelse(cigDF$q101b_01 + cigDF$q101b_02 + cigDF$q101b_03 + cigDF$q101b_04 + cigDF$q101b_05 + cigDF$q101b_06 == -59988, NA,
                            ifelse (cigDF$q101b_06 == -9998, 0 , 
                                    ifelse(cigDF$q101b_06 == 1, 0, 
                                           ifelse(cigDF$q101b_06 ==2, 1, 
                                                  ifelse(cigDF$q101b_06 ==3,  2, 
                                                         ifelse(cigDF$q101b_06 ==4, 3,
                                                                ifelse(cigDF$q101b_06 ==5, 4, NA)))))))
    
    
  }
  #descriptives Waal
  { # executive 
    cigDF$q100_01rr <- ifelse( !is.na(cigDF$q100_01r), cigDF$q100_01r,
                               ifelse(cigDF$WLfinal == 0 & is.na(cigDF$q100_01r), 0, NA))
    cigDF$q100_02rr <- ifelse( !is.na(cigDF$q100_02r), cigDF$q100_02r,
                               ifelse(cigDF$WLfinal == 0 & is.na(cigDF$q100_02r), 0, NA))
    cigDF$q100_03rr <- ifelse( !is.na(cigDF$q100_03r), cigDF$q100_03r,
                               ifelse(cigDF$WLfinal == 0 & is.na(cigDF$q100_03r), 0, NA))
    
    cigDF$q101_01rr <- ifelse( !is.na(cigDF$q101_01r), cigDF$q101_01r,
                               ifelse(cigDF$WLfinal == 0 & is.na(cigDF$q101_01r), 0, NA))
    cigDF$q101_02rr <- ifelse( !is.na(cigDF$q101_02r), cigDF$q101_02r,
                               ifelse(cigDF$WLfinal == 0 & is.na(cigDF$q101_02r), 0, NA))
    cigDF$q101_03rr <- ifelse( !is.na(cigDF$q101_03r), cigDF$q101_03r,
                               ifelse(cigDF$WLfinal == 0 & is.na(cigDF$q101_03r), 0, NA))
    
    
    a <- as.data.frame(prop.table(table(cigDF$q100_01rr)))
    b <- as.data.frame(prop.table(table(cigDF$q100_02rr)))
    c <- as.data.frame(prop.table(table(cigDF$q100_03rr)))
    
    aa <- as.data.frame(prop.table(table(cigDF$q101_01rr)))
    bb <- as.data.frame(prop.table(table(cigDF$q101_02rr)))
    cc <- as.data.frame(prop.table(table(cigDF$q101_03rr)))
    
    A <- (a[1,2] + b[1,2] + c[1,2] + aa[1,2] + bb[1,2] + cc[1,2])/6
    B <- (a[2,2] + b[2,2] + c[2,2] + aa[2,2] + bb[2,2] + cc[2,2])/6
    C <- (a[3,2] + b[3,2] + c[3,2] + aa[3,2] + bb[3,2] + cc[3,2])/6
    D <- (a[4,2] + b[4,2] + c[4,2] + aa[4,2] + bb[4,2] + cc[4,2])/6
    E <- (a[5,2] + b[5,2] + c[5,2] + aa[5,2] + bb[5,2] + cc[5,2])/6
    
    #mean contact to the executive
    walloonexec <-  c(A,B,C,D,E)
    walloonexec
    #bureaucracy
    cigDF$q100_04rr <- ifelse( !is.na(cigDF$q100_04r), cigDF$q100_04r,
                               ifelse(cigDF$WLfinal == 0 & is.na(cigDF$q100_04r), 0, NA))
    
    cigDF$q101_04rr <- ifelse( !is.na(cigDF$q101_04r), cigDF$q101_04r,
                               ifelse(cigDF$WLfinal == 0 & is.na(cigDF$q101_04r), 0, NA))
    
    
    
    a <- as.data.frame(prop.table(table(cigDF$q100_04rr)))
    b <- as.data.frame(prop.table(table(cigDF$q101_04rr)))
    
    A <- (a[1,2] + b[1,2])/2
    B <- (a[2,2] + b[2,2])/2
    C <- (a[3,2] + b[3,2])/2
    D <- (a[4,2] + b[4,2])/2
    E <- (a[5,2] + b[5,2])/2
    
    walloonbur <-  c(A,B,C,D,E)
    walloonbur
    #Parliament
    
    cigDF$q100_05rr <- ifelse( !is.na(cigDF$q100_05r), cigDF$q100_05r,
                               ifelse(cigDF$WLfinal == 0 & is.na(cigDF$q100_05r), 0, NA))
    cigDF$q100_06rr <- ifelse( !is.na(cigDF$q100_06r), cigDF$q100_06r,
                               ifelse(cigDF$WLfinal == 0 & is.na(cigDF$q100_06r), 0, NA))
    cigDF$q101_05rr <- ifelse( !is.na(cigDF$q101_05r), cigDF$q101_05r,
                               ifelse(cigDF$WLfinal == 0 & is.na(cigDF$q101_05r), 0, NA))
    cigDF$q101_06rr <- ifelse( !is.na(cigDF$q101_06r), cigDF$q101_06r,
                               ifelse(cigDF$WLfinal == 0 & is.na(cigDF$q101_06r), 0, NA))
    
    
    a <- as.data.frame(prop.table(table(cigDF$q100_05rr)))
    b <- as.data.frame(prop.table(table(cigDF$q100_06rr)))
    c <- as.data.frame(prop.table(table(cigDF$q101_05rr)))
    D <- as.data.frame(prop.table(table(cigDF$q101_06rr)))
    
    A <- (a[1,2] + b[1,2] + c[1,2] + d[1,2])/4
    B <- (a[2,2] + b[2,2] + c[2,2] + d[2,2])/4
    C <- (a[3,2] + b[3,2] + c[3,2] + d[3,2])/4
    D <- (a[4,2] + b[4,2] + c[4,2] + d[4,2])/4
    E <- (a[5,2] + b[5,2] + c[5,2] + d[5,2])/4
    
    walloonpar <-  c(A,B,C,D,E)
    walloonpar
  }
  #Vlaanderen
  {
    cigDF$q103_01r<- ifelse(cigDF$q103b_01 + cigDF$q103b_02 + cigDF$q103b_03 + cigDF$q103b_04 + cigDF$q103b_05 + cigDF$q103b_06 == -59988, NA,
                            ifelse (cigDF$q103b_01 == -9998, 0 , 
                                    ifelse(cigDF$q103b_01 == 1, 0, 
                                           ifelse(cigDF$q103b_01 ==2, 1, 
                                                  ifelse(cigDF$q103b_01 ==3,  2, 
                                                         ifelse(cigDF$q103b_01 ==4, 3,
                                                                ifelse(cigDF$q103b_01 ==5, 4, NA))))))) 
    
    cigDF$q103_02r<- ifelse(cigDF$q103b_01 + cigDF$q103b_02 + cigDF$q103b_03 + cigDF$q103b_04 + cigDF$q103b_05 + cigDF$q103b_06 == -59988, NA,
                            ifelse (cigDF$q103b_02 == -9998, 0 , 
                                    ifelse(cigDF$q103b_02 == 1, 0, 
                                           ifelse(cigDF$q103b_02 ==2, 1, 
                                                  ifelse(cigDF$q103b_02 ==3,  2, 
                                                         ifelse(cigDF$q103b_02 ==4, 3,
                                                                ifelse(cigDF$q103b_02 ==5, 4, NA)))))))
    
    cigDF$q103_03r<- ifelse(cigDF$q103b_01 + cigDF$q103b_02 + cigDF$q103b_03 + cigDF$q103b_04 + cigDF$q103b_05 + cigDF$q103b_06 == -59988, NA,
                            ifelse (cigDF$q103b_03 == -9998, 0 , 
                                    ifelse(cigDF$q103b_03 == 1, 0, 
                                           ifelse(cigDF$q103b_03 ==2, 1, 
                                                  ifelse(cigDF$q103b_03 ==3,  2, 
                                                         ifelse(cigDF$q103b_03 ==4, 3,
                                                                ifelse(cigDF$q103b_03 ==5, 4, NA)))))))
    
    cigDF$q103_04r<- ifelse(cigDF$q103b_01 + cigDF$q103b_02 + cigDF$q103b_03 + cigDF$q103b_04 + cigDF$q103b_05 + cigDF$q103b_06 == -59988, NA,
                            ifelse (cigDF$q103b_04 == -9998, 0 , 
                                    ifelse(cigDF$q103b_04 == 1, 0, 
                                           ifelse(cigDF$q103b_04 ==2, 1, 
                                                  ifelse(cigDF$q103b_04 ==3,  2, 
                                                         ifelse(cigDF$q103b_04 ==4, 3,
                                                                ifelse(cigDF$q103b_04 ==5, 4, NA)))))))
    
    cigDF$q103_05r<- ifelse(cigDF$q103b_01 + cigDF$q103b_02 + cigDF$q103b_03 + cigDF$q103b_04 + cigDF$q103b_05 + cigDF$q103b_06 == -59988, NA,
                            ifelse (cigDF$q103b_05 == -9998, 0 , 
                                    ifelse(cigDF$q103b_05 == 1, 0, 
                                           ifelse(cigDF$q103b_05 ==2, 1, 
                                                  ifelse(cigDF$q103b_05 ==3,  2, 
                                                         ifelse(cigDF$q103b_05 ==4, 3,
                                                                ifelse(cigDF$q103b_05 ==5, 4, NA)))))))
    
    cigDF$q103_06r<- ifelse(cigDF$q103b_01 + cigDF$q103b_02 + cigDF$q103b_03 + cigDF$q103b_04 + cigDF$q103b_05 + cigDF$q103b_06 == -59988, NA,
                            ifelse (cigDF$q103b_06 == -9998, 0 , 
                                    ifelse(cigDF$q103b_06 == 1, 0, 
                                           ifelse(cigDF$q103b_06 ==2, 1, 
                                                  ifelse(cigDF$q103b_06 ==3,  2, 
                                                         ifelse(cigDF$q103b_06 ==4, 3,
                                                                ifelse(cigDF$q103b_06 ==5, 4, NA)))))))
  }
  #descriptives Flanders
  { # executive 
    
    cigDF$q103_01rr <- ifelse( !is.na(cigDF$q103_01r), cigDF$q103_01r,
                               ifelse(cigDF$VLfinal == 0 & is.na(cigDF$q103_01r), 0, NA))
    cigDF$q103_02rr <- ifelse( !is.na(cigDF$q103_02r), cigDF$q103_02r,
                               ifelse(cigDF$VLfinal == 0 & is.na(cigDF$q103_02r), 0, NA))
    cigDF$q103_03rr <- ifelse( !is.na(cigDF$q103_03r), cigDF$q103_03r,
                               ifelse(cigDF$VLfinal == 0 & is.na(cigDF$q103_03r), 0, NA))
    
    
    a <- as.data.frame(prop.table(table(cigDF$q103_01rr)))
    b <- as.data.frame(prop.table(table(cigDF$q103_02rr)))
    c <- as.data.frame(prop.table(table(cigDF$q103_03rr)))
    
    
    A <- (a[1,2] + b[1,2] + c[1,2])/3
    B <- (a[2,2] + b[2,2] + c[2,2])/3
    C <- (a[3,2] + b[3,2] + c[3,2])/3
    D <- (a[4,2] + b[4,2] + c[4,2])/3
    E <- (a[5,2] + b[5,2] + c[5,2])/3
    
    #mean contact to the executive
    flexec <-  c(A,B,C,D,E)
    flexec
    
    #bureaucracy
    
    cigDF$q103_04rr <- ifelse( !is.na(cigDF$q103_04r), cigDF$q103_04r,
                               ifelse(cigDF$VLfinal == 0 & is.na(cigDF$q103_04r), 0, NA))
    
    flbur <- prop.table(table(cigDF$q103_04rr))
    flbur
    #Parliament
    
    cigDF$q103_05rr <- ifelse( !is.na(cigDF$q103_05r), cigDF$q103_05r,
                               ifelse(cigDF$VLfinal == 0 & is.na(cigDF$q103_05r), 0, NA))
    cigDF$q103_06rr <- ifelse( !is.na(cigDF$q103_06r), cigDF$q103_06r,
                               ifelse(cigDF$VLfinal == 0 & is.na(cigDF$q103_06r), 0, NA))
    
    a <- as.data.frame(prop.table(table(cigDF$q103_05rr)))
    b <- as.data.frame(prop.table(table(cigDF$q103_06rr)))
    
    A <- (a[1,2] + b[1,2])/2
    B <- (a[2,2] + b[2,2])/2
    C <- (a[3,2] + b[3,2])/2
    D <- (a[4,2] + b[4,2])/2
    E <- (a[5,2] + b[5,2])/2
    
    flpar <-  c(A,B,C,D,E)
    flpar
  }
  
  
  
}


#________________________________________Analysis__________________________________________#

#distribution across the different levels of government
prop.table(table(cigDF$q73r))

#table 1
prop.table(table(cigDF$q73r, cigDF$type),1)
prop.table(table(cigDF$type))

#table 2
## col 1
mean(na.omit(cigDF[which(cigDF$type==1), which(colnames(cigDF)=="q02r" )]))
mean(na.omit(cigDF[which(cigDF$type==2), which(colnames(cigDF)=="q02r" )]))
mean(na.omit(cigDF[which(cigDF$type==3), which(colnames(cigDF)=="q02r" )]))
mean(na.omit(cigDF[which(cigDF$type==4), which(colnames(cigDF)=="q02r" )]))
mean(na.omit(cigDF[which(cigDF$type==5), which(colnames(cigDF)=="q02r" )]))
mean(na.omit(cigDF[which(cigDF$type==6), which(colnames(cigDF)=="q02r" )]))
mean(na.omit(cigDF[which(cigDF$type==7), which(colnames(cigDF)=="q02r" )]))

## col 2
mean(na.omit(cigDF[which(cigDF$type==1), which(colnames(cigDF)=="q21_01r" )]))
mean(na.omit(cigDF[which(cigDF$type==2), which(colnames(cigDF)=="q21_01r" )]))
mean(na.omit(cigDF[which(cigDF$type==3), which(colnames(cigDF)=="q21_01r" )]))
mean(na.omit(cigDF[which(cigDF$type==4), which(colnames(cigDF)=="q21_01r" )]))
mean(na.omit(cigDF[which(cigDF$type==5), which(colnames(cigDF)=="q21_01r" )]))
mean(na.omit(cigDF[which(cigDF$type==6), which(colnames(cigDF)=="q21_01r" )]))
mean(na.omit(cigDF[which(cigDF$type==7), which(colnames(cigDF)=="q21_01r" )]))

## col 3
median(na.omit(cigDF[which(cigDF$type==1), which(colnames(cigDF)=="q08r" )]))
median(na.omit(cigDF[which(cigDF$type==2), which(colnames(cigDF)=="q08r" )]))
median(na.omit(cigDF[which(cigDF$type==3), which(colnames(cigDF)=="q08r" )]))
median(na.omit(cigDF[which(cigDF$type==4), which(colnames(cigDF)=="q08r" )]))
median(na.omit(cigDF[which(cigDF$type==5), which(colnames(cigDF)=="q08r" )]))
median(na.omit(cigDF[which(cigDF$type==6), which(colnames(cigDF)=="q08r" )]))
median(na.omit(cigDF[which(cigDF$type==7), which(colnames(cigDF)=="q08r" )]))


#plot: hist of founding dates
##general plot
qplot(cigDF[which(cigDF$q02r > 1850),which(colnames(cigDF)=="q02r" )],
      geom="histogram",
      binwidth = 5, 
      main = "Founding dates", 
      xlab = "Number of organisations",  
      fill=I("grey"), 
      col=I("black"), 
      alpha=I(.5))

##zoom in on 1990 and later
qplot(cigDF[which(cigDF$q02r > 1990),which(colnames(cigDF)=="q02r" )],
      geom="histogram",
      binwidth = 1, 
      main = "Founding dates", 
      xlab = "Number of organisations",  
      fill=I("grey"), 
      col=I("black"), 
      alpha=I(.5))

    # plot1 = ggplot(totalsurvey, aes(YoF, na.rm = 'true')) + geom_histogram(breaks = seq (1830, 2020, by = 5), col="black", fill="lightgreen", alpha = 1, binwidth = 10 )+
    # labs(title="") + 
    # labs(x="Year", y="Number of founded organisations") +
    # xlim(c (1830, 2020))+ scale_x_continuous(breaks=seq(1830, 2020, 10)) 
    # 
    # plot2 = ggplot(totalsurvey, aes(YoF, na.rm = 'true')) + geom_histogram(breaks = seq (1830, 2030, by = 5), col="black", fill="lightgreen", alpha = 1, binwidth = 10 )+
    # facet_grid(~type3)+
    # labs(title="") + 
    # labs(x="Year", y="Number of founded organisations") +
    # xlim(c (1830, 2030))+ scale_x_continuous(breaks=seq(1830, 2020, 40)) 


##EVELIEN CODE###

totalsurvey$yes_no <- ifelse(totalsurvey$X..AC == 0, 0,
                             ifelse (totalsurvey$X..AC  != 0, 1 , NA))


write.table(totalsurvey, file="totalsurvey.csv", row.names=FALSE,sep=";",qmethod=c("double"),fileEncoding = "UTF-8")



## financial resources 
totalsurvey <- within (totalsurvey,{
  q08r <- Recode(q08, ' -9998 = NA; -9999 = NA; 9 = NA'
  )}) 
## staff
totalsurvey <- within (totalsurvey,{
  q21_01r <- Recode(q21_01, ' -9998 = NA; -9999 = NA'
  )}) 


##advocacy energy
#OUTSIDE
#scale on a year
{totalsurvey$q34_01r<- ifelse(totalsurvey$q34_01 + totalsurvey$q34_03 + totalsurvey$q34_04 + totalsurvey$q34_05 + totalsurvey$q34_06 + totalsurvey$q34_07 + totalsurvey$q34_08 == -69986, NA,
                              ifelse (totalsurvey$q34_01 == -9998, 0 , 
                                      ifelse(totalsurvey$q34_01 == 1, 0, 
                                             ifelse(totalsurvey$q34_01 ==2, 1, 
                                                    ifelse(totalsurvey$q34_01 ==3,  4, 
                                                           ifelse(totalsurvey$q34_01 ==4, 12,
                                                                  ifelse(totalsurvey$q34_01 ==5, 52, NA))))))) 
  
  totalsurvey$q34_03r<- ifelse(totalsurvey$q34_01 + totalsurvey$q34_03 + totalsurvey$q34_04 + totalsurvey$q34_05 + totalsurvey$q34_06 + totalsurvey$q34_07 + totalsurvey$q34_08 == -69986, NA,
                               ifelse (totalsurvey$q34_03 == -9998, 0 , 
                                       ifelse(totalsurvey$q34_03 == 1, 0, 
                                              ifelse(totalsurvey$q34_03 ==2, 1, 
                                                     ifelse(totalsurvey$q34_03 ==3,  4, 
                                                            ifelse(totalsurvey$q34_03 ==4, 12,
                                                                   ifelse(totalsurvey$q34_03 ==5, 52, NA))))))) 
  
  totalsurvey$q34_04r<- ifelse(totalsurvey$q34_01 + totalsurvey$q34_03 + totalsurvey$q34_04 + totalsurvey$q34_05 + totalsurvey$q34_06 + totalsurvey$q34_07 + totalsurvey$q34_08 == -69986, NA,
                               ifelse (totalsurvey$q34_04 == -9998, 0 , 
                                       ifelse(totalsurvey$q34_04 == 1, 0, 
                                              ifelse(totalsurvey$q34_04 ==2, 1, 
                                                     ifelse(totalsurvey$q34_04 ==3,  4, 
                                                            ifelse(totalsurvey$q34_04 ==4, 12,
                                                                   ifelse(totalsurvey$q34_04 ==5, 52, NA))))))) 
  
  totalsurvey$q34_05r<- ifelse(totalsurvey$q34_01 + totalsurvey$q34_03 + totalsurvey$q34_04 + totalsurvey$q34_05 + totalsurvey$q34_06 + totalsurvey$q34_07 + totalsurvey$q34_08 == -69986, NA,
                               ifelse (totalsurvey$q34_05 == -9998, 0 , 
                                       ifelse(totalsurvey$q34_05 == 1, 0, 
                                              ifelse(totalsurvey$q34_05 ==2, 1, 
                                                     ifelse(totalsurvey$q34_05 ==3,  4, 
                                                            ifelse(totalsurvey$q34_05 ==4, 12,
                                                                   ifelse(totalsurvey$q34_05 ==5, 52, NA))))))) 
  
  totalsurvey$q34_06r<- ifelse(totalsurvey$q34_01 + totalsurvey$q34_03 + totalsurvey$q34_04 + totalsurvey$q34_05 + totalsurvey$q34_06 + totalsurvey$q34_07 + totalsurvey$q34_08 == -69986, NA,
                               ifelse (totalsurvey$q34_06 == -9998, 0 , 
                                       ifelse(totalsurvey$q34_06 == 1, 0, 
                                              ifelse(totalsurvey$q34_06 ==2, 1, 
                                                     ifelse(totalsurvey$q34_06 ==3,  4, 
                                                            ifelse(totalsurvey$q34_06 ==4, 12,
                                                                   ifelse(totalsurvey$q34_06 ==5, 52, NA))))))) 
  
  totalsurvey$q34_07r<- ifelse(totalsurvey$q34_01 + totalsurvey$q34_03 + totalsurvey$q34_04 + totalsurvey$q34_05 + totalsurvey$q34_06 + totalsurvey$q34_07 + totalsurvey$q34_08 == -69986, NA,
                               ifelse (totalsurvey$q34_07 == -9998, 0 , 
                                       ifelse(totalsurvey$q34_07 == 1, 0, 
                                              ifelse(totalsurvey$q34_07 ==2, 1, 
                                                     ifelse(totalsurvey$q34_07 ==3,  4, 
                                                            ifelse(totalsurvey$q34_07 ==4, 12,
                                                                   ifelse(totalsurvey$q34_07 ==5, 52, NA))))))) 
  
  totalsurvey$q34_08r<- ifelse(totalsurvey$q34_01 + totalsurvey$q34_03 + totalsurvey$q34_04 + totalsurvey$q34_05 + totalsurvey$q34_06 + totalsurvey$q34_07 + totalsurvey$q34_08 == -69986, NA,
                               ifelse (totalsurvey$q34_08 == -9998, 0 , 
                                       ifelse(totalsurvey$q34_08 == 1, 0, 
                                              ifelse(totalsurvey$q34_08 ==2, 1, 
                                                     ifelse(totalsurvey$q34_08 ==3,  4, 
                                                            ifelse(totalsurvey$q34_08 ==4, 12,
                                                                   ifelse(totalsurvey$q34_08 ==5, 52, NA))))))) 
  #scale on a week
  totalsurvey$outside <- with(totalsurvey, q34_01r +q34_03r+q34_04r+ q34_05r+q34_06r+q34_07r+q34_08r)
  
  totalsurvey$outsider <- with(totalsurvey, outside/52)
  
  #total number of policy areas involvement
  totalsurvey$numberpol <- with(totalsurvey,q16_01r + q16_02r + q16_03r + q16_04r +
                                  q16_05r + q16_06r + q16_08r +
                                  q16_09r + q16_11r + q16_12r + q16_13r +
                                  q16_15r + q16_16r + q16_17r +  
                                  q16_20r + q16_21r + q16_22r)
  
  totalsurvey <- within (totalsurvey,{
    numberpolr <- Recode(numberpol, '0 = NA'
    )}) 
  
  
  #average advocacy energy per week per policy area
  totalsurvey$outenergy <- with(totalsurvey, outsider/numberpolr)
}

#INSIDE -> per gov level!
{# the values of -9998 and -9999 were switched in the orginal data set
  ## Waalse Gewest
  # scale on a year
  totalsurvey$q100_01r<- ifelse(totalsurvey$q100b_01 + totalsurvey$q100b_02 + totalsurvey$q100b_03 + totalsurvey$q100b_04 + totalsurvey$q100b_05 + totalsurvey$q100b_06 == -59988, NA,
                                ifelse (totalsurvey$q100b_01 == -9998, 0 , 
                                        ifelse(totalsurvey$q100b_01 == 1, 0, 
                                               ifelse(totalsurvey$q100b_01 ==2, 1, 
                                                      ifelse(totalsurvey$q100b_01 ==3, 4, 
                                                             ifelse(totalsurvey$q100b_01 ==4, 12,
                                                                    ifelse(totalsurvey$q100b_01 ==5, 52, NA))))))) 
  
  totalsurvey$q100_02r<- ifelse(totalsurvey$q100b_01 + totalsurvey$q100b_02 + totalsurvey$q100b_03 + totalsurvey$q100b_04 + totalsurvey$q100b_05 + totalsurvey$q100b_06 == -59988, NA,
                                ifelse (totalsurvey$q100b_02 == -9998, 0 , 
                                        ifelse(totalsurvey$q100b_02 == 1, 0, 
                                               ifelse(totalsurvey$q100b_02 ==2, 1, 
                                                      ifelse(totalsurvey$q100b_02 ==3,  4, 
                                                             ifelse(totalsurvey$q100b_02 ==4, 12,
                                                                    ifelse(totalsurvey$q100b_02 ==5, 52, NA)))))))
  
  totalsurvey$q100_03r<- ifelse(totalsurvey$q100b_01 + totalsurvey$q100b_02 + totalsurvey$q100b_03 + totalsurvey$q100b_04 + totalsurvey$q100b_05 + totalsurvey$q100b_06 == -59988, NA,
                                ifelse (totalsurvey$q100b_03 == -9998, 0 , 
                                        ifelse(totalsurvey$q100b_03 == 1, 0, 
                                               ifelse(totalsurvey$q100b_03 ==2, 1, 
                                                      ifelse(totalsurvey$q100b_03 ==3, 4, 
                                                             ifelse(totalsurvey$q100b_03 ==4, 12,
                                                                    ifelse(totalsurvey$q100b_03 ==5, 52, NA)))))))
  
  totalsurvey$q100_04r<- ifelse(totalsurvey$q100b_01 + totalsurvey$q100b_02 + totalsurvey$q100b_03 + totalsurvey$q100b_04 + totalsurvey$q100b_05 + totalsurvey$q100b_06 == -59988, NA,
                                ifelse (totalsurvey$q100b_04 == -9998, 0 , 
                                        ifelse(totalsurvey$q100b_04 == 1, 0, 
                                               ifelse(totalsurvey$q100b_04 ==2, 1, 
                                                      ifelse(totalsurvey$q100b_04 ==3,  4, 
                                                             ifelse(totalsurvey$q100b_04 ==4, 12,
                                                                    ifelse(totalsurvey$q100b_04 ==5, 52, NA)))))))
  
  totalsurvey$q100_05r<- ifelse(totalsurvey$q100b_01 + totalsurvey$q100b_02 + totalsurvey$q100b_03 + totalsurvey$q100b_04 + totalsurvey$q100b_05 + totalsurvey$q100b_06 == -59988, NA,
                                ifelse (totalsurvey$q100b_05 == -9998, 0 , 
                                        ifelse(totalsurvey$q100b_05 == 1, 0, 
                                               ifelse(totalsurvey$q100b_05 ==2, 1, 
                                                      ifelse(totalsurvey$q100b_05 ==3,  4, 
                                                             ifelse(totalsurvey$q100b_05 ==4,12,
                                                                    ifelse(totalsurvey$q100b_05 ==5, 52, NA)))))))
  
  totalsurvey$q100_06r<- ifelse(totalsurvey$q100b_01 + totalsurvey$q100b_02 + totalsurvey$q100b_03 + totalsurvey$q100b_04 + totalsurvey$q100b_05 + totalsurvey$q100b_06 == -59988, NA,
                                ifelse (totalsurvey$q100b_06 == -9998, 0 , 
                                        ifelse(totalsurvey$q100b_06 == 1, 0, 
                                               ifelse(totalsurvey$q100b_06 ==2, 1, 
                                                      ifelse(totalsurvey$q100b_06 ==3,  4, 
                                                             ifelse(totalsurvey$q100b_06 ==4, 12,
                                                                    ifelse(totalsurvey$q100b_06 ==5, 52, NA))))))) 
  
  
  #scale on a week (first run below recode of q67b !!)
  totalsurvey$insideWLG <- with(totalsurvey, q100_01r + q100_02r +q100_03r+ q100_04r+q100_05r+q100_06r)
  
  totalsurvey$insideWLGr <- with(totalsurvey, insideWLG/52)
  
  ## Franstalige Gemeenschap
  #scale on a year
  totalsurvey$q101_01r<- ifelse(totalsurvey$q101b_01 + totalsurvey$q101b_02 + totalsurvey$q101b_03 + totalsurvey$q101b_04 + totalsurvey$q101b_05 + totalsurvey$q101b_06 == -59988, NA,
                                ifelse (totalsurvey$q101b_01 == -9998, 0 , 
                                        ifelse(totalsurvey$q101b_01 == 1, 0, 
                                               ifelse(totalsurvey$q101b_01 ==2, 1, 
                                                      ifelse(totalsurvey$q101b_01 ==3,  4, 
                                                             ifelse(totalsurvey$q101b_01 ==4, 12,
                                                                    ifelse(totalsurvey$q101b_01 ==5, 52, NA))))))) 
  
  totalsurvey$q101_02r<- ifelse(totalsurvey$q101b_01 + totalsurvey$q101b_02 + totalsurvey$q101b_03 + totalsurvey$q101b_04 + totalsurvey$q101b_05 + totalsurvey$q101b_06 == -59988, NA,
                                ifelse (totalsurvey$q101b_02 == -9998, 0 , 
                                        ifelse(totalsurvey$q101b_02 == 1, 0, 
                                               ifelse(totalsurvey$q101b_02 ==2, 1, 
                                                      ifelse(totalsurvey$q101b_02 ==3,  4, 
                                                             ifelse(totalsurvey$q101b_02 ==4, 12,
                                                                    ifelse(totalsurvey$q101b_02 ==5, 52, NA)))))))
  
  totalsurvey$q101_03r<- ifelse(totalsurvey$q101b_01 + totalsurvey$q101b_02 + totalsurvey$q101b_03 + totalsurvey$q101b_04 + totalsurvey$q101b_05 + totalsurvey$q101b_06 == -59988, NA,
                                ifelse (totalsurvey$q101b_03 == -9998, 0 , 
                                        ifelse(totalsurvey$q101b_03 == 1, 0, 
                                               ifelse(totalsurvey$q101b_03 ==2, 1, 
                                                      ifelse(totalsurvey$q101b_03 ==3,  4, 
                                                             ifelse(totalsurvey$q101b_03 ==4, 12,
                                                                    ifelse(totalsurvey$q101b_03 ==5, 52, NA)))))))
  
  totalsurvey$q101_04r<- ifelse(totalsurvey$q101b_01 + totalsurvey$q101b_02 + totalsurvey$q101b_03 + totalsurvey$q101b_04 + totalsurvey$q101b_05 + totalsurvey$q101b_06 == -59988, NA,
                                ifelse (totalsurvey$q101b_04 == -9998, 0 , 
                                        ifelse(totalsurvey$q101b_04 == 1, 0, 
                                               ifelse(totalsurvey$q101b_04 ==2, 1, 
                                                      ifelse(totalsurvey$q101b_04 ==3,  4, 
                                                             ifelse(totalsurvey$q101b_04 ==4, 12,
                                                                    ifelse(totalsurvey$q101b_04 ==5, 52, NA)))))))
  
  totalsurvey$q101_05r<- ifelse(totalsurvey$q101b_01 + totalsurvey$q101b_02 + totalsurvey$q101b_03 + totalsurvey$q101b_04 + totalsurvey$q101b_05 + totalsurvey$q101b_06 == -59988, NA,
                                ifelse (totalsurvey$q101b_05 == -9998, 0 , 
                                        ifelse(totalsurvey$q101b_05 == 1, 0, 
                                               ifelse(totalsurvey$q101b_05 ==2, 1, 
                                                      ifelse(totalsurvey$q101b_05 ==3,  4, 
                                                             ifelse(totalsurvey$q101b_05 ==4, 12,
                                                                    ifelse(totalsurvey$q101b_05 ==5, 52, NA)))))))
  
  totalsurvey$q101_06r<- ifelse(totalsurvey$q101b_01 + totalsurvey$q101b_02 + totalsurvey$q101b_03 + totalsurvey$q101b_04 + totalsurvey$q101b_05 + totalsurvey$q101b_06 == -59988, NA,
                                ifelse (totalsurvey$q101b_06 == -9998, 0 , 
                                        ifelse(totalsurvey$q101b_06 == 1, 0, 
                                               ifelse(totalsurvey$q101b_06 ==2, 1, 
                                                      ifelse(totalsurvey$q101b_06 ==3,  4, 
                                                             ifelse(totalsurvey$q101b_06 ==4, 12,
                                                                    ifelse(totalsurvey$q101b_06 ==5, 52, NA)))))))
  
  
  #scale on a week (first run below recode of q67b!!)
  totalsurvey$insideFG <- with(totalsurvey, q101_01r + q101_02r +q101_03r+ q101_04r+q101_05r+q101_06r)
  
  totalsurvey$insideFGr <- with(totalsurvey, insideFG/52)
  
  ## Vlaanderen
  # scale on a year
  totalsurvey$q103_01r<- ifelse(totalsurvey$q103b_01 + totalsurvey$q103b_02 + totalsurvey$q103b_03 + totalsurvey$q103b_04 + totalsurvey$q103b_05 + totalsurvey$q103b_06 == -59988, NA,
                                ifelse (totalsurvey$q103b_01 == -9998, 0 , 
                                        ifelse(totalsurvey$q103b_01 == 1, 0, 
                                               ifelse(totalsurvey$q103b_01 ==2, 1, 
                                                      ifelse(totalsurvey$q103b_01 ==3,  4, 
                                                             ifelse(totalsurvey$q103b_01 ==4, 12,
                                                                    ifelse(totalsurvey$q103b_01 ==5, 52, NA))))))) 
  
  totalsurvey$q103_02r<- ifelse(totalsurvey$q103b_01 + totalsurvey$q103b_02 + totalsurvey$q103b_03 + totalsurvey$q103b_04 + totalsurvey$q103b_05 + totalsurvey$q103b_06 == -59988, NA,
                                ifelse (totalsurvey$q103b_02 == -9998, 0 , 
                                        ifelse(totalsurvey$q103b_02 == 1, 0, 
                                               ifelse(totalsurvey$q103b_02 ==2, 1, 
                                                      ifelse(totalsurvey$q103b_02 ==3,  4, 
                                                             ifelse(totalsurvey$q103b_02 ==4, 12,
                                                                    ifelse(totalsurvey$q103b_02 ==5, 52, NA)))))))
  
  totalsurvey$q103_03r<- ifelse(totalsurvey$q103b_01 + totalsurvey$q103b_02 + totalsurvey$q103b_03 + totalsurvey$q103b_04 + totalsurvey$q103b_05 + totalsurvey$q103b_06 == -59988, NA,
                                ifelse (totalsurvey$q103b_03 == -9998, 0 , 
                                        ifelse(totalsurvey$q103b_03 == 1, 0, 
                                               ifelse(totalsurvey$q103b_03 ==2, 1, 
                                                      ifelse(totalsurvey$q103b_03 ==3,  4, 
                                                             ifelse(totalsurvey$q103b_03 ==4, 12,
                                                                    ifelse(totalsurvey$q103b_03 ==5, 52, NA)))))))
  
  totalsurvey$q103_04r<- ifelse(totalsurvey$q103b_01 + totalsurvey$q103b_02 + totalsurvey$q103b_03 + totalsurvey$q103b_04 + totalsurvey$q103b_05 + totalsurvey$q103b_06 == -59988, NA,
                                ifelse (totalsurvey$q103b_04 == -9998, 0 , 
                                        ifelse(totalsurvey$q103b_04 == 1, 0, 
                                               ifelse(totalsurvey$q103b_04 ==2, 1, 
                                                      ifelse(totalsurvey$q103b_04 ==3,  4, 
                                                             ifelse(totalsurvey$q103b_04 ==4, 12,
                                                                    ifelse(totalsurvey$q103b_04 ==5, 52, NA)))))))
  
  totalsurvey$q103_05r<- ifelse(totalsurvey$q103b_01 + totalsurvey$q103b_02 + totalsurvey$q103b_03 + totalsurvey$q103b_04 + totalsurvey$q103b_05 + totalsurvey$q103b_06 == -59988, NA,
                                ifelse (totalsurvey$q103b_05 == -9998, 0 , 
                                        ifelse(totalsurvey$q103b_05 == 1, 0, 
                                               ifelse(totalsurvey$q103b_05 ==2, 1, 
                                                      ifelse(totalsurvey$q103b_05 ==3,  4, 
                                                             ifelse(totalsurvey$q103b_05 ==4, 12,
                                                                    ifelse(totalsurvey$q103b_05 ==5, 52, NA)))))))
  
  totalsurvey$q103_06r<- ifelse(totalsurvey$q103b_01 + totalsurvey$q103b_02 + totalsurvey$q103b_03 + totalsurvey$q103b_04 + totalsurvey$q103b_05 + totalsurvey$q103b_06 == -59988, NA,
                                ifelse (totalsurvey$q103b_06 == -9998, 0 , 
                                        ifelse(totalsurvey$q103b_06 == 1, 0, 
                                               ifelse(totalsurvey$q103b_06 ==2, 1, 
                                                      ifelse(totalsurvey$q103b_06 ==3,  4, 
                                                             ifelse(totalsurvey$q103b_06 ==4, 12,
                                                                    ifelse(totalsurvey$q103b_06 ==5, 52, NA)))))))
  
  
  #scale on a week (first run below recode of q67b!)
  totalsurvey$insideVL <- with(totalsurvey, q103_01r + q103_02r +q103_03r+ q103_04r+q103_05r+q103_06r)
  
  totalsurvey$insideVLr <- with(totalsurvey, insideVL/52)
  
  ## Federaal 
  #scale on a year
  totalsurvey$q33_01r<- ifelse(totalsurvey$q33_01 + totalsurvey$q33_02 + totalsurvey$q33_03 + totalsurvey$q33_04 + totalsurvey$q33_05 + totalsurvey$q33_06 + totalsurvey$q33b_07 + totalsurvey$q33b_08 == -79984, NA,
                               ifelse (totalsurvey$q33_01 == -9998, 0 , 
                                       ifelse(totalsurvey$q33_01 == 1, 0, 
                                              ifelse(totalsurvey$q33_01 ==2, 1, 
                                                     ifelse(totalsurvey$q33_01 ==3,  4, 
                                                            ifelse(totalsurvey$q33_01 ==4, 12,
                                                                   ifelse(totalsurvey$q33_01 ==5, 52, NA))))))) 
  
  totalsurvey$q33_02r<- ifelse(totalsurvey$q33_01 + totalsurvey$q33_02 + totalsurvey$q33_03 + totalsurvey$q33_04 + totalsurvey$q33_05 + totalsurvey$q33_06 + totalsurvey$q33b_07 + totalsurvey$q33b_08 == -79984, NA,
                               ifelse (totalsurvey$q33_02 == -9998, 0 , 
                                       ifelse(totalsurvey$q33_02 == 1, 0, 
                                              ifelse(totalsurvey$q33_02 ==2, 1, 
                                                     ifelse(totalsurvey$q33_02 ==3,  4, 
                                                            ifelse(totalsurvey$q33_02 ==4, 12,
                                                                   ifelse(totalsurvey$q33_02 ==5, 52, NA))))))) 
  
  totalsurvey$q33_03r<- ifelse(totalsurvey$q33_01 + totalsurvey$q33_02 + totalsurvey$q33_03 + totalsurvey$q33_04 + totalsurvey$q33_05 + totalsurvey$q33_06 + totalsurvey$q33b_07 + totalsurvey$q33b_08 == -79984, NA,
                               ifelse (totalsurvey$q33_03 == -9998, 0 , 
                                       ifelse(totalsurvey$q33_03 == 1, 0, 
                                              ifelse(totalsurvey$q33_03 ==2, 1, 
                                                     ifelse(totalsurvey$q33_03 ==3,  4, 
                                                            ifelse(totalsurvey$q33_03 ==4, 12,
                                                                   ifelse(totalsurvey$q33_03 ==5, 52, NA)))))))
  
  totalsurvey$q33_04r<- ifelse(totalsurvey$q33_01 + totalsurvey$q33_02 + totalsurvey$q33_03 + totalsurvey$q33_04 + totalsurvey$q33_05 + totalsurvey$q33_06 + totalsurvey$q33b_07 + totalsurvey$q33b_08 == -79984, NA,
                               ifelse (totalsurvey$q33_04 == -9998, 0 , 
                                       ifelse(totalsurvey$q33_04 == 1, 0, 
                                              ifelse(totalsurvey$q33_04 ==2, 1, 
                                                     ifelse(totalsurvey$q33_04 ==3,  4, 
                                                            ifelse(totalsurvey$q33_04 ==4, 12,
                                                                   ifelse(totalsurvey$q33_04 ==5, 52, NA))))))) 
  
  totalsurvey$q33_05r<- ifelse(totalsurvey$q33_01 + totalsurvey$q33_02 + totalsurvey$q33_03 + totalsurvey$q33_04 + totalsurvey$q33_05 + totalsurvey$q33_06 + totalsurvey$q33b_07 + totalsurvey$q33b_08 == -79984, NA,
                               ifelse (totalsurvey$q33_05 == -9998, 0 , 
                                       ifelse(totalsurvey$q33_05 == 1, 0, 
                                              ifelse(totalsurvey$q33_05 ==2, 1, 
                                                     ifelse(totalsurvey$q33_05 ==3,  4, 
                                                            ifelse(totalsurvey$q33_05 ==4, 12,
                                                                   ifelse(totalsurvey$q33_05 ==5, 52, NA))))))) 
  
  totalsurvey$q33_06r<- ifelse(totalsurvey$q33_01 + totalsurvey$q33_02 + totalsurvey$q33_03 + totalsurvey$q33_04 + totalsurvey$q33_05 + totalsurvey$q33_06 + totalsurvey$q33b_07 + totalsurvey$q33b_08 == -79984, NA,
                               ifelse (totalsurvey$q33_06 == -9998, 0 , 
                                       ifelse(totalsurvey$q33_06 == 1, 0, 
                                              ifelse(totalsurvey$q33_06 ==2, 1, 
                                                     ifelse(totalsurvey$q33_06 ==3,  4, 
                                                            ifelse(totalsurvey$q33_06 ==4, 12,
                                                                   ifelse(totalsurvey$q33_06 ==5, 52, NA))))))) 
  
  totalsurvey$q33_07r<- ifelse(totalsurvey$q33_01 + totalsurvey$q33_02 + totalsurvey$q33_03 + totalsurvey$q33_04 + totalsurvey$q33_05 + totalsurvey$q33_06 + totalsurvey$q33b_07 + totalsurvey$q33b_08 == -79984, NA,
                               ifelse (totalsurvey$q33b_07 == -9998, 0 , 
                                       ifelse(totalsurvey$q33b_07 == 1, 0, 
                                              ifelse(totalsurvey$q33b_07 ==2, 1, 
                                                     ifelse(totalsurvey$q33b_07 ==3,  4, 
                                                            ifelse(totalsurvey$q33b_07 ==4, 12,
                                                                   ifelse(totalsurvey$q33b_07 ==5, 52, NA))))))) 
  
  totalsurvey$q33_08r<- ifelse(totalsurvey$q33_01 + totalsurvey$q33_02 + totalsurvey$q33_03 + totalsurvey$q33_04 + totalsurvey$q33_05 + totalsurvey$q33_06 + totalsurvey$q33b_07 + totalsurvey$q33b_08 == -79984, NA,
                               ifelse (totalsurvey$q33b_08 == -9998, 0 , 
                                       ifelse(totalsurvey$q33b_08 == 1, 0, 
                                              ifelse(totalsurvey$q33b_08 ==2, 1, 
                                                     ifelse(totalsurvey$q33b_08 ==3,  4, 
                                                            ifelse(totalsurvey$q33b_08 ==4, 12,
                                                                   ifelse(totalsurvey$q33b_08 ==5, 52, NA))))))) 
  
  #scale on a week
  totalsurvey$insideFED <- with(totalsurvey, q33_01r + q33_02r +q33_03r+ q33_04r+q33_05r+q33_06r + q33_07r + q33_08r)
  
  totalsurvey$insideFEDr <- with(totalsurvey, insideFED/52) 
  
  
  # do different levels scale with each other?
  data <- totalsurvey [, c( "insideWLGr", "insideFGr", "insideFEDr", "insideVLr")]
  psych::alpha(data) #regular chronbach's Alpha 
  
  #total number of governments contacted 
  totalsurvey <- within (totalsurvey,{
    q30_03r <- Recode(q30_03, '0 = 0; -9998 = 0; else = 1'
    )}) 
  totalsurvey <- within (totalsurvey,{
    q67b_01r <- Recode( q67b_01, ' -9998 = 0'
    )}) 
  totalsurvey <- within (totalsurvey,{
    q67b_02r <- Recode( q67b_02, ' -9998 = 0'
    )}) 
  totalsurvey <- within (totalsurvey,{
    q67b_04r <- Recode( q67b_04, ' -9998 = 0'
    )}) 
  
  totalsurvey$numbergov <- with(totalsurvey, q30_03r + q67b_01r + q67b_02r+ q67b_04r)
  
  # NA reset to zero, otherwise summing not possible without creating a lot of NAs! 
  totalsurvey <- within (totalsurvey,{
    insideFEDrr <- Recode( insideFEDr, 'NA = 0'
    )}) 
  
  
  totalsurvey <- within (totalsurvey,{
    insideVLrr <- Recode( insideVLr, 'NA = 0'
    )}) 
  
  
  totalsurvey <- within (totalsurvey,{
    insideFGrr <- Recode( insideFGr, 'NA = 0'
    )}) 
  
  
  totalsurvey <- within (totalsurvey,{
    insideWLGrr <- Recode( insideWLGr, 'NA = 0'
    )}) 
  
  totalsurvey$inside <- with(totalsurvey, insideFEDrr+ insideVLrr+ insideWLGrr+ insideFGrr)
  
  #average advocacy energy per week per policy area
  totalsurvey$insider <- with(totalsurvey, inside/numbergov)
  totalsurvey$insiderav <- with(totalsurvey, insider/numberpolr)
}

#INSIDE + OUTSIDE
totalsurvey$energy <- with(totalsurvey, (insider + outsider)/numberpolr)

#INSIDE -> QID40
{#scale on a year
  totalsurvey$q40_01r<- ifelse(totalsurvey$q40_01 + totalsurvey$q40_02  == -19996, NA,
                               ifelse (totalsurvey$q40_01 == -9998, 0 , 
                                       ifelse(totalsurvey$q40_01 == 1, 0, 
                                              ifelse(totalsurvey$q40_01 ==2, 1, 
                                                     ifelse(totalsurvey$q40_01 ==3,  4, 
                                                            ifelse(totalsurvey$q40_01 ==4, 12,
                                                                   ifelse(totalsurvey$q40_01 ==5, 52, NA))))))) 
  #scale on a week
  totalsurvey$inside40 <- with(totalsurvey, q40_01r/52)
}

#INSIDE + OUTSIDE
totalsurvey$energy <- with(totalsurvey, (inside40 + outsider)/numberpolr)

#assigning values of advocacy energy for each organisation per policy domain
{totalsurvey$q16_01rr <- ifelse(totalsurvey$q16_01r == 1, totalsurvey$insiderav ,
                                ifelse(totalsurvey$q16_01r == 0, 0, NA))
  
  totalsurvey$q16_02rr <- ifelse(totalsurvey$q16_02r == 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_02r == 0, 0, NA))
  
  totalsurvey$q16_03rr <- ifelse(totalsurvey$q16_03r == 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_03r == 0, 0, NA))
  
  totalsurvey$q16_04rr <- ifelse(totalsurvey$q16_04r == 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_04r == 0, 0, NA))
  
  totalsurvey$q16_05rr <- ifelse(totalsurvey$q16_05r == 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_05r == 0, 0, NA))
  
  totalsurvey$q16_06rr <- ifelse(totalsurvey$q16_06r == 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_06r == 0, 0, NA))
  
  totalsurvey$q16_07rr <- ifelse(totalsurvey$q16_07r == 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_07r == 0, 0, NA))
  
  totalsurvey$q16_08rr <- ifelse(totalsurvey$q16_08r == 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_08r == 0, 0, NA))
  
  totalsurvey$q16_09rr <- ifelse(totalsurvey$q16_09r == 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_09r == 0, 0, NA))
  
  totalsurvey$q16_10rr <- ifelse(totalsurvey$q16_10r == 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_10r == 0, 0, NA))
  
  totalsurvey$q16_11rr <- ifelse(totalsurvey$q16_11r == 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_11r == 0, 0, NA))
  
  totalsurvey$q16_12rr <- ifelse(totalsurvey$q16_12r == 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_12r == 0, 0, NA))
  
  totalsurvey$q16_13rr <- ifelse(totalsurvey$q16_13r == 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_13r == 0, 0, NA))
  
  totalsurvey$q16_14rr <- ifelse(totalsurvey$q16_14r == 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_14r == 0, 0, NA))
  
  totalsurvey$q16_15rr <- ifelse(totalsurvey$q16_15r== 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_15r == 0, 0, NA))
  
  totalsurvey$q16_16rr <- ifelse(totalsurvey$q16_16r== 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_16r == 0, 0, NA))
  
  totalsurvey$q16_17rr <- ifelse(totalsurvey$q16_17r== 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_17r == 0, 0, NA))
  
  totalsurvey$q16_18rr <- ifelse(totalsurvey$q16_18r== 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_18r == 0, 0, NA))
  
  totalsurvey$q16_19rr <- ifelse(totalsurvey$q16_19r== 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_19r == 0, 0, NA))
  
  totalsurvey$q16_20rr <- ifelse(totalsurvey$q16_20r== 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_20r == 0, 0, NA))
  
  totalsurvey$q16_21rr <- ifelse(totalsurvey$q16_21r== 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_21r == 0, 0, NA))
  
  totalsurvey$q16_22rr <- ifelse(totalsurvey$q16_22r== 1, totalsurvey$insiderav ,
                                 ifelse(totalsurvey$q16_22r == 0, 0, NA))
}

energy1 <- sum(na.omit(totalsurvey$q16_01rr))
energy2 <- sum(na.omit(totalsurvey$q16_02rr))
energy3 <- sum(na.omit(totalsurvey$q16_03rr))
energy4 <- sum(na.omit(totalsurvey$q16_04rr))
energy5 <- sum(na.omit(totalsurvey$q16_05rr))
energy6 <- sum(na.omit(totalsurvey$q16_06rr))
energy7 <- sum(na.omit(totalsurvey$q16_07rr))
energy8 <- sum(na.omit(totalsurvey$q16_08rr))
energy9 <- sum(na.omit(totalsurvey$q16_09rr))
energy10 <- sum(na.omit(totalsurvey$q16_10rr))
energy11 <- sum(na.omit(totalsurvey$q16_11rr))
energy12 <- sum(na.omit(totalsurvey$q16_12rr))
energy13 <- sum(na.omit(totalsurvey$q16_13rr))
energy14 <- sum(na.omit(totalsurvey$q16_14rr))
energy15 <- sum(na.omit(totalsurvey$q16_15rr))
energy16 <- sum(na.omit(totalsurvey$q16_16rr))
energy17 <- sum(na.omit(totalsurvey$q16_17rr))
energy18 <- sum(na.omit(totalsurvey$q16_18rr))
energy19 <- sum(na.omit(totalsurvey$q16_19rr))
energy20 <- sum(na.omit(totalsurvey$q16_20rr))
energy21 <- sum(na.omit(totalsurvey$q16_21rr))
energy22 <- sum(na.omit(totalsurvey$q16_22rr))

energy <- as.data.frame(rbind(energy1, energy2, energy3, energy4, energy5, energy6, energy7, energy8, energy9, energy10, energy11,
                              energy12, energy13, energy14, energy15, energy16, energy17,energy18,energy19, energy20, energy21, energy22))

energy$polarea <- plyr::revalue(rownames(energy), c(
  "energy1" = "Migration policy",
  "energy2" = "Economic and monetary policy",
  "energy3" = "Health care",
  "energy4" = "Crime",
  "energy5" = "Energy policy",
  "energy6" = "Education",
  "energy7" = "Gender",
  "energy8" = "Social policy",
  "energy9" = "Environmental policy",
  "energy10" = "Consumer protection",
  "energy11" = "Agriculture policy",
  "energy12" = "Rights",
  "energy13" = "Foreign affairs",
  "energy14" = "Foreign affairs",
  "energy15" = "Defense",
  "energy16" = "European integration & cooperation",
  "energy17" = "Scientific research policy",
  "energy18" = "Regional EU policy",
  "energy19" = "Human rights",
  "energy20" = "Transport & mobility",
  "energy21" = "Cultural policy",
  "energy22" = "Employment")
)


boxplot(numberpolr~type3, data=totalsurvey, col=(c("lightblue")))


##Advocacy tactics
#Outside tactics
totalsurvey <- within (totalsurvey,{
  q34_01r <- Recode(q34_01, ' -9998 = NA; -9999 = NA'
  )}) 
totalsurvey <- within (totalsurvey,{
  q34_02r <- Recode(q34_02, ' -9998 = NA; -9999 = NA'
  )}) 
totalsurvey <- within (totalsurvey,{
  q34_03r <- Recode(q34_03, ' -9998 = NA; -9999 = NA'
  )}) 
totalsurvey <- within (totalsurvey,{
  q34_05r <- Recode(q34_05, ' -9998 = NA; -9999 = NA'
  )}) 
totalsurvey <- within (totalsurvey,{
  q34_06r <- Recode(q34_06, ' -9998 = NA; -9999 = NA'
  )}) 
totalsurvey <- within (totalsurvey,{
  q34_07r <- Recode(q34_07, ' -9998 = NA; -9999 = NA'
  )}) 
totalsurvey <- within (totalsurvey,{
  q34_08r <- Recode(q34_08, ' -9998 = NA; -9999 = NA'
  )}) 
totalsurvey <- within (totalsurvey,{
  q34_10r <- Recode(q34_10, ' -9998 = NA; -9999 = NA'
  )}) 

totalsurvey <- within (totalsurvey,{
  directcontacts <- Recode(insider, ' 0 = 1; 0.001:0.082=2; 0.083:0.249=3; 0.25:0.99=4; 1:8=5'
  )}) 
#0/52 = geen contact
#1/52 = 1x per jaar
#4.3/52 = 1x per 3 maanden
#13/52 = 1x per maand
#52/52 = 1x per week


cormax <- c("q34_01r", "q34_02r", "q34_03r", "q34_05r", "q34_06r", "q34_07r", "q34_08r", "q34_10r", "directcontacts")


sub <- totalsurvey[,c(cormax)]
sub <- as.matrix(sub)
sub <- na.omit(sub)

tacticscor <- rcorr(sub, type=c("spearman"))
#recode in 0/1 use of tactic


plot1 <- boxplot(directcontacts~type3, data=totalsurvey, col=(c("lightblue")))
plot2 <- boxplot(q34_03r~type3, data=totalsurvey, col=(c("lightblue")))
plot3 <- boxplot(q34_07r~type3, data=totalsurvey, col=(c("lightblue")))


#overall inside vs outside
tactics <- as.data.frame(cbind(totalsurvey$insiderav, totalsurvey$outenergy))
tactics1 <- na.omit(tactics)

names(tactics1)[c(1)] <- c("Inside")
names(tactics1)[c(2)] <- c("Outside")
head(tactics1)

tactics1$Outside1 <- (tactics1$Outside - mean(tactics1$Outside))/sd(tactics1$Outside)
tactics1$Inside1 <- (tactics1$Inside - mean(tactics1$Inside))/sd(tactics1$Inside)

tactics1 <- subset(tactics1, Outside1 < "3" & Inside1 < "3", select=Inside:Inside1)
tactics1 <- subset(tactics1, Outside < "1" & Inside1 < "1", select=Inside:Inside1)

boxplot(tactics1$Inside, tactics1$Outside)

ggplot(data = tactics1, mapping = aes(x = Outside, y = Inside, xlim =c('-0,1'), ylim = c('-0,0.7') )) +
  geom_point() +
  scale_x_continuous(limits = c(-0,1))+
  scale_y_continuous(limits = c(-0,0.7)) +
  geom_hline(aes(yintercept=0))+
  geom_vline(aes(xintercept=0))+
  theme_bw() 

rcorr(tactics1$Inside, tactics1$Outside)

##Access to AC
totalsurvey <- within (totalsurvey,{
  access <- Recode(X..AC, '0 = NA'
  )}) 
boxplot(access~type3, data=totalsurvey, col= "lightblue")
access1 <- sort(totalsurvey$access)
access1 <- as.data.frame(as.numeric(access1))
head(access1)
names(access1)[c(1)] <- c("access")

library(easyGgplot2)
ggplot2.barplot(data=access1, xName="access", backgroundColor="white", 
                removePanelGrid=TRUE,removePanelBorder=TRUE,
                axisLine=c(0.5, "solid", "black"))

totalsurvey$seats <- totalsurvey$X..E + totalsurvey$X..A + totalsurvey$X..O                       
table(totalsurvey$seats) 


totalsurvey <- within (totalsurvey,{
  seats1 <- Recode(seats, '0 = NA'
  )}) 
describeBy(totalsurvey$seats1, totalsurvey$type3)

##media access
media <- as.data.frame(table(accessmedia$issues_articles_stakeholders_stakeholder_id))
names(media)[c(1)] <- c("ID_ac")
names(media)[c(2)] <- c("accessmedia")
head(media)
totalsurvey2 <- merge(media, totalsurvey, by="ID_ac", all.y = TRUE) 

describeBy(totalsurvey2$accessmedia, totalsurvey2$type3)

totalsurvey2 <- within (totalsurvey2,{
  accessmedia2 <- Recode(accessmedia, 'NA = 0; 35:53 = NA'
  )}) 
totalsurvey2 <- within (totalsurvey2,{
  seats2 <- Recode(seats, 'NA = 0;50:422 = NA'
  )}) 


rcorr(totalsurvey2$accessmedia2, totalsurvey2$seats2, type=c("spearman"))


ggplot(data = totalsurvey2, mapping = aes(x = accessmedia2, y = seats2), 
       xlim =c('0,20'), ylim = c('0,50')) +
  geom_point() +
  xlab("Number of media appearances") +
  ylab("Number of seats in AC") +
  scale_x_continuous(limits = c(0,20))+
  scale_y_continuous(limits = c(0,50)) +
  geom_hline(aes(yintercept=0))+
  geom_vline(aes(xintercept=0))+
  geom_abline()+
  theme_bw() 

##Multi-level players
#member of EU/int. associations
totalsurvey <- within (totalsurvey,{
  q58r <- Recode(q58, ' -9998 = NA'
  )}) 
describeBy(totalsurvey$q58r)
table(totalsurvey$q58r)
table(totalsurvey$q58)
totalsurvey <- within (totalsurvey,{
  q59r <- Recode(q59, ' -9998 = NA; -9999=NA'
  )}) 
describeBy(totalsurvey$q59r)

#multi-level advocacy 
totalsurvey <- within (totalsurvey,{
  local <- Recode(q30_01, ' -9998 = NA'
  )}) 
totalsurvey <- within (totalsurvey,{
  subnat <- Recode(q30_02, ' -9998 = NA'
  )}) 
totalsurvey <- within (totalsurvey,{
  federal <- Recode(q30_03, ' -9998 = NA'
  )}) 
totalsurvey <- within (totalsurvey,{
  european <- Recode(q30_04, ' -9998 = NA'
  )}) 
totalsurvey <- within (totalsurvey,{
  internat <- Recode(q30_05, ' -9998 = NA'
  )}) 

cormax <- c("local", "subnat", "federal", "european", "internat")
sub <- totalsurvey[,c(cormax)]
sub <- as.matrix(sub)
multilevel <- rcorr(sub, type=c("pearson"))
