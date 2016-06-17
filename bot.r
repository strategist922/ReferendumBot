library(httr)
library(jsonlite)
library(devtools)
library(magrittr)
library(twitteR)

apikey <- "" #API Key
apisecret <- "" #API Secret
token <- "" #Access Token
tokensecret <- "" #Access token secret
setup_twitter_oauth(apikey, apisecret, token, tokensecret)



getLatestOdds=function(){
  #'http://sports.ladbrokes.com/sports-central/uk-eu-referendum/'
  #Page loads live data from:
  url="https://prod-web-lbnewsng.gp-cloud.com/ladbrokes-referendum/get-data.php"
  dat=GET(url)%>% content()
  or=fromJSON(dat)$odds[1] %>% fromJSON %>% extract2(1) %>% extract2(2)
  ol=fromJSON(dat)$odds[2] %>% fromJSON %>% extract2(1) %>% extract2(2)
  
  k = or %>% strsplit("/") %>% extract2(1) %>% as.numeric()
  l = ol %>% strsplit("/") %>% extract2(1) %>% as.numeric()
  
  #Maths from http://sports.ladbrokes.com/sports-central/uk-eu-referendum/scripts/main.js
  m = 1 + round(k[1] / k[2] * 100) / 100
  n = 1 + round(l[1] / l[2] * 100) / 100
  o = m + n
  p = 180 * m / o
  q = 90 - p
  
  percentLeave = round(100 * m / o) 
  percentRemain = round(100 - 100 * m / o) 
  return(percentRemain)
}

percentRemain = getLatestOdds()
print(paste("Latest ladbrokes odds for remain:",percentRemain,'%'))

#Check that referendum has not yet happened:
while(Sys.Date()<"2016-06-24"){
  #Get previous odds
  previousOdds=readLines('previousOdds.txt') %>% as.numeric()
  #Each minute check latest odds
  percentRemain = getLatestOdds()
  #Check if odds have changed
  #If they have:
  if (!percentRemain == previousOdds){
    #update the odds
    write(percentRemain,file('previousOdds.txt'))
    #Issue Tweet
    if (percentRemain > previousOdds){
      change='up'
    }else{
      change='down'
    }
    message=paste('New Ladbrooks odds just in. Percentage likelihood for #Remain: ',percentRemain,'%, ',change,' from ',previousOdds,'% #ReferendumBot https://github.com/fredheir/referendumBot',sep='')
    print(message)
    tweet(message) # make sure you can see a 
  }else{
    print ('odds unchanged')
  }
  #Sleep for a minute
  Sys.sleep(60)
}
  

