#d={'Doorda':'q','Samuel_Huang_':'w'}

import csv
uid=[]
scnm=[]
with open('user-keywords4.csv') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        #uid.append(row['uid'])
        scnm.append(row['Screen name'])
        #usedict={}
        #usedict=dict(zip(uid,scnm))
    #print usedict

import time
import tweepy

  

api = tweepy.API(auth)
ids = {}

for i in scnm:
    
    current_cursor = ""
    ids[i] = []
    for page in tweepy.Cursor(api.followers_ids, screen_name=i).pages():
        ids[i].extend(page)
        for a in ids[i]:
            with open ('USER_FOLLOWER_6.csv','a') as csvfile1:
                csvfile1.write(str(i)+","+str(a)+'\n')
            

    print len(ids[i])
        
      
    

#replace user screen_name to user id
    
#with open('try it hard10.csv') as csvfile:
 #   reader = csv.DictReader(csvfile)
  #  for row in reader:
   #     for i in d.keys():
    #        if i in row['screen_name']:
     #           with open ('hahaha.csv','a') as csvfile1:
      #              csvfile1.write(str(d[i])+','+row['follower_ID']+'\n')



