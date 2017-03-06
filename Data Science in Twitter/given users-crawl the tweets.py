# import required package
from TwitterAPI import TwitterAPI

### customized part: parameters for 'search/tweets' API

# set the search term to 'Halloween'

# set the number of returned tweet to 100
COUNT = 100

### customized part end
### other available optional parameter please refer to the link below:
### https://dev.twitter.com/rest/reference/get/search/tweets


# Enter the four API keys: 
CONSUMER_KEY = '#################' 
CONSUMER_SECRET = '######################'
ACCESS_TOKEN_KEY = '###########################'
ACCESS_TOKEN_SECRET = '############################'

# attach the keys and tokens
api = TwitterAPI(
    CONSUMER_KEY,
    CONSUMER_SECRET,
    ACCESS_TOKEN_KEY,
    ACCESS_TOKEN_SECRET)

# make request using 'search/tweets' api, parameters are attached in a dictionary, such as 'q' and 'count'
#r = api.request('statuses/filter', {'locations':'-74,40,-73,41'})
r = api.request('statuses/user_timeline', {'user_id': 249475308, 'count': COUNT})

# set the initial count to 0
c = 0

# open an empty file for output
with open ('purple group.txt','a') as csvfile1:

  # iterate through response r
  for item in r.get_iterator():

    try:
      # index the output
      c += 1
      print c

      # parse data to filter text, favorite_count, and id
      if 'text' in item:
        text= item['text']
      if 'user' in item:
        name = item['user']['screen_name']
      if 'retweet_count' in item:
        count = item['retweet_count']
      if 'favourites_count' in item:
        Fcount = item['favorite_count']

      # write parsed data into a .txt file names Twitter_data.txt     
      csvfile1.write(str(item['user']['screen_name'])+'\t'+str(item['text'])+'\t'+str(item['retweet_count'])+'\t'+str(item['favorite_count'])+'\n')
    except UnicodeEncodeError:
      pass
