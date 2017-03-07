import os
import xlrd
import tmdbsimple as tmdb
import csv
import pandas as pd
import numpy as np
from pandas import DataFrame, Series

output_file = open("/Users/Singsing/Documents/IMDB_Project/8.3/8.3/m_revenue4.csv", 'w+')
writer = csv.writer(output_file)
tmdb.API_KEY = '0cb2a198b8467500826086f516e08d6b'
df = pd.read_csv('moive_list.csv')
ls = df['List']
ls_test = ls[:10]
count = 0
kls = []
output_file1 = open("/Users/Singsing/Documents/IMDB_Project/8.3/8.3/m_revenue_1.csv", 'w+')
writer1 = csv.writer(output_file1)
for m in ls:
    count+=1
    print count
    try:
        search = tmdb.Search()
        response = search.movie(query=m)
        #print response
        for s in search.results:
            idnum=(s['id'])
            movie = tmdb.Movies(idnum)
            response = movie.info()
            k = (m, response['revenue'], response['popularity'], response['vote_count'], response['vote_average'])
            kls.append(k)
            #print (k)
            writer.writerows(k)
    except Exception:
        print ('error')
    pass

df = DataFrame(kls, columns=['Moive','Revenue','popularity','vote_count','vote_average'])
df.to_csv('m_r(c).csv')


output_file.close()
output_file1.close()
