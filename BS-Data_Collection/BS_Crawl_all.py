from bs4 import BeautifulSoup
import urllib
import pandas as pd
import os
import csv
with open('pics_counts.csv', 'a') as csvfile:
    fieldnames = ['id', 'leading','leading_url','content','content_url','gallery','gallery_url']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()
def main():
    try:
        os.makedirs('picture_all_lc')
        os.makedirs('picture_g')
    except OSError, msg:
        pass
    df = pd.read_csv('OnlineNewsPopularity_40.csv')
    pic_count=0
    no_count=0
    for index,row in df.iterrows():
        id=row[0]
        url=row[1]
        print('ID: %d'% id)
        try:
            get_pic(url,id)
            pic_count+=1
            print 'total websites:%d'% pic_count
        except:
            no_count+=1
            print 'no picture found,no_pic sum:%d'% no_count
            pass

def get_pic(url,id):
    ##leading picture##
    l_pic_count=0
    l_ls=[]
    try:
        page = urllib.urlopen(url)
        soup = BeautifulSoup(page.read(),'html.parser')
        lp = soup.find_all('figure',"article-image")
        for i in lp:
            lead_pic = i.find('img')['src']
            print 'lead_pic:',lead_pic
            l_pic_count+=1
            l_ls.append(lead_pic)
            urllib.urlretrieve(lead_pic, 'picture_all_lc/'+str(id)+'.jpg')
    except:
        pass
    ##content pictures##
    c_pic_count=0
    c_ls=[]
    try:
        c_pic = soup.find_all('figure',class_='image')
        for i in c_pic:
            cpic=i.find('img')['src']
            print 'content_pic:', cpic
            c_pic_count+=1
            c_ls.append(cpic)
            urllib.urlretrieve(cpic, 'picture_all_lc/'+str(id)+'('+str(c_pic_count)+')'+'.jpg')
    except:
        pass
    ##gallery pictures##
    g_pic_count=0
    g_ls=[]
    try:
        g_pic = soup.find('section',class_='gallery')
        f = g_pic.findAll('figure')
        for i in f:
            gpic = i.find('img')['src']
            print 'gallery_pic:', gpic
            g_pic_count+=1
            g_ls.append(gpic)
            urllib.urlretrieve(gpic, 'picture_g/'+str(id)+'('+str(g_pic_count)+')'+'.jpg')
    except:
        pass
    with open('pics_counts.csv', 'a') as csvfile:
        fieldnames = ['id', 'leading','leading_url','content','content_url','gallery','gallery_url']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writerow({'id':id,'leading':l_pic_count,'leading_url':l_ls,'content':c_pic_count,
        'content_url':c_ls,'gallery':g_pic_count,'gallery_url':g_ls})
main()
