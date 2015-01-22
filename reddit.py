#!/usr/bin/python

import requests
import json

with open("/home/adam/dotfiles/redditpass.txt","r") as infile:
    data = infile.readlines()
    user = data[0]
    passwd = data[1]

url="http://www.reddit.com/api/login"
url2="http://www.reddit.com/message/unread.json"
data= {"api_type":"json",
       "passwd":passwd[:-1],
       "user":user[:-1],
       "rem":"true"
    }
r1 = requests.post(url, data=data, headers={"User-Agent":"adams-compulsive-message-checker"})
results = json.loads(requests.get(url2, cookies=r1.cookies, data=data, headers={"User-Agent":"adams-compulsive-message-checker"}).text)
count=len(results['data']['children'])
if count > 0:
    line  = "<fc=#ff3f37>"
    line += "<icon=/home/adam/dotfiles/mail.xbm/> "
    line += str(count)
    line += "</fc>"
else:
    line  = "<icon=/home/adam/dotfiles/mail.xbm/> "
print(line)
