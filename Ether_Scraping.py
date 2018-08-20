%matplotlib inline

from bs4 import BeautifulSoup
import re
import requests
import pandas as pd
import time
from datetime import datetime, timedelta
import datetime
from dateutil.parser import parse
import seaborn as sns
import matplotlib.pyplot as plt
from scipy import stats
import numpy as np


# check if a string is a date
def is_date(string):
    try: 
        parse(string)
        return True
    except ValueError:
        return False

def EthereumDownloader(sleeper = 2):
    # Create soup to get initial information
    start_url = 'https://etherscan.io/contractsVerified/1'
    html      = requests.get(start_url)
    soup      = BeautifulSoup(html.text, "html5lib")
    
    # number of pages
    n_page    = int(str(soup.select('.col-sm-6 b')[1])[3:-4])
    
    # declare number of pages to be scraped and length-vector
    frame     = pd.DataFrame(columns=('Date', 'Hash', 'Tx', 'Balance'))
    l         = [0]
    
    # loop through pages and downlaod necessary information
    for i in range(1,n_page + 1):
        print("Page " + str(i) + " of " + str(n_page))
        iter_url = 'https://etherscan.io/contractsVerified/' + str(i)
        html     = requests.get(iter_url)
        soup     = BeautifulSoup(html.text, "html5lib")

        # select data
        data     = soup.select('.table-hover td')
        
        # hash
        add  = [re.findall('#code">.*?</a>', str(d))[0][7:-4] for d in data if re.findall('href="/address/', str(d)) != []]
        # date
        date = [re.findall('<td>.*?</td>', str(d))[0][4:-5] for d in data if len(re.findall('<td>.*?</td>', str(d))[0][4:-5]) >= 5 if is_date(re.findall('<td>.*?</td>', str(d))[0][4:-5])]
        # number of transactions
        tx   = [int(re.findall('<td>[0-9]+</td>', str(d))[0][4:-5]) for d in data if re.findall('<td>[0-9]+</td>', str(d)) != []]
        # contracts balance
        bal  = []
        k = 3
        while k <= len(data):
            if len(re.findall('[0-9]+<b>.</b>|[0-9]+ \w+', str(data[k]))) > 1:
                a= re.findall('[0-9]+<b>.</b>|[0-9]+ \w+', str(data[k]))[0][:-8] + '.' + re.findall('[0-9]+<b>.</b>|[0-9]+ \w+', str(data[k]))[1]
                bal.append(a)
            else:
                try:
                    bal.append(re.findall('[0-9]+<b>.</b>|[0-9]+ \w+', str(data[k]))[0])
                except:
                    bal.append('None')
            k += 7
        
        l.append(len(add))
        
        # write data into .csv-file for later usage
        with open('Ethereum.csv','a') as file:
            for j in range(sum(l[0:i]),sum(l[0:(i+1)])):
                row          = [date[j - sum(l[0:i])],add[j - sum(l[0:i])], tx[j - sum(l[0:i])], bal[j - sum(l[0:i])]]
                frame.loc[j] = row
                file.write(', '.join([str(element) for element in row]))
                file.write('\n')
            
        # sleeper for server
        time.sleep(sleeper)
    
    return frame

# Main

try:
    df = pd.read_csv("/Users/MarvinGauer/Ethereum.csv")
    q  = input("An Ethereum.csv-file already exists. Do you want to update it?(y/n) ")
    if q == 'y':
        df = EthereumDownloader(sleeper = 0)
    elif q == 'n':
        print('-> Existing file is used')
except:
    df = EthereumDownloader(sleeper = 0)

