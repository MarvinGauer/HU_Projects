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
import json

# Functions

# check if a string is a date
def is_date(string):
    try: 
        parse(string)
        return True
    except ValueError:
        return False

def EthereumContractDownloader(sleeper = 2, dat_output = 'Ethereum_Contracts.csv'):
    # Create soup to get initial information
    start_url = 'https://etherscan.io/contractsVerified/1'
    html      = requests.get(start_url)
    soup      = BeautifulSoup(html.text, "html5lib")
    
    # number of pages
    n_page    = int(str(soup.select('.col-sm-6 b')[1])[3:-4])
    
    # declare number of pages to be scraped and length-vector
    frame     = pd.DataFrame(columns=('Date', 'Hash', 'Tx', 'Balance'))
    l         = [0]
    
    with open(dat_output,'w') as file:
        file.write("Date, Hash, Tx, Balance")
        file.write('\n')
    
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
        with open(dat_output,'a') as file:
            for j in range(sum(l[0:i]),sum(l[0:(i+1)])):
                row          = [date[j - sum(l[0:i])],add[j - sum(l[0:i])], tx[j - sum(l[0:i])], bal[j - sum(l[0:i])]]
                frame.loc[j] = row
                file.write(', '.join([str(element) for element in row]))
                file.write('\n')
            
        # sleeper for server
        time.sleep(sleeper)
    
    return frame
        
def EthereumTransactionDownloader(contract_identifier = None, sleeper = 2, dat_output = 'Ethereum_Tx.csv', mode = 'a'):
    if mode == 'w':
        # Add Header
        with open(dat_output,'w') as file:
                file.write("Block, Contract, Date, From, Gas, To, Value")
                file.write('\n')
    j = 1
    for contract in contract_identifier:
        html      = requests.get('https://etherscan.io/txs?a=' + contract + '&p=1')
        soup      = BeautifulSoup(html.text, "html5lib")
        tx_n_page = int(str(soup.select('.pull-right b')[1])[3:-4])
        print(67*'-')
        print(67*'-')
        print('Starting download for contract ' + str(j) + ' of ' + str(len(contract_identifier)))
        print('Initializing download of ' + contract)
        print(67*'-')
        j += 1

        for page in range(1,tx_n_page+1):
            print('-> Downloading transactions on page ' + str(page) + ' of ' + str(tx_n_page))

            html      = requests.get('https://etherscan.io/txs?a=' + contract + '&p=' + str(page))
            soup      = BeautifulSoup(html.text, "html5lib")
            tx_data   = soup.select('td')
            tx_block  = [re.findall('/block/[0-9]{7}">', str(d))[0][7:-2] for d in tx_data if re.findall('/block/[0-9]{7}">', str(d)) != []]
            tx_date   = [re.findall('rel="tooltip" title="[A-Za-z0-9]{1,3}-[0-9]{2}-[0-9]{2,4} ', str(d))[0][21:-1] for d in tx_data if re.findall('rel="tooltip" title="[A-Za-z0-9]{1,3}-[0-9]{2}-[0-9]{2,4} ', str(d)) != [] if is_date(re.findall('rel="tooltip" title="[A-Za-z0-9]{1,3}-[0-9]{2}-[0-9]{2,4} ', str(d))[0][21:-1])]
            tx_value  = []
            k         = 6
            while k <= len(tx_data):
                if len(re.findall('[0-9]+<b>.</b>|[0-9]+ \w+', str(tx_data[k]))) > 1:
                    a = re.findall('[0-9]+<b>.</b>|[0-9]+ \w+', str(tx_data[k]))[0][:-8] + '.' + re.findall('[0-9]+<b>.</b>|[0-9]+ \w+', str(tx_data[k]))[1]
                    tx_value.append(a)
                else:
                    try:
                        tx_value.append(re.findall('[0-9]+<b>.</b>|[0-9]+ \w+', str(tx_data[k]))[0])
                    except:
                        tx_value.append('None')
                k += 8
            tx_value = tx_value[:-1]
            tx_gas = []
            k        = 7
            while k <= len(tx_data):
                if len(re.findall('[0-9]+<b>.</b>|[0-9]+<', str(tx_data[k]))) > 1:
                    a = re.findall('[0-9]+<b>.</b>|[0-9]+<', str(tx_data[k]))[0][:-8] + '.' + re.findall('[0-9]+<b>.</b>|[0-9]+<', str(tx_data[k]))[1][:-1]
                    tx_gas.append(a)
                else:
                    try:
                        tx_gas.append(re.findall('[0-9]+<b>.</b>|[0-9]+<', str(tx_data[k]))[0][:-1])
                    except:
                        tx_gas.append('None')
                k += 8
            tx_gas = tx_gas[:-1]
            tx_from  = [re.findall('">[A-Za-z0-9]{42}</a></span></td>', str(d))[0][2:-16] for d in tx_data if re.findall('">[A-Za-z0-9]{42}</a></span></td>', str(d)) != []]
            tx_to    = [re.findall('"address-tag">[A-Za-z0-9]+</span></span></td>', str(d))[0][14:-19] for d in tx_data if re.findall('"address-tag">[A-Za-z0-9]+</span></span></td>', str(d)) != []]
            if len(tx_to) != 50:
                tx_to.append('Contract Creation')
            tx_contract= [contract for d in range(len(tx_date))]
            try:
                page_frame = pd.DataFrame(
                    {'tx_contract': tx_contract,
                     'tx_date'    : tx_date,
                     'tx_value'   : tx_value,
                     'tx_gas'     : tx_gas,
                     'tx_block'   : tx_block,
                     'tx_from'    : tx_from,
                     'tx_to'      : tx_to
                    })
            except:
                print(25*'-'+'Error-Report-Start'+23*'-')
                print("---> Hash " + str(contract))
                print("Contract: " + str(len(tx_contract)))
                print('Value: ' + str(len(tx_value)))
                print('Gas: ' + str(len(tx_gas)))
                print('Date: ' + str(len(tx_date)))
                print('From: ' + str(len(tx_from)))
                print('To: ' + str(len(tx_to)))
                print(25*'-'+'Error-Report-Start'+23*'-')

            with open(dat_output,'a') as file:
                for j in range(0,len(page_frame)):
                    file.write(', '.join([str(element) for element in page_frame.loc[j]]))
                    file.write('\n')

            time.sleep(sleeper)
    print(67*'-')
    print(46*'-' + '> Download completed!')

def EthereumSourceCode(contract_identifier = None, dat_output = 'Ethereum_SourceCode.csv', API_Code = 'KPDQ8J6VWUH7UE61YSU14U3A76AYB8S7KN'):
    #with open(dat_output,'w') as file:
    #        file.write("Contract, Name, SourceCode")
    #        file.write('\n')
    j = 1
    for contract in contract_identifier:
        print('Downloading SourceCode ' + str(j) + ' of ' + str(len(contract_identifier)))
        link = 'https://api.etherscan.io/api?module=contract&action=getsourcecode&address=' + contract + '&apikey=' + API_Code
        json = requests.get(link)
        data = dict(json.json())
        try:
            name = data['result'][0]['ContractName']
            code = '"' + data['result'][0]['SourceCode'] + '"'
        except:
            name = ''
            code = ''
            print(25*'-'+'Error-Report-Start'+23*'-')
            print("---> Hash " + str(contract))
            print("No Source Code Available")
            print(25*'-'+'Error-Report-Start'+23*'-')
        try:
            page_frame = pd.DataFrame(
                {'Contract'     : [contract],
                 'Name'         : [name],
                 'SourceCode'   : [code]
                })
        except:
            print(25*'-'+'Error-Report-Start'+23*'-')
            print("---> Hash " + str(contract))
            print(25*'-'+'Error-Report-Start'+23*'-')
        with open(dat_output,'a') as file:
                file.write(', '.join([str(element) for element in page_frame.loc[0]]))
                file.write('\n')
        j += 1
        
# Main

API_Key = 'Type in your API-Key here'

# Contract's Source Code
dat_output_SC = 'Ethereum_SourceCode.csv'

# Download Contracts
# Takes about 2 hours
# EthereumContractDownloader()    
    
# Example Contracts to download Transactions from
ci = ['0x29d1487bbbce6f3766db1d36a5eea93ca01a2a75', '0x365267181bc0ef38bbb8d8ca9b330dc0c3ac01d1']
# It will download roughly 100 Mio. transactions if all contracts in csv-file are used. 
EthereumTransactionDownloader(contract_identifier = ci, mode = 'w')

# Import Data
dat_input_C  = 'Ethereum.csv'
dat_output_Tx = 'Ethereum_Tx.csv'

#df_Tx        = pd.read_csv(dat_output_Tx, sep =',')
df_Contracts = pd.read_csv(dat_input_C, sep =';')

contracts = [c.strip() for c in df_Contracts['Contract_Identifier'].tolist()]
#Downloading the Source Codes needs about 8 hours and 500 MB
#EthereumSourceCode(contract_identifier = contracts, dat_output = dat_output_SC, API_Code = API_Key)
