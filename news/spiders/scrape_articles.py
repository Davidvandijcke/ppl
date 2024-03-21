import sys
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver import ActionChains
from selenium.common.exceptions import TimeoutException
from webdriver_manager.chrome import ChromeDriverManager
from bs4 import BeautifulSoup as soup
from bs4 import SoupStrainer
from lxml import html
from news.items import NewsItem
import numpy as np
from tqdm import tqdm
import time
import csv
import polars as pl
import pandas as pd
#import win32com.client as win32
from chromedriver_py import binary_path # this will get you the path variable
import re
from math import ceil, isnan
from datetime import date
import numpy.random as random
import pdb
import scrapy
from scrapy.crawler import CrawlerProcess
from scrapy.http.request import Request
import os


class MySpider(scrapy.Spider):

    name = 'myspider'
    start_urls = ["https://infoweb-newsbank-com.proxy.lib.umich.edu/apps/news/document-view?p=WORLDNEWS&t=continent%3ANorth%2BAmerica%21North%2BAmerica/stp%3ANewspaper%7CWeb-Only%2BSource%21Multiple%2520Source%2520Types%2520%282%29/country%3AUSA%21USA&sort=YMD_date%3AD&maxresults=20&f=advanced&val-base-0=%28%22election%22%20OR%20%22voter%22%20OR%20%22votes%22%20OR%20%22vote%22%20OR%20%22voted%22%20OR%20%22voting%22%20OR%20%22electoral%22%20OR%20%22voters%22%20OR%20%22ballot%22%20OR%20%22ballots%22%29%20AND%20%28%22stolen%22%20OR%20%22steal%22%20OR%20%22stealing%22%20OR%20%22fraud%22%20OR%20%22rigged%22%20OR%20%22fraudulent%22%29&fld-base-0=alltext&bln-base-1=and&val-base-1=after%2011/01/2020&fld-base-1=YMD_date&bln-base-2=and&val-base-2=before%2001/20/2021&fld-base-2=YMD_date&docref=news/1801C5E130339980"]

    # def __init__(self, **kwargs): 
    #     self.start_urls = [kwargs.get('start_url')]
    #     super().__init__(**kwargs) 
    #     #print(self.start_urls)

    def parse(self, response):
        print('test')
        cookies = self.get_cookies()
        request = Request(url=self.start_urls[0],
            cookies=cookies,
            callback=self.after_login)
        request.meta['cookies'] = cookies
        return request

    def get_cookies(self):
        browser = webdriver.Chrome(ChromeDriverManager().install())
        browser.implicitly_wait(90)
        browser.get(self.start_urls[0])
        wait = WebDriverWait(browser, 10)
        results = wait.until(lambda browser: browser.find_elements_by_xpath("//img[@alt='NewsBank home']"))
        cookies = browser.get_cookies()
        browser.close()
        return cookies
    
    def after_login(self, response):
        print("Success!")



        # get data
        main_dir = '/home/antonvocalis/Dropbox (University of Michigan)/insurrection_DC'
        data_in = os.path.join(os.path.join(main_dir, 'data', 'in'))
        data_by = os.path.join(os.path.join(main_dir, 'data', 'by'))

        cookies = response.meta.get('cookies')

        print('test')


        import pandas as pd

        df = pd.read_csv(os.path.join(data_by, 'articles_subset_text_1.csv'), delimiter = '\t')

        print('test 1')


        df = df.drop_duplicates(subset = ['source', 'headline', 'date'])
        df = df[df['headline'] != 'News Article']


        with open('article_text.csv', 'w') as f1:
            writer=csv.writer(f1, delimiter='\t',lineterminator='\n',)
            writer.writerow(['index', 'text'])


        # loop over urls (they are numbered by page)
        df_temp = df[df.text.isnull()]
        print('test2')

        # from scrapy.shell import inspect_response
        # inspect_response(response, self)
        # print(df_temp['href'][1])

        
        for i, row in tqdm(df_temp.iterrows(), total = df_temp.shape[0]):
            try:
                link_full = "https://infoweb-newsbank-com.proxy.lib.umich.edu" + row['href']
                request = Request(url = link_full, callback = self.parse_articles,
                                  cookies = cookies)   
                request.meta['i'] = i
                yield request
            except: 
                pass
                


    def parse_articles(self, response):
        from scrapy.shell import inspect_response
        inspect_response(response, self)
        text = [
            ' '.join(
                line.strip()
                for line in p.xpath('.//text()').extract()
                if line.strip()
            )
            for p in response.xpath("//*[@id='document-view--ascii']")
            ]

        with open('article_text.csv', 'a') as f1:
            writer=csv.writer(f1, delimiter='\t',lineterminator='\n',)
            writer.writerow([response.meta.get('i'), text])
        
        



class ArticleSpider(scrapy.Spider):

    name = 'articlespider'
    start_urls = ['https://infoweb-newsbank-com.proxy.lib.umich.edu/apps/news/results?sort=YMD_date%3AD&p=WORLDNEWS&t=continent%3ANorth%20America%21North%20America/stp%3ANewswire%7CWeb-Only%20Source%21Multiple%20Source%20Types%20%282%29/country%3AUSA%21USA&maxresults=20&f=advanced&val-base-0=&fld-base-0=alltext&bln-base-1=and&val-base-1=after%2009/30/2020&fld-base-1=YMD_date&bln-base-2=and&val-base-2=before%2001/06/2021&fld-base-2=YMD_date']

    # def __init__(self, **kwargs): 
    #     self.start_urls = [kwargs.get('start_url')]
    #     super().__init__(**kwargs) 
    #     #print(self.start_urls)

    # https://infoweb-newsbank-com.proxy.lib.umich.edu/apps/news/results?page=1&sort=YMD_date%3AD&p=WORLDNEWS&t=continent%3ANorth%20America%21North%20America/stp%3ANewswire%7CWeb-Only%20Source%21Multiple%20Source%20Types%20%282%29/country%3AUSA%21USA&maxresults=20&f=advanced&val-base-0=&fld-base-0=alltext&bln-base-1=and&val-base-1=after%2009/30/2020&fld-base-1=YMD_date&bln-base-2=and&val-base-2=before%2001/06/2021&fld-base-2=YMD_date

    def parse(self, response):
        print('test')
        cookies = self.get_cookies()
        request = Request(url=self.start_urls[0],
            cookies=cookies,
            callback=self.after_login)
        request.meta['cookies'] = cookies
        return request

    def get_cookies(self):
        browser = webdriver.Chrome(ChromeDriverManager().install())
        browser.implicitly_wait(90)
        browser.get(self.start_urls[0])
        wait = WebDriverWait(browser, 10)
        results = wait.until(lambda browser: browser.find_elements_by_xpath("//img[@alt='NewsBank home']"))
        cookies = browser.get_cookies()
        browser.close()
        return cookies
    
    def after_login(self, response):
        print("Success!")
        # from scrapy.shell import inspect_response
        # inspect_response(response, self)

        cookies = response.meta.get('cookies')

        # start writing to file
        with open('all_articles.csv', 'w') as f1:
            writer=csv.writer(f1, delimiter='\t',lineterminator='\n',)
            writer.writerow(['date', 'source', 'location', 'headline', 'href', 'type'])


        # get total hits
        total_hits = response.xpath('//*[contains(@class, "search-hits__meta")]/text()').extract()
        total_hits = total_hits[1].replace(' Results', '').replace('\n', '').strip()
        total_hits = int(re.sub(",", "", total_hits))

        

        # loop over urls (they are numbered by page)
        for i in tqdm(range(0, ceil(total_hits / 20))):
            try:
                if i > 0:
                    url_get = 'https://infoweb-newsbank-com.proxy.lib.umich.edu/apps/news/results?' + \
                    'page=' + str(i) + 'sort=YMD_date%3AD&p=WORLDNEWS&t=continent%3ANorth%20America%21North%20America/stp%3ANewswire%7CWeb-Only%20Source%21Multiple%20Source%20Types%20%282%29/country%3AUSA%21USA&maxresults=20&f=advanced&val-base-0=&fld-base-0=alltext&bln-base-1=and&val-base-1=after%2009/30/2020&fld-base-1=YMD_date&bln-base-2=and&val-base-2=before%2001/06/2021&fld-base-2=YMD_date'
                else: 
                    url_get = self.start_urls[0]
                yield Request(url = url_get, callback = self.parse_articles,
                                  cookies = cookies)      
            except ValueError: 
                print("URL not found, breaking loop")
                break
            except BaseException as err:
                print("Unexpected " + str(err) + ", " + str(type(err)))
                raise

    def parse_articles(self, response):

        # get articles from response
        html = response.body
        html = soup(html, 'html.parser')
        mydivs = html.find_all("article")


        # enter each of 20 articles in page into csv
        for j in range(0,len(mydivs)-1):

            item = NewsItem()
            
            article = mydivs[j]
            
            # date
            myli = article.find('li', {"class" : "search-hits__hit__meta__item search-hits__hit__meta__item--display-date"})
            item['date'] = myli.text.replace('\n', '').strip()

            # source and location
            myli = article.find('li', {"class" : "search-hits__hit__meta__item search-hits__hit__meta__item--source"})
            paper = myli.text.replace('\n', '').strip()
            item['location'] = paper[paper.find("(")+1:paper.find(")")]
            item['source'] = paper[0:paper.find("(")].strip()

            # href
            item['href'] = article.find('a', href = True)['href']

            # headline
            item['title'] = article.find('input')['data-title']

            # type
            type = article.find('div', {"class" : "search-hits__hit--type"})['class'][1]
            if 'web-only-source' in type: 
                item['type'] = 'W'
            else:
                item['type'] = 'N'
            
            yield item

            
        # with open('all_articles.csv', 'a') as f1:
        #     writer=csv.writer(f1, delimiter='\t',lineterminator='\n',)
        #     writer.writerow([date, source, location, title, href, type])

        




# //*[@id="document-view--ascii"]/div/div[1]/div[3]

        # self.browser.get(self.start_url)
        # _cookies = {cookie['name']: cookie['value'] for cookie in self.browser.get_cookies()}
        # request = Request(self.start_url, cookies=self.browser.get_cookies(), callback=self.parse)
        # html = soup(response.body, 'html.parser')
        # mydivs = html.find_all("article")
        # for j in range(0,len(mydivs)-1):
        #     article = mydivs[j]
            
        #     # date
        #     myli = article.find('li', {"class" : "search-hits__hit__meta__item search-hits__hit__meta__item--display-date"})
        #     date = myli.text.replace('\n', '').strip()

        #     # source and location
        #     myli = article.find('li', {"class" : "search-hits__hit__meta__item search-hits__hit__meta__item--source"})
        #     paper = myli.text.replace('\n', '').strip()
        #     location = paper[paper.find("(")+1:paper.find(")")]
        #     source = paper[0:paper.find("(")].strip()

        #     # href
        #     href = article.find('a', href = True)['href']

        #     # headline
        #     title = article.find('input')['data-title']

        #     # type
        #     type = article.find('div', {"class" : "search-hits__hit--type"})['class'][1]
        #     if 'web-only-source' in type: 
        #         type = 'W'
        #     else:
        #         type = 'N'
            
        #     with open('large2.csv', 'a') as f1:
        #         writer=csv.writer(f1, delimiter='\t',lineterminator='\n',)
        #         writer.writerow([date, source, location, title, href, type])
            
        # # look for next page
        # next_page_number = response.xpath('//*[@id="search-hits-gnus-search-hits-pane"][contains(@text, "Next")]').get()
        # # If it exists and there is a next page enter if statement
        # if next_page_number:
        #     yield scrapy.FormRequest.from_response(
        #         response=response,
        #         formid="category_form",
        #         formdata={
        #             'page-no': next_page_number,
        #         },
        #         callback=self.parse
        # )






# //*[@id="document-view--ascii"]/div/div[1]/div[3]

        # self.browser.get(self.start_url)
        # _cookies = {cookie['name']: cookie['value'] for cookie in self.browser.get_cookies()}
        # request = Request(self.start_url, cookies=self.browser.get_cookies(), callback=self.parse)
        # html = soup(response.body, 'html.parser')
        # mydivs = html.find_all("article")
        # for j in range(0,len(mydivs)-1):
        #     article = mydivs[j]
            
        #     # date
        #     myli = article.find('li', {"class" : "search-hits__hit__meta__item search-hits__hit__meta__item--display-date"})
        #     date = myli.text.replace('\n', '').strip()

        #     # source and location
        #     myli = article.find('li', {"class" : "search-hits__hit__meta__item search-hits__hit__meta__item--source"})
        #     paper = myli.text.replace('\n', '').strip()
        #     location = paper[paper.find("(")+1:paper.find(")")]
        #     source = paper[0:paper.find("(")].strip()

        #     # href
        #     href = article.find('a', href = True)['href']

        #     # headline
        #     title = article.find('input')['data-title']

        #     # type
        #     type = article.find('div', {"class" : "search-hits__hit--type"})['class'][1]
        #     if 'web-only-source' in type: 
        #         type = 'W'
        #     else:
        #         type = 'N'
            
        #     with open('large2.csv', 'a') as f1:
        #         writer=csv.writer(f1, delimiter='\t',lineterminator='\n',)
        #         writer.writerow([date, source, location, title, href, type])
            
        # # look for next page
        # next_page_number = response.xpath('//*[@id="search-hits-gnus-search-hits-pane"][contains(@text, "Next")]').get()
        # # If it exists and there is a next page enter if statement
        # if next_page_number:
        #     yield scrapy.FormRequest.from_response(
        #         response=response,
        #         formid="category_form",
        #         formdata={
        #             'page-no': next_page_number,
        #         },
        #         callback=self.parse
        # )




