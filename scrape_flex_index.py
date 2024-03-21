# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

# -*- coding: utf-8 -*-
"""
Created on Thu May 24 13:34:26 2018
@author: Shuai
"""

# pip install chromedriver-py

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver import ActionChains
from selenium.common.exceptions import TimeoutException
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
from selenium.common.exceptions import NoSuchElementException, StaleElementReferenceException
from bs4 import BeautifulSoup as soup
from bs4 import SoupStrainer
from lxml import html
import numpy as np
from tqdm import tqdm
import time
import csv
#import win32com.client as win32
from chromedriver_py import binary_path # this will get you the path variable
import re
from datetime import date
import numpy.random as random
import pdb
import pandas as pd

# get code_dir as location of this file
import os
code_dir = os.path.dirname(os.path.abspath(__file__))
main_dir = os.path.dirname(code_dir)
data_dir = os.path.join(main_dir, 'data', 'out')
            


# --- it runs without project and saves in `output.csv` ---



def get_flex_agg(login_url, user, pw, years_to_get, col_codes):
    """ Logs in to FAME, retrieves saved company set for which we want to download
    data, selects columns and years to download and iteratively calls "export_orbis" for 
    range of companies that can exported at once
    
    login_url -- BvD url
    user -- username for fame
    pw -- password for fame
    years_to_get -- list of years for which to get data
    col_codes -- list of variable codes to get !! make sure to put a time-dependent variable first
    
    """
            
    # grab chrome browser
    
    browser = webdriver.Chrome()
    browser.implicitly_wait(10) # seconds
    
    ## log in to world access news database  
    login_url = "https://www.flex.scoopforwork.com/explore"
    browser.get(login_url)

    df = pd.DataFrame(columns=['company_name', 'company_info', 'office_requirements', 'requirement_details'])

    page = 1
    while True:
        print(f"Scraping page {page}")
        if page == 82:
            break
            # Locating all company card links
        while True:
            time.sleep(random.uniform(1, 2))
            try:
                df_try = pd.DataFrame(columns=['company_name', 'company_info', 'office_requirements', 'requirement_details'])
                company_cards = browser.find_elements(By.CSS_SELECTOR, "[data-test='company-card-link']")

                for card in company_cards:
                    try: # some company names just throw errors
                        company_name = card.find_element(By.CSS_SELECTOR, "[data-test='company-name']").text
                        company_info = card.find_element(By.CSS_SELECTOR, "[data-test='company-info']").text
                        office_requirements = card.find_element(By.CSS_SELECTOR, "[data-test='office-requirements']").text
                        requirement_details = card.find_element(By.CSS_SELECTOR, "[data-test='requirement-details']").text

                        # processing the extracted information
                        temp = pd.DataFrame({'company_name': company_name, 'company_info': company_info, 'office_requirements': office_requirements, 'requirement_details': requirement_details}, index=[0])
                        df_try = pd.concat([df_try,temp], ignore_index=True)
                    except:
                        pass
                df = pd.concat([df, df_try], ignore_index=True)
                break
            # except and print exception
            except Exception as e:
                print(e)
                break

        # Check for 'chevron-right' button and click to go to the next page
        while True:
            try:
                wait = WebDriverWait(browser, timeout=2)
                next_page_button = wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR, "[data-test='pagination-next-button']")))
                browser.execute_script("arguments[0].click();", next_page_button)


                page += 1
                
                # random wait
                time.sleep(random.uniform(1, 2))
                break
            except NoSuchElementException:
                # If 'chevron-right' button is not found, break the loop (end of pages)
                break
            except:
                pass
    df.to_csv(os.path.join(data_dir, 'flex_index_aggregate.csv'), index=False)



# TODO: I can scrape the details by just adding /company/[company_name] to the url with spider
def get_flex_detail(login_url, user, pw, years_to_get, col_codes):
    """ Logs in to FAME, retrieves saved company set for which we want to download
    data, selects columns and years to download and iteratively calls "export_orbis" for 
    range of companies that can exported at once
    
    login_url -- BvD url
    user -- username for fame
    pw -- password for fame
    years_to_get -- list of years for which to get data
    col_codes -- list of variable codes to get !! make sure to put a time-dependent variable first
    
    """
            
    # grab chrome browser
    
    browser = webdriver.Chrome()
    browser.implicitly_wait(10) # seconds
    
    ## log in to world access news database  
    login_url = "https://www.flex.scoopforwork.com/explore"
    browser.get(login_url)


    page = 1
    while True:
        print(f"Scraping page {page}")
        if page == 82:
            break
            # Locating all company card links
        while True:
            time.sleep(random.uniform(1, 2))
            try:
                company_cards = browser.find_elements(By.CSS_SELECTOR, "[data-test='company-card-link']")

                for i in range(len(company_cards)):
                    try: # some company names just throw errors
                        
                        details = {}
                        try: 
                            card = company_cards[i]
                            company_name = card.find_element(By.CSS_SELECTOR, "[data-test='company-name']")
                        except StaleElementReferenceException:
                            company_cards = browser.find_elements(By.CSS_SELECTOR, "[data-test='company-card-link']")
                            card = company_cards[i]
                            company_name = card.find_element(By.CSS_SELECTOR, "[data-test='company-name']")
                        details['company_name'] = company_name.text
                        details['company_info'] = card.find_element(By.CSS_SELECTOR, "[data-test='company-info']").text
                        details['office_requirements'] = card.find_element(By.CSS_SELECTOR, "[data-test='office-requirements']").text
                        details['requirement_details'] = card.find_element(By.CSS_SELECTOR, "[data-test='requirement-details']").text

                        # get details (employees, website and industry)
                        company_name.click()
                        # wait for a bit
                        time.sleep(random.uniform(0.5, 2))

                        header = browser.find_element(By.CSS_SELECTOR, "[data-test='company-info']").text
                        details['employees'], details['industry'], details['website'] = header.split('\n')
   
                        browser.back()
                        time.sleep(random.uniform(0.5, 2))


                        # processing the extracted information
                        temp = pd.DataFrame(details, index=[0])
                        if i == 0: 
                            df_try = temp.copy()
                        else: 
                            df_try = pd.concat([df_try,temp], ignore_index=True)
                    except:
                        pass
                if page == 1:
                    df = df_try.copy()
                else:
                    df = pd.concat([df, df_try], ignore_index=True)
                break
            # except and print exception
            except Exception as e:
                print(e)
                break

        # Check for 'chevron-right' button and click to go to the next page
        while True:
            try:
                wait = WebDriverWait(browser, timeout=2)
                next_page_button = wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR, "[data-test='pagination-next-button']")))
                browser.execute_script("arguments[0].click();", next_page_button)


                page += 1
                
                # random wait
                time.sleep(random.uniform(1, 2))
                break
            except NoSuchElementException:
                # If 'chevron-right' button is not found, break the loop (end of pages)
                break
            except:
                pass
    df.to_csv(os.path.join(data_dir, 'flex_index_detail.csv'), index=False)


