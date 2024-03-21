import pandas as pd
import scrapy
from bs4 import BeautifulSoup
import re
import json
import os
from scrapy.linkextractors import LinkExtractor
from scrapy.spiders import CrawlSpider, Rule
from scrapy.http.request import Request

# NOTE: some companies end with .com or something like that and the idiots at scoop didn't account for that


# class CompanySpider(CrawlSpider):
#     name = 'company_spider'
    
#     allowed_domains = ['flex.scoopforwork.com']
#     start_urls = ['https://www.flex.scoopforwork.com/explore/']

#     rules = (
#         Rule(LinkExtractor(allow=(r'/company/[^/]+$')), callback='parse_company'),
#     )

    
#     def parse_start_url(self, response):
#         # Extract company links from the first page
#         yield from self.parse_company(response)
        
#         # Navigate through pagination
#         for page in range(2, 83):  # Assuming there are 82 pages; adjust accordingly
#             next_page = f'https://www.flex.scoopforwork.com/explore?page={page}'
#             yield Request(next_page, callback=self.parse_company)

    
#     def parse_company(self, response):
#         main_dir = "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/"
#         data_dir = os.path.join(main_dir, 'data', 'out')
    
#         # This function will be called for each extracted link that matches the pattern
#         # You can process each company page as needed here

#         soup = BeautifulSoup(response.body, 'html.parser')

#         # Example of parsing the company page
#         # Update these selectors based on the actual data you wish to scrape

#         # Regex pattern to find the relevant self.__next_f.push call.
#         # Adjust this pattern based on the actual structure and how the data is embedded.
#         pattern = r"(aboutDescription.*\))([\s\S]*)"

#         # Find all matches in the HTML content. This returns a list of all matches.
#         matches = re.findall(pattern, str(soup), re.DOTALL)

#         # Clean the string: Unescape the string to convert it into a valid JSON format.
#         cleaned_string = matches[0][0].replace('\\"', '"').replace('\\\\n', '\n').replace('\\', '')

#         # Remove the invalid trailing characters (example based on the ending you provided, adjust as necessary)
#         if cleaned_string.endswith(']n"])'):
#             cleaned_string = cleaned_string[:-5]

#         # Try parsing the JSON
#         try:
#             data_json = json.loads('{"' + cleaned_string, strict=False)
#             # If parsing is successful, you can now work with `data_json` as a dictionary
#         except json.JSONDecodeError as e:
#             print(f"Error parsing JSON: {e}")

#         comp = data_json['company']
#         qstn = comp.pop('questions')
#         qstn = pd.DataFrame(qstn)
#         comp = pd.DataFrame([comp])
        

#         # append the company and question data to a CSV file
#         comp.to_csv(os.path.join(data_dir, 'companies.csv'), mode='a', header=False)
#         qstn.to_csv(os.path.join(data_dir, 'questions.csv'), mode='a', header=False)



import scrapy
from bs4 import BeautifulSoup
import re
import json
import os
import pandas as pd
from scrapy.linkextractors import LinkExtractor
from scrapy.spiders import Spider
from scrapy.http import Request
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from scrapy.http import HtmlResponse
from selenium.common.exceptions import NoSuchElementException



class CompanySpider(Spider):
    name = 'company_spider'
    allowed_domains = ['flex.scoopforwork.com']
    start_urls = ['https://www.flex.scoopforwork.com/explore/']
    
    def __init__(self):
        chrome_options = Options()
        chrome_options.add_argument("--headless")
        self.driver = webdriver.Chrome(options=chrome_options)

    def parse(self, response):
        self.driver.get(response.url)

        # Assuming pagination is possible, let's handle it here
        current_page = 1
        last_page = 82  # Example last page, adjust based on the site's pagination
        
        while current_page <= last_page:
            # Process the current page
            sel_response = HtmlResponse(url=self.driver.current_url, body=self.driver.page_source, encoding='utf-8')
            yield from self.extract_companies(sel_response)
            
            current_page += 1
            try:
                # Try to navigate to the next page. This part depends on how the site's pagination works.
                # If there's a "Next" button:
                # next_page_button = self.driver.find_element_by_xpath('//button[@id="next-page"]')
                # next_page_button.click()

                # Or directly go to the next page by URL manipulation:
                next_page_url = f'https://www.flex.scoopforwork.com/explore?page={current_page}'
                self.driver.get(next_page_url)
            except NoSuchElementException:
                # If no "Next" button is found or you're unable to navigate, break the loop
                self.logger.info('No more pages to load.')
                break

    def extract_companies(self, response):
        for link in LinkExtractor(allow=(r'/company/[^/]+')).extract_links(response):
            yield response.follow(link, callback=self.parse_company)


    def parse_company(self, response):
        # Initialize directories for storing data
        main_dir = "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/"
        data_dir = os.path.join(main_dir, 'data', 'out')

        # This function will be called for each extracted link that matches the pattern
        # You can process each company page as needed here

        soup = BeautifulSoup(response.body, 'html.parser')

        # Example of parsing the company page
        # Update these selectors based on the actual data you wish to scrape

        # Regex pattern to find the relevant self.__next_f.push call.
        # Adjust this pattern based on the actual structure and how the data is embedded.
        pattern = r"(aboutDescription.*\))([\s\S]*)"

        # Find all matches in the HTML content. This returns a list of all matches.
        matches = re.findall(pattern, str(soup), re.DOTALL)

        # Clean the string: Unescape the string to convert it into a valid JSON format.
        cleaned_string = matches[0][0].replace('\\\\n', '\\n').replace('\\"', '"').replace('\\\\"', '\\\"')

        # Remove the invalid trailing characters (example based on the ending you provided, adjust as necessary)
        cleaned_string = cleaned_string[:-6]

        decoder = json.JSONDecoder()

        data_json, index = decoder.raw_decode('{"' + cleaned_string)
            # If parsing is successful, you can now work with `data_json` as a dictionary


        comp = data_json['company']
        qstn = comp.pop('questions')
        qstn = pd.DataFrame(qstn)
        comp = pd.DataFrame([comp])
        

        # append the company and question data to a CSV file
        comp.to_csv(os.path.join(data_dir, 'companies.csv'), mode='a', header=False)
        qstn.to_csv(os.path.join(data_dir, 'questions.csv'), mode='a', header=False)
