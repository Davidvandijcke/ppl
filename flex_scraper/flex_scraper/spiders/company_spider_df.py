import pandas as pd
import scrapy
from bs4 import BeautifulSoup
import re
import json
import os

class CompanySpiderDf(scrapy.Spider):
    name = 'company_spider_df'
    
    
    
    def __init__(self):
        self.main_dir = "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/"
        self.data_dir = os.path.join(self.main_dir, 'data', 'out')
    
    
    def start_requests(self):
        
    
        # Load your company names from a CSV file into a DataFrame
        # Replace 'your_file.csv' with the path to your CSV file
        # Ensure your CSV has a column named 'company_name' with the company names
        df = pd.read_csv(os.path.join(self.data_dir, "flex_index_aggregate.csv"))
        df['company_name'] = df['company_name'].str.lower()
        df['company_name'] = df['company_name'].str.replace(' ', '-')
        
        # Generate URLs for each company and yield a Request for each
        base_url = 'https://www.flex.scoopforwork.com/company/{}'
        for company_name in df['company_name']:
            url = base_url.format(company_name)
            yield scrapy.Request(url=url, callback=self.parse)
    
    

    def parse(self, response):
        
        
        soup = BeautifulSoup(response.body, 'html.parser')

        # Example of parsing the company page
        # Update these selectors based on the actual data you wish to scrape

        # Regex pattern to find the relevant self.__next_f.push call.
        # Adjust this pattern based on the actual structure and how the data is embedded.
        pattern = r"(aboutDescription.*\))([\s\S]*)"

        # Find all matches in the HTML content. This returns a list of all matches.
        matches = re.findall(pattern, str(soup), re.DOTALL)

        # Clean the string: Unescape the string to convert it into a valid JSON format.
        cleaned_string = matches[0][0].replace('\\"', '"').replace('\\\\n', '\n').replace('\\', '')

        # Remove the invalid trailing characters (example based on the ending you provided, adjust as necessary)
        if cleaned_string.endswith(']n"])'):
            cleaned_string = cleaned_string[:-5]

        # Try parsing the JSON
        try:
            data_json = json.loads('{"' + cleaned_string, strict=False)
            # If parsing is successful, you can now work with `data_json` as a dictionary
        except json.JSONDecodeError as e:
            print(f"Error parsing JSON: {e}")

        comp = data_json['company']
        qstn = comp.pop('questions')
        qstn = pd.DataFrame(qstn)
        comp = pd.DataFrame([comp])
        
        # append the company and question data to a CSV file
        comp.to_csv(os.path.join(self.data_dir, 'companies.csv'), mode='a', header=False)
        qstn.to_csv(os.path.join(self.data_dir, 'questions.csv'), mode='a', header=False)