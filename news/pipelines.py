# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: https://docs.scrapy.org/en/latest/topics/item-pipeline.html


# useful for handling different item types with a single interface
from itemadapter import ItemAdapter
import csv


def write_to_csv(item):
    writer = csv.writer(open('all_articles.csv', 'a'), delimiter = "\t", lineterminator='\n')
    writer.writerow([item[key] for key in item.keys()])

class NewsPipeline:
        
    def process_item(self, item, spider):
        write_to_csv(item)
        return item
