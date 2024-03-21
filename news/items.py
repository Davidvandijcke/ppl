# Define here the models for your scraped items
#
# See documentation in:
# https://docs.scrapy.org/en/latest/topics/items.html

import scrapy


class NewsItem(scrapy.Item):
    # define the fields for your item here like:
    # name = scrapy.Field()
    
    date = scrapy.Field()
    source = scrapy.Field()
    location = scrapy.Field()
    title = scrapy.Field()
    href = scrapy.Field()
    type = scrapy.Field()
    
    
    
    
    
    
