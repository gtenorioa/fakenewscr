#!/usr/bin/env python
# coding: utf-8

# In[15]:


import json
import newspaper
from newspaper import Article
import time
from time import mktime
from datetime import datetime


LIMIT = 30
data = {}
data['noticias'] = {}

#Abrir archivo json con sitios de noticias

with open('empresas.json') as datos:
    noticieros = json.load(datos)

 

#Iterar por todas las empresas
for noticiero, value in noticieros.items():
    contador = 1
    print("Haciendo lista para: ", noticiero)
    paper = newspaper.build(value['link'], memoize_articles = False)
    newsPaper = {
        "link" : value['link'],
        "articulos": []
    }
    for content in paper.articles:
        if contador > LIMIT:
            break
        try:
            content.download()
            content.parse()
        except Exception as e:
            print(e)
            print("Continuando...")
            continue
        articulo = {}
        articulo['title'] = content.title
        articulo['text'] = content.text
        articulo['published'] = str(content.publish_date)
        articulo['author'] = content.authors
        articulo['link'] = content.url
        newsPaper['articulos'].append(articulo)
        print(contador, "articulos bajados de", noticiero, "usando noticiero, url: ", content.url)
        contador = contador + 1
        time.sleep(1)
    data['noticias'][noticiero] = newsPaper
        
try:
    with open('articulos_noticias.json', 'w') as outfile:
        json.dump(data, outfile)
except Exception as e: print(e)


# In[ ]:




