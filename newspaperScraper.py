#!/usr/bin/env python
# coding: utf-8

# In[51]:


import nltk
import json
import csv
import time
import pandas as pd
#nltk.download('punkt')
from newspaper import Article


enlace = 'ListaEnlaces.json'


df = pd.read_json(enlace, orient='index')

df_a = pd.DataFrame()
for index, row in df.iterrows():
    url = row['url']
    a = Article(url, language='es')
    a.download()
    a.parse()
    a.nlp()
    df_a = df_a.append([{'ulr':url,
                         'Autor':a.authors,
                         'Fecha':str(a.publish_date),
                         'Titulo':a.title,
                         'Texto': a.text,
                         'Resumen':a.summary,
                         'Keywords':a.keywords}], ignore_index=True)
    time.sleep(2)

df_a.to_excel('Noticias_Final.xlsx')   


# In[29]:


import urllib.request
response = urllib.request.urlopen('https://l.facebook.com/l.php?u=https%3A%2F%2Famprensa.com%2F2019%2F07%2Fmaestra-relacionada-con-audios-dios-es-justo-y-sabe-por-que-hace-las-cosas%2F%3Ffbclid%3DIwAR0WfX7zZn3nUQJ5u-dBhxMdxfczWGTeA4811o1ohd8AqnZ74EyqnXewNZQ&h=AT3ZnaMlqHYxWLcJiJDMkFW7Aebo3ChD-5mIm_27jQ1CAU4Xy-8tF5J1oXV-ZL8FP67DNs03VHWOiVt-X6ZwJGuAHUZeikakNnO0kzRX0BXwTCMYClKZg8NkV2HZMg_WHfX2fRcx48ymOpUvkWlGnGQdQbmDoO3U9sE')
response.geturl()
  


# In[4]:


import newspaper
import csv

from newspaper import news_pool
lanacion_paper = newspaper.build('http://www.nacion.com/')
republica_paper = newspaper.build('http://www.larepublica.net/', memoize_articles=False)
delfino_paper = newspaper.build('http://delfino.cr/')
amprensa_paper = newspaper.build('http://amprensa.com/')
laregion_paper = newspaper.build('http://laregion.cr/')
diarioextra_paper = newspaper.build('http://www.diarioextra.com')
diariolacarta_paper = newspaper.build('http://diariolacarta.com/')
ncrnoticias_paper = newspaper.build('http://ncrnoticias.com/')
culturacr_paper = newspaper.build('http://www.culturacr.net/')
elguardian_paper = newspaper.build('http://elguardian.cr/')
laprensalibre_paper = newspaper.build('http://www.laprensalibre.cr/')
laprendalibre_paper = newspaper.build('https://laprendalibre.com/')
noticostarica_paper = newspaper.build('http://noticostarica.com/')
latejacr_paper = newspaper.build('http://www.lateja.cr/')

papers = [lanacion_paper, republica_paper]
news_pool.set(papers, threads_per_source=2)
news_pool.join()


csvfile = "enlaces.csv"
with open(csvfile, "a", newline='') as fp:
    for article in republica_paper.articles:
        csvRow = [article.url]
        wr = csv.writer(fp, dialect='excel')
        wr.writerow(csvRow)    
fp.close()    


# In[23]:


import newspaper

nacion_paper = newspaper.build('http://www.nacion.com//', memoize_articles=False)
print(nacion_paper.size())

for category in nacion_paper.category_urls():     
    print(category)

print(nacion_paper.brand)

for article in nacion_paper.articles:
   print(article.url)
    


# In[16]:


import pandas as pd
url = 'ListaEnlaces.json'

# Load the first sheet of the JSON file into a data frame
df = pd.read_json(url, orient='index')

for index, row in df.head(n=5).iterrows():
     url = row['url']
     print(url)


# In[ ]:




