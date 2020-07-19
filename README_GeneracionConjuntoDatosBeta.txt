El proceso para la generación de las variables cuantitativas para los conjuntos de datos en Inglés y Español empleando la herramienta LIWC requirió
diferentes etapas, luego de realizar la extracción de contenido "crudo" se procedió a hacer uso seleccionar a seleccionar los atributos a emplear en la investigación, y se realizó la traducción
del título y texto de la noticia empleando la función Translate de Google, el resultado de este proceso es el archivo RawData_Espannol_Ingles_09222019.csv el cual contiene los siguientes atributos
URLNoticia;Fuente;Clasificacion;MedioPeriodistico;Titulo;Texto;TipoSitio;TituloIngles;TextoIngles.

Posterior a esto se realizó el preprocesamiento de los datos mediante el archivo "Evaluación_de_la_precisión_de_los_modelos_de_clasificación_binaria_para_la_identificación_de_noticias_verdaderas_o_falsas_en_Costa_Rica_Preprocesamiento_del_texto.ipynb"
el cual es un notebook de Python ejecutado en la herramienta Google Collab y a través del cual se usan librerias como Pandas, nltk para la realización de la tokenización del texto, así comoel uso de la librería spacy para la utilización de los diccionarios en español,
el resultado de este proceso es el archivo ConjuntoDatosPreprocesados_20190922_lematizado.xlsx, el cual contiene las siguientes atributos: URLNoticia,Fuente,Clasificacion,MedioPeriodistico,Titulo,Texto,TipoSitio,TituloIngles,TextoIngles,textoLematizadoIngles,textoLematizado,textoTokenizado,textoTokenizadoIngles,textoPreprocesadoIngles,textoPreprocesado

Para la generación de las variables cuantitativas empleando la herramienta LIWC, se procedió a configurar la herramienta primeramente con el diccionario en español LIWC2007
y se procedió a seguir los pasos descritos en el "Manual de Operador de LIWC" https://s3-us-west-2.amazonaws.com/downloads.liwc.net/LIWC2015_OperatorManual.pdf
haciendo uso del archivo "ConjuntoDatosPreprocesados_20190922_lematizado.xlsx" y seleccionando la columna textoPreprocesado, el resultado de exportar el resultado de esta operación
es el archivo "LIWC2007_ResultsSPA.xlsx". Para el caso del conjunto de datos en inglés, se procedió a configurar LIWC usando el diccionario LIWC2015 y de igual manera se hizo uso del
archivo "ConjuntoDatosPreprocesados_20190922_lematizado.xlsx" y seleccionando la columna textoPreprocesado, el resultado de exportar el resultado de esta operación es
"LIWC2015_ResultsENG.xlsx".

Finalmente ambos conjuntos de datos fueron utilizados por el archivo de R, llamado "AplicacionModelosMineria.R", el cual describe las diferentes fases de transformación
y análisis aplicado.
