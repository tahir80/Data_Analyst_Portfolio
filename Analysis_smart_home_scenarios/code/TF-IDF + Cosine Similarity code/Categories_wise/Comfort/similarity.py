import pandas as pd
from sklearn.metrics.pairwise import linear_kernel
from sklearn.feature_extraction.text import TfidfVectorizer


#Reading Excel file containing data
file_path = 'comfort.xlsx'
data= pd.read_excel(file_path)
#print(data.columns)    #to make sure data is read correctly
 
corpus = []     #corpus containing bag of words

#For each entry in the file, will append its data to corpus
for i in range(len(data)):
    corpus.append(data['Scenario'].iloc[i])     
    
#Apply TF-IDF on corpus data 
#build a TF-IDF matrix for each scenario:
tf = TfidfVectorizer(analyzer='word', ngram_range=(1,5), min_df = 0, stop_words = 'english')
tfidf_matrix =  tf.fit_transform([content for content in corpus])

final = pd.DataFrame(pd.DataFrame(corpus))  #Final DataFrame holding results
averages =[]

for index in range(len(data)):
    #Calculate cosine similarity between selected comment and others
    cosine_similarities = linear_kernel(tfidf_matrix[index:index+1], tfidf_matrix).flatten()
    
    #Calculating average similarity (with removing 1's from sum and from count)
    avg_similarity = (sum(cosine_similarities)-1)/(len(cosine_similarities)-1)
    averages.append(avg_similarity)
    
    cosine_similarities_ = pd.DataFrame(cosine_similarities) #convert it to dataFrame to append it to final DataFrame
    final = pd.concat([final, cosine_similarities_], axis=1) #appending to final


final = pd.concat([final, pd.DataFrame(averages)], axis=1) #adding averages

final = final.round(4)  #rounding floating-point numbers to 4 
#final.to_csv('foo.csv', index = False, header=False)     #Writing results to csv file 

final.to_excel('comfort_results.xlsx', index = False, header=False)