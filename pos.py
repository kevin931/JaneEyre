from nltk.tokenize import sent_tokenize, word_tokenize
from nltk import pos_tag
import numpy as np
import pandas as pd

def open_file(path):
    with open(path, 'r') as f:
        file = f.read()
        f.close()
        
    return file 

def pos(file):
    sentences = sent_tokenize(file)
    word_tags = []
    for sentence in sentences:
        tokens = word_tokenize(sentence)
        tags = pos_tag(tokens)
        word_tags.append(tags)

    return word_tags
        
def to_csv(pos_tags):
    words = []
    tags = []
    sent_num = []
    count = 1
    
    for sentence in pos_tags:
        
        sent_len = len(sentence)
        for _ in range(sent_len):
            sent_num.append(count)
        count += 1
        
        for pair in sentence: 
            word, pos = pair
            words.append(word)
            tags.append(pos)
            
    df = np.array([words, sent_num, tags])
    df = df.transpose()
    df = pd.DataFrame(df,
                      columns = ['word', 'sent_num', 'pos'])
    df.to_csv("data/janeeyre.csv", index = False)
        

if __name__ == "__main__":
    file = open_file("data/janeeyre.txt")
    tags = pos(file)
    to_csv(tags)