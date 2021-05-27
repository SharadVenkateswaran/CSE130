#PA 4

import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned."""
    if len(l) == 0:
        return None

    closest = l[0]
    for item in l:
        if abs((v - item)) < abs(v - closest):
            closest = item

    return closest

def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""
    mydict = {}
    for i in range(len(keys)):
         mydict[keys[i]] = values[i]

    return mydict
# file IO functions
def word_count(fn):
    """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""

    f = open(fn, 'r')
    myDict = {}
    for line in f:
        myStrArr = re.split("\W", line)
        for word in myStrArr:
            if word == '':
                continue
            if word.lower() in myDict:
                myDict[word.lower()] = myDict[word.lower()] + 1
            else:
                myDict[word.lower()] = 1

    return myDict 
