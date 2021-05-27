
from misc import *
import crypt
import re

def load_words(filename,regexp):
    """Load the words from the file filename that match the regular
       expression regexp.  Returns a list of matching words in the order
       they are in the file."""

    f = open(filename, 'r')
    p = re.compile(regexp)
    myList = []
    myStrArray = [line.strip() for line in f]
    for word in myStrArray:
        if(p.match(word)):
            myList.append(word)

    return myList 

def transform_reverse(str):
    """Return an array sontaining the original string and the reversed string
       String is reversed with slicing"""

    return [str,str[::-1]]

def digits_helper(c):
    """Helper function for generating digit-transformed passwords
       Maps characters to their possible transformations and returns
       an array of all valid replacements for the character (including untransformed)"""

    if c.lower() == "o":
        return [c, "0"]
    if c.lower() == "z":
        return [c, "2"]
    if c.lower() == "a":
        return [c, "4"]
    if c.lower() == "b":
        return [c, "6", "8"]
    if c.lower() == "i" or c.lower() == "l":
        return [c, "1"]
    if c.lower() == "e":
        return [c, "3"]
    if c.lower() == "s":
        return [c, "5"]
    if c.lower() == "t":
        return [c, "7"]
    if c.lower() == "g" or c.lower() == "q":
        return [c, "9"]

    return [c]

def transform_capitalize(str):
    """Return an array containing all possible ways to capitalize the string
       List comprehensions iteratively generate capitalization combinations"""

    myList = [""]
    for c in str:
        if c.lower() == c.upper():
            myList = [x + c for x in myList]
        else:
            myList = [x + c.lower() for x in myList] + [x + c.upper() for x in myList]

    return myList
    

def transform_digits(str):
    """Return an array containing all possible ways to transform the string by
       substituting characters for digits. Uses digits_helper to generate transformations."""
    myList = [""]
    for c in str:
        charArr = digits_helper(c)
        newList = []
        for x in charArr:
            newList = [s + x for s in myList] + newList

        myList = newList

    return myList
        
def transform_same(str):
    return [str]

def check_pass(plain,enc):
    """Check to see if the plaintext plain encrypts to the encrypted
       text enc"""

    salt = enc[:2]
    if crypt.crypt(plain, salt) == enc:
        return True

    return False

def load_passwd(filename):
    """Load the password file filename and returns a list of
       dictionaries with fields "account", "password", "UID", "GID",
       "GECOS", "directory", and "shell", each mapping to the
       corresponding field of the file."""

    f = open(filename, 'r')
    myArr = f.read().splitlines()
    myList = []
    for x in myArr:
        line = x.split(":")
        myList.append({"account": line[0], "password": line[1], "UID": int(line[2]), "GID": int(line[3]), "GECOS": line[4], "directory": line[5], "shell": line[6]})

    return myList

def find_pass(password_line, words, transform_fun):
    """Checks if the given password in password_line is an encryption of a word
       in words using the given transform_fun."""

    passwd = password_line["password"]
    user = password_line["account"]

    # Only compare to the reverse element, though transform_reverse also returns untransformed
    if transform_fun == transform_reverse:
        for word in words:
            transform = transform_fun(word)[1]
            if check_pass(transform, passwd):
                    return user + "=" + transform + "\n"

    for word in words:
        for transform in transform_fun(word):
            if check_pass(transform, passwd):
                return user + "=" + transform + "\n"

    return None

def transform_rev_cap(str):
    """Apply the reverse and capitalize transformations to str
       Returns an array contaning all ways to perform both transformations"""

    myCapList = transform_capitalize(str)
    myRevCapList = [transform_reverse(x) for x in myCapList]

    return [item for sublist in myRevCapList for item in sublist]

def transform_rev_digits(str):
    """Apply the reverse and digits transformations to str
       Returns an array containing all ways to perform both transformations"""

    myDigitsList = transform_digits(str)
    myRevDigitsList = [transform_reverse(x) for x in myDigitsList]

    return [item for sublist in myRevDigitsList for item in sublist]

def transform_cap_digits(str):
    """Apply the capitalize and digits transformations to str
       Returns an array containing all ways to perform both transformations"""

    myCapList = transform_capitalize(str)
    myCapDigitsList = [transform_digits(x) for x in myCapList]

    return [item for sublist in myCapDigitsList for item in sublist]

def run_checks(passList, wordsList, out, transform_fun):
    """Checks all passwords against all words using the given transformation function
       If a password is found, it is written to file "out"""

    newList = passList[:]
    for passwd_line in passList:
        pass_found = find_pass(passwd_line, wordsList, transform_fun)
        if pass_found:
            out.write(pass_found)
            out.flush()
            newList.remove(passwd_line)
            print("Cracked: " + pass_found)

    return newList

def crack_pass_file(pass_filename,words_filename,out_filename):
    """Crack as many passwords in file fn_pass as possible using words
       in the file words"""

    wordsList = load_words(words_filename, "\w")
    passList = load_passwd(pass_filename)
    out = open(out_filename, "w")

    # Check for unstransformed passwords
    passList = run_checks(passList, wordsList, out, transform_same)

    # Check for reversed passwords
    passList = run_checks(passList, wordsList, out, transform_reverse)

    # Check for digit-transformed passwords
    passList = run_checks(passList, wordsList, out, transform_digits)

    # Check for capitalized passwords
    passList = run_checks(passList, wordsList, out, transform_capitalize)

    # Check for reversed and digit-transformed passwords
    passList = run_checks(passList, wordsList, out, transform_rev_digits)

    # Check for reversed and capitalized passwords
    passList = run_checks(passList, wordsList, out, transform_rev_cap)
    
    # Check for digit-transformed and capitalized passwords
    passList = run_checks(passList, wordsList, out, transform_cap_digits)
    

