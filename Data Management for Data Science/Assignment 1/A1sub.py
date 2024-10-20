import csv
import numpy as np

from typing import List, Dict, Tuple

### BEGIN SOLUTION PROBLEM 1

def reverse_sorted_words(filename: str) -> List[str]:
    """
    This function will take in a filename containing strings and return a list of sorted words alphabetically.
    """
    lines = []
    for line in open(filename):
        print(line)
        words = line.split()
        lines.extend(words)
    
    return lines

### BEGIN SOLUTION PROBLEM 2


def reformat_student_info(filename: str, outputfilename: str) -> None:
    pass

### BEGIN SOLUTION PROBLEM 3

def getDailySongStreamingData(filename: str) -> Dict[str, List[str]]:
    pass

### BEGIN SOLUTION PROBLEM 4

def getTopSongsInfo(filename: str) -> Dict[str, List[str]]:
    pass

### BEGIN SOLUTION PROBLEM 5

def getTopArtistsInfo(song_dict: Dict[str, List[str]]) -> Dict[str, List[str]]:
    pass


### BEGIN SOLUTION PROBLEM 6

def getTopArtistsPop(song_pop_dict: Dict[str, List[str]], artist_dict: Dict[str, List[str]]) -> Dict[str, float]:
    pass


### BEGIN SOLUTION PROBLEM 7

def TopSongsPerSeason(filename: str) -> Dict[str, List[Tuple[str, str, float]]]:
    pass


if __name__ == "__main__":
    reverse_sorted_words("words1.txt")
        
    
    

