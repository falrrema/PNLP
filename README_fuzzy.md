# Fuzzy Features

Fuzzy string matching like a boss. It uses Levenshtein Distance to calculate the differences between sequences in a simple-to-use package.

In information theory and computer science, the *Levenshtein distance* is a string metric for measuring the difference between two sequences. Informally, the Levenshtein distance between two words is the minimum number of single-character edits (**insertions, deletions or substitutions**) required to change one word into the other.  

· fuzz.ratio = The simplest way to compare two strings is with a measurement of edit distance with Levenshtein distance
· fuzz.partial_ratio = Return the ratio of the most similar substring as a number between 0 and 100.
· fuzz.partial_token_sort_ratio = Return the ratio of the most similar substring as a number between 0 and 100 but sorting the token before comparing
· fuzz.partial_token_set_ratio = Includes:
	- treat them as a set
        - construct two strings of the form:
            <sorted_intersection><sorted_remainder>
        - take ratios of those two strings
        - controls for unordered partial matches
· When its not partial is exact. 
· fuzz.Qratio: It does preprocessing before:
        -- removing all but letters and numbers
        -- trim whitespace
        -- force to lower case
        if force_ascii == True, force convert to ascii
        Short circuits if either of the strings is empty after processing (devuelve cero)
· fuzz.UQratio = Unicode quick ratio Calls QRatio with force_ascii set to False
· fuzz.Wratio =  Return a measure of the sequences' similarity between 0 and 100, using different algorithms:

Run full_process from utils on both strings
    - Short circuit if this makes either string empty
    - Take the ratio of the two processed strings (fuzz.ratio)
    - Run checks to compare the length of the strings
        * If one of the strings is more than 1.5 times as long as the other
          use partial_ratio comparisons - scale partial results by 0.9
          (this makes sure only full results can return 100)
        * If one of the strings is over 8 times as long as the other
          instead scale by 0.6
    - Run the other ratio functions
        * if using partial ratio functions call partial_ratio,
          partial_token_sort_ratio and partial_token_set_ratio
          scale all of these by the ratio based on length
        * otherwise call token_sort_ratio and token_set_ratio
        * all token based comparisons are scaled by 0.95
          (on top of any partial scalars)
    - Take the highest value from these results
       round it and return it as an integer.

· fuzz.UWRatio = Return a measure of the sequences' similarity between 0 and 100, using different algorithms. Same as WRatio but preserving unicode