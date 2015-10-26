module String_Functions  
( getWordCount,
  getDistinctWords,
  getFreq
) where  

getWordCount::String->Int
getWordCount = length . words


quickSort::Ord a => [a] -> [a]
quickSort []=[]
quickSort (p:xs)=(quickSort lesser) ++ [p] ++ (quickSort greater)
    where 
    	lesser=filter (<p) xs
    	greater=filter (>=p) xs


removeConsecutiveDuplicatesHelper::(Eq a)=>a->[a]->[a]
removeConsecutiveDuplicatesHelper cur [] = []
removeConsecutiveDuplicatesHelper cur (x:xs) = if x==cur then removeConsecutiveDuplicatesHelper cur xs
else [x]++removeConsecutiveDuplicatesHelper x xs

removeConsecutiveDuplicates::(Eq a)=>[a]->[a]
removeConsecutiveDuplicates []=[]
removeConsecutiveDuplicates (x:xs) = [x] ++ removeConsecutiveDuplicatesHelper x xs
 

getDistinctWords::String->[String]
getDistinctWords = removeConsecutiveDuplicates . quickSort . words

list_stopwords=["a","an","the","who"]

isStopword::(Eq a)=>a->[a]->Bool
isStopword word []=False
isStopword word (x:xs)= if x==word then True
			else isStopword word xs

removeStopwords::(Eq a)=>[a]->[a]->[a]
removeStopwords [] stopwords=[]
removeStopwords (x:xs) stopwords=if (isStopword x stopwords ) == True then removeStopwords xs stopwords
				 else [x]++ (removeStopwords xs stopwords)


getFreqWord::(Eq a)=>a->[a]->Int
getFreqWord word list = length .  filter (==word) $ list

getFreqList::(Eq a)=>[a]->[a]->[(a,Int)]
getFreqList []  list=[]
getFreqList (x:xs) list=[(x,getFreqWord x list)] ++ getFreqList xs list

getFreq::String->[String]->[(String,Int)]
getFreq str stopwords= getFreqList ( removeStopwords ( getDistinctWords str) stopwords
	)  (words str)
