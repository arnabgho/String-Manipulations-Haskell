import String_Functions
import System.IO
import Data.Char
main =do
	in_h<- openFile "input.txt" ReadMode
	stopwords_h<- openFile "stopwords.txt" ReadMode
	str<- hGetContents in_h
	str_stopwords<- hGetContents stopwords_h
	let lower_str=processData str
	let stopwords= words str_stopwords
	let wordCount=getWordCount lower_str
	let distinctWords=getDistinctWords lower_str
	let freq=getFreq lower_str stopwords
	putStrLn (show wordCount)
	putStrLn (show distinctWords)
	putStrLn (show freq)
	hClose in_h
	hClose stopwords_h
	
processData:: String -> String
processData = map toLower
				
