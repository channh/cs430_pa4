--
-- CS 430 Spring 2019 P4 (Haskell 2)
--

module Main where
import Parser
import P4

testEval =
  [(eval $ parse "1")     == 1,
   (eval $ parse "1+2")   == 3]

testCountOps =
  [(countOps $ parse "1+2")         == 1]

testHeight =
  [(height $ parse "1")               == 1]

testPostfix =
  [(postfix $ parse "1+2")       == "1 2 +"]

testUniqInts =
  [(uniqInts $ parse "1+2")       == [1,2]]

testAll = testEval ++ testCountOps ++ testHeight ++ testPostfix ++ testUniqInts

countCorrect []         = 0
countCorrect (True:xs)  = 1 + (countCorrect xs)
countCorrect (False:xs) =     (countCorrect xs)

buildRatioStr results =
  let correct = countCorrect results
      total   = length results
  in (show correct) ++ "/" ++ (show total)

calcRatio results =
  let correct = countCorrect results
      total   = length results
  in truncate $ (fromIntegral correct) / (fromIntegral total) * 100.0

summarize results =
  let key = map (\t -> if t then '.' else 'X') results
  in (buildRatioStr results) ++ "   " ++ key

main = do
  let allTestResults = testAll
  let result = "  eval:      " ++ (summarize testEval)     ++ "\n" ++
               "  countOps:  " ++ (summarize testCountOps) ++ "\n" ++
               "  height:    " ++ (summarize testHeight)   ++ "\n" ++
               "  postfix:   " ++ (summarize testPostfix)  ++ "\n" ++
               "  uniqInts:  " ++ (summarize testUniqInts) ++ "\n" ++
               "  TOTAL:     " ++ (buildRatioStr allTestResults)     ++ "\n" ++
               "  " ++ (show $ calcRatio allTestResults) ++ "% passed"
  putStrLn $ "\n" ++ result

