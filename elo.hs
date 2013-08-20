import System.Random
import System.IO
import Data.Map as M
import Data.List as L
import Data.Maybe (fromJust)


type EloDatabase  = Map String Float

--Where 'a' is the winner, 'b' is the loser and 's' is the outcome for a
updateScore :: Float -> String -> String -> EloDatabase -> EloDatabase
updateScore s a b eloBase = M.insert a ra' $ M.insert b rb' eloBase 
  where ra' = ra + kFactor*(s-ea) 
        rb' = rb + kFactor*((1-s)-eb)
        ea = qa / (qa + qb)
        eb = qb / (qa + qb)
        qa = 10**(ra/400)
        qb = 10**(rb/400)
        ra = fromJust $ M.lookup a eloBase
        rb = fromJust $ M.lookup b eloBase
        kFactor = 30

win :: String -> String -> EloDatabase -> EloDatabase
win = updateScore 1

draw :: String -> String -> EloDatabase -> EloDatabase
draw = updateScore 0.5 

strRanks :: EloDatabase -> String
strRanks elos = concatMap (\(x,y) -> x ++ " " ++ show y ++ "\n") $ L.reverse sortedElos
  where sortedElos = L.sortBy (\(_,x) (_,y) -> compare x y) $ M.toList elos

main =  do startScores <- getNames
           hSetBuffering stdin NoBuffering
           hSetBuffering stdout NoBuffering  
           elo <- ask startScores
           putStrLn $ strRanks elo

getNames :: IO EloDatabase
getNames = do s <- readFile "names.txt"
              return $ M.fromList $ L.map (\x-> (x,1200)) $ lines s

ask :: EloDatabase -> IO EloDatabase
ask m = do (a,b) <- rand2Elems m
           putStrLn $ strRanks m
           putStrLn $ "Do you like 1) " ++ a ++ " or 2) " ++ b ++".  You can also type d for draw. x will exit."
           choice <- getChar
           putStrLn "\n"
           case choice of 
             '1' -> ask $ win a b m 
             '2' -> ask $ win b a m
             'd' -> ask $ draw a b m
             'x' -> return m
             _   -> ask m

rand2Elems :: Map k a -> IO (k,k)
rand2Elems m = do r1 <- randomRIO (0,M.size m - 1)
                  r2 <- randomRIO (0,M.size m - 2)
                  let elem1 = fst $ M.elemAt r1 m
                  let elem2 = fst $ M.elemAt r2 $ M.deleteAt r1 m
                  return (elem1,elem2)
