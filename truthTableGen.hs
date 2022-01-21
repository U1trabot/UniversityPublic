import System.Environment ( getArgs )
import Control.Monad ( replicateM )
import Data.Char (digitToInt)

main :: IO()
main = do args <- getArgs
          run args

gates :: [String]
gates = ["AND","OR","NAND","NOR","XOR","XNOR","NOT"]

charToBool :: Char -> Bool
charToBool x | digitToInt x == 0 = False
            | otherwise = True

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

boolToString :: Bool -> String
boolToString x| x = "1"
             | otherwise = "0"

operation :: String -> String -> (Bool,Bool)
operation s (i1:i2:is) | s == "AND"  = (charToBool i1 && charToBool i2,False)
                       | s == "OR"   = (charToBool i1 || charToBool i2,False)
                       | s == "NAND" = (not(charToBool i1 && charToBool i2),False)
                       | s == "NOR"  = (not(charToBool i1 || charToBool i2),False)
                       | s == "XOR"  = (xor (charToBool i1) (charToBool i2),False)
                       | s == "XNOR" = (not(xor (charToBool i1) (charToBool i2)),False)
                       | s == "NOT"  = (not(charToBool i1),True)
operation _ _  = error "Requires Gate and Inputs List"


splitInput :: [String] -> ([String],[String])
splitInput s = (filter (`elem` gates) s,replicateM (length(filter (not.(`elem` gates)) s)) "01")

combineInAndOut :: [String] -> [String] -> [String]
combineInAndOut (o:os) ((_:_:is):iss) = (o++is):combineInAndOut os iss
combineInAndOut o [] = o
combineInAndOut _ _ = error "CombineInAndOut Error"

tableGen :: ([String],[String]) -> [String]
tableGen ([],_) = []
tableGen (g,i) = concat x : tableGen (tail g,combineInAndOut x i)
                 where x = map ((boolToString . fst) . operation (head g)) i

(<~>) :: [a] -> [a] -> [[a]]
(<~>) (a:as) (b:bs) = [a,b]:as <~> bs
(<~>) [] [] = []
(<~>) [a] [] = [[a]]
(<~>) [] [b] = [[b]]
(<~>) _ _ = error "<~> Error"

tilda2 :: [[a]] -> [a] -> [[a]]
tilda2 (a:as) (b:bs) = (a ++ [b]):tilda2 as bs
tilda2 [] [] = []
tilda2 _ _ = error "tilda2 error"

tilda3 :: [[a]] -> [[a]] -> [[a]]
tilda3 (i:is) (x:xs) = (i++x):tilda3 is xs
tilda3 [] [] = []
tilda3 _ _ = [[]]

prepare :: [String] -> [String]
prepare s = tilda3 i (foldl tilda2 (head x <~> head(tail x)) (tail(tail x)))
      where x = tableGen(splitInput s)
            i = replicateM (length(filter (not.(`elem` gates)) s)) "01"

prettyPrint :: [String] -> IO ()
prettyPrint (z:zs) | null (tail z) = do putStrLn (head z : "     ")
                                        prettyPrint zs
                   | otherwise     = do putStr (head z: "     ")
                                        prettyPrint (tail z:zs)
prettyPrint _ = pure ()

top :: [String] -> IO ()
top (p:ps) = do putStr (p++"     ")
                top ps
top []     = putStrLn ""

run :: [String] -> IO ()
run s = do top (filter (not.(`elem` gates)) s++fst (splitInput s))
           prettyPrint (prepare s)