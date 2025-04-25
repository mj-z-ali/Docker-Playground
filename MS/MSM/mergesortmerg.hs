import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as Bin


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge allX@(x:xs) allY@(y:ys)
    | x <= y = x:merge xs allY
    | otherwise = y: merge allX ys

main :: IO ()
main = do

    leftCont <- BL.readFile "/shared/left/left.bin"
    rightCont <- BL.readFile "/shared/right/right.bin"

    let (left, right) = (Bin.decode leftCont :: [Int], Bin.decode rightCont :: [Int])
        merged = merge left right
    
    print $ merged