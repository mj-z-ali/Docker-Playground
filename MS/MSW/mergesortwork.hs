import System.Environment (getArgs)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as Bin


mergeSort :: Ord a => [a] -> [a]
mergeSort arr 
    | length arr < 2 = arr
    | otherwise = merge (mergeSort l) (mergeSort r)
    where 
        (l,r) = splitAt (length arr `div` 2) arr

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge allX@(x:xs) allY@(y:ys)
    | x <= y = x:merge xs allY
    | otherwise = y: merge allX ys

main :: IO ()
main = do
    [inputPath] <- getArgs
    strictBytes <- BS.readFile inputPath -- entire handle, closed
    let arr = Bin.decode (BL.fromStrict strictBytes) :: [Int]
        sorted = mergeSort arr
    print sorted
    BL.writeFile inputPath (Bin.encode sorted)

