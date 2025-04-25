import System.Random
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as Bin


main :: IO ()
main = do
    gen <- getStdGen
    let randomInts = take 50 (randomRs (1,100) gen) :: [Int]
        (l,r) = splitAt (length randomInts `div` 2) randomInts
    BL.writeFile "/shared/left/left.bin" (Bin.encode l)
    BL.writeFile "/shared/right/right.bin" (Bin.encode r)
