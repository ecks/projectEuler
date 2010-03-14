import System.IO

main = do
	h <- openFile "num" ReadMode
        num <- hGetContents h
        print ((take 10 . show . sum) ((map read . lines) num))
        hClose h
