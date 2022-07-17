module Learn (doit) where

data Segment = Segment {left::Int, right::Int, cross::Int}

segments = [50,10,30, 5,90,20, 40,2,25, 10,8,0]

lPath :: [Int] -> Int
lPath [] = 0
lPath (l:r:c:[]) = l
lPath (l:r:c:ss) = min (l + lPath ss) (l + c + rPath ss)

rPath :: [Int] -> Int
rPath [] = 0
rPath (l:r:c:[]) = r
rPath (l:r:c:ss) = min (r + rPath ss) (r + c + lPath ss)

minPath :: [Int] -> Int
minPath ss = min (lPath ss) (rPath ss)

-- minPath :: [Int] -> Either Int Int
-- minPath [] = Left 0
-- minPath [l, r, c] = if l < r then Left l else Right r
-- minPath [l, r, c]:ss =
--     where mp = minPath ss

doit :: IO ()
doit = do
    -- print (minPath segments)
    print (minPath segments)
    return ()