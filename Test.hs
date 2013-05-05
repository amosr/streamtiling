import qualified Stream as S

main = print s

s = S.map (+1)
  $ S.map (*2)
  $ S.map snd
  $ S.filter (\x -> fst x `mod` 3 == 2)
  $ S.zip (S.gen id 1000) (S.gen (\x -> x*x) 200)

{-
s = S.map (+1)
  $ S.gen id 1000
  -}



{-
s = unstream' $ Map (mkF (+1)) $ FromList 
  $ unstream' $ Gen (mkF id) 1000

 -}

{-
s = S.map (+1)
  $ S.map (*2)
  $ S.map snd
  $ S.filter (\x -> fst x `mod` 3 == 2)
  $ S.zip (S.gen id 1000) (S.gen (\x -> x*x) 200)
-}
