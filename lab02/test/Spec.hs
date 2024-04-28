import Test.DocTest

main :: IO()
main = do 
    doctest ["-isrc", "app/Main.hs"]