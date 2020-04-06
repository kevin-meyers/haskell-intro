main :: IO ()
main = putStrLn "help"

data Car =
  Car
    { company_of :: String
    , model_of :: String
    , year_of :: Int
    }
  deriving (Show)
