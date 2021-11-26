module Language.Eberban.Parser.Mercury.Samples where

import Prelude hiding (Word)
import Language.Eberban.Parser.Mercury

displayText :: String -> IO ()
displayText input = do
    putStrLn $ concat ["Input text: ", if '\n' `elem` input then "\n" else "", input]
    let parsingOutput = case (parseText input) of
            Left msg -> "FAILURE!"
            Right tree -> show tree
    putStrLn $ "Parsing output: " ++ parsingOutput

peanoAxioms :: String
peanoAxioms =
    "\
     \  pai mai\n\
     \    ve pi bo igin\n\
     \    fe pi bo igini\n\
     \    fe pi bo ki ma igin\n\
     \    fe pi mae\n\
     \      ve ke pi zoblo\n\
     \        ve ke igin\n\
     \        fa zohin\n\
     \          ve igin\n\
     \          fa igini ke\n\
     \        vei\n\
     \      vei\n\
     \    fe pi mae\n\
     \      ve ke pi zoblo\n\
     \        ve ke igin bi ki\n\
     \        fa zohin\n\
     \          ve igin\n\
     \          fa sae igini ke\n\
     \        vei\n\
     \      vei\n\
     \    fe pi mae\n\
     \      ve ke pi zoblo\n\
     \        ve ke igin bi ki\n\
     \        fa ki kidvo\n\
     \          va ke\n\
     \          fo igini\n\
     \          vei\n\
     \        vei\n\
     \      vei\n\
     \    fe pi bi igin sae igini ki\n\
     \  po gin igin\n\
     \  po gini igini\n\
     \  po ginin ki\n"

displaySamples :: IO ()
displaySamples = do
    displayText "mi don eberban"
    displayText peanoAxioms
