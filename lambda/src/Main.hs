import Data.Maybe

import Common
import Parser
import Bruijn

rewriteClosedTerm = rewrite []
parseThenRun = (run <$>) . (fromJust . rewriteClosedTerm <$>) . (fst <$>) . (runParser pTerm)

strAdd = fromString "(λm.(λn.(λf.(λx.((m f) ((n f) x))))))"
strMul = fromString "(λm.(λn.(λf.(λx.((m (n f)) x)))))"
strPow = fromString "(λm.(λn.(λf.(λx.(((n m) f) x)))))"

main = interact $ show . parseThenRun

check:: Int -> String -> Bool
check n = (== n) . sum . map (\x -> if x == '1' then 1 else 0)