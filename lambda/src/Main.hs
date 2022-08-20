import Data.Maybe (fromJust)

import Common
import Parser
import Bruijn

rewriteClosedTerm:: Term' -> Maybe Term
rewriteClosedTerm = rewrite []

parseThenRun:: String -> Either (Err Char) Term
parseThenRun = (run <$>) . (fromJust . rewriteClosedTerm <$>) . (fst <$>) . runParser pTerm

strAdd:: Term'
strAdd = fromString "(λm.(λn.(λf.(λx.((m f) ((n f) x))))))"

strMul:: Term'
strMul = fromString "(λm.(λn.(λf.(λx.((m (n f)) x)))))"

strPow:: Term'
strPow = fromString "(λm.(λn.(λf.(λx.(((n m) f) x)))))"

main:: IO ()
main = interact $ show . parseThenRun

check:: Int -> String -> Bool
check n = (== n) . sum . map (\x -> if x == '1' then 1 else 0)
