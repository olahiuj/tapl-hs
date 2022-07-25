import Common
import Parser
import Bruijn

main = interact $ show . (run <$>) . (rewrite []) . getTerm . (runParser pTerm)