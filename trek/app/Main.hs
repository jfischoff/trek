import Database.Trek.Run
import Options.Generic

main :: IO ()
main = getRecord "trek" >>= eval
