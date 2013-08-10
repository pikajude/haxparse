import qualified Data.ByteString.Lazy   as LB
import           Data.Maybe
import           HaxParse.Parser
import           HaxParse.Options
import           HaxParse.Output
import           Options.Applicative

main :: IO ()
main = do opts <- execParser fullOpts
          res <- parseFile $ file opts
          outputWith (fromMaybe Plain $ outputType opts) res
