import qualified Data.ByteString.Lazy   as LB
import           Data.Maybe
import           HaxParse.Parser
import           HaxParse.Options
import           HaxParse.Output
import           Options.Applicative
import           System.Exit
import           System.IO

main :: IO ()
main = do opts <- execParser fullOpts
          res <- parseFile $ file opts
          case res of Left m -> do hPutStrLn stderr $ "Parsing failed: " ++ show m
                                   exitFailure
                      Right s -> outputWith (fromMaybe Plain $ outputType opts) s
