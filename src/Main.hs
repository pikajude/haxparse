import qualified Data.ByteString.Lazy   as LB
import           HaxParse.Parser
import           HaxParse.Options
import           HaxParse.Output
import           Options.Applicative
import           System.Exit
import           System.IO

main :: IO ()
main = do opts <- execParser fullOpts
          res <- parseFile $ file opts
          let newOpts = if null (eventTypes opts) then opts else opts { showEvents = True }
          case res of Left m -> do hPutStrLn stderr $ "Parsing failed: " ++ show m
                                   exitFailure
                      Right s -> outputWith newOpts s
