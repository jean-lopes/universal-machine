module Main
where
import Options.Applicative
import Data.Semigroup ((<>))
import UniversalMachine
import UniversalMachine.Parser

data Args = Args { tape :: String
                 , machine :: String }

args :: Parser Args
args = Args
    <$> strOption
        (  long    "tape"
        <> short   't'
        <> metavar "STRING"
        <> help    "A string representing the tape in which the universal machine will execute the algorithm." )
    <*> strOption
        (  long    "machine"
        <> short   'm'
        <> metavar "FILE"
        <> help    "The file with the machine data to be parsed.\nDSL: https://github.com/jean-lopes/calculator/tree/master/machine-examples/machine.dsl" )

exec :: Args -> IO ()
exec (Args ts mp) = do
    ms <- readFile mp
    case parse mp ms of
        (Left er) -> putStrLn er
        (Right m) -> do
            let um = mkUniversalMachine m ts 0
            case eval um of
                (Left err) -> putStrLn err
                (Right us) -> view us

main :: IO ()
main = exec =<< execParser opts
    where opts = info (args <**> helper)
            (  fullDesc
            <> progDesc "Shows the exectution of a universal machine based on the inputs (tape and machine data)"
            <> header   "universal-machine - simulation of a Turing machine" )