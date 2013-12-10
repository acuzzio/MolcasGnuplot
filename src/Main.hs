import System.ShQQ
import System.Process
import Data.List
import Control.Monad
import Text.Parsec
import Text.ParserCombinators.Parsec (parseFromFile)
import Data.Functor.Identity

-- to TRY parsers in ghci
-- runParser (manyTill anyChar (try $ string "a")) () "" "vcascdacaca"

type MyParser a b  = ParsecT [Char] a Identity b

data PlotType = Pop | Ene | Dyn deriving (Eq,Show)

main = do
       a <- readShell "ls *.out"
       let files = lines a
       mapM_ graficami files

graficami file = do 
       system $ ("grep -A5 OOLgnuplt: " ++ file ++ " > ciao")
       a <- readShell "head ciao"
       case a of
            "" -> print $ "File " ++ file ++ " is not a Tully Molcas Output"
            otherwise -> do
                         i <- parseFromFile (many1 $ try parseGnuplot) "ciao"
                         case i of 
                           Left msg -> print msg
                           Right content -> do
                                            dt <- readShell $ "grep \"Time Step is:\" " ++ file ++" | head -1 | awk '{print $4}'"
                                            writeGnuplots dt file content
                                            system "gnuplot < gnuplotScript"
                                            system "rm sdafrffile* gnuplotScript ciao"
                                            print "done"


writeGnuplots :: String -> FilePath -> [String] -> IO()
writeGnuplots dt file xss = do
      let values    = transpose $ map words xss
          rlxRoot   = findRlxRT values
          valuesS   = map unlines values
          filenames = map (\x -> "sdafrffile" ++ (show x)) [1..]
          lengthV   = length values
--      print values
      zipWithM writeFile filenames valuesS     
      createGnuplotFile file dt lengthV rlxRoot

createGnuplotFile :: FilePath -> String -> Int -> Int -> IO()
createGnuplotFile file dt n rlxRt = do 
      let fileZ  = takeWhile (/='.') file
          hexColo= ["#E5E5E5","#778899","#A9A9A9","#5F9EA0","#778899","#B0C4DE"]
          colors = ["blue","red","green","yellow","blueviolet","darkgoldenrod"]
          tag    = map (\x -> "S" ++ (show x)) [0..]
          header = "set title \"" ++ fileZ ++ " Population and Energies\"\nset xlabel \"fs\"\nset format y \"%6.3f\"\nset y2range[0:1.001]\nset output '" ++ fileZ ++ "EnergiesPopulation.png'\nset terminal pngcairo size 1024,630 enhanced font \", 15\"\nplot " 
          states = div (n-1) 2
          filenames = map (\x -> "sdafrffile" ++ (show x)) [1..]
          list   = (take states $ repeat Pop) ++ (take states $ repeat Ene) ++ [Dyn]
          removerlXrootPopu = take rlxRt list ++ drop (succ rlxRt) list
          removerlXtootfilename = take rlxRt filenames ++ drop (succ rlxRt) filenames
          groupZ = group removerlXrootPopu
          jen x  = case head x of
                    Pop -> zip3 x hexColo tag
                    Ene -> zip3 x colors tag
                    Dyn -> zip3 x hexColo tag --hexColo tag does not matter
          lol    = concat $ map jen groupZ -- lol :: [(PlotType, String, String)]
          almost = zipWith (\x y -> createPlotLine x y dt) lol removerlXtootfilename
          secondPart = concat almost
          wholeFile  = header ++ secondPart
      writeFile "gnuplotScript" wholeFile

createPlotLine :: (PlotType, String, String) -> FilePath -> String -> String
createPlotLine (Pop,c,d) b dt = "\"" ++ b ++ "\"" ++ " u ($0*" ++ (fromAUtoFemtoDT dt) ++ "):1 axes x1y2 w filledcurves x1 lt 1 lc rgb " ++ "\"" ++ c ++ "\"" ++ " t '" ++ d ++ " Population',"
createPlotLine (Ene,c,d) b dt = "\"" ++ b ++ "\"" ++ " u ($0*" ++ (fromAUtoFemtoDT dt) ++ "):1 w lines linecolor rgb " ++ "\"" ++ c ++ "\"" ++ " t " ++ "\"" ++ d ++ "\","
createPlotLine (Dyn,c,d) b dt = "\"" ++ b ++ "\"" ++ " u ($0*" ++ (fromAUtoFemtoDT dt) ++ "):1 w lines linecolor rgb \"black\" lt \"dashed\" lw 3 t \"RlxRoot\""

fromAUtoFemtoDT :: String -> String
fromAUtoFemtoDT dt = let read2 x = read x :: Double
                     in show ((read2 dt) / 41.34144728138643)

parseGnuplot :: MyParser st String
parseGnuplot = do
      manyTill anyChar (try $ string "OOLgnuplt:")
      a <- manyTill anyChar $ (try $ string "Memory") <|> (try $ string "HOP")
      return a

findRlxRT a = let len           = length a
                  states        = div (len-1) 2
                  (initA,tailA) = (init a, last a)
                  c             = zip [0..] initA
                  currentState  = fst . head $ filter (\x -> (head $ snd x) == (head tailA)) c
              in (currentState - states)


