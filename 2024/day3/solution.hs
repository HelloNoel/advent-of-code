import Text.ParserCombinators.Parsec

part1 :: FilePath -> IO ()
part1 p = printFromFile (parse program p) p

part2 :: FilePath -> IO ()
part2 p = printFromFile (parse program2 p) p

printFromFile :: (String -> Either ParseError Int) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  case f content of
    Right val -> print val
    Left err -> print err

program :: GenParser Char st Int
program = do
  xs <- many $ multiplication <|> garbage
  return $ sum xs

multiplication :: GenParser Char st Int
multiplication = try $ do
  string "mul("
  x <- number
  char ','
  y <- number
  char ')'
  return $ x * y

garbage :: GenParser Char st Int
garbage = do
  anyChar
  return 0

number :: GenParser Char st Int
number =
  do
    x <- try $ count 3 digit
    return $ read x
    <|> do
      x <- try $ count 2 digit
      return $ read x
    <|> do
      x <- digit
      return $ read [x]

dontDo :: GenParser Char st Int
dontDo = do
  try $ string "don't()"
  manyTill anyChar $ try $ string "do()"
  return 0

program2 :: GenParser Char st Int
program2 = do
  xs <- many $ dontDo <|> multiplication <|> garbage
  return $ sum xs
