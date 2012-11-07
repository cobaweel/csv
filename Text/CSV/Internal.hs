module Text.CSV.Internal (SV
                 , Record
                 , Field
                 , sv
                 , record
                 , field
                 , quotedField
                 , parseSV
                 , parseSVFromFile
                 , parseSVTest
                 , printSV
                 ) where

import Text.ParserCombinators.Parsec
import Data.List (intersperse)

-- | A CSV file is a series of records. According to the RFC, the
-- records all have to have the same length. As an extension, I
-- allow variable length records.
type SV = [Record]

-- | A record is a series of fields
type Record = [Field]

-- | A field is a string
type Field = String

-- | A Parsec parser for parsing CSV files
sv :: Char -> Parser SV
sv c = do
        x <- (record c) `sepEndBy` many1 (oneOf "\n\r")
        eof
        return x

record :: Char -> Parser Record
record c = (quotedField <|> field c) `sepBy` char c

field :: Char -> Parser Field
field c = many (noneOf (c:"\n\r\""))

quotedField :: Parser Field
quotedField = between (char '"') (char '"') $ many (noneOf "\"" <|> try (string "\"\"" >> return '"'))

-- | Given a file name (used only for error messages) and a string to
-- parse, run the parser.
parseSV :: Char -> FilePath -> String -> Either ParseError SV
parseSV c = parse $ sv c

-- | Given a file name, read from that file and run the parser
parseSVFromFile :: Char -> FilePath -> IO (Either ParseError SV)
parseSVFromFile c = parseFromFile $ sv c

-- | Given a string, run the parser, and print the result on stdout.
parseSVTest :: Char -> String -> IO ()
parseSVTest c = parseTest $ sv c

-- | Given an object of type CSV, generate a CSV formatted
-- string. Always uses escaped fields.
printSV :: Char -> SV -> String
printSV c records = unlines (printRecord `map` records)
    where printRecord = concat . intersperse [c] . map printField
          printField f = "\"" ++ concatMap escape f ++ "\""
          escape '"' = "\"\""
          escape x = [x]
          unlines = concat . intersperse "\n"

