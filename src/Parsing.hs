module Parsing (parseProto, ParseResult (..), ParsingError (..)) where

import qualified Ast as A
import Control.Monad.Writer
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Pos (SourcePos, unPos)

-- TODO remove parse prefix?

type ParseResult = (A.ProtoFile, [A.Comment])

type ParsingError = String

type Parser = ParsecT Void Text (Writer [A.Comment])

loc :: Parser A.Loc
loc = do
  pos <- getSourcePos
  return $ A.Loc (unPos . sourceLine $ pos) (unPos . sourceColumn $ pos)

-- NOTE: this only works if we never backtrack whitespace...
-- backtracking whitespace will break everything!!!
lineComment :: Parser ()
lineComment = do
  l <- loc
  content <- string "//" *> (anySingleBut '\n' `manyTill` end)
  lift $ tell [A.Comment (T.pack ("//" ++ content)) l]
  where
    end = void eol <|> eof

blockComment :: Parser ()
blockComment = do
  l <- loc
  content <- string "/*" *> manyTill L.charLiteral (string "*/")
  lift $ tell [A.Comment (T.concat ["/*", T.pack content, "*/"]) l]

sc :: Parser ()
sc = L.space space1 lineComment blockComment

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

signedInteger :: Parser Int
signedInteger = L.signed sc integer

semicolons :: Parser ()
semicolons = void $ some (symbol ";")

stringLiteral :: Parser Text
stringLiteral = T.pack <$> lexeme (try doubleQuotes <|> singleQuotes)
  where
    doubleQuotes = char '"' >> manyTill L.charLiteral (char '"')
    singleQuotes = char '\'' >> manyTill L.charLiteral (char '\'')

parseConstant :: Parser A.Constant
parseConstant = stringLit <|> wordLit <|> numLit
  where
    stringLit = A.CStringLiteral <$> stringLiteral
    wordLit = A.COtherLiteral <$> parseFullIdent
    numLit = A.COtherLiteral . T.pack <$> lexeme (some (choice [alphaNumChar, char '.', char '+', char '-']))

-- TODO can we do this better?
fieldOptions :: Parser [A.FieldOption]
fieldOptions = [] <$ lookAhead (char ';') <|> between (symbol "[") (symbol "]") ((:) <$> singleFieldOption <*> multipleFieldOptions)
  where
    singleFieldOption :: Parser A.FieldOption
    singleFieldOption = A.FieldOption <$> (parseOptionName <* symbol "=") <*> parseConstant
    multipleFieldOptions :: Parser [A.FieldOption]
    multipleFieldOptions = ([] <$ lookAhead (char ']')) <|> symbol "," *> singleFieldOption `sepBy1` symbol ","

parseCompoundElement :: Text -> Parser a -> Parser (Text, [a], A.Loc, A.Loc)
parseCompoundElement name elementParser = do
  l <- loc
  symbol name
  n <- parseIdent
  symbol "{"
  many $ symbol ";"
  elements <- many elementParser
  endLoc <- loc
  symbol "}"
  many $ symbol ";"
  return (n, elements, l, endLoc)

parseEnum :: Parser A.Enumeration
parseEnum = do
  (name, elements, begin, end) <- parseCompoundElement "enum" enumElement
  return $ A.Enumeration name elements begin end
  where
    enumElement :: Parser A.EnumElement
    enumElement = (A.EEOption <$> parseOption) <|> (A.EEField <$> enumField)
    enumField :: Parser A.EnumField
    enumField = do
      l <- loc
      name <- parseIdent
      symbol "="
      number <- signedInteger
      os <- fieldOptions
      semicolons
      return $ A.EnumField name number os l

parseTopLevelEnum :: Parser A.TopLevelElement
parseTopLevelEnum = A.TLEEnum <$> parseEnum

parseType :: Parser Text
parseType = lexeme $ T.pack <$> some (alphaNumChar <|> char '.')

parseIntLit :: Parser Text
parseIntLit = lexeme $ T.pack <$> some (digitChar <|> octDigitChar <|> hexDigitChar <|> char 'X' <|> char 'x')

parseMessage :: Parser A.Message
parseMessage = do
  (name, elements, begin, end) <- parseCompoundElement "message" messageElement
  return $ A.Message name elements begin end
  where
    messageElement :: Parser A.MessageElement
    messageElement = choice [A.MEMessage <$> parseMessage, A.MEEnum <$> parseEnum, A.MEOption <$> parseOption <|> A.MEOneOfField <$> oneOfField <|> A.MEMapField <$> mapField, A.MEReservedStatement <$> reservedStatement, A.MENormalField <$> normalField]
    normalField :: Parser A.NormalField
    normalField = do
      l <- loc
      repeated <- optional $ symbol "repeated"
      fieldType <- parseType
      (name, number, opts) <- fieldSuffix
      return $ A.NormalField (isJust repeated) fieldType name number opts l
    oneOfField :: Parser A.OneOfField
    oneOfField = do
      (name, elements, begin, end) <- parseCompoundElement "oneof" oneOfFieldElement
      return $ A.OneOfField name elements begin end
    oneOfFieldElement :: Parser A.OneOfFieldElement
    oneOfFieldElement = A.OFEOption <$> parseOption <|> A.OFEVariant <$> oneOfVariant
    oneOfVariant :: Parser A.OneOfVariant
    oneOfVariant = do
      l <- loc
      fieldType <- parseType
      (name, number, opts) <- fieldSuffix
      return $ A.OneOfVariant name fieldType number opts l
    mapField :: Parser A.MapField
    mapField = do
      l <- loc
      symbol "map"
      symbol "<"
      ktype <- parseType
      symbol ","
      vtype <- parseType
      symbol ">"
      (name, number, opts) <- fieldSuffix
      return $ A.MapField ktype vtype name number opts l
    fieldSuffix :: Parser (Text, Text, [A.FieldOption])
    fieldSuffix = do
      name <- parseIdent
      symbol "="
      number <- parseIntLit
      opts <- fieldOptions
      semicolons
      return (name, number, opts)
    reservedStatement :: Parser A.ReservedStatement
    reservedStatement = do
      l <- loc
      symbol "reserved"
      elements <- reservedField `sepBy1` symbol ","
      semicolons
      return $ A.ReservedStatement elements l
    reservedField :: Parser A.ReservedField
    reservedField = (A.RFNumberSingle <$> try (parseIntLit <* notFollowedBy (string "to"))) <|> (A.RFFieldName <$> stringLiteral) <|> (A.RFNumberRange <$> parseIntLit <*> (symbol "to" *> (A.RFNRESingle <$> parseIntLit <|> A.RFNREMax <$ symbol "max")))

parseTopLevelMessage :: Parser A.TopLevelElement
parseTopLevelMessage = A.TLEMessage <$> parseMessage

parseOptionName :: Parser Text
parseOptionName = T.pack <$> lexeme name
  where
    name = some (alphaNumChar <|> char '(' <|> char ')' <|> char '.' <|> char '_')

parseOption :: Parser A.Option
parseOption = do
  l <- loc
  symbol "option"
  n <- parseOptionName
  symbol "="
  v <- parseConstant
  semicolons
  return $ A.Option n v l

parseTopLevelOption :: Parser A.TopLevelElement
parseTopLevelOption = do
  o <- parseOption
  return $ A.TLEOptions [o]

parseSyntaxSpecifier :: Parser A.SyntaxSpecifier
parseSyntaxSpecifier =
  do
    sc
    l <- loc
    symbol "syntax"
    symbol "="
    s <- parseSyntax (char '\'') <|> parseSyntax (char '"')
    semicolons
    return $ A.SyntaxSpecifier s l
  where
    parseSyntax sep = between sep sep (string "proto3") <* sc

parseIdent :: Parser Text
parseIdent = T.pack <$> ((:) <$> letterChar <*> many (char '_' <|> digitChar <|> letterChar)) <* sc

parseFullIdent :: Parser Text
parseFullIdent = T.intercalate (T.singleton '.') <$> parseIdent `sepBy1` char '.'

parsePackageSpecifier :: Parser A.TopLevelElement
parsePackageSpecifier = do
  l <- loc
  symbol "package"
  p <- parseFullIdent
  semicolons
  return $ A.TLEPackage (A.Package p l)

parseImportStatement :: Parser A.TopLevelElement
parseImportStatement = do
  l <- loc
  symbol "import"
  a <- access
  p <- stringLiteral
  semicolons
  return $ A.TLEImports [A.Import a p l]
  where
    access :: Parser A.ImportAccess
    access = do
      a <- accessM
      case a of
        Just ia -> return ia
        Nothing -> return A.None
    accessM :: Parser (Maybe A.ImportAccess)
    accessM = optional . try $ (A.Weak <$ symbol "weak" <|> A.Public <$ symbol "public")

parseTopLevelElements :: Parser [A.TopLevelElement]
parseTopLevelElements = many $ choice [parseImportStatement, parsePackageSpecifier, parseTopLevelOption, parseTopLevelEnum, parseTopLevelMessage]

parseProtoFile :: Parser A.ProtoFile
parseProtoFile = (A.ProtoFile <$> parseSyntaxSpecifier <*> parseTopLevelElements) <* eof

parseProto :: String -> String -> Either ParsingError ParseResult
parseProto file input =
  let (parseFile, comments) = runWriter (runParserT parseProtoFile file (T.pack input))
   in case parseFile of
        Left errorBundle -> Left $ errorBundlePretty errorBundle
        Right s -> Right (s, comments)
