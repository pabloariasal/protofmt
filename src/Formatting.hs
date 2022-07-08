module Formatting (formatProto) where

import qualified Ast as A
import Control.Monad.State
import Data.Char (isSpace)
import Data.List (dropWhileEnd, partition)
import Data.Sort (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Parsing (ParseResult)
import Prettyprinter
import Prettyprinter.Internal.Type
import Prettyprinter.Render.Text

-- | As we traverse the AST and render the document we need to reinsert the comments at the correct locations.
--  For this we use a state monad that keeps track of the comments that are still to be inserted.
type StateDoc = State [A.Comment] (Doc ())

indentation = 2

-- | Takes a list of comments and formats them
formatComments :: [A.Comment] -> [Doc ()]
formatComments = map (pretty . A.contents)

insertCommentsAbove :: [A.Comment] -> Doc () -> StateDoc
insertCommentsAbove [] doc = return doc
insertCommentsAbove comments doc
  | isEmpty doc = return $ vsep commentDocs -- avoid trailing newline if doc is empty
  | otherwise = return $ vsep (commentDocs ++ [doc])
  where
    commentDocs = formatComments comments

insertCommentsAfter :: [A.Comment] -> Doc () -> StateDoc
insertCommentsAfter [] doc = return doc
insertCommentsAfter comments doc
  | isEmpty doc = return $ hsep commentDocs
  | otherwise = return $ hsep (doc : commentDocs)
  where
    commentDocs = formatComments comments

insertCommentsBefore :: [A.Comment] -> Doc () -> StateDoc
insertCommentsBefore [] doc = return doc
insertCommentsBefore comments doc
  | isEmpty doc = return $ hsep commentDocs
  | otherwise = return $ hsep (commentDocs ++ [doc])
  where
    commentDocs = formatComments comments

-- | 'insertComments' takes a documents and its location and formats it alongside comments.
-- The relevant comments are inserted at the right locations.
insertComments :: A.Loc -> Doc () -> StateDoc
insertComments loc doc = do
  cs <- get -- the comments still to be inserted
  if null cs
    then return doc
    else
      let (above, before, after, rest) = partitionComments loc cs
       in do
            put rest
            d1 <- insertCommentsBefore before doc
            d2 <- insertCommentsAfter after d1
            insertCommentsAbove above d2
  where
    -- partitionComments takes a location and a list of comments and partitions the comments in four categories:
    -- 1) above -> comment should be rendered above the location i.e. in lines above
    --   // comment
    --   loc
    -- 2) before -> comment should be rendered on the same line, but before the given location
    --   /* comment */ loc
    -- 3) after -> comment should be rendered on the same line, but after the given location
    --   loc /* comment */
    -- 4) rest of comments which should not be rendered for the given location
    partitionComments :: A.Loc -> [A.Comment] -> ([A.Comment], [A.Comment], [A.Comment], [A.Comment])
    partitionComments loc cs =
      let (commentsToRender, rest) = partition (ff shouldRenderComment) cs
          above = filter (ff shouldRenderAbove) commentsToRender
          before = filter (ff shouldRenderBefore) commentsToRender
          after = filter (ff shouldRenderAfter) commentsToRender
       in (above, before, after, rest)
      where
        ff :: (A.Loc -> A.Loc -> Bool) -> A.Comment -> Bool
        ff f c = f loc ((A.loc :: A.Comment -> A.Loc) c)
        shouldRenderAbove :: A.Loc -> A.Loc -> Bool
        shouldRenderAbove (A.Loc rowCurrLoc colCurrLoc) (A.Loc rowComment colComment) = rowCurrLoc > rowComment
        shouldRenderAfter :: A.Loc -> A.Loc -> Bool
        shouldRenderAfter (A.Loc rowCurrLoc colCurrLoc) (A.Loc rowComment colComment) = rowCurrLoc == rowComment && colCurrLoc < colComment
        shouldRenderBefore :: A.Loc -> A.Loc -> Bool
        shouldRenderBefore (A.Loc rowCurrLoc colCurrLoc) (A.Loc rowComment colComment) = rowCurrLoc == rowComment && colCurrLoc > colComment
        shouldRenderComment :: A.Loc -> A.Loc -> Bool
        shouldRenderComment (A.Loc rowCurrLoc _) (A.Loc rowComment _) = rowCurrLoc >= rowComment

blankLine :: Doc () -> StateDoc
blankLine d = return $ d <> line

-- | Convert every element to a Doc and concatenates them vertically
mapAndConcatVertically :: (a -> StateDoc) -> [a] -> StateDoc
mapAndConcatVertically f es = vsep <$> mapM f es

-- | gets the location above and to the left
upAndLeftLoc :: A.Loc -> A.Loc
upAndLeftLoc A.Loc {..} = A.Loc (row - 1) (-1)

formatCompoundElement :: Text -> Text -> (a -> StateDoc) -> [a] -> A.Loc -> A.Loc -> StateDoc
formatCompoundElement typeName name formatElement elements loc endLoc = do
  header <- formatCompoundElementHeader typeName name loc
  body <- mapAndConcatVertically formatElement elements
  trailingComments <- insertComments (upAndLeftLoc endLoc) emptyDoc
  tail <- insertComments endLoc rbrace
  return (compoundElementDoc header (vsep . filter (not . isEmpty) $ [body, trailingComments]) tail)
  where
    compoundElementDoc :: Doc () -> Doc () -> Doc () -> Doc ()
    compoundElementDoc header body tail =
      if isEmpty body
        then header <> rbrace
        else header <> line <> indent indentation body <> line <> tail

    formatCompoundElementHeader :: Text -> Text -> A.Loc -> StateDoc
    formatCompoundElementHeader elementType name loc = insertComments loc (pretty elementType <+> pretty name <+> lbrace)

formatPackage :: A.Package -> StateDoc
formatPackage A.Package {..} = insertComments loc (pretty ("package" :: Text) <+> pretty package <> semi) >>= blankLine

formatSyntax :: A.SyntaxSpecifier -> StateDoc
formatSyntax (A.SyntaxSpecifier s l) = insertComments l formattedSyntax >>= blankLine
  where
    formattedSyntax = pretty ("syntax" :: Text) <+> equals <+> enclose dquote dquote (pretty s) <> semi

formatMessage :: A.Message -> StateDoc
formatMessage m@A.Message {..} = formatCompoundElement "message" name formatMessageElement elements loc endLoc
  where
    formatMessageElement :: A.MessageElement -> StateDoc
    formatMessageElement e = case e of
      A.MEMapField mf -> formatMapField mf
      A.MENormalField nf -> formatNormalField nf
      A.MEOneOfField oof -> formatOneOfField oof
      A.MEReservedStatement rs -> formatReservedStatement rs
      A.MEOption o -> formatOption o
      A.MEMessage m -> formatMessage m
      A.MEEnum e -> formatEnum e
    formatMapField :: A.MapField -> StateDoc
    formatMapField m@A.MapField {..} = insertComments loc (mapFieldDoc m)
    mapFieldDoc :: A.MapField -> Doc ()
    mapFieldDoc A.MapField {..} =
      let begin = pretty ("map" :: Text) <> langle <> pretty keyType <> comma <> space <> pretty valueType <> rangle <+> pretty name <+> equals <+> pretty number
       in assembleField begin semi (formatFieldOptions options)
    formatOneOfField :: A.OneOfField -> StateDoc
    formatOneOfField A.OneOfField {..} = formatCompoundElement "oneof" name formatOneOfFieldElement elements loc endLoc
    formatOneOfFieldElement :: A.OneOfFieldElement -> StateDoc
    formatOneOfFieldElement e = case e of
      A.OFEVariant v -> formatOneOfVariant v
      A.OFEOption o -> formatOption o
    formatOneOfVariant :: A.OneOfVariant -> StateDoc
    formatOneOfVariant v@A.OneOfVariant {..} = insertComments loc (oneOfVariantDoc v)
    oneOfVariantDoc :: A.OneOfVariant -> Doc ()
    oneOfVariantDoc A.OneOfVariant {..} =
      let begin = pretty fieldType <+> pretty name <+> equals <+> pretty number
       in assembleField begin semi (formatFieldOptions options)
    formatNormalField :: A.NormalField -> StateDoc
    formatNormalField n@A.NormalField {..} = insertComments loc (normalFieldDoc n)
    normalFieldDoc :: A.NormalField -> Doc ()
    normalFieldDoc A.NormalField {..} =
      let middle = pretty fieldType <+> pretty name <+> equals <+> pretty number
          begin = if repeated then pretty ("repeated" :: Text) <+> middle else middle
       in assembleField begin semi (formatFieldOptions options)
    formatReservedStatement :: A.ReservedStatement -> StateDoc
    formatReservedStatement A.ReservedStatement {..} =
      let head = pretty ("reserved" :: Text)
          body = reservedFieldsDoc fields
       in insertComments loc (head <+> body <> semi)
    reservedFieldsDoc :: [A.ReservedField] -> Doc ()
    reservedFieldsDoc elements =
      hsep $ punctuate comma (map reservedElementToDoc elements)
      where
        reservedElementToDoc :: A.ReservedField -> Doc ()
        reservedElementToDoc e = case e of
          A.RFFieldName e -> dquotes . pretty $ e
          A.RFNumberSingle e -> pretty e
          A.RFNumberRange from to -> pretty from <+> pretty ("to" :: Text) <+> (\case A.RFNRESingle n -> pretty n; A.RFNREMax -> pretty ("max" :: Text)) to

formatTopLevelMessage :: A.Message -> StateDoc
formatTopLevelMessage m = formatMessage m >>= blankLine

formatService :: A.Service -> StateDoc
formatService = undefined

assembleField :: Doc () -> Doc () -> Doc () -> Doc ()
assembleField prefix suffix fieldOptions =
  if isEmpty fieldOptions
    then prefix <> suffix
    else prefix <+> fieldOptions <> suffix

formatFieldOptions :: [A.FieldOption] -> Doc ()
formatFieldOptions os
  | null os = emptyDoc
  | otherwise = encloseSep lbracket rbracket comma (map formatFieldOption os)
  where
    formatFieldOption :: A.FieldOption -> Doc ()
    formatFieldOption A.FieldOption {..} = pretty name <+> equals <+> formatConstant value

formatConstant :: A.Constant -> Doc ()
formatConstant c = case c of
  A.CStringLiteral t -> enclose dquote dquote (pretty t)
  A.COtherLiteral t -> pretty t

formatEnum :: A.Enumeration -> StateDoc
formatEnum e@A.Enumeration {..} = formatCompoundElement "enum" name formatEnumElement elements loc endLoc
  where
    formatEnumElement :: A.EnumElement -> StateDoc
    formatEnumElement a = case a of
      A.EEOption o -> formatOption o
      A.EEField f -> formatEnumField f

    formatEnumField :: A.EnumField -> StateDoc
    formatEnumField ef@A.EnumField {..} = insertComments loc (enumFieldDoc ef)

    enumFieldDoc :: A.EnumField -> Doc ()
    enumFieldDoc A.EnumField {..} =
      let begin = pretty (T.toUpper name) <+> equals <+> pretty number
       in assembleField begin semi (formatFieldOptions options)

formatTopLevelEnum :: A.Enumeration -> StateDoc
formatTopLevelEnum e = formatEnum e >>= blankLine

formatOption :: A.Option -> StateDoc
formatOption A.Option {..} = insertComments loc optionDoc
  where
    optionDoc :: Doc ()
    optionDoc = pretty ("option" :: Text) <+> pretty name <+> equals <+> formatConstant value <> semi

formatOptionGroup :: [A.Option] -> StateDoc
formatOptionGroup opt = mapM formatOption opt >>= blankLine . vsep

formatImport :: A.Import -> StateDoc
formatImport i@A.Import {..} = insertComments loc (importDoc i)
  where
    importDoc :: A.Import -> Doc ()
    importDoc A.Import {..} = addAccess (pretty ("import" :: Text)) access <+> enclose dquote dquote (pretty path) <> semi
    addAccess :: Doc () -> A.ImportAccess -> Doc ()
    addAccess d a = case a of
      A.Weak -> d <+> pretty ("weak" :: Text)
      A.Public -> d <+> pretty ("public" :: Text)
      _ -> d

formatImportGroup :: [A.Import] -> StateDoc
formatImportGroup imports = do
  let locFirstImport = (A.loc :: A.Import -> A.Loc) . head $ imports
  commentsPrev <- insertComments (upAndLeftLoc locFirstImport) emptyDoc
  unsortedImports <- mapM formatImport imports
  let sortedImports = sortOn render unsortedImports
  let importDocs = (vsep . filter (not . isEmpty)) (commentsPrev : sortedImports)
  return $ importDocs <> line

formatTopLevelElements :: [A.TopLevelElement] -> StateDoc
formatTopLevelElements es = mapAndConcatVertically f es
  where
    f :: A.TopLevelElement -> StateDoc
    f (A.TLEPackage e) = formatPackage e
    f (A.TLEImports es) = formatImportGroup es
    f (A.TLEOptions es) = formatOptionGroup es
    f (A.TLEMessage e) = formatTopLevelMessage e
    f (A.TLEEnum e) = formatTopLevelEnum e
    f (A.TLEService e) = formatService e

formatFile :: A.ProtoFile -> StateDoc
formatFile (A.ProtoFile s tle) = concatenateVertically [formatSyntax s, (formatTopLevelElements . groupElements) tle, restOfComments]
  where
    restOfComments :: StateDoc
    restOfComments = do
      cs <- get
      insertCommentsAbove cs emptyDoc
    concatenateVertically :: [StateDoc] -> StateDoc
    concatenateVertically docs = vsep . filter (not . isEmpty) <$> sequence docs

-- default is line lenght of 80 characters
render :: Doc () -> Text
render = renderStrict . layoutSmart defaultLayoutOptions

formatProto :: ParseResult -> String
formatProto (pr, c) = trim . T.unpack $ render (evalState (formatFile pr) c)
  where
    trim :: String -> String
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- rather immoral hack to check if a Doc is empty, see https://github.com/quchen/prettyprinter/issues/64
isEmpty :: Doc () -> Bool
isEmpty = \case Empty -> True; _ -> False

class Localizable a where
  getRow :: a -> Int

instance Localizable A.Option where
  getRow = A.row . (A.loc :: A.Option -> A.Loc)

instance Localizable A.Import where
  getRow = A.row . (A.loc :: A.Import -> A.Loc)

-- | The 'groupElements' function finds import statements and options that are adjacent groups them together.
-- Adjacent means that elements are directly on top of each other (no newlines inbetween) or in the same line
--
-- For example, these import statements will be groupped together:
-- import "a";
-- import "b";
--
-- these ones as well:
--
-- import "a"; import "b";
--
-- The following statements will not be groupped together (newline inbetween):
--
-- import "a";
--
-- import "b";
--
-- Each import group is rendered as a single entity
-- This way we can:
-- 1) avoid rendering new lines between grouped import statements
-- 2) sort the imports within a group alphabetically
--
-- All other entities in the AST remain untouched
groupElements :: [A.TopLevelElement] -> [A.TopLevelElement]
groupElements = foldr f []
  where
    f x [] = [x]
    f x@(A.TLEImports [i]) (is : yss) = case is of
      (A.TLEImports iss) ->
        if belongsToGroup iss i
          then A.TLEImports (i : iss) : yss
          else x : is : yss
      _ -> x : is : yss
    f x@(A.TLEOptions [o]) (os : yss) = case os of
      (A.TLEOptions oss) ->
        if belongsToGroup oss o
          then A.TLEOptions (o : oss) : yss
          else x : os : yss
      _ -> x : os : yss
    f e acc = e : acc
    belongsToGroup :: Localizable a => [a] -> a -> Bool
    belongsToGroup (i : _) c = abs (getRow i - getRow c) <= 1
    belongsToGroup _ _ = error "elements are empty"
