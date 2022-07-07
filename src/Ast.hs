module Ast where

import Data.Text (Text)

data Loc = Loc {row :: Int, col :: Int} deriving (Eq, Show)

data Package = Package {package :: Text, loc :: Loc} deriving (Eq, Show)

data ImportAccess = Weak | None | Public deriving (Eq, Show)

data Import = Import {access :: ImportAccess, path :: Text, loc :: Loc} deriving (Eq, Show)

data SyntaxSpecifier = SyntaxSpecifier {syntax :: Text, loc :: Loc} deriving (Eq, Show)

type Type = Text

data Constant = CStringLiteral Text | COtherLiteral Text deriving (Eq, Show)

data Option = Option {name :: Text, value :: Constant, loc :: Loc} deriving (Eq, Show)

data FieldOption = FieldOption {name :: Text, value :: Constant} deriving (Eq, Show)

data MapField = MapField {keyType :: Type, valueType :: Type, name :: Text, number :: Text, options :: [FieldOption], loc :: Loc} deriving (Eq, Show)

data NormalField = NormalField {repeated :: Bool, fieldType :: Type, name :: Text, number :: Text, options :: [FieldOption], loc :: Loc} deriving (Eq, Show)

data OneOfVariant = OneOfVariant {name :: Text, fieldType :: Type, number :: Text, options :: [FieldOption], loc :: Loc} deriving (Eq, Show)

data OneOfFieldElement = OFEVariant OneOfVariant | OFEOption Option deriving (Eq, Show)

data OneOfField = OneOfField {name :: Text, elements :: [OneOfFieldElement], loc :: Loc, endLoc :: Loc} deriving (Eq, Show)

data ReservedFieldNumberRangeEnding = RFNRESingle Text | RFNREMax deriving (Eq, Show)

data ReservedField = RFFieldName Text | RFNumberSingle Text | RFNumberRange Text ReservedFieldNumberRangeEnding deriving (Eq, Show)

data ReservedStatement = ReservedStatement {fields :: [ReservedField], loc :: Loc} deriving (Eq, Show)

data EnumField = EnumField {name :: Text, number :: Int, options :: [FieldOption], loc :: Loc} deriving (Eq, Show)

data EnumElement = EEOption Option | EEField EnumField deriving (Eq, Show)

data Enumeration = Enumeration {name :: Text, elements :: [EnumElement], loc :: Loc, endLoc :: Loc} deriving (Eq, Show)

data MessageElement = MEMapField MapField | MENormalField NormalField | MEOneOfField OneOfField | MEReservedStatement ReservedStatement | MEOption Option | MEMessage Message | MEEnum Enumeration deriving (Eq, Show)

data Message = Message {name :: Text, elements :: [MessageElement], loc :: Loc, endLoc :: Loc} deriving (Eq, Show)

data Service = Service deriving (Eq, Show)

data TopLevelElement = TLEMessage Message | TLEEnum Enumeration | TLEService Service | TLEPackage Package | TLEImports [Import] | TLEOptions [Option] deriving (Eq, Show)

data ProtoFile = ProtoFile {syntax :: SyntaxSpecifier, elements :: [TopLevelElement]} deriving (Eq, Show)

data Comment = Comment {contents :: Text, loc :: Loc} deriving (Eq, Show)
