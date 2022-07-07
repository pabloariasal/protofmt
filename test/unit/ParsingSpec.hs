module ParsingSpec (spec) where

import qualified Ast as A
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Parsing
import Test.Hspec

passedSyn :: [A.TopLevelElement] -> [A.Comment] -> Either ParsingError ParseResult
passedSyn elements comments = Right (A.ProtoFile (A.SyntaxSpecifier "proto3" (A.Loc 1 1)) elements, comments)

runSyn :: String -> Either ParsingError ParseResult
runSyn s = parseProto "" ("syntax = \"proto3\";\n" ++ s)

run :: String -> Either ParsingError ParseResult
run = parseProto ""

passed :: A.SyntaxSpecifier -> [A.TopLevelElement] -> [A.Comment] -> Either ParsingError ParseResult
passed syn elements comments = Right (A.ProtoFile syn elements, comments)

spec :: Spec
spec = do
  syntaxSpecification
  emptyStatements
  importStatements
  packageSpecifier
  topLevelOptions
  enumDefinitions
  messageDefinitons

syntaxSpecification :: Spec
syntaxSpecification = do
  describe "Syntax Specification" $ do
    it "double quotes" $ do
      run "syntax  = \"proto3\" ;" `shouldBe` passed (A.SyntaxSpecifier "proto3" (A.Loc 1 1)) [] []
    it "single quotes" $ do
      run "syntax  =\t 'proto3';     " `shouldBe` passed (A.SyntaxSpecifier "proto3" (A.Loc 1 1)) [] []
    it "syntax with comments" $ do
      parseProto "" "/* comment 1 */ syntax \t\n = 'proto3'   ;\n// comment 2" `shouldBe` passed (A.SyntaxSpecifier "proto3" (A.Loc 1 17)) [] [A.Comment "/* comment 1 */" (A.Loc 1 1), A.Comment "// comment 2" (A.Loc 3 1)]
    it "fails if syntax empty" $ do
      run "syntax = '';" `shouldSatisfy` isLeft
    it "fails if syntax is not 3" $ do
      run "syntax = 'proto2';" `shouldSatisfy` isLeft
    it "fails if missing semicolon" $ do
      run "syntax = 'proto3'" `shouldSatisfy` isLeft

emptyStatements :: Spec
emptyStatements = do
  describe "Empty Statements" $ do
    it "are ignored" $ do
      runSyn ";  \n ; \t ;;;" `shouldBe` passedSyn [] []
    it "comments are still parsed" $ do
      runSyn "// comment1\n ; /* comment 2 */ \n ;;;; // comment 3"
        `shouldBe` passedSyn
          []
          [ A.Comment "// comment1" (A.Loc 2 1),
            A.Comment "/* comment 2 */" (A.Loc 3 4),
            A.Comment "// comment 3" (A.Loc 4 7)
          ]

-- TODO add failure cases?
importStatements :: Spec
importStatements = do
  describe "Import Statements" $ do
    it "simple import" $ do
      runSyn "import \"other.proto\";" `shouldBe` passedSyn [A.TLEImports [A.Import A.None "other.proto" (A.Loc 2 1)]] []
    it "simple import single quotes" $ do
      runSyn "import weak 'other.proto';" `shouldBe` passedSyn [A.TLEImports [A.Import A.Weak "other.proto" (A.Loc 2 1)]] []
    it "simple public import" $ do
      runSyn "import public   \"other.proto\";;;" `shouldBe` passedSyn [A.TLEImports [A.Import A.Public "other.proto" (A.Loc 2 1)]] []
    it "multiple imports with comments" $ do
      runSyn "import /* comment 1 */ \"foo\";\n   import \n'bar'; // comment 2"
        `shouldBe` passedSyn
          [ A.TLEImports [A.Import A.None "foo" (A.Loc 2 1)],
            A.TLEImports [A.Import A.None "bar" (A.Loc 3 4)]
          ]
          [ A.Comment "/* comment 1 */" (A.Loc 2 8),
            A.Comment "// comment 2" (A.Loc 4 8)
          ]

packageSpecifier :: Spec
packageSpecifier = do
  describe "Package Specifier" $ do
    it "simple" $ do
      runSyn "package foo.bar;" `shouldBe` passedSyn [A.TLEPackage $ A.Package "foo.bar" (A.Loc 2 1)] []
    it "white space" $ do
      runSyn "\n    package    foo.bar\r;\t" `shouldBe` passedSyn [A.TLEPackage $ A.Package "foo.bar" (A.Loc 3 5)] []
    it "multiple semicolons" $ do
      runSyn "package foo.bar;;  ;;" `shouldBe` passedSyn [A.TLEPackage $ A.Package "foo.bar" (A.Loc 2 1)] []
    it "with comments" $ do
      runSyn "\n/* comment 1 */ package /* comment 2 */ foo.bar;/* comment 3 */ // comment 4"
        `shouldBe` passedSyn
          [A.TLEPackage $ A.Package "foo.bar" (A.Loc 3 17)]
          [ A.Comment "/* comment 1 */" (A.Loc 3 1),
            A.Comment "/* comment 2 */" (A.Loc 3 25),
            A.Comment "/* comment 3 */" (A.Loc 3 49),
            A.Comment "// comment 4" (A.Loc 3 65)
          ]
    it "with number and underscores" $ do
      runSyn "package f12oo.b__a2r_;" `shouldBe` passedSyn [A.TLEPackage $ A.Package "f12oo.b__a2r_" (A.Loc 2 1)] []
    it "fails if package starts with a number" $ do
      runSyn "package 1a;" `shouldSatisfy` isLeft
    it "fails if package does not end with semicolon" $ do
      runSyn "package a" `shouldSatisfy` isLeft
    it "fails if package starts with point" $ do
      runSyn "package .a" `shouldSatisfy` isLeft

topLevelOptions :: Spec
topLevelOptions = do
  describe "Top Level Options" $ do
    it "string literals" $ do
      runSyn "option java_package =   \"com.example.foo\";;;;;option o2 = 'my_string';" `shouldBe` passedSyn [A.TLEOptions [A.Option "java_package" (A.CStringLiteral "com.example.foo") (A.Loc 2 1)], A.TLEOptions [A.Option "o2" (A.CStringLiteral "my_string") (A.Loc 2 47)]] []
    it "complex option name" $ do
      runSyn "\n    option (option.buzz).bar.foo =   false   ;" `shouldBe` passedSyn [A.TLEOptions [A.Option "(option.buzz).bar.foo" (A.COtherLiteral "false") (A.Loc 3 5)]] []
    it "full ident" $ do
      runSyn "option bar45.foo = my_2.ident;" `shouldBe` passedSyn [A.TLEOptions [A.Option "bar45.foo" (A.COtherLiteral "my_2.ident") (A.Loc 2 1)]] []
    it "integer literals" $
      do
        runSyn "option o1=+09000 ;\noption o2=-16;option o3=0xefea;\noption o4 = 0777;\noption o5 = -0XFFED;option o6=+0666;"
        `shouldBe` passedSyn
          [ A.TLEOptions [A.Option "o1" (A.COtherLiteral "+09000") (A.Loc 2 1)],
            A.TLEOptions [A.Option "o2" (A.COtherLiteral "-16") (A.Loc 3 1)],
            A.TLEOptions [A.Option "o3" (A.COtherLiteral "0xefea") (A.Loc 3 15)],
            A.TLEOptions [A.Option "o4" (A.COtherLiteral "0777") (A.Loc 4 1)],
            A.TLEOptions [A.Option "o5" (A.COtherLiteral "-0XFFED") (A.Loc 5 1)],
            A.TLEOptions [A.Option "o6" (A.COtherLiteral "+0666") (A.Loc 5 21)]
          ]
          []
    it "float literals" $
      do
        runSyn "option o1=+5.5 ;\noption o2=-16.;option o3=10.1e23;\noption o4 = +1.E-10;\noption o5 = -56E+04;option o6=+.6;option o7 = -.9;\noption o8 = 76.E5;"
        `shouldBe` passedSyn
          [ A.TLEOptions [A.Option "o1" (A.COtherLiteral "+5.5") (A.Loc 2 1)],
            A.TLEOptions [A.Option "o2" (A.COtherLiteral "-16.") (A.Loc 3 1)],
            A.TLEOptions [A.Option "o3" (A.COtherLiteral "10.1e23") (A.Loc 3 16)],
            A.TLEOptions [A.Option "o4" (A.COtherLiteral "+1.E-10") (A.Loc 4 1)],
            A.TLEOptions [A.Option "o5" (A.COtherLiteral "-56E+04") (A.Loc 5 1)],
            A.TLEOptions [A.Option "o6" (A.COtherLiteral "+.6") (A.Loc 5 21)],
            A.TLEOptions [A.Option "o7" (A.COtherLiteral "-.9") (A.Loc 5 35)],
            A.TLEOptions [A.Option "o8" (A.COtherLiteral "76.E5") (A.Loc 6 1)]
          ]
          []
    it "with comments" $ do
      do runSyn "//c1\n/*c2*/ option /*c3*/ o /*c4*/ =/*c5*/ 4/*c6*/;//c7"
        `shouldBe` passedSyn
          [A.TLEOptions [A.Option "o" (A.COtherLiteral "4") (A.Loc 3 8)]]
          [ A.Comment "//c1" (A.Loc 2 1),
            A.Comment "/*c2*/" (A.Loc 3 1),
            A.Comment "/*c3*/" (A.Loc 3 15),
            A.Comment "/*c4*/" (A.Loc 3 24),
            A.Comment "/*c5*/" (A.Loc 3 32),
            A.Comment "/*c6*/" (A.Loc 3 40),
            A.Comment "//c7" (A.Loc 3 47)
          ]

enumDefinitions :: Spec
enumDefinitions = do
  describe "Enum Definition" $ do
    it "simple" $ do
      runSyn "enum MyEnum { A = 0;B = -1; }" `shouldBe` passedSyn [A.TLEEnum $ A.Enumeration "MyEnum" [A.EEField (A.EnumField "A" 0 [] (A.Loc 2 15)), A.EEField (A.EnumField "B" (-1) [] (A.Loc 2 21))] (A.Loc 2 1) (A.Loc 2 29)] []
    it "empty" $ do
      runSyn "enum E1 {\n       }; ; enum E2 {;  ;;;;}" `shouldBe` passedSyn [A.TLEEnum $ A.Enumeration "E1" [] (A.Loc 2 1) (A.Loc 3 8), A.TLEEnum $ A.Enumeration "E2" [] (A.Loc 3 13) (A.Loc 3 29)] []
    it "with options" $ do
      runSyn "enum MyEnum {option allow_alias = true;;\nRUNNING=2;option (a) = \"foo\";}"
        `shouldBe` passedSyn
          [ A.TLEEnum $
              A.Enumeration
                "MyEnum"
                [ A.EEOption (A.Option "allow_alias" (A.COtherLiteral "true") (A.Loc 2 14)),
                  A.EEField (A.EnumField "RUNNING" 2 [] (A.Loc 3 1)),
                  A.EEOption (A.Option "(a)" (A.CStringLiteral "foo") (A.Loc 3 11))
                ]
                (A.Loc 2 1)
                (A.Loc 3 30)
          ]
          []
    it "with field options" $ do
      runSyn "enum MyEnum {a=2 [(custom_option) = \"hello world\"  ] ;;b=-3[ (foo_bar).p= true, o2=2.3 ];;}"
        `shouldBe` passedSyn
          [ A.TLEEnum $
              A.Enumeration
                "MyEnum"
                [ A.EEField (A.EnumField "a" 2 [A.FieldOption "(custom_option)" (A.CStringLiteral "hello world")] (A.Loc 2 14)),
                  A.EEField (A.EnumField "b" (-3) [A.FieldOption "(foo_bar).p" (A.COtherLiteral "true"), A.FieldOption "o2" (A.COtherLiteral "2.3")] (A.Loc 2 56))
                ]
                (A.Loc 2 1)
                (A.Loc 2 91)
          ]
          []
    it "with comments" $ do
      runSyn "enum MyEnum /*c1*/ {//c2\n//c3\n A = /*c4*/ 0;/*c5*/;B = -1; /*c6*/\n//c7\n }"
        `shouldBe` passedSyn
          [ A.TLEEnum $
              A.Enumeration
                "MyEnum"
                [ A.EEField (A.EnumField "A" 0 [] (A.Loc 4 2)),
                  A.EEField (A.EnumField "B" (-1) [] (A.Loc 4 22))
                ]
                (A.Loc 2 1)
                (A.Loc 6 2)
          ]
          [ A.Comment "/*c1*/" (A.Loc 2 13),
            A.Comment "//c2" (A.Loc 2 21),
            A.Comment "//c3" (A.Loc 3 1),
            A.Comment "/*c4*/" (A.Loc 4 6),
            A.Comment "/*c5*/" (A.Loc 4 15),
            A.Comment "/*c6*/" (A.Loc 4 30),
            A.Comment "//c7" (A.Loc 5 1)
          ]

messageDefinitons :: Spec
messageDefinitons = do
  describe "Message Definition" $ do
    it "empty" $ do
      runSyn "message M {}\nmessage N{;;; ;}"
        `shouldBe` passedSyn
          [ A.TLEMessage $ A.Message "M" [] (A.Loc 2 1) (A.Loc 2 12),
            A.TLEMessage $ A.Message "N" [] (A.Loc 3 1) (A.Loc 3 16)
          ]
          []
    it "with options" $ do
      runSyn "  message M {option  (my_option).a = true ;;\n;option o =\"foo\";}"
        `shouldBe` passedSyn
          [ A.TLEMessage $
              A.Message
                "M"
                [ A.MEOption $ A.Option "(my_option).a" (A.COtherLiteral "true") (A.Loc 2 14),
                  A.MEOption $ A.Option "o" (A.CStringLiteral "foo") (A.Loc 3 2)
                ]
                (A.Loc 2 3)
                (A.Loc 3 18)
          ]
          []
    it "with normal fields" $ do
      runSyn "  message M {;foo.Bar field = 2; repeated string name = 1; repeated int32 samples=0X123Fa[packed=true];}"
        `shouldBe` passedSyn
          [ A.TLEMessage $
              A.Message
                "M"
                [ A.MENormalField $ A.NormalField False "foo.Bar" "field" "2" [] (A.Loc 2 15),
                  A.MENormalField $ A.NormalField True "string" "name" "1" [] (A.Loc 2 34),
                  A.MENormalField $ A.NormalField True "int32" "samples" "0X123Fa" [A.FieldOption "packed" (A.COtherLiteral "true")] (A.Loc 2 60)
                ]
                (A.Loc 2 3)
                (A.Loc 2 104)
          ]
          []
    it "with map fields" $ do
      runSyn "message M {map <string, Project> /*c1*/ projects = 2;;;map<fixed64,uint32>foo=0xa[packed=\"bar\"]/* c2 */;}"
        `shouldBe` passedSyn
          [ A.TLEMessage $
              A.Message
                "M"
                [ A.MEMapField $ A.MapField "string" "Project" "projects" "2" [] (A.Loc 2 12),
                  A.MEMapField $ A.MapField "fixed64" "uint32" "foo" "0xa" [A.FieldOption "packed" (A.CStringLiteral "bar")] (A.Loc 2 56)
                ]
                (A.Loc 2 1)
                (A.Loc 2 105)
          ]
          [ A.Comment "/*c1*/" (A.Loc 2 34),
            A.Comment "/* c2 */" (A.Loc 2 96)
          ]
    it "with oneof fields" $ do
      runSyn "message M {oneof foo {string name = 4;\n;;SubMessage sub_message = 0x6[packed=true];;} oneof bar {option (a).b = 0; double baz = 3;}oneof baz{;}}"
        `shouldBe` passedSyn
          [ A.TLEMessage $
              A.Message
                "M"
                [ A.MEOneOfField $
                    A.OneOfField
                      "foo"
                      [ A.OFEVariant $ A.OneOfVariant "name" "string" "4" [] (A.Loc 2 23),
                        A.OFEVariant $ A.OneOfVariant "sub_message" "SubMessage" "0x6" [A.FieldOption "packed" (A.COtherLiteral "true")] (A.Loc 3 3)
                      ]
                      (A.Loc 2 12)
                      (A.Loc 3 46),
                  A.MEOneOfField $
                    A.OneOfField
                      "bar"
                      [ A.OFEOption $ A.Option "(a).b" (A.COtherLiteral "0") (A.Loc 3 59),
                        A.OFEVariant $ A.OneOfVariant "baz" "double" "3" [] (A.Loc 3 77)
                      ]
                      (A.Loc 3 48)
                      (A.Loc 3 92),
                  A.MEOneOfField $ A.OneOfField "baz" [] (A.Loc 3 93) (A.Loc 3 104)
                ]
                (A.Loc 2 1)
                (A.Loc 3 105)
          ]
          []
    it "with inner messages" $ do
      runSyn "message outer { message inner { enum innerinner {} } }"
        `shouldBe` passedSyn
          [ A.TLEMessage $
              A.Message
                "outer"
                [A.MEMessage $ A.Message "inner" [A.MEEnum $ A.Enumeration "innerinner" [] (A.Loc 2 33) (A.Loc 2 50)] (A.Loc 2 17) (A.Loc 2 52)]
                (A.Loc 2 1)
                (A.Loc 2 54)
          ]
          []
    it "with enums" $ do
      runSyn "message m {  enum e { FOO = 2; }  }"
        `shouldBe` passedSyn
          [ A.TLEMessage $
              A.Message
                "m"
                [A.MEEnum $ A.Enumeration "e" [A.EEField $ A.EnumField "FOO" 2 [] (A.Loc 2 23)] (A.Loc 2 14) (A.Loc 2 32)]
                (A.Loc 2 1)
                (A.Loc 2 35)
          ]
          []
    it "with reserved statements" $ do
      runSyn "message m {\n  // comment 1\n  reserved 2,  \"foo\", 9 to max;;; /* comment 2 */\n  reserved /* comment 3 */ 8 to  10; // comment 4\n}"
        `shouldBe` passedSyn
          [ A.TLEMessage $
              A.Message
                "m"
                [ A.MEReservedStatement $ A.ReservedStatement [A.RFNumberSingle "2", A.RFFieldName "foo", A.RFNumberRange "9" A.RFNREMax] (A.Loc 4 3),
                  A.MEReservedStatement $ A.ReservedStatement [A.RFNumberRange "8" (A.RFNRESingle "10")] (A.Loc 5 3)
                ]
                (A.Loc 2 1)
                (A.Loc 6 1)
          ]
          [ A.Comment "// comment 1" (A.Loc 3 3),
            A.Comment "/* comment 2 */" (A.Loc 4 35),
            A.Comment "/* comment 3 */" (A.Loc 5 12),
            A.Comment "// comment 4" (A.Loc 5 38)
          ]
    it "with comments" $ do
      runSyn "message /*c1*/ m /*c2*/ { /*c3*/\n//c4\n enum e { FOO = 2; }\n//c5\n  }"
        `shouldBe` passedSyn
          [ A.TLEMessage $
              A.Message
                "m"
                [A.MEEnum $ A.Enumeration "e" [A.EEField $ A.EnumField "FOO" 2 [] (A.Loc 4 11)] (A.Loc 4 2) (A.Loc 4 20)]
                (A.Loc 2 1)
                (A.Loc 6 3)
          ]
          [ A.Comment "/*c1*/" (A.Loc 2 9),
            A.Comment "/*c2*/" (A.Loc 2 18),
            A.Comment "/*c3*/" (A.Loc 2 27),
            A.Comment "//c4" (A.Loc 3 1),
            A.Comment "//c5" (A.Loc 5 1)
          ]
