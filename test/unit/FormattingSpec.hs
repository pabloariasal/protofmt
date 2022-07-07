module FormattingSpec (spec) where

import qualified Ast as A
import Formatting (formatProto)
import Parsing (ParseResult)
import Test.Hspec

spec :: Spec
spec = do
  syntaxFormatting
  packageFormatting
  importFormatting
  optionFormatting
  enumFormating
  messageFormatting
  topLevelCommentAssembly

-- TODO use consistent test naming
ast :: [A.TopLevelElement] -> [A.Comment] -> ParseResult
ast elements comments = (A.ProtoFile (A.SyntaxSpecifier "proto3" (A.Loc 1 1)) elements, comments)

syn :: String -> String
syn = ("syntax = \"proto3\";\n\n" ++)

syntaxFormatting :: Spec
syntaxFormatting = do
  describe "syntax specification" $ do
    it "simple" $ do
      formatProto (ast [] []) `shouldBe` "syntax = \"proto3\";"

packageFormatting :: Spec
packageFormatting = do
  describe "package definitions" $ do
    it "simple" $ do
      formatProto (ast [A.TLEPackage (A.Package "my.pack.age" (A.Loc 1 0))] []) `shouldBe` syn "package my.pack.age;"
    it "with comments" $ do
      formatProto (ast [A.TLEPackage (A.Package "pa" (A.Loc 2 0))] [A.Comment "//comment" (A.Loc 2 3)]) `shouldBe` syn "package pa; //comment"

optionFormatting :: Spec
optionFormatting = do
  describe "top level options" $ do
    it "string literals" $
      do
        formatProto
          ( ast
              [A.TLEOptions [A.Option "java_package" (A.CStringLiteral "\ntest\t") (A.Loc 2 0)]]
              []
          )
        `shouldBe` syn "option java_package = \"\ntest\t\";"
    it "other literals" $
      do
        formatProto
          ( ast
              [ A.TLEOptions [A.Option "java.(nested)" (A.COtherLiteral "-1.0e10-4") (A.Loc 2 0)],
                A.TLEOptions [A.Option "opt_name" (A.COtherLiteral "0xffFF") (A.Loc 3 0)]
              ]
              []
          )
        `shouldBe` syn "option java.(nested) = -1.0e10-4;\noption opt_name = 0xffFF;"
    it "with comments" $
      do
        formatProto
          ( ast
              [ A.TLEOptions [A.Option "o1" (A.COtherLiteral "true") (A.Loc 2 0)],
                A.TLEOptions [A.Option "o2" (A.COtherLiteral "-5") (A.Loc 4 9)]
              ]
              [ A.Comment "//middle" (A.Loc 3 0),
                A.Comment "/*before*/" (A.Loc 4 0)
              ]
          )
        `shouldBe` syn "option o1 = true;\n\n//middle\n/*before*/ option o2 = -5;"
    it "groups" $
      do
        formatProto
          ( ast
              [ A.TLEOptions [A.Option "o1" (A.COtherLiteral "true") (A.Loc 2 0)],
                A.TLEOptions [A.Option "o2" (A.COtherLiteral "-5") (A.Loc 3 0)],
                A.TLEOptions [A.Option "o3" (A.COtherLiteral "+6") (A.Loc 5 0)],
                A.TLEOptions [A.Option "o4" (A.CStringLiteral "bla\a") (A.Loc 5 20)]
              ]
              []
          )
        `shouldBe` syn "option o1 = true;\noption o2 = -5;\n\noption o3 = +6;\noption o4 = \"bla\a\";"

importFormatting :: Spec
importFormatting = do
  describe "import statements" $ do
    it "simple import statement" $ do
      formatProto
        ( ast
            [A.TLEImports [A.Import A.None "other.proto" (A.Loc 3 0)]]
            [A.Comment "// comment" (A.Loc 2 0)]
        )
        `shouldBe` syn "// comment\nimport \"other.proto\";"
    it "single import group sorted" $ do
      formatProto
        ( ast
            [ A.TLEImports [A.Import A.Weak "proto1.proto" (A.Loc 3 8)],
              A.TLEImports [A.Import A.Public "proto2.proto" (A.Loc 4 0)]
            ]
            [ A.Comment "// comment1" (A.Loc 2 0),
              A.Comment "/* comment2 */" (A.Loc 3 1)
            ]
        )
        `shouldBe` syn "// comment1\n/* comment2 */ import weak \"proto1.proto\";\nimport public \"proto2.proto\";"
    it "single import group unsorted with comments" $ do
      formatProto
        ( ast
            [ A.TLEImports [A.Import A.None "proto2.proto" (A.Loc 2 0)],
              A.TLEImports [A.Import A.None "proto1.proto" (A.Loc 3 0)],
              A.TLEImports [A.Import A.None "proto3.proto" (A.Loc 3 10)]
            ]
            [ A.Comment "// comment2" (A.Loc 2 10),
              A.Comment "// comment1" (A.Loc 3 20)
            ]
        )
        `shouldBe` syn "import \"proto1.proto\"; // comment1\nimport \"proto2.proto\"; // comment2\nimport \"proto3.proto\";"
    it "two import groups" $ do
      formatProto
        ( ast
            [ A.TLEImports [A.Import A.None "proto3.proto" (A.Loc 2 0)],
              A.TLEImports [A.Import A.None "proto2.proto" (A.Loc 3 0)],
              A.TLEImports [A.Import A.None "proto1.proto" (A.Loc 5 0)]
            ]
            [ A.Comment "// comment1" (A.Loc 2 10),
              A.Comment "// comment2" (A.Loc 5 10)
            ]
        )
        `shouldBe` syn "import \"proto2.proto\";\nimport \"proto3.proto\"; // comment1\n\nimport \"proto1.proto\"; // comment2"
    it "two import groups comment in the middle" $ do
      formatProto
        ( ast
            [ A.TLEImports [A.Import A.None "proto3.proto" (A.Loc 2 0)],
              A.TLEImports [A.Import A.None "proto2.proto" (A.Loc 3 0)],
              A.TLEImports [A.Import A.None "proto1.proto" (A.Loc 5 0)]
            ]
            [ A.Comment "// comment1" (A.Loc 2 10),
              A.Comment "// comment2" (A.Loc 4 10),
              A.Comment "// comment3" (A.Loc 5 10)
            ]
        )
        `shouldBe` syn "import \"proto2.proto\";\nimport \"proto3.proto\"; // comment1\n\n// comment2\nimport \"proto1.proto\"; // comment3"

enumFormating :: Spec
enumFormating = do
  describe "enums" $ do
    it "empty enum" $ do
      formatProto (ast [A.TLEEnum (A.Enumeration "MyEnum" [] (A.Loc 2 0) (A.Loc 2 10))] []) `shouldBe` syn "enum MyEnum {}"
    it "simple enum" $
      do
        formatProto
          ( ast
              [ A.TLEEnum
                  ( A.Enumeration
                      "MyEnum"
                      [ A.EEOption (A.Option "allow_alias" (A.COtherLiteral "true") (A.Loc 3 4)),
                        A.EEField (A.EnumField "unknown" 0 [] (A.Loc 3 4)),
                        A.EEField (A.EnumField "STARTED" (-1) [] (A.Loc 3 20)),
                        A.EEField (A.EnumField "running" 2 [] (A.Loc 4 4))
                      ]
                      (A.Loc 1 0)
                      (A.Loc 5 0)
                  )
              ]
              []
          )
        `shouldBe` syn "enum MyEnum {\n  option allow_alias = true;\n  UNKNOWN = 0;\n  STARTED = -1;\n  RUNNING = 2;\n}"
    it "enum with field options" $
      do
        formatProto
          ( ast
              [ A.TLEEnum
                  ( A.Enumeration
                      "MyEnum"
                      [ A.EEField (A.EnumField "a" 0 [A.FieldOption "(opt1)" (A.CStringLiteral "my_string")] (A.Loc 3 4)),
                        A.EEField
                          ( A.EnumField
                              "b"
                              1
                              [ A.FieldOption "opt2" (A.COtherLiteral "false"),
                                A.FieldOption "(opt3)" (A.COtherLiteral "-1.1")
                              ]
                              (A.Loc 4 20)
                          )
                      ]
                      (A.Loc 1 0)
                      (A.Loc 5 0)
                  )
              ]
              []
          )
        `shouldBe` syn "enum MyEnum {\n  A = 0 [(opt1) = \"my_string\"];\n  B = 1 [opt2 = false,(opt3) = -1.1];\n}"
    it "enum with comments" $
      do
        formatProto
          ( ast
              [ A.TLEEnum
                  ( A.Enumeration
                      "MyEnum"
                      [ A.EEField (A.EnumField "a" 0 [] (A.Loc 5 20)),
                        A.EEField (A.EnumField "b" 1 [A.FieldOption "o1" (A.COtherLiteral "2")] (A.Loc 5 30)),
                        A.EEOption (A.Option "o" (A.COtherLiteral "true") (A.Loc 6 4)),
                        A.EEField (A.EnumField "c" 2 [A.FieldOption "o2" (A.CStringLiteral "foo")] (A.Loc 7 4))
                      ]
                      (A.Loc 3 1)
                      (A.Loc 9 10)
                  )
              ]
              [ A.Comment "// comment1" (A.Loc 2 1),
                A.Comment "/* header */" (A.Loc 3 12),
                A.Comment "// comment2" (A.Loc 4 4),
                A.Comment "/*comment3*/" (A.Loc 5 4),
                A.Comment "// comment4" (A.Loc 6 20),
                A.Comment "/*comment5*/" (A.Loc 7 10),
                A.Comment "// comment6" (A.Loc 8 10),
                A.Comment "/*comment7*/" (A.Loc 9 1)
              ]
          )
        `shouldBe` syn "// comment1\nenum MyEnum { /* header */\n  // comment2\n  /*comment3*/ A = 0;\n  B = 1 [o1 = 2];\n  option o = true; // comment4\n  C = 2 [o2 = \"foo\"]; /*comment5*/\n  // comment6\n/*comment7*/ }"

messageFormatting :: Spec
messageFormatting = do
  describe "messages" $ do
    it "empty message" $ do
      formatProto (ast [A.TLEMessage (A.Message "MyMsg" [] (A.Loc 2 0) (A.Loc 2 10))] []) `shouldBe` syn "message MyMsg {}"
    it "message with enum" $
      do
        formatProto
          ( ast
              [ A.TLEMessage
                  ( A.Message
                      "MyMsg"
                      [A.MEEnum $ A.Enumeration "MyEnum" [A.EEField (A.EnumField "a" 0 [] (A.Loc 5 4))] (A.Loc 4 1) (A.Loc 6 1)]
                      (A.Loc 2 1)
                      (A.Loc 7 1)
                  )
              ]
              [A.Comment "//comment1" (A.Loc 3 0)]
          )
        `shouldBe` syn "message MyMsg {\n  //comment1\n  enum MyEnum {\n    A = 0;\n  }\n}"
    it "message with option" $
      do
        formatProto
          ( ast
              [ A.TLEMessage
                  ( A.Message
                      "MyMsg"
                      [ A.MEOption (A.Option "(my_option).a" (A.COtherLiteral "true") (A.Loc 3 3)),
                        A.MEOption (A.Option "b" (A.CStringLiteral "foo") (A.Loc 4 3))
                      ]
                      (A.Loc 2 1)
                      (A.Loc 5 1)
                  )
              ]
              []
          )
        `shouldBe` syn "message MyMsg {\n  option (my_option).a = true;\n  option b = \"foo\";\n}"
    it "message with oneof" $
      do
        formatProto
          ( ast
              [ A.TLEMessage
                  ( A.Message
                      "MyMsg"
                      [ A.MEOneOfField
                          ( A.OneOfField
                              "foo"
                              [ A.OFEVariant
                                  ( A.OneOfVariant
                                      "name"
                                      "string"
                                      "4"
                                      [ A.FieldOption "packed" (A.COtherLiteral "true"),
                                        A.FieldOption "a" (A.CStringLiteral "bar")
                                      ]
                                      (A.Loc 5 7)
                                  ),
                                A.OFEOption (A.Option "o" (A.COtherLiteral "45") (A.Loc 6 22)),
                                A.OFEVariant (A.OneOfVariant "sub_message" "SubMessage" "0XF" [] (A.Loc 7 7))
                              ]
                              (A.Loc 4 3)
                              (A.Loc 9 3)
                          )
                      ]
                      (A.Loc 2 1)
                      (A.Loc 10 1)
                  )
              ]
              [ A.Comment "// comment 1" (A.Loc 3 3),
                A.Comment "// comment 2" (A.Loc 4 15),
                A.Comment "// comment 3" (A.Loc 5 43),
                A.Comment "/* comment 4 */" (A.Loc 6 7),
                A.Comment "/* comment 5 */" (A.Loc 7 18),
                A.Comment "// comment 6" (A.Loc 8 7),
                A.Comment "/* comment 7 */" (A.Loc 9 10)
              ]
          )
        `shouldBe` syn "message MyMsg {\n  // comment 1\n  oneof foo { // comment 2\n    string name = 4 [packed = true,a = \"bar\"]; // comment 3\n    /* comment 4 */ option o = 45;\n    SubMessage sub_message = 0XF; /* comment 5 */\n    // comment 6\n  } /* comment 7 */\n}"
    it "message with empy oneof" $
      do
        formatProto
          ( ast
              [ A.TLEMessage
                  ( A.Message
                      "MyMsg"
                      [A.MEOneOfField (A.OneOfField "foo" [] (A.Loc 3 3) (A.Loc 3 10))]
                      (A.Loc 2 1)
                      (A.Loc 4 1)
                  )
              ]
              []
          )
        `shouldBe` syn "message MyMsg {\n  oneof foo {}\n}"
    it "message with normal fields" $
      do
        formatProto
          ( ast
              [ A.TLEMessage
                  ( A.Message
                      "MyMsg"
                      [ A.MENormalField (A.NormalField False "foo.Bar" "nested_message" "2" [] (A.Loc 4 3)),
                        A.MENormalField (A.NormalField True "int32" "samples" "0xFFF" [A.FieldOption "packed" (A.COtherLiteral "true")] (A.Loc 6 3))
                      ]
                      (A.Loc 2 1)
                      (A.Loc 7 1)
                  )
              ]
              [ A.Comment "// comment 1" (A.Loc 3 3),
                A.Comment "/* comment 2 */" (A.Loc 4 26),
                A.Comment "// comment 3" (A.Loc 5 3),
                A.Comment "// comment 4" (A.Loc 6 45)
              ]
          )
        `shouldBe` syn "message MyMsg {\n  // comment 1\n  foo.Bar nested_message = 2; /* comment 2 */\n  // comment 3\n  repeated int32 samples = 0xFFF [packed = true]; // comment 4\n}"
    it "message with map fields" $
      do
        formatProto
          ( ast
              [ A.TLEMessage
                  ( A.Message
                      "MyMsg"
                      [ A.MEMapField (A.MapField "string" "Project" "projects" "3" [] (A.Loc 4 3)),
                        A.MEMapField (A.MapField "string" "int32" "m" "9" [A.FieldOption "a" (A.COtherLiteral "2"), A.FieldOption "b" (A.CStringLiteral "foo")] (A.Loc 6 3))
                      ]
                      (A.Loc 2 1)
                      (A.Loc 7 1)
                  )
              ]
              [ A.Comment "// comment 1" (A.Loc 3 3),
                A.Comment "/* comment 2 */" (A.Loc 4 38),
                A.Comment "// comment 3" (A.Loc 5 3),
                A.Comment "/* comment 4 */" (A.Loc 6 45)
              ]
          )
        `shouldBe` syn "message MyMsg {\n  // comment 1\n  map<string, Project> projects = 3; /* comment 2 */\n  // comment 3\n  map<string, int32> m = 9 [a = 2,b = \"foo\"]; /* comment 4 */\n}"
    it "message with reserved statements" $
      do
        formatProto
          ( ast
              [ A.TLEMessage
                  ( A.Message
                      "MyMsg"
                      [ A.MEReservedStatement (A.ReservedStatement [A.RFNumberSingle "2", A.RFFieldName "foo", A.RFNumberRange "9" A.RFNREMax] (A.Loc 4 5)),
                        A.MEReservedStatement (A.ReservedStatement [A.RFNumberRange "8" (A.RFNRESingle "10")] (A.Loc 5 5))
                      ]
                      (A.Loc 2 1)
                      (A.Loc 6 1)
                  )
              ]
              [ A.Comment "// comment 1" (A.Loc 3 5),
                A.Comment "/* comment 2 */" (A.Loc 4 36),
                A.Comment "/* comment 3 */" (A.Loc 5 14),
                A.Comment "// comment 4" (A.Loc 5 39)
              ]
          )
        `shouldBe` syn "message MyMsg {\n  // comment 1\n  reserved 2, \"foo\", 9 to \"max\"; /* comment 2 */\n  reserved 8 to 10; /* comment 3 */ // comment 4\n}"
    it "message with comments" $
      do
        formatProto
          ( ast
              [ A.TLEMessage
                  ( A.Message
                      "MyMsg"
                      [ A.MEOption (A.Option "(my_option).a" (A.COtherLiteral "true") (A.Loc 4 3)),
                        A.MEOption (A.Option "b" (A.CStringLiteral "foo") (A.Loc 6 17))
                      ]
                      (A.Loc 3 1)
                      (A.Loc 8 1)
                  )
              ]
              [ A.Comment "// comment1" (A.Loc 2 0),
                A.Comment "/* header */" (A.Loc 3 15),
                A.Comment "// comment2" (A.Loc 4 32),
                A.Comment "// comment3" (A.Loc 5 3),
                A.Comment "/* comment4 */" (A.Loc 6 3),
                A.Comment "// comment5" (A.Loc 7 3),
                A.Comment "// comment6" (A.Loc 8 3)
              ]
          )
        `shouldBe` syn "// comment1\nmessage MyMsg { /* header */\n  option (my_option).a = true; // comment2\n  // comment3\n  /* comment4 */ option b = \"foo\";\n  // comment5\n} // comment6"

topLevelCommentAssembly :: Spec
topLevelCommentAssembly = do
  describe "Top Level Comment Assembly" $ do
    it "comment below" $ do
      formatProto (ast [] [A.Comment "// below" (A.Loc 2 0)]) `shouldBe` "syntax = \"proto3\";\n\n// below"
    it "comment above" $ do
      formatProto (ast [] [A.Comment "/* above */" (A.Loc 0 1)]) `shouldBe` "/* above */\nsyntax = \"proto3\";"
    it "comment before" $ do
      formatProto (ast [] [A.Comment "/* before */" (A.Loc 1 0)]) `shouldBe` "/* before */ syntax = \"proto3\";"
    it "comment after" $ do
      formatProto (ast [] [A.Comment "//after" (A.Loc 1 2)]) `shouldBe` "syntax = \"proto3\"; //after"
    it "comment above and below" $
      do
        formatProto
          ( ast
              []
              [ A.Comment "/* above 1 */" (A.Loc 0 1),
                A.Comment "//above2" (A.Loc 0 20),
                A.Comment "/*bellow1*/" (A.Loc 2 3),
                A.Comment "//bellow2" (A.Loc 3 0)
              ]
          )
        `shouldBe` "/* above 1 */\n//above2\nsyntax = \"proto3\";\n\n/*bellow1*/\n//bellow2"
