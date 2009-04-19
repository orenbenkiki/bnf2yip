module ParseSyntax (
 parseSyntax
) where

import           CharSet
import qualified Data.Map as Map
import           Data.Maybe
import           FixPrecedence
import           Language.Haskell.Parser
import           Language.Haskell.Syntax
import           Syntax
import           Text.Regex

-- The list of precedences that are used in the YamlReference BNF file.
precedences = Map.fromList [ (UnQual (HsSymbol "^"),  (HsAssocNone,  3)),
                             (UnQual (HsSymbol "%"),  (HsAssocNone,  3)),
                             (UnQual (HsSymbol "<%"), (HsAssocNone,  3)),
                             (UnQual (HsSymbol "!"),  (HsAssocNone,  3)),
                             (UnQual (HsSymbol "?!"), (HsAssocNone,  3)),
                             (UnQual (HsSymbol "-"),  (HsAssocLeft,  3)),
                             (UnQual (HsSymbol "&"),  (HsAssocRight, 2)),
                             (UnQual (HsSymbol "/"),  (HsAssocRight, 1)),
                             (UnQual (HsSymbol "?"),  (HsAssocNone,  0)),
                             (UnQual (HsSymbol "*"),  (HsAssocNone,  0)),
                             (UnQual (HsSymbol "+"),  (HsAssocNone,  0)),
                             (UnQual (HsSymbol "<?"), (HsAssocNone,  0)),
                             (UnQual (HsSymbol ">?"), (HsAssocNone,  0)),
                             (UnQual (HsSymbol "<!"), (HsAssocNone,  0)),
                             (UnQual (HsSymbol ">!"), (HsAssocNone,  0)),
                             (UnQual (HsSymbol "$"),  (HsAssocRight, 0)) ]


-- @parseSyntax text@ converts the @text@ to a @Syntax@ object.
parseSyntax :: String -> (Syntax, [ String ])

parseSyntax text = parsedToSyntax $ parseModule text


-- @parsedToSyntax@ converts the parsed Haskell code of the BNF into a
-- @Syntax@ object we can work with.
parsedToSyntax (ParseOk (HsModule _ _ _ _ productions)) =
  let syntax = take 70 $ catMaybes $ map parsedToProduction productions
      index = map fst syntax
  in (Map.fromList syntax, index)

parsedToSyntax (ParseFailed _ problem) = error $ "Parse failed: " ++ problem


-- @parsedToProduction@ converts a parsed Haskell function to a @Production@.
parsedToProduction (HsInfixDecl _ _ _ _) = Nothing

parsedToProduction (HsPatBind _ (HsPVar (HsIdent name)) (HsUnGuardedRhs pattern) _) =
  Just $ (fixName name, ([], patternToSyntax $ withPrecExp precedences pattern))

parsedToProduction (HsFunBind [HsMatch _ (HsIdent name) parameters (HsUnGuardedRhs pattern) _]) =
  Just $ (fixName name, (map parameterToSyntax parameters, patternToSyntax $ withPrecExp precedences pattern))

parsedToProduction unmatched = error $ "Production: " ++ show unmatched


-- @parameterToSyntax@ converts a parsed Haskell parameter to a @Syntax@
-- variable @Node@.
parameterToSyntax (HsPVar (HsIdent name)) = Variable name

parameterToSyntax unmatched = error $ "Parameter: " ++ show unmatched


-- @patternToSyntax@ converts a parsed Haskell code fragment to a @Node@.
patternToSyntax (HsParen pattern) = patternToSyntax pattern

patternToSyntax (HsLit (HsChar code)) = Chars $ codeSet code

patternToSyntax (HsTuple [HsLit (HsChar low), HsLit (HsChar high)]) = Chars $ rangeSet low high

patternToSyntax (HsApp (HsVar (UnQual (HsIdent "bom"))) (HsLit (HsChar '\65279'))) = Token "Bom" $ Chars $ rangeSet '\65279' '\65279'

patternToSyntax (HsApp (HsVar (UnQual (HsIdent "nest"))) (HsCon (UnQual (HsIdent name)))) = EmptyToken name

patternToSyntax (HsApp (HsApp (HsVar (UnQual (HsIdent "token"))) (HsCon (UnQual (HsIdent name)))) pattern) = Token name $ patternToSyntax pattern

patternToSyntax (HsVar (UnQual (HsIdent name))) =
  case name of
       "asInteger" -> Results [ AsInteger ]
       "nextLine"  -> NextLine
       "empty"     -> Empty
       "sol"       -> StartOfLine
       "eof"       -> EndOfFile
       _           -> Call (fixName name) []

patternToSyntax (HsCon (UnQual (HsIdent name))) =
  case name of
       "BlockIn"  -> Results [ Symbol name ]
       "BlockOut" -> Results [ Symbol name ]
       "FlowIn"   -> Results [ Symbol name ]
       "FlowOut"  -> Results [ Symbol name ]
       "FlowKey"  -> Results [ Symbol name ]

patternToSyntax (HsApp (HsVar (UnQual (HsIdent "result"))) (HsTuple expressions)) = Results $ map expressionToSyntax expressions

patternToSyntax (HsApp (HsVar (UnQual (HsIdent name))) argument) =
  case name of
       "indicator" -> Token "Indicator" $ patternToSyntax argument
       "meta"      -> Token "Meta" $ patternToSyntax argument
       "text"      -> Token "Text" $ patternToSyntax argument
       "peek"      -> Peek $ patternToSyntax argument
       "nonEmpty"  -> NonEmpty $ patternToSyntax argument
       "result"    -> Results [ expressionToSyntax argument ]
       _           -> Call (fixName name) [ expressionToSyntax argument ]

patternToSyntax (HsApp (HsApp (HsVar (UnQual (HsIdent name))) argument1) argument2) =
  Call (fixName name) [ expressionToSyntax argument1, expressionToSyntax argument2 ]

patternToSyntax pattern@(HsInfixApp left (HsQVarOp (UnQual (HsSymbol operator))) right) =
  case operator of
       "&"  -> And        [ patternToSyntax left,         patternToSyntax right ]
       "/"  -> Or         [ patternToSyntax left,         patternToSyntax right ]
       "-"  -> AndNot      (patternToSyntax left)        (patternToSyntax right)
       "^"  -> Choice      (choiceToSyntax  left)        (patternToSyntax right)
       "!"  -> And       [  patternToSyntax left, Commit $ choiceToSyntax right ]
       "%"  -> RepeatN  (expressionToSyntax right)       (patternToSyntax left)
       "<%" -> UptoN    (expressionToSyntax right)       (patternToSyntax left)
       "?!" -> And [ Peek $ patternToSyntax left, Commit $ choiceToSyntax right ]
       "$"  -> patternToSyntax (HsApp left right)
       ".-" -> Results [ expressionToSyntax pattern ]
       ".+" -> Results [ expressionToSyntax pattern ]
       _    -> error $ "Infix: " ++ show (operator, left, right)

patternToSyntax (HsDo statements) = And $ map statementToSyntax statements

patternToSyntax (HsLeftSection pattern (HsQVarOp (UnQual (HsSymbol operator)))) =
  case operator of
       "?"  -> Optional   $ patternToSyntax pattern
       "+"  -> OneOrMore  $ patternToSyntax pattern
       "*"  -> ZeroOrMore $ patternToSyntax pattern
       ">?" -> Peek       $ patternToSyntax pattern
       "<?" -> Prev       $ patternToSyntax pattern
       ">!" -> Reject     $ patternToSyntax pattern
       _   -> error $ "Postfix: " ++ show (operator, pattern)

patternToSyntax (HsCase (HsVar (UnQual (HsIdent name))) alternatives) = Case (Variable name) $ map alternativeToSyntax alternatives

patternToSyntax (HsList patterns) = And $ map patternToSyntax patterns

patternToSyntax (HsInfixApp pattern (HsQVarOp (UnQual (HsIdent "limitedTo"))) (HsLit (HsInt limit))) = LimitedTo (Value $ fromIntegral limit) (patternToSyntax pattern)

patternToSyntax (HsInfixApp pattern (HsQVarOp (UnQual (HsIdent "forbidding"))) forbidden) = Forbidding (patternToSyntax forbidden) (patternToSyntax pattern)

patternToSyntax unmatched = error $ "Pattern: " ++ show unmatched


-- @alternativeToSyntax@ converts a parse Haskell code fragment to a @Case@
-- alternative.
alternativeToSyntax (HsAlt _ (HsPApp (UnQual (HsIdent value)) []) (HsUnGuardedAlt alternative@(HsVar (UnQual (HsIdent name)))) []) =
  case name of
       "n" -> (Symbol value, Results [ expressionToSyntax alternative ])
       _   -> (Symbol value, Call (fixName name) [])

alternativeToSyntax (HsAlt _ (HsPApp (UnQual (HsIdent value)) []) (HsUnGuardedAlt action) []) = (Symbol value, patternToSyntax action)

alternativeToSyntax unmatched = error $ "Alternative: " ++ show unmatched


-- @choiceToSyntax@ converts a parsed Haskell code fragment to a @Choice@
-- name.
choiceToSyntax (HsLit (HsString name)) = name

choiceToSyntax unmatched = error $ "Choice: " ++ show unmatched


-- @expressionToSyntax@ converts a parsed Haskell code fragment to a @Node@.
expressionToSyntax (HsVar (UnQual (HsIdent name))) = Variable name

expressionToSyntax (HsLit (HsInt value)) = Value $ fromIntegral value

expressionToSyntax (HsCon (UnQual (HsIdent name))) = Symbol name

expressionToSyntax (HsInfixApp left (HsQVarOp (UnQual (HsSymbol ".+"))) right) = Plus (expressionToSyntax left) (expressionToSyntax right)

expressionToSyntax (HsInfixApp left (HsQVarOp (UnQual (HsSymbol ".-"))) right) = Minus (expressionToSyntax left) (expressionToSyntax right)

expressionToSyntax (HsNegApp expression) = Minus (Value 0) (expressionToSyntax expression)

expressionToSyntax (HsParen expression) = expressionToSyntax expression

expressionToSyntax (HsApp (HsVar (UnQual (HsIdent name))) argument) = Call (fixName name) [ expressionToSyntax argument ]

expressionToSyntax (HsApp (HsApp (HsVar (UnQual (HsIdent name))) argument1) argument2) =
  case name of
       "max" -> Max (expressionToSyntax argument1) (expressionToSyntax argument2)
       _     -> Call (fixName name) [ expressionToSyntax argument1, expressionToSyntax argument2 ]

expressionToSyntax unmatched = error $ "Expression: " ++ show unmatched


-- @statementToSyntax@ converts a parsed Haskell code fragment to an @Assign@
-- statement.
statementToSyntax (HsGenerator _ (HsPTuple variables) pattern) = Assign (map variableToSyntax variables) (patternToSyntax pattern)

statementToSyntax (HsGenerator _ variable pattern) = Assign [ variableToSyntax variable ] (patternToSyntax pattern)

statementToSyntax (HsQualifier pattern) = patternToSyntax pattern

statementToSyntax unmatched = error $ "Statement: " ++ show unmatched


-- @variableToSyntax@ converts a parsed Haskell code fragment to a variable
-- name.
variableToSyntax (HsPVar (HsIdent name)) = Variable name

variableToSyntax unmatched = error $ "Variable: " ++ show unmatched


-- @fixName name@ fixes the \"_\" based input names to a spec-compatible \"-\"
-- based name.
fixName name   = name''
  where name'  = subRegex (mkRegex "__") name "+"
        name'' = subRegex (mkRegex "_") name' "-"
