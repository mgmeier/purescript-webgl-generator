-----------------------------------------------------------------------------
--
-- Module      :  IDL.Parser
-- Copyright   :
-- License     :  GPL-2
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDL.Parser (
    idlParser
) where

import  IDL.AST

import qualified Text.Parsec.Token as PP
import qualified Text.Parsec as PP
import qualified Text.Parsec.Error as PP
import Data.Functor.Identity (Identity(..))
import qualified Text.ParserCombinators.Parsec.Language as PP
import qualified Text.ParserCombinators.Parsec as PP (Parser)
import Control.Monad (liftM)
trace _ b = b

type Parse a = PP.Parsec String () a

idlParser :: Parse Idl
idlParser = PP.manyTill
                (do whiteSpace'
                    d <- declParser
                    trace ("decl: " ++ show d) $ return d)
                PP.eof
    PP.<?> "expecting idl"

lexer :: PP.GenTokenParser String u Identity
lexer = PP.makeTokenParser PP.emptyDef

symbol'      = PP.symbol   lexer
whiteSpace'  = PP.whiteSpace lexer
identifier'  = PP.identifier lexer
integer'     = PP.integer lexer
semi'        = PP.semi lexer
parens'      = PP.parens lexer
brackets'    = PP.brackets lexer
angles'      = PP.angles lexer

declParser :: Parse Decl
declParser = PP.try (do
      symbol' "const"
      symbol' "GLenum"
      name <- identifier'
      symbol' "="
      value <- integer'
      semi'
      return Enum {
        enumName = name,
        enumValue = value})
  PP.<|> PP.try (do
      symbol' "/*"
      comment <- PP.manyTill PP.anyChar (symbol' "*/")
      return (Comment {comment = comment}))
  PP.<|> PP.try (do
      returnType <- parseType
      methodName <- identifier'
      args <- parens' (PP.sepBy parseArg (symbol' ","))
      condRaises <- PP.option Nothing parseRaises
      semi'
      return (Function
                { methodName = methodName,
                  methodRetType = returnType,
                  methodArgs = args,
                  methodRaises = condRaises}))
  PP.<|> PP.try (do
      isReadonly <- PP.option False (do
                                symbol' "readonly"
                                return True)
      symbol' "attribute"
      typ <- parseType
      name <- identifier'
      semi'
      return (Attribute
                { attIsReadonly = isReadonly,
                  attType = typ,
                  attName = name}))
  PP.<?> "expecting decl"

parseType :: Parse Type
parseType = do
    id      <- identifier'
    isArray <- PP.option False (do
                                brackets' whiteSpace'
                                return True)
    condPara <- if id == "sequence"
                        then (do
                                id <- angles' identifier'
                                return (Just id))
                        else return Nothing
    return (Type {typeName = id,
                  typeIsArray = isArray,
                  typeCondPara = condPara})
  PP.<?> "expecting type"

parseArg :: Parse Arg
parseArg = do
    typ <- parseType
    id <-  identifier'
    return (Arg {argType = typ,
                 argName = id})

parseRaises :: Parse (Maybe String)
parseRaises = do
    symbol' "raises"
    liftM Just (parens' identifier')

