-----------------------------------------------------------------------------
--
-- Module      :  IDL.AST
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

module IDL.AST where

type Idl = [Decl]

isEnum :: Decl -> Bool
isEnum Enum{}  = True
isEnum _       = False

isFunction :: Decl -> Bool
isFunction Function{}  = True
isFunction _       = False

data Decl =
  Enum
    { enumName  :: String
    , enumValue :: Integer
    }
  |
  Comment
      { comment  :: String
      }
  | Function
    { methodName :: String
    , methodRetType :: Type
    , methodArgs :: [Arg]
    , methodRaises :: Maybe String
    }
  | Attribute
    { attIsReadonly :: Bool
    , attType :: Type
    , attName :: String
    }
  deriving (Eq,Show)

data Type =
    Type { typeName :: String
         , typeIsArray :: Bool
         , typeCondPara :: Maybe String}
  deriving (Eq,Show)

data Arg =
    Arg {argType :: Type,
         argName :: String}
  deriving (Eq,Show)


