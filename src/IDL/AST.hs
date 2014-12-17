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


{-
  = MPMessage
    { msgName :: T.Text
    , msgParam :: [T.Text]
    , msgFields :: [Field]
    }
  | MPException
    { excName :: T.Text
    , excParam :: [T.Text]
    , excSuper :: Maybe T.Text
    , excFields :: [Field]
    }
  | MPType
    { tyName :: T.Text
    , tyType :: Type
    }
  | MPEnum
    { enumName :: T.Text
    , enumMem :: [(Int, T.Text)]
    }
  | MPService
    { serviceName :: T.Text
    , serviceVersion :: Maybe Int
    , serviceMethods :: [Method]
    }
  deriving (Eq, Show, Data, Typeable)

data Field
  = Field
    { fldId :: Int
    , fldType :: Type
    , fldName :: T.Text
    , fldDefault :: Maybe Literal
    }
  deriving (Eq, Show, Data, Typeable)

data Method
  = Function
    { methodInherit :: Bool
    , methodName :: T.Text
    , methodRetType :: Maybe Type
    , methodArgs :: [Field]
    }
  | InheritName T.Text
  | InheritAll
  deriving (Eq, Show, Data, Typeable)

data Type
  = TInt Bool Int -- signed? bits
  | TFloat Bool   -- double prec?
  | TBool
  | TRaw
  | TString
  | TNullable Type
  | TList Type
  | TMap Type Type
  | TTuple [Type]
  | TUserDef T.Text [Type]
  | TObject
  deriving (Eq, Show, Data, Typeable)

data Literal
  = LInt Int
  | LFloat Double
  | LBool Bool
  | LNull
  | LString T.Text
  deriving (Eq, Show, Data, Typeable)
-}
