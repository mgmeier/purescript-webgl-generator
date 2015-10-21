-----------------------------------------------------------------------------
--
-- Module      :  IDL.Printer
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

module IDL.Printer (
  ppPureScriptFFI,
  ppPureJavaScript
) where

import IDL.AST

import Text.PrettyPrint
       (sep, ($$), hcat, punctuate, semi, braces, empty, parens, nest,
        (<>), integer, (<+>), text, vcat, ($+$), Doc)
import Data.List (nubBy, nub)
import Data.Maybe (isNothing)
import Data.Function(on)

ppPureScriptFFI :: Idl -> Doc
ppPureScriptFFI idl =
        header
    $+$ text "-- *TypeDecls" $+$ typeDecls $+$ text ""
    $+$ text "-- *Constants" $+$ constants $+$ text ""
    $+$ text "-- *Methods" $+$ methods

  where
    header = vcat (map text
        (["-- Auto generated: don't change manually, use purescript-webgl-generator to modify!!",
        "module Graphics.WebGLRaw where",
        "",
        "import Control.Monad.Eff",
        "import Control.Monad.Eff.WebGL",
        "import Data.ArrayBuffer.Types",
        "import Data.TypedArray",
        "import Data.Function",
        "import Prelude",
        "",
        ""] ++ typedefs))

    typeDecls = vcat $ map printTypeDecl $ nubBy ((==) `on` typeName)
                    [t  | d <- idl, t <- extractTypes d, typeName t `notElem` standardTypes]
    printTypeDecl d = text "foreign import data" <+> text (typeName d) <+> text ":: *"

    constants = vcat [printConstant c | c <- idl , isEnum c]
    printConstant Enum{enumName = n,enumValue = v} =
            (text (makeConstantName n) <+> text "::" <+> text "Int")
        $+$ (text (makeConstantName n) <+> text "=" <+> integer v)
        $+$ text ""

    methods = vcat $ map printMethod $ nubBy ((==) `on` methodName)
                    [c | c <- idl , isUsableFunction c]
    printMethod f =
        text "foreign import" <+> text (methodName f) <> text "_" <>
            nest 2 (printPurescriptTypes f)
                $$ text ""

    printPurescriptTypes f | typeName (methodRetType f) == "any" ||
                             typeName (methodRetType f) == "object" =
        text ":: forall eff ret." <+> text (fnFrom (length (methodArgs f))) <+>
        sep (punctuate (text " ") (map (pureScriptType . argType) (methodArgs f)) ++
                [parens (text "Eff (webgl :: WebGl | eff) ret")])

    printPurescriptTypes f = text ":: forall eff." <+> text (fnFrom (length (methodArgs f))) <+>
        sep (punctuate (text " ") (map (pureScriptType . argType) (methodArgs f)) ++
                [parens (
                        text "Eff (webgl :: WebGl | eff)" <+>
                        pureScriptType (methodRetType f))])

    pureScriptType Type{typeName = t} | t == "void" = text "Unit"
    pureScriptType Type{typeName = t} | t == "boolean" = text "Boolean"
    pureScriptType Type{typeName = t} | t == "DOMString" = text "String"
    pureScriptType Type{typeName = t} | t == "ArrayBuffer" = text "Float32Array"
    pureScriptType Type{typeName = t} = text t

fnFrom n | n < 11 = "Fn" ++ show n
         | otherwise = error ("fnFrom called on n > 10 n:" ++ show n)


makeConstantName n = '_' : n

javascriptQuotes :: Doc -> Doc
javascriptQuotes p       = text "\"\"\"" <> p <> text "\"\"\""

extractTypes :: Decl -> [Type]
extractTypes Function{methodRetType = t1, methodArgs = args} = t1 : map extractArgType args
extractTypes Attribute{attType = t1} = [t1]
extractTypes _ = []

extractArgType :: Arg -> Type
extractArgType Arg{argType = t} = t

isUsableFunction i =
    isFunction i && all (isNothing . typeCondPara . argType) (methodArgs i)

standardTypes = ["GLenum", "GLboolean", "GLbitfield", "GLbyte","GLshort","GLint",
    "GLsizei","GLintptr","GLsizeiptr","GLubyte","GLushort","GLuint","GLfloat","GLclampf",
    "sequence", "void","boolean","any","object","DOMString","HTMLCanvasElement",
    "Float32Array","Int32Array","FloatArray","ArrayBuffer"]

typedefs = [
    "type GLenum = Int",
    "type GLboolean = Boolean",
    "type GLbitfield = Int",
    "type GLbyte = Int",
    "type GLshort = Int",
    "type GLint = Int",
    "type GLsizei = Int",
    "type GLintptr = Int",
    "type GLsizeiptr = Int",
    "type GLubyte = Int",
    "type GLushort = Int",
    "type GLuint = Int",
    "type GLfloat = Number",
    "type GLclampf = Number",
    "type FloatArray = Float32Array",
    ""]

ppPureJavaScript :: Idl -> Doc
ppPureJavaScript idl =
        jsHeader
    $+$ jsMethods
    $+$ jsFooter
  where
    jsHeader = vcat (map text
        ["// Auto generated: don't change manually, use purescript-webgl-generator to modify!!",
         "/* global exports */",
         "",
         "// module Graphics.WebGLRaw",
         "",
         "  \"use strict\";",
         "",
         ""])
    jsFooter = text ""
    jsMethods = nest 2 (vcat $ map (\f -> printJSMethod f (methodArgs f))
                                $ nubBy ((==) `on` methodName)
                                    [c | c <- idl , isUsableFunction c])
    printJSMethod f args =
        nest 2 (text "exports." <> text (methodName f) <> text "_" <+> text "=" <+>
            (if null (methodArgs f)
                then text "function () "
                else text "function" <+>
                        parens (hcat (punctuate (text ",") (map (text . argName) (methodArgs f)))))
                $$ braces (nest 2 (text "return function ()")
                                $$ braces (nest 2 (printJavascriptRest f)) $$ semi)
        $$ text "")

    printJavascriptRest f | typeName (methodRetType f) == "void" =
        text "gl." <> text (methodName f) <> parens
            (hcat (punctuate (text ",") (map (text . argName) (methodArgs f)))) <>  semi
                          | otherwise =
        text "var res = gl." <> text (methodName f) <> parens
            (hcat (punctuate (text ",") (map (text . argName) (methodArgs f)))) <>  semi
        $$ text "if (res === undefined){"
        $$ text "  throw \"Undefined in " <+> text (methodName f) <> text "\"}" <> semi
        $$ text "return res" <> semi
