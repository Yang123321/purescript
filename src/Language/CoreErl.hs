-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CoreErl
-- Copyright   :  (c) Feng Lee 2020
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Feng Lee <feng@emqx.io>
-- Stability   :  experimental
-- Portability :  portable
--
-- The Syntax/Parser/Pretty Printer for Core Erlang.
--
-----------------------------------------------------------------------------
module Language.CoreErl
  ( parseFile
  , moduleToErl
  , module CErl
  ) where

import Prelude
import System.IO

import Language.CoreErl.Syntax as CErl
import Language.CoreErl.Parser as CErl
import Language.CoreErl.Pretty as CErl

import Language.PureScript.AST.SourcePos
import Language.PureScript.CodeGen.JS.Common as Common
import Language.PureScript.CoreImp.AST (AST, everywhereTopDownM, withSourceSpan)
import qualified Language.PureScript.CoreImp.AST as AST
import Language.PureScript.CoreImp.Optimizer
import Language.PureScript.CoreFn
import Language.PureScript.Crash
import Language.PureScript.Errors (ErrorMessageHint(..), SimpleErrorMessage(..),
                                   MultipleErrors(..), rethrow, errorMessage,
                                   errorMessage', rethrowWithPosition, addHint)
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.PSString (PSString, mkString)
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants.Prim as C

-- purescript AST -> CoreErl AST
{-
moduleToErl :: (Module Ann) -> EModule
moduleToErl (Module _ coms mn _ imps exps foreigns decls) =
    -- 1. Dump ps AST
    transAST
    -- 2. Transform: PS AST -> Erl AST
    EModule
    -- 3. print EModule -> String
    prettyPrint
    -- 4. String -> FilePath
--
-}

moduleToErl :: EModule -> String
moduleToErl = prettyPrint

parseFile :: FilePath -> IO (Either ParseError (EAnn EModule))
parseFile fp = readUTF8File fp >>= return . parseModule

readUTF8File :: FilePath -> IO (String)
readUTF8File fp = do
    h <- openFile fp ReadMode
    hSetEncoding h utf8
    hGetContents h

