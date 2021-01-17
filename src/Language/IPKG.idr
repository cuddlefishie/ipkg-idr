-- SPDX-FileCopyrightText: 2021 The ipkg-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.IPKG

import public Language.IPKG.Value
import Language.IPKG.Lexer
import Language.IPKG.Parser

export
parseIPKG : (source : String) -> Either String PkgDesc
parseIPKG source = do
    Just toks <- pure $ lexIPKG source
        | Nothing => Left "Lexer error"
    
    parseIPKGFromToks toks