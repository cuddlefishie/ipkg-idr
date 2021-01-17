-- SPDX-FileCopyrightText: 2021 The ipkg-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.IPKG.Lexer

import Text.Lexer
import Text.Token

import public Language.IPKG.Tokens

%default total

private
bareKey : Lexer
bareKey =
    -- start of the identifier can not be `-`
    (alphaNum <|> is '_') <+>
    many (alphaNum <|> is '_' <|> is '-')

-- TODO doesn't handle single quoted strings or escapes yet
private
stringLit : Lexer
stringLit = Text.Lexer.stringLit

private
key : String -> Lexer
key = exact

private
ipkgTokenMap : TokenMap IPKGToken
ipkgTokenMap = toTokenMap $
    [
        (lineComment (exact "--"), ITIgnored),
        (spaces, ITIgnored),
        (is ',', ITPunct Comma),
        (is '.', ITPunct Dot),
        (is '=', ITPunct Equal),
       
        (exact "package", ITPackage),

        -- keys
        (exact "version", ITKey KVersion),
        (exact "authors", ITKey KAuthors),
        (exact "maintainers", ITKey KMaintainers),
        (exact "license", ITKey KLicense),
        (exact "brief", ITKey KBrief),
        (exact "readme", ITKey KReadme),
        (exact "homepage", ITKey KHomepage),
        (exact "sourceloc", ITKey KSourceLoc),
        (exact "bugtracker", ITKey KBugtracker),
        (exact "depends", ITKey KDepends),
        (exact "modules", ITKey KModules),
        (exact "main", ITKey KMainMod),
        (exact "executable", ITKey KExecutable),
        (exact "options", ITKey KOptions),
        (exact "opts", ITKey KOptions),
        (exact "sourcedir", ITKey KSourceDir),
        (exact "builddir", ITKey KBuildDir),
        (exact "outputdir", ITKey KOutputDir),
        (exact "prebuild", ITKey KPreBuild),
        (exact "postbuild", ITKey KPostBuild),
        (exact "preinstall", ITKey KPreInstall),
        (exact "postinstall", ITKey KPostInstall),
        (exact "preclean", ITKey KPreClean),
        (exact "postclean", ITKey KPostClean),

        (Language.IPKG.Lexer.stringLit, ITString), -- TODO doesn't handle all escapes
        (bareKey, ITBare)
    ]

export
lexIPKGTokData : String -> Maybe (List (TokenData IPKGToken))
lexIPKGTokData str =
    case lex ipkgTokenMap str of
        (tokens, (_, _, "")) => Just tokens
        _ => Nothing

export
lexIPKG : String -> Maybe (List (IPKGToken))
lexIPKG str = do
    toks <- lexIPKGTokData str
    pure $ map (.tok) toks