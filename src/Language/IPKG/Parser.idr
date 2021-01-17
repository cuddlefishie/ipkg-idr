-- SPDX-FileCopyrightText: 2021 The ipkg-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.IPKG.Parser

import Language.IPKG.Tokens
import Language.IPKG.Value

import Text.Parser
import Text.Token

import Data.List
import Data.List1

%default total

private
punct : Punctuation -> Grammar IPKGToken True ()
punct p = match $ ITPunct p


private
string : Grammar IPKGToken True String
string = match ITString

private
bare : Grammar IPKGToken True Identifier
bare = match ITBare

private
moduleIdent : Grammar IPKGToken True ModuleIdent
moduleIdent = do
    x <- sepBy1' (punct Dot) bare
    pure $ case x of
            (s :: rest ** _) => s ::: rest

private
keyEq : Key -> Grammar IPKGToken True ()
keyEq k = do
    match (ITKey k)
    commit
    punct Equal
    pure ()

private
keyVal : (key : Key) -> (val : Grammar IPKGToken True a) -> (update : a -> PkgDesc) -> Grammar IPKGToken True PkgDesc
keyVal key val update = do
    keyEq key
    v <- val
    pure $ update v


private
field : (p : PkgDesc) -> Grammar IPKGToken True PkgDesc
field p = 
        keyVal KVersion string (\s => { version := s } p)
    <|> keyVal KAuthors string (\s => { authors := s } p)

    <|> keyVal KMaintainers string (\s => { maintainers := Just s } p)

    <|> keyVal KLicense string (\s => { license := Just s } p)
    <|> keyVal KBrief string (\s => { brief := Just s } p)
    <|> keyVal KReadme string (\s => { readme := Just s } p)
    <|> keyVal KHomepage string (\s => { homepage := Just s } p)
    <|> keyVal KSourceLoc string (\s => { sourceloc := Just s } p)
    <|> keyVal KBugtracker string (\s => { bugtracker := Just s } p)

    <|> keyVal KDepends (sepBy1 (punct Comma) bare) (\m => { depends := m } p)
    <|> keyVal KModules (sepBy1 (punct Comma) moduleIdent) (\m => { modules := m } p)

    <|> keyVal KMainMod moduleIdent (\s => { mainmod := Just s } p)
    
    <|> keyVal KExecutable (string <|> bare) (\s => { executable := Just s } p)
    <|> keyVal KOptions string (\s => { options := Just s } p)
    <|> keyVal KSourceDir string (\s => { sourcedir := Just s } p)
    <|> keyVal KBuildDir string (\s => { builddir := Just s } p)
    <|> keyVal KOutputDir string (\s => { outputdir := Just s } p)

    <|> keyVal KPreBuild string (\s => { prebuild := Just s } p)
    <|> keyVal KPostBuild string (\s => { postbuild := Just s } p)
    <|> keyVal KPreInstall string (\s => { preinstall := Just s } p)
    <|> keyVal KPostInstall string (\s => { postinstall := Just s } p)
    <|> keyVal KPreClean string (\s => { preclean := Just s } p)
    <|> keyVal KPostClean string (\s => { postclean := Just s } p)



private
fields : (p : PkgDesc) -> Grammar IPKGToken False PkgDesc
fields p = (eof >>= \_ => pure p)
       <|> (field p >>= fields)


private
packageDesc : Grammar IPKGToken True PkgDesc
packageDesc = do
    match ITPackage
    name <- bare

    pkg <- pure $ MkPkgDesc {
                name = name,
                version = "",
                authors = "",

                maintainers = Nothing,
                license     = Nothing,
                brief       = Nothing,
                readme      = Nothing,
                homepage    = Nothing,
                sourceloc   = Nothing,
                bugtracker  = Nothing,
                depends     = [],
                modules     = [],
                mainmod     = Nothing,
                executable  = Nothing,
                options     = Nothing,
                sourcedir   = Nothing,
                builddir    = Nothing,
                outputdir   = Nothing,
                prebuild    = Nothing,
                postbuild   = Nothing,
                preinstall  = Nothing,
                postinstall = Nothing,
                preclean    = Nothing,
                postclean   = Nothing
            }

    fields pkg <* eof

export
parseIPKGFromToks : (toks : List IPKGToken) -> Either String PkgDesc
parseIPKGFromToks toks = case parse packageDesc $ filter (not . ignored) toks of
    Right (pkg, []) => Right pkg
    Right _ => Left "unconsumed input"
    Left (Error msg _) => Left msg
