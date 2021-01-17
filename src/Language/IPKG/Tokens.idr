-- SPDX-FileCopyrightText: 2021 The ipkg-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.IPKG.Tokens

import Text.Token

%default total

public export
data Punctuation
    = Comma
    | Dot
    | Equal

public export
Eq Punctuation where
    (==) Comma Comma = True
    (==) Dot Dot = True
    (==) Equal Equal = True
    (==) _ _ = False


public export
data Key
    = KVersion
    | KAuthors
    | KMaintainers
    | KLicense
    | KBrief
    | KReadme
    | KHomepage
    | KSourceLoc
    | KBugtracker
    | KDepends
    | KModules
    | KMainMod
    | KExecutable
    | KOptions
    | KSourceDir
    | KBuildDir
    | KOutputDir
    | KPreBuild
    | KPostBuild
    | KPreInstall
    | KPostInstall
    | KPreClean
    | KPostClean

public export
Eq Key where
    KVersion == KVersion = True
    KAuthors == KAuthors = True
    KMaintainers == KMaintainers = True
    KLicense == KLicense = True
    KBrief == KBrief = True
    KReadme == KReadme = True
    KHomepage == KHomepage = True
    KSourceLoc == KSourceLoc = True
    KBugtracker == KBugtracker = True
    KDepends == KDepends = True
    KModules == KModules = True
    KMainMod == KMainMod = True
    KExecutable == KExecutable = True
    KOptions == KOptions = True
    KSourceDir == KSourceDir = True
    KBuildDir == KBuildDir = True
    KOutputDir == KOutputDir = True
    KPreBuild == KPreBuild = True
    KPostBuild == KPostBuild = True
    KPreInstall == KPreInstall = True
    KPostInstall == KPostInstall = True
    KPreClean == KPreClean = True
    KPostClean == KPostClean = True
    _ == _ = False

public export
data IPKGTokenKind
    = ITPackage
    | ITKey Key
    | ITString
    | ITPunct Punctuation
    | ITBare
    | ITIgnored

public export
IPKGToken : Type
IPKGToken = Token IPKGTokenKind

public export
Eq IPKGTokenKind where
    (==) ITPackage ITPackage = True
    (==) (ITKey x) (ITKey y) = x == y
    (==) ITString ITString = True
    (==) (ITPunct x) (ITPunct y) = x == y
    (==) ITBare ITBare = True
    (==) ITIgnored ITIgnored = True
    (==) _ _ = False


private
unescape : List Char -> List Char
unescape ('"'::rest) = loop rest
    where
        loop : List Char -> List Char
        loop [] = []
        loop ('"'::_) = []
        loop ('\\'::'"'::rest) = '"' :: loop rest
        loop (x::rest) = x :: loop rest
unescape _ = []

public export
TokenKind IPKGTokenKind where
    TokType ITPackage = ()
    TokType (ITKey _) = Key
    TokType ITString = String
    TokType (ITPunct _) = ()
    TokType ITBare = String
    TokType ITIgnored = ()

    tokValue ITPackage _ = ()
    tokValue (ITKey k) _ = k
    tokValue ITString s = pack . unescape . unpack $ s
    tokValue (ITPunct _) _ = ()
    tokValue ITBare s = s
    tokValue ITIgnored _ = ()



export
ignored : IPKGToken -> Bool
ignored (Tok ITIgnored _) = True
ignored _ = False