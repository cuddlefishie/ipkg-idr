-- SPDX-FileCopyrightText: 2021 The ipkg-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.IPKG.Value

import Data.List
import Data.List1
import Data.Strings

public export
Identifier : Type
Identifier = String

public export
ModuleIdent : Type
ModuleIdent = List1 Identifier

public export
record PkgDesc where
    constructor MkPkgDesc
    name        : Identifier
    version     : String -- TODO should be a proper type
    authors     : String
    maintainers : Maybe String
    license     : Maybe String
    brief       : Maybe String
    readme      : Maybe String
    homepage    : Maybe String
    sourceloc   : Maybe String
    bugtracker  : Maybe String
    depends     : List Identifier -- packages to add to search path
    modules     : List ModuleIdent -- modules to install (namespace, filename)
    mainmod     : Maybe ModuleIdent -- main file (i.e. file to load at REPL)
    executable  : Maybe String -- name of executable
    options     : Maybe String
    sourcedir   : Maybe String
    builddir    : Maybe String
    outputdir   : Maybe String

    prebuild    : Maybe String -- Script to run before building
    postbuild   : Maybe String -- Script to run after building
    preinstall  : Maybe String -- Script to run after building, before installing
    postinstall : Maybe String -- Script to run after installing
    preclean    : Maybe String -- Script to run before cleaning
    postclean   : Maybe String -- Script to run after cleaning

private
showModule : ModuleIdent -> String
showModule m = fastConcat . intersperse "." $ forget m


export
Show PkgDesc where
    show pkg = "Package: " ++ name pkg ++ "\n" ++
                "Version: " ++ version pkg ++ "\n" ++
                "Authors: " ++ authors pkg ++ "\n" ++
                maybe "" (\m => "Maintainers: " ++ m ++ "\n") (maintainers pkg) ++
                maybe "" (\m => "License: "     ++ m ++ "\n") (license pkg)     ++
                maybe "" (\m => "Brief: "       ++ m ++ "\n") (brief pkg)       ++
                maybe "" (\m => "ReadMe: "      ++ m ++ "\n") (readme pkg)      ++
                maybe "" (\m => "HomePage: "    ++ m ++ "\n") (homepage pkg)    ++
                maybe "" (\m => "SourceLoc: "   ++ m ++ "\n") (sourceloc pkg)   ++
                maybe "" (\m => "BugTracker: "  ++ m ++ "\n") (bugtracker pkg)  ++
                "Depends: " ++ show (depends pkg) ++ "\n" ++
                "Modules: " ++ show (map showModule $ modules pkg) ++ "\n" ++
                maybe "" (\m => "Main: " ++ showModule m ++ "\n") (mainmod pkg) ++
                maybe "" (\m => "Exec: " ++ m ++ "\n") (executable pkg) ++
                maybe "" (\m => "Opts: " ++ m ++ "\n") (options pkg) ++
                maybe "" (\m => "SourceDir: " ++ m ++ "\n") (sourcedir pkg) ++
                maybe "" (\m => "BuildDir: " ++ m ++ "\n") (builddir pkg) ++
                maybe "" (\m => "OutputDir: " ++ m ++ "\n") (outputdir pkg) ++
                maybe "" (\m => "Prebuild: " ++ m ++ "\n") (prebuild pkg) ++
                maybe "" (\m => "Postbuild: " ++ m ++ "\n") (postbuild pkg) ++
                maybe "" (\m => "Preinstall: " ++ m ++ "\n") (preinstall pkg) ++
                maybe "" (\m => "Postinstall: " ++ m ++ "\n") (postinstall pkg) ++
                maybe "" (\m => "Preclean: " ++ m ++ "\n") (preclean pkg) ++
                maybe "" (\m => "Postclean: " ++ m ++ "\n") (postclean pkg)