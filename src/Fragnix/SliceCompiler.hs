module Fragnix.SliceCompiler where

import Fragnix.Slice (
    Slice(Slice),SliceID,Language(Language),Fragment(Fragment),Use(Use),
    Reference(OtherSlice,Builtin),
    UsedName(ValueName,TypeName,ConstructorName),Name(Identifier,Operator),
    InstanceID,Instance(Instance),
    InstancePart(OfClass,OfClassForBuiltinType,ForType,ForTypeOfBuiltinClass),
    readSliceDefault)

import Prelude hiding (writeFile)

import Language.Haskell.Exts.Syntax (
    Module(Module),ModuleHead(ModuleHead),
    ModuleName(ModuleName),ModulePragma(LanguagePragma),
    Decl(InstDecl,DataDecl,GDataDecl,PatBind,FunBind,
         ForImp,DerivDecl,TypeSig,TypeInsDecl,DataInsDecl,GDataInsDecl),
    Name(Ident,Symbol),QName(UnQual),
    ExportSpecList(ExportSpecList),ExportSpec(EThingWith),EWildcard(EWildcard),
    ImportSpecList(ImportSpecList),ImportDecl(ImportDecl,importSrc,importModule),
    ImportSpec(IVar,IAbs,IThingWith),
    CName(ConName),Namespace(NoNamespace))
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Parser (
    parseModuleWithMode,ParseMode(parseFilename,extensions,fixities),defaultParseMode,
    fromParseResult)
import Language.Haskell.Exts.Fixity (baseFixities)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Extension (parseExtension)

import Data.Text.IO (writeFile)
import Data.Text (Text,pack,unpack)

import System.FilePath ((</>),(<.>))
import System.Directory (createDirectoryIfMissing,doesFileExist)
import System.Process (rawSystem)
import System.Exit (ExitCode)
import Control.Exception (SomeException,catch,evaluate)

import Control.Monad.Trans.State.Strict (StateT,execStateT,get,put)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map.Strict as Map (
    fromList,toList,lookup,(!),keys,map,mapWithKey)
import Data.Set (Set)
import qualified Data.Set as Set (
    fromList,toList,map,filter,insert,
    union,intersection,unions,difference)
import Control.Monad (forM,forM_,unless)
import Data.Maybe (mapMaybe, isJust)
import Data.List (partition)
import Data.Char (isDigit)


-- | Compile the slice with the given slice ID. Set verbosity to zero and
-- turn all warnings off.
-- Assumes that all necessary compilation units have been written to disk.
invokeGHC :: SliceID -> IO ExitCode
invokeGHC sliceID = rawSystem "ghc" [
    "-v0","-w",
    "-ifragnix/temp/compilationunits",
    sliceModulePath sliceID]

-- | Invoke GHC to compile the slice with the given ID. The slice with the
-- given ID has to contain a definition for 'main :: IO ()'.
-- Asssumes that all necessary compilation units have been generated.
invokeGHCMain :: SliceID -> IO ExitCode
invokeGHCMain sliceID = rawSystem "ghc" ([
    "-o","main",
    "-ifragnix/temp/compilationunits",
    "-main-is",sliceModuleName sliceID,
    "-Ifragnix/include"] ++
    (map (\filename -> "fragnix/cbits/" ++ filename) cFiles) ++ [
    "-lpthread","-lz","-lutil",
    sliceModulePath sliceID])


-- | Generate and write all modules necessary to compile the slice with the given ID.
writeSliceModules :: SliceID -> IO ()
writeSliceModules sliceID = do
    createDirectoryIfMissing True sliceModuleDirectory
    (slices, instanceSlices) <- loadSlicesTransitive sliceID
    let sliceModules = map (sliceModule sliceInstancesMap) slices
        sliceHSBoots = map bootModule sliceModules
        instanceSliceModules = map (sliceModule sliceInstancesMap) instanceSlices
        instanceSliceHSBoots = map bootInstanceModule instanceSliceModules
        sliceInstancesMap = sliceInstances (slices ++ instanceSlices)
    _ <- evaluate sliceInstancesMap
    forM_ (sliceModules ++ instanceSliceModules) writeModule
    forM_ (sliceHSBoots ++ instanceSliceHSBoots) writeHSBoot


-- | Given a slice generate the corresponding module.
sliceModule :: Map SliceID (Set InstanceID) -> Slice -> Module ()
sliceModule sliceInstancesMap (Slice sliceID language fragment uses _) =
    let Fragment declarations = fragment
        moduleHead = ModuleHead () moduleName Nothing maybeExportSpecList
        pragmas = [LanguagePragma () languagepragmas]
        imports =
            map useImport uses ++
            map instanceImport (Set.toList (sliceInstancesMap Map.! sliceID))
        decls = map (parseDeclaration sliceID ghcextensions) declarations
        moduleName = ModuleName () (sliceModuleName sliceID)
        -- We need an export list to export the data family even
        -- though a slice only contains the data family instance
        maybeExportSpecList = dataFamilyInstanceExports decls imports
        languagepragmas =
            [Ident () "NoImplicitPrelude"] ++
            (map (Ident () . unpack) ghcextensions)
        Language ghcextensions _ = language
    in Module () (Just moduleHead) pragmas imports decls


-- | Reparse a declaration from its textual representation in a slice.
parseDeclaration :: SliceID -> [Text] -> Text -> Decl ()
parseDeclaration sliceID ghcextensions declaration = fmap (const ()) decl where
    -- Workaround because HSE couldn't parse all standalone declarations.
    Module _ _ _ _ [decl] = fromParseResult (parseModuleWithMode parseMode (unpack declaration))
    parseMode = defaultParseMode {
        parseFilename = show sliceID,
        extensions = map (parseExtension . unpack) ghcextensions,
        fixities = Just baseFixities}


-- | Import decl for a given use of a symbol.
useImport :: Use -> ImportDecl ()
useImport (Use maybeQualification usedName symbolSource) =
    let moduleName = case symbolSource of
            OtherSlice sliceID -> ModuleName () (sliceModuleName sliceID)
            Builtin originalModule -> ModuleName () (unpack originalModule)
        qualified = isJust maybeQualification
        maybeAlias = fmap (ModuleName () . unpack) maybeQualification
        importSpec = case usedName of
            ValueName name ->
                IVar () (toName name)
            TypeName name ->
                IAbs () (NoNamespace ()) (toName name)
            ConstructorName typeName name ->
                IThingWith () (toName typeName) [ConName () (toName name)]
        importSpecList = Just (ImportSpecList () False [importSpec])
        toName (Identifier name) = Ident () (unpack name)
        toName (Operator name) = Symbol () (unpack name)

    in ImportDecl () moduleName qualified False False Nothing maybeAlias importSpecList


-- | Import Decl for an instance from the given SliceID. Instance imports
-- are source imports.
instanceImport :: SliceID -> ImportDecl ()
instanceImport sliceID =
    ImportDecl () (ModuleName () (sliceModuleName sliceID)) False True False Nothing Nothing Nothing


-- | We export every type that we import in a data family instance slice
dataFamilyInstanceExports :: [Decl a] -> [ImportDecl ()] -> Maybe (ExportSpecList ())
dataFamilyInstanceExports [DataInsDecl _ _ _ _ _] imports =
    Just (ExportSpecList () (do
        ImportDecl _ _ _ _ _ _ _ (Just importSpecList) <- imports
        ImportSpecList _ False [IAbs _ _ typeName] <- [importSpecList]
        return (EThingWith () (EWildcard () 0) (UnQual () typeName) [])))
dataFamilyInstanceExports [GDataInsDecl _ _ _ _ _ _] imports =
    Just (ExportSpecList () (do
        ImportDecl _ _ _ _ _ _ _ (Just importSpecList) <- imports
        ImportSpecList _ False [IAbs _ _ typeName] <- [importSpecList]
        return (EThingWith () (EWildcard () 0) (UnQual () typeName) [])))
dataFamilyInstanceExports _ _ =
    Nothing


-- | Create a boot module from an ordinary module.
bootModule :: Module a -> Module a
bootModule (Module annotation maybeModuleHead pragmas imports decls) =
    Module annotation maybeModuleHead pragmas bootImports bootDecls where
        bootImports = mapMaybe bootImport imports
        bootDecls = concatMap bootDecl decls


-- | Create a boot module for an instance module
-- WORKAROUND: We can not put data family constructors into hs-boot files.
-- https://ghc.haskell.org/trac/ghc/ticket/8441
-- hs-boot files contain source imports for constructors.
-- We remove constructor imports from hs-boot files that contain only instances.
bootInstanceModule :: Module a -> Module a
bootInstanceModule (Module annotation maybeModuleHead pragmas imports decls) =
    Module annotation maybeModuleHead pragmas bootImports bootDecls where
        bootImports = mapMaybe bootInstanceImport imports
        bootDecls = concatMap bootDecl decls


-- | Remove all source imports and make all other imports source except those
-- from builtin modules. The removal of source imports means that no instance
-- modules will be imported.
bootImport :: ImportDecl a -> Maybe (ImportDecl a)
bootImport importDecl
    | importSrc importDecl = Nothing
    | isSliceModule (importModule importDecl) = Just (importDecl {importSrc = True})
    | otherwise = Just importDecl


-- | Remove all source imports and make all other imports source except those
-- from builtin modules. The removal of source imports means that no instance
-- modules will be imported.
-- Also remove constructor imports.
bootInstanceImport :: ImportDecl a -> Maybe (ImportDecl a)
bootInstanceImport importDecl
    | importSrc importDecl = Nothing
    | isConstructorImport importDecl = Nothing
    | isSliceModule (importModule importDecl) = Just (importDecl {importSrc = True})
    | otherwise = Just importDecl


-- | Remove instance bodies, make derived instances into bodyless instance decls,
-- remove value bindings and generate a type signature for foreign imports.
bootDecl :: Decl a -> [Decl a]
bootDecl (InstDecl annotation overlap instRule _) =
    [InstDecl annotation overlap instRule Nothing]
bootDecl (DataDecl annotation dataOrNew context declHead constructors _) =
    [DataDecl annotation dataOrNew context declHead constructors Nothing]
bootDecl (GDataDecl annotation dataOrNew context declHead kind constructors _) =
    [GDataDecl annotation dataOrNew context declHead kind constructors Nothing]
bootDecl (DerivDecl annotation overlap instRule) =
    [InstDecl annotation overlap instRule Nothing]
bootDecl (PatBind _ _ _ _) =
    []
bootDecl (FunBind _ _) =
    []
bootDecl (ForImp srcloc _ _ _ name typ) =
    [TypeSig srcloc [name] typ]
bootDecl (TypeInsDecl _ _ _) =
    []
bootDecl (DataInsDecl _ _ _ _ _) =
    []
bootDecl (GDataInsDecl _ _ _ _ _ _) =
    []
bootDecl decl =
    [decl]


-- | Given a list of slices, find for each slice the list of relevant
-- instances. Then reduce these lists to only those instances that have
-- to be imported explicitly and put them into a Map for easy lookup.
sliceInstances :: [Slice] -> Map SliceID (Set InstanceID)
sliceInstances slices = sliceInstancesMap where

    -- Map from slice ID to set of all instances that have to be imported explicitly
    sliceInstancesMap = Map.fromList (do
        (sliceID,instanceIDSet) <- Map.toList explicitInstancesMap
        let reducedInstanceIDSet = Set.difference instanceIDSet directlyUsedInstanceIDSet
            directlyUsedInstanceIDSet = Set.unions (do
                directlyUsedSliceID <- Set.toList (directlyUsedSliceMap Map.! sliceID)
                return (explicitInstancesMap Map.! directlyUsedSliceID))
        return (sliceID,reducedInstanceIDSet))

    -- Map from slice ID to set of all required instances. Contains instances that would be
    -- in scope implicitly.
    explicitInstancesMap = Map.map (Set.filter (\sliceID -> isInstanceMap Map.! sliceID)) usedSliceMapFixpoint

    -- Fixpoint of transitively adding more and more instances.
    usedSliceMapFixpoint = fixpoint (addInstances instancesMap . addUsedSlices) initialUsedSliceMap

    isInstanceMap = Map.fromList (do
        slice@(Slice sliceID _ _ _ _) <- slices
        return (sliceID,isInstance slice))
    initialUsedSliceMap = Map.mapWithKey (\sliceID usedSliceIDSet ->
        Set.insert sliceID usedSliceIDSet) directlyUsedSliceMap
    directlyUsedSliceMap = Map.fromList (do
        slice@(Slice sliceID _ _ _ _) <- slices
        return (sliceID,Set.fromList (usedSliceIDs slice)))
    instancesMap = Map.fromList (do
        Slice sliceID _ _ _ instances <- slices
        return (sliceID,instances))


-- | Iterate the given function untile the argument doesn't change anymore.
fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x
    | x == x' = x
    | otherwise = f x' where
        x' = f x

-- | Given a Map from slice ID to set of used slices, does one step of transitivity
-- i.e. adds all slices the used slices use.
addUsedSlices :: Map SliceID (Set SliceID) -> Map SliceID (Set SliceID)
addUsedSlices usedSliceMap = Map.fromList (do
    sliceID <- Map.keys usedSliceMap
    let usedSliceIDSet = usedSliceMap Map.! sliceID
        usedSliceIDSet' = Set.union usedSliceIDSet (Set.unions (do
            usedSliceID <- Set.toList usedSliceIDSet
            return (usedSliceMap Map.! usedSliceID)))
    return (sliceID,usedSliceIDSet'))


-- | Given a Map from slice ID to list of instances of that slice and a Map from slice ID
-- to list of used slices, adds for each key in the map the relevant instances.
addInstances :: Map SliceID [Instance] -> Map SliceID (Set SliceID) -> Map SliceID (Set SliceID)
addInstances instancesMap usedSliceMap = Map.fromList (do
    sliceID <- Map.keys usedSliceMap
    let usedSliceIDSet = usedSliceMap Map.! sliceID
        instances = Set.fromList (do
            usedSliceID <- Set.toList usedSliceIDSet
            instancesMap Map.! usedSliceID)
        usedSliceIDSet' = Set.union usedSliceIDSet (relevantInstanceIDs instances)
    return (sliceID,usedSliceIDSet'))

-- An instance is relevant for a slice if one of the
-- following four conditions holds:
--     * The slice transitively uses the instance's class and type
--     * The slice transitively uses the instance's class and the type is builtin
--     * The slice transitively uses the instance's type and the class is builtin
--     * The slices that a relevant instance uses make one of the previous
--       conditions true.
-- The last condition requires us to do a fixed point iteration.
relevantInstanceIDs :: Set Instance -> Set InstanceID
relevantInstanceIDs instances = Set.union
    (Set.union
        (instancesPlayingPart OfClassForBuiltinType instances)
        (instancesPlayingPart ForTypeOfBuiltinClass instances))
    (Set.intersection
        (instancesPlayingPart OfClass instances)
        (instancesPlayingPart ForType instances))

-- | Given a set of instances find the set of instance IDs of all instances playing
-- the given part.
instancesPlayingPart :: InstancePart -> Set Instance -> Set InstanceID
instancesPlayingPart desiredInstancePart =
    Set.map getInstanceID . Set.filter playsDesiredInstancePart where
        playsDesiredInstancePart (Instance instancePart _) = instancePart == desiredInstancePart
        getInstanceID (Instance _ instanceID) = instanceID


-- | Directory for generated modules
sliceModuleDirectory :: FilePath
sliceModuleDirectory = "fragnix" </> "temp" </> "compilationunits"

-- | The path where we put a generated module for GHC to find it.
modulePath :: Module a -> FilePath
modulePath (Module _ (Just (ModuleHead _ (ModuleName _ moduleName) _ _)) _ _ _) =
    sliceModuleDirectory </> moduleName <.> "hs"

-- | The path where we put a generated boot module for GHC to find it.
moduleHSBootPath :: Module a -> FilePath
moduleHSBootPath (Module _ (Just (ModuleHead _ (ModuleName _ moduleName) _ _)) _ _ _) =
    sliceModuleDirectory </> moduleName <.> "hs-boot"

-- | The path for the module generated for the slice with the given ID
sliceModulePath :: SliceID -> FilePath
sliceModulePath sliceID = sliceModuleDirectory </> sliceModuleName sliceID <.> "hs"

-- | The name we give to the module generated for a slice with the given ID.
sliceModuleName :: SliceID -> String
sliceModuleName sliceID = "F" ++ show sliceID

-- | The module name of the module that reexports all instances.
allInstancesModuleName :: ModuleName ()
allInstancesModuleName = ModuleName () "ALLINSTANCES"

-- | Is the module name from a fragnix generated module
isSliceModule :: ModuleName a -> Bool
isSliceModule (ModuleName _ ('F':rest)) = all isDigit rest
isSliceModule _ = False

-- | Does the import import a constructor
isConstructorImport :: ImportDecl a -> Bool
isConstructorImport (ImportDecl _ _ _ _ _ _ _ (Just (ImportSpecList _ False [IThingWith _ _ _]))) = True
isConstructorImport _ = False


writeModule :: Module a -> IO ()
writeModule modul = writeFileStrictIfMissing (modulePath modul) (prettyPrint modul)


writeHSBoot :: Module a -> IO ()
writeHSBoot modul = writeFileStrictIfMissing (moduleHSBootPath modul) (addRoleAnnotation (prettyPrint modul))


-- | Write out the given string to the given file path but only if
-- there is no file at the given path.
writeFileStrictIfMissing :: FilePath -> String -> IO ()
writeFileStrictIfMissing filePath content = do
    exists <- doesFileExist filePath
    unless exists (writeFileStrict filePath content)


-- | Write the given string to the given file path but completely evaluate it before
-- doing so and print any exceptions.
writeFileStrict :: FilePath -> String -> IO ()
writeFileStrict filePath content = (do
    evaluatedContent <- evaluate (pack content)
    writeFile filePath evaluatedContent)
        `catch` (print :: SomeException -> IO ())


-- | Given a slice ID load all slices and all instance slices nedded
-- for compilation.
loadSlicesTransitive :: SliceID -> IO ([Slice], [Slice])
loadSlicesTransitive sliceID = do
    sliceIDs <- loadSliceIDsTransitive sliceID
    slices <- forM sliceIDs readSliceDefault
    return (partitionInstances slices)


-- | Given a list of slices partition it into non-instance
-- slices and instance slices.
partitionInstances :: [Slice] -> ([Slice], [Slice])
partitionInstances = partition (not . isInstance)


-- | Given a slice ID find all IDs of all the slices needed
-- for compilation.
loadSliceIDsTransitive :: SliceID -> IO [SliceID]
loadSliceIDsTransitive sliceID = execStateT (loadSliceIDsStateful sliceID) []


-- | Given a slice ID load all IDs of all the slices needed for
-- compilation. Keep track of visited slice IDs to avoid loops.
loadSliceIDsStateful :: SliceID -> StateT [SliceID] IO ()
loadSliceIDsStateful sliceID = do
    seenSliceIDs <- get
    unless (elem sliceID seenSliceIDs) (do
        put (sliceID : seenSliceIDs)
        slice <- liftIO (readSliceDefault sliceID)
        let recursiveSliceIDs = usedSliceIDs slice
            recursiveInstanceSliceIDs = sliceInstanceIDs slice
        forM_ recursiveSliceIDs loadSliceIDsStateful
        forM_ recursiveInstanceSliceIDs loadSliceIDsStateful)


usedSliceIDs :: Slice -> [SliceID]
usedSliceIDs (Slice _ _ _ uses _) = do
    Use _ _ (OtherSlice sliceID) <- uses
    return sliceID

sliceInstanceIDs :: Slice -> [InstanceID]
sliceInstanceIDs (Slice _ _ _ _ instances) = do
    Instance _ instanceID <- instances
    return instanceID

isInstance :: Slice -> Bool
isInstance (Slice _ (Language _ True) _ _ _) = True
isInstance _ = False

doesSliceModuleExist :: SliceID -> IO Bool
doesSliceModuleExist sliceID = doesFileExist (sliceModulePath sliceID)


-- | List of gathered C-files.
cFiles :: [FilePath]
cFiles = [
    "ancilData.c",
    "cbits.c",
    "consUtils.c",
    "conv.c",
    "DarwinUtils.c",
    "directory.c",
    "dirUtils.c",
    "execvpe.c",
    "fnv.c",
    "fpstring.c",
    "ghcrts.c",
    "HsNet.c",
    "HsTime.c",
    "HsUnix.c",
    "HsUnixCompat.c",
    "iconv.c",
    "inputReady.c",
    "itoa.c",
    "md5.c",
    "myfree.c",
    "PrelIOUtils.c",
    "primFloat.c",
    "primitive-memops.c",
    "runProcess.c",
    "SetEnv.c",
    "sysconf.c",
    "text-helper.c",
    "timeUtils.c",
    "WCsubst.c",
    "Win32Utils.c",
    "zlib-helper.c"]


-- | Sadly GHC generates different role annotations in hs-boot files than
-- in regular modules.
-- See ghc.haskell.org/trac/ghc/ticket/9204
-- Will be fixed in GHC 7.12
addRoleAnnotation :: String -> String
addRoleAnnotation code = case Map.lookup (last (lines code)) roleAnnotations of
    Nothing -> code
    Just roleAnnotation -> unlines [
        "{-# LANGUAGE RoleAnnotations #-}",
        code,
        roleAnnotation]

roleAnnotations :: Map String String
roleAnnotations = Map.fromList [
    ("data UArray i e = UArray !i !i !Int ByteArray#",
        "type role UArray representational phantom"),
    ("data StorableArray i e = StorableArray !i !i Int !(ForeignPtr e)",
        "type role StorableArray representational phantom"),
    ("data STUArray s i e = STUArray !i !i !Int (MutableByteArray# s)",
        "type role STUArray nominal representational phantom"),
    ("newtype IOUArray i e = IOUArray (STUArray RealWorld i e)",
        "type role IOUArray representational phantom"),
    ("newtype Constant a b = Constant{getConstant :: a}",
        "type role Constant representational phantom"),
    ("                           {-# UNPACK #-} !(ForeignPtr a)",
        "type role MVector phantom phantom"),
    ("                           {-# UNPACK #-} !(MutableByteArray s)",
        "type role MVector nominal phantom"),
    ("                       {-# UNPACK #-} !(ForeignPtr a)",
        "type role Vector phantom"),
    ("                       {-# UNPACK #-} !ByteArray",
        "type role Vector phantom"),
    ("newtype Vault s = Vault (Map Unique Any)",
        "type role Vault phantom"),
    ("newtype Tagged s b = Tagged{unTagged :: b}",
        "type role Tagged phantom representational"),
    ("newtype Tagged2 (s :: * -> *) b = Tagged2{unTagged2 :: b}",
        "type role Tagged2 phantom representational"),
    ("newtype Tagged (s :: * -> *) = Tagged{unTagged :: Int}",
        "type role Tagged phantom")]

