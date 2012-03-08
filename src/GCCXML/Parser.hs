module Development.GCCXML.Parser where

type FileLookup = M.Map String FilePath

data LocationInfo = LocationInfo {
    fileName :: String,
    lineNum  :: String,
} deriving (Eq, Show, Read)

data Attribute   = Attribute


data CPPType = VOID
             | CHAR
             | SIGNED_CHAR
             | UNSIGNED_CHAR
             | WCHAR
             | SHORT_INT
             | SHORT_UNSIGNED_INT
             | BOOL
             | INT
             | UNSIGNED_INT
             | LONG_INT
             | LONG_UNSIGNED_INT
             | LONG_LONG_INT
             | LONG_LONG_UNSIGNED_INT
             | FLOAT
             | DOUBLE
             | LONG_DOUBLE
             | COMPLEX_DOUBLE
             | COMPLEX_LONG_DOUBLE
             | COMPLEX_FLOAT
             | UNKNOWN
    deriving (Eq)

cppToString :: CPPType -> String
cppToString x = case x of
    VOID                    -> "void"
    CHAR                    -> "char"
    SIGNED_CHAR             -> "signed char"
    UNSIGNED_CHAR           -> "unsigned char"
    WCHAR                   -> "wchar_t"
    SHORT_INT               -> "short int"
    SHORT_UNSIGNED_INT      -> "short unsigned int"
    BOOL                    -> "bool"
    INT                     -> "int"
    UNSIGNED_INT            -> "unsigned int"
    LONG_INT                -> "long int"
    LONG_UNSIGNED_INT       -> "long unsigned int"
    LONG_LONG_INT           -> "long long int"
    LONG_LONG_UNSIGNED_INT  -> "long long unsigned int"
    FLOAT                   -> "float"
    DOUBLE                  -> "double"
    LONG_DOUBLE             -> "long double"
    COMPLEX_DOUBLE          -> "complex double"
    COMPLEX_LONG_DOUBLE     -> "complex long double"
    COMPLEX_FLOAT           -> "complex float"
    UNKNOWN                 -> "//"

cppFromString :: String -> CPPType
cppFromString x = case x of
    "void"                   ->  VOID
    "char"                   ->  CHAR
    "signed char"            ->  SIGNED_CHAR
    "unsigned char"          ->  UNSIGNED_CHAR
    "wchar_t"                ->  WCHAR
    "short int"              ->  SHORT_INT
    "short unsigned int"     ->  SHORT_UNSIGNED_INT
    "bool"                   ->  BOOL
    "int"                    ->  INT
    "unsigned int"           ->  UNSIGNED_INT
    "long int"               ->  LONG_INT
    "long unsigned int"      ->  LONG_UNSIGNED_INT
    "long long int"          ->  LONG_LONG_INT
    "long long unsigned int" ->  LONG_LONG_UNSIGNED_INT
    "float"                  ->  FLOAT
    "double"                 ->  DOUBLE
    "long double"            ->  LONG_DOUBLE
    "complex double"         ->  COMPLEX_DOUBLE
    "complex long double"    ->  COMPLEX_LONG_DOUBLE
    "complex float"          ->  COMPLEX_FLOAT
    _                        ->  UNKNOWN









data CPPInfo = {
    byteSize  :: Int,
    byteAlign :: Int,
    cppType   :: CPPType
} deriving (Eq, Show, Ord)

data Virtuality  = NOT_VIRTUAL | VIRTUAL | PURE_VIRTUAL | ALL
    deriving (Eq, Show, Read)
data AccessTypes = PUBLIC | PRIVATE | PROTECTED | ALL
data ClassTypes  = CLASS | STRUCT | UNION | ALL


data Declaration = Declaration {
    name          :: String,
    location      :: LocationInfo,
    isArtificial  :: Bool,
    mangled       :: String,
    demangled     :: String,
    attributes    :: [Attribute],
    parent        :: Maybe Declaration,
    compiler      :: Maybe String,
    partialName   :: Maybe String
} deriving (Eq, Show, Read)

data Argument = Argument {
    name         :: String,
    defaultValue :: Maybe String
    argType      :: CPPType,
    attributes   :: [String]
} deriving (Eq, Show, Read)

data CallDef = CallDef {
    arguments    :: [Argument],
    doesThrow    :: Bool,
    exceptions   :: [],
    returnType   :: CPPType,
    hasExtern    :: Bool,
    demangled    :: String
} deriving (Eq, Show, Read)


data MemberCallDef = MemberCallDef {
    callDef      :: CallDef,
    virtuality   :: Virtuality,
    hasConst     :: Bool,
    hasStatic    :: Bool
} deriving (Eq, Show, Read)