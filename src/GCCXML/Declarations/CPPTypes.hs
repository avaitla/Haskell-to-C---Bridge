module Development.GCCXML.Declarations.CPPTypes where

import Text.Printf

data CoreTypeInfo = CoreTypeInfo {
    byteSize   :: Int,
    byteAlign  :: Int
}

data TypeQualifs = TypeQualifs {
    hasStatic  :: Bool,
    hasMutable :: Bool
}

data DummyType = DummyType {
    coreInfo  :: CoreTypeInfo,
    declStr   :: String
}

data UnknownType = UnknownType {
    coreInfo  :: CoreTypeInfo
}

data EllipsisType = EllipsisType {
    coreInfo  :: CoreTypeInfo
}

data FundamentalType = FundamentalType {
    coreInfo  :: CoreTypeInfo,
    name      :: String
}

data CompoundType a = CompoundType {
    coreInfo  :: CoreTypeInfo,
    base :: a
}

data CallDefType = CallDefType {
    arguments  :: [String],
    returnType :: String
}


class baseFund a where
    buildDeclString :: a -> String
    toCore          :: a -> CoreTypeInfo
    name            :: (Show a) => a -> String

    buildDeclString = name
    toCore _        = CoreTypeInfo 0 0
    name            = show


newtype Void = Void
instance Show     Void where (show _ = "void")
instance baseFund Void


newtype CPPChar = CPPChar
instance Show     CPPChar where (show _ = "char")
instance baseFund CPPChar


newtype SignedChar = SignedChar
instance Show     SignedChar where (show _ = "signed char")
instance baseFund SignedChar


newtype UnsignedChar = UnsignedChar
instance Show     UnsignedChar where (show _ = "unsigned char")
instance baseFund UnsignedChar


newtype WChar = WChar
instance Show     WChar where (show _ = "wchar_t")
instance baseFund WChar


newtype ShortInt = ShortInt
instance Show     ShortInt where (show _ = "short int")
instance baseFund ShortInt


newtype ShortUnsignedInt = ShortUnsignedInt
instance Show     ShortUnsignedInt where (show _ = "short unsigned ints")
instance baseFund ShortUnsignedInt


newtype CPPBool = CPPBool
instance Show     CPPBool where (show _ = "bool")
instance baseFund CPPBool


newtype CPPInt = CPPInt
instance Show     CPPInt where (show _ = "int")
instance baseFund CPPInt


newtype UnsignedInt = UnsignedInt
instance Show     UnsignedInt where (show _ = "unsigned int")
instance baseFund UnsignedInt


newtype LongInt = LongInt
instance Show     LongInt where (show _ = "long int")
instance baseFund LongInt


newtype LongUnsignedInt = LongUnsignedInt
instance Show     LongUnsignedInt where (show _ = "long unsigned int")
instance baseFund LongUnsignedInt


newtype LongLongInt = LongLongInt
instance Show     LongLongInt where (show _ = "long long int")
instance baseFund LongLongInt


newtype LongLongUnsignedInt = LongLongUnsignedInt
instance Show     LongLongUnsignedInt where (show _ = "long long unsigned int")
instance baseFund LongLongUnsignedInt


newtype CPPFloat = CPPFloat
instance Show     CPPFloat where (show _ = "float")
instance baseFund CPPFloat


newtype CPPDouble = CPPDouble
instance Show     CPPDouble where (show _ = "double")
instance baseFund CPPDouble


newtype LongDouble = LongDouble
instance Show     LongDouble where (show _ = "long double")
instance baseFund LongDouble


newtype ComplexDouble = ComplexDouble
instance Show     ComplexDouble where (show _ = "complex double")
instance baseFund ComplexDouble


newtype ComplexLongDouble = ComplexLongDouble
instance Show     ComplexLongDouble where (show _ = "complex long double")
instance baseFund ComplexLongDouble


newtype ComplexFloat = ComplexFloat
instance Show     ComplexFloat where (show _ = "complex float")
instance baseFund ComplexFloat


newtype Volatile a = Volatile (CompoundType a)
instance Show     (Volatile a) where (show _ = "volatile")
instance baseFund (Volatile a) where
    buildDeclString (Volatile (CompoundType x)) = "volatile " ++ (buildDeclString $ base x)


newtype Restrict a = Restrict (CompoundType a)
instance Show     (Restrict a) where (show _ = "restrict")
instance baseFund (Restrict a) where
    buildDeclString (Restrict (CompoundType x)) = "__restrict__ " ++ (buildDeclString $ base x)


newtype Const a = Const (CompoundType a)
instance Show     (Const a) where (show _ = "const")
instance baseFund (Const a) where
    buildDeclString (Const (CompoundType x)) = (buildDeclString $ base x) ++ " const"


newtype Pointer a = Pointer (CompoundType a)
instance Show     (Pointer a) where (show _ = "pointer")
instance baseFund (Pointer a) where
    buildDeclString (Pointer (CompoundType x)) = (buildDeclString $ base x) ++ " *"


newtype Reference a = Reference (CompoundType a)
instance Show     (Reference a) where (show _ = "reference")
instance baseFund (Reference a) where
    buildDeclString (Reference (CompoundType x)) = (buildDeclString $ base x) ++ " &"
    

data Array a = Array (CompoundType a) Int
instance Show     (Array a) where (show _ = "array")
instance baseFund (Array a) where
    buildDeclString (Array (CompoundType x) y) = (buildDeclString $ base x) ++ "[" ++ show y ++ "]"


nameTemplate :: String -> String -> String
nameTemplate retType args = printf "%s (*)( %s )" retType args

typedefTemplate :: String -> String -> String
typedefTemplate retType typname args = printf "%s ( *%s )( %s )" retType typname args

data FreeFuncType = FreeFuncType {
    coreInfo :: CoreTypeInfo,
    callDef  :: CallDefType
}


membFuncNameTemplate :: String -> String -> String -> String -> String
nameTemplate retType clas args hasConst = printf "%s ( %s::* )( %s ) %s" retType clas args hasConst

membFuncNameTemplate :: String -> String -> String -> String -> String -> String
typedefTemplate retType clas typname args hasConst = printf "%s ( %s::*%s )( %s ) %s" retType clas typname args hasConst

data MembFuncType = MembFuncType {
    hasConst  :: Bool,
    classInst :: String,
    funcInfo  :: FreeFuncType }
instance Show     (MemberVar a b) where (show _ = "member function")
instance baseFund (MemberVar a b) where
    buildDeclString (MemberVar (CompoundType x) y) = result where
        result = membVarNameTemplate (buildDeclString y) (buildDeclString $ base x)





membVarNameTemplate :: String -> String -> String
membVarNameTemplate typ clas = printf "%s ( %s::* )" typ clas

data MemberVar a b = MemberVar (CompoundType a) b
instance Show     (MemberVar a b) where (show _ = "member variable")
instance baseFund (MemberVar a b) where
    buildDeclString (MemberVar (CompoundType x) y) = result where
        result = membVarNameTemplate (buildDeclString y) (buildDeclString $ base x)










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