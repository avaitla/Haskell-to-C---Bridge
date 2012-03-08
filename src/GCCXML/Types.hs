module Development.GCCxml.Types where


deepDeclarations = [
    xml_NN_CASTING_OPERATOR
    , xml_NN_CONSTRUCTOR
    , xml_NN_DESTRUCTOR
    , xml_NN_ENUMERATION
    , xml_NN_FILE
    , xml_NN_FUNCTION
    , xml_NN_FREE_OPERATOR
    , xml_NN_MEMBER_OPERATOR
    , xml_NN_METHOD
    , xml_NN_FUNCTION_TYPE
    , xml_NN_METHOD_TYPE ]

-- Convention
-- xml_NN - XML Node Name
-- xml_AN - XML Attribute Name
xml_AN_ABSTRACT = "abstract"
xml_AN_ACCESS = "access"
xml_AN_ALIGN = "align"
xml_AN_ARTIFICIAL = "artificial"
xml_AN_ATTRIBUTES = "attributes"
xml_AN_BASE_TYPE = "basetype"
xml_AN_BASES = "bases"
xml_AN_BITS = "bits"
xml_AN_CONST = "const"
xml_AN_CONTEXT = "context"
xml_AN_CVS_REVISION = "cvs_revision"
xml_AN_DEFAULT = "default"
xml_AN_DEMANGLED = "demangled"
xml_AN_EXTERN = "extern"
xml_AN_FILE = "file"
xml_AN_ID = "id"
xml_AN_INCOMPLETE = "incomplete"
xml_AN_INIT = "init"
xml_AN_LINE = "line"
xml_AN_MANGLED = "mangled"
xml_AN_MAX = "max"
xml_AN_MEMBERS = "members"
xml_AN_MUTABLE = "mutable"
xml_AN_NAME = "name"
xml_AN_OFFSET = "offset"
xml_AN_PURE_VIRTUAL = "pure_virtual"
xml_AN_RESTRICT = "restrict"
xml_AN_RETURNS = "returns"
xml_AN_SIZE = "size"
xml_AN_STATIC = "static"
xml_AN_THROW = "throw"
xml_AN_TYPE = "type"
xml_AN_VIRTUAL = "virtual"
xml_AN_VOLATILE = "volatile"
xml_NN_ARGUMENT = "Argument"
xml_NN_ARRAY_TYPE = "ArrayType"
xml_NN_CASTING_OPERATOR = "Converter"
xml_NN_CLASS = "Class"
xml_NN_CONSTRUCTOR = "Constructor"
xml_NN_CV_QUALIFIED_TYPE = "CvQualifiedType"
xml_NN_DESTRUCTOR = "Destructor"
xml_NN_ELLIPSIS = "Ellipsis"
xml_NN_ENUMERATION = "Enumeration"
xml_NN_ENUMERATION_VALUE = "EnumValue"
xml_NN_FIELD = "Field"
xml_NN_FILE = "File"
xml_NN_FUNCTION = "Function"
xml_NN_FUNCTION_TYPE = "FunctionType"
xml_NN_FUNDAMENTAL_TYPE = "FundamentalType"
xml_NN_FREE_OPERATOR = "OperatorFunction"
xml_NN_GCC_xml = "GCC_xml"
xml_NN_MEMBER_OPERATOR = "OperatorMethod"
xml_NN_METHOD = "Method"
xml_NN_METHOD_TYPE = "MethodType"
xml_NN_NAMESPACE = "Namespace"
xml_NN_OFFSET_TYPE = "OffsetType"
xml_NN_POINTER_TYPE = "PointerType"
xml_NN_REFERENCE_TYPE = "ReferenceType"
xml_NN_ROOT = "GCC_xml"
xml_NN_STRUCT = "Struct"
xml_NN_TYPEDEF = "Typedef"
xml_NN_UNION = "Union"
xml_NN_VARIABLE = "Variable"