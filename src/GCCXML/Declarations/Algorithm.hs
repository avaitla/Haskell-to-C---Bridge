module Development.GCCXML.Declarations.Algorithm where

declarationPath :: DeclType a => a -> Maybe Bool -> [Declaration]
declarationPath decl Nothing = declarationPath string (Just True)
declarationPath [] _ = []
declarationPath decl (Just x) = case (parent $ getDeclType decl)
    parent = getDeclType decl
    getParent parent)
    
    
    getParent Nothing  = []
    getParent (Just x) = parent x