{-
 cl = convertLoop
 ul = unrollLoop
-}

module Unroll where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Language.C.Syntax.Constants

-- Given an AST, unroll all loops with bound n
unrollLoop :: CTranslationUnit a -> Int -> CTranslationUnit a
unrollLoop ast0 n = unrollWhileLoop ast1 n
  where ast1 = convertLoop ast0

-----

-- Given an AST, convert all do-while and for loops to while loops
convertLoop :: CTranslationUnit a -> CTranslationUnit a
convertLoop (CTranslUnit extDecls a) = CTranslUnit nExtDecls a
  where nExtDecls = map clExtDecl extDecls

-- Helper functions of convertLoop, for different levels of AST
-- Does not change any part of the tree except loops

clExtDecl :: CExternalDeclaration a -> CExternalDeclaration a
clExtDecl (CDeclExt a) = CDeclExt a
clExtDecl (CFDefExt a) = CFDefExt (clFunc a)
clExtDecl _            = error "CAsmExt not Implemented"

clFunc :: CFunctionDef a -> CFunctionDef a
clFunc (CFunDef a b c stmt d) = CFunDef a b c nstmt d
  where nstmt = clStmt stmt

clStmt :: CStatement a -> CStatement a
clStmt a@(CExpr _ _)             = a
clStmt a@(CReturn _ _)           = a
clStmt (CIf a tstmt fstmt b)     = CIf a ntstmt nfstmt b
  where ntstmt = clStmt tstmt
        nfstmt = case fstmt of
                   Nothing -> Nothing
                   Just s  -> Just (clStmt s)
-- evaluating inner loop before outer loop
clStmt (CWhile a stmt False b) = CWhile a nstmt False b
  where nstmt = clStmt stmt
clStmt (CWhile a stmt True b)  = clDoWhile (CWhile a nstmt True b)
  where nstmt = clStmt stmt
clStmt (CFor a b c stmt d)     = clFor (CFor a b c nstmt d)
  where nstmt = clStmt stmt
clStmt (CCompound a cbis b)    = CCompound a ncbis b
  where ncbis = map clCBI cbis
clStmt _ = error "Statement type not implemented"

clCBI :: CCompoundBlockItem a -> CCompoundBlockItem a
clCBI (CBlockStmt stmt) = CBlockStmt nStmt
  where nStmt = clStmt stmt
clCBI (CBlockDecl decl) = CBlockDecl decl
clCBI _ = error "CBI nested function type not implemented"

-- The easiest way to convert is to simply turn it into a compound block
-- statement. This might not match the convention of C AST, but it works
-- fine in this context.
clDoWhile :: CStatement a -> CStatement a
clDoWhile (CWhile cond stmt True a) = CCompound [] [initCBI, whileCBI] a
  where initCBI = CBlockStmt stmt
        whileCBI = CBlockStmt (CWhile cond stmt False a)
clDoWhile _ = error "Invalid do-while statement"

clFor :: CStatement a -> CStatement a
clFor (CFor init cond step stmt a) = CCompound [] [initCBI, whileCBI] a
  where stepCBI = CBlockStmt (CExpr step a)
        iterationStmt = CCompound [] [(CBlockStmt stmt), stepCBI] a
        initCBI = case init of
                    Left e  -> CBlockStmt (CExpr e a)
                    Right d -> CBlockDecl d
        ncond = case cond of
                  Nothing -> CConst (CIntConst (CInteger 1 DecRepr noFlags) a)
                  Just c  -> c
        whileCBI = CBlockStmt (CWhile ncond iterationStmt False a)
clFor _ = error "Invalid for statement"

-----

-- Given an AST, unroll all while loops with bound n
-- Assume no for or do-while loop
unrollWhileLoop :: CTranslationUnit a -> Int -> CTranslationUnit a
unrollWhileLoop (CTranslUnit extDecls a) n = CTranslUnit extDecls a
