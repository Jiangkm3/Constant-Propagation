import Init
import AbstractMonad
import Abstract1
import Eval
import Printer

main = do
  let fileName = "/home/jiangkm3/peppersieve/app/test.c"
  iast <- analyzeAST fileName
  let nast = evalProg iast
  initPrinter nast fileName
