import Init
import AbstractMonad
import Abstract1
import Eval
import Printer
import Simplify

main = do
  let fileName = "/home/jiangkm3/compiler-test/test1.c"
  iast <- analyzeAST fileName
  let nast = evalProg iast
  let fast = removeBot nast
  initPrinter fast fileName
