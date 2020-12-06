import Init
import Eval
import Printer
import Analysis

main = do
  let fileName = "/home/jiangkm3/Constant-Propagation/app/test.c"
  iast <- analyzeAST fileName
  let nast = evalProg iast
  initPrinter nast fileName
