import Init
import Eval
import Printer
import Simplify

main = do
  let fileName = "/home/jiangkm3/Constant-Propagation/app/test.c"
  iast <- analyzeAST fileName
  let nast = evalProg iast
  let fast = removeBot nast
  initPrinter fast fileName
