package edu.colorado.csci3155.project1

object StackMachineCompiler {



    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stack machine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        e match{
            case Const(f) => List[StackMachineInstruction](PushI(f)) //if const, push it into the list of instructions.

              //if const, push.
                //binary ops below
            case Plus(e1,e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(AddI)

            case Minus(e1,e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(SubI)

            case Mult(e1,e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(MultI)

            case Div(e1,e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(DivI)

            //let binding activate!
            case Let(str,e2,e3) => { //e1 is just name of value,input

                compileToStackMachineCode(e2) ++  List(LoadI(str)) ++ compileToStackMachineCode(e3)

            }// The rule simply asks you to generate a list with a single instruction for an identifier expression.
            case Ident(str) =>  List(StoreI(str)) //++ compileToStackMachineCode(str)

             //unary ops below. Load in the expression, then the operation
            case Exp(e1)  => compileToStackMachineCode(e1) ++ List(ExpI)
            case Log(e1) => compileToStackMachineCode(e1) ++ List(LogI)



            case Sine(e1) => compileToStackMachineCode(e1) ++ List(SinI)
           case Cosine(e1) => compileToStackMachineCode(e1) ++ List(CosI)

        }
    }

}
