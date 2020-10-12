package edu.colorado.csci3155.project1


/* -- Here are all the instructions to be supported --*/
sealed trait StackMachineInstruction
case class LoadI(s: String) extends StackMachineInstruction
case class  StoreI(s: String) extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {


    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */

    def isWellFormed_unaryOp(stack: List[Double], op: Double => Double): List[Double]   = {
        if(stack.length < 1){ throw new IllegalArgumentException()}
        val result = op(stack(0)) //first step for unary op, get the first element of the stack


        val newStack =  result :: stack.drop(1) //stick on the stack
        return newStack
    }

    def isWellFormed_binOp(stack: List[Double], op:  (Double, Double) => Double): List[Double] = {
        if(stack.length < 2){ throw new IllegalArgumentException()}

        val result = op(stack(0),  stack(1)) //first step for unary op, get the first element of the stack
                                                //for binary opp, get two elements from stack, then operate on them, then return
                                                // push results onto stack

        val newStack = result::stack.drop(2) //stick on the stack
        return newStack
    }



    def emulateSingleInstruction(stack: List[Double],
                                 env: Map[String, Double],
                                 ins: StackMachineInstruction): (List[Double], Map[String, Double]) = {


        val valid = stack.length
        //  if(valid == 0 or valid == -1){ //check for validity
        //      throw new IllegalAccessException()
        //  }

        //match up the ins: instruction param, and use cases
        //check that things are valid, then => to the operation on the list of stack
        ins match {
            case LoadI(str) => {
                if(str.length() == 0){
                    throw new IllegalArgumentException()
                }
                //if(stack.length == 0){
                //    throw new IllegalArgumentException()
               // }
                val v = stack.head;
                val ins1 = PopI;
                val env2 = env + (str -> v)
                return emulateSingleInstruction(stack, env2, ins1)
            }
            case StoreI(str) => {

                if(str.length() == 0){
                    throw new IllegalArgumentException()
                }

               if(!env.contains(str)){
                   throw new IllegalArgumentException()
               }

                val x = env(str)
                val ins1 = PushI(x)
                return emulateSingleInstruction(stack, env, ins1)
            }
            case PushI(d) => {
                val newStack = d::stack
                return (newStack, env) //push on the double
            }

            case PopI => {
                if(stack.length == 0){
                    throw new IllegalArgumentException()
                }
                val newStack = stack.drop(1) //drop the last element. return.
                return (newStack, env)
            }
            case CosI => {
                val newStack = isWellFormed_unaryOp(stack, math.cos)
                return (newStack, env)
            }//do the cos op on the stack head, then return it to the tail

            case SinI => {
                val newStack = isWellFormed_unaryOp(stack, math.sin)
                return (newStack, env)
            }
            case LogI => {
                val newStack = isWellFormed_unaryOp(stack, math.log) //return a nd  update the stack
                return (newStack, env)
            }

            case ExpI => {
                val newStack = isWellFormed_unaryOp(stack, math.exp) //return a nd  update the stack
                return (newStack, env)
            }

            case DivI => {
                val newStack = isWellFormed_binOp(stack, _/_) //return a nd  update the stack
                return (newStack, env)
            } //same as sub, but just division instead

            case MultI => {
                val newStack = isWellFormed_binOp(stack, _*_) //return a nd  update the stack
                return (newStack, env)
            }

            case SubI => {
                val newStack = isWellFormed_binOp(stack, _-_) //return a nd  update the stack
                return (newStack, env)
            }

            case AddI => {

                val newStack = isWellFormed_binOp(stack, _+_) //return a nd  update the stack
                return (newStack, env)
            }

            //catch exceptions
            case _ => throw new IllegalArgumentException()

        }


    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be the final environment.

       Hint: accumulator for foldLeft must be a tuple (List[Double], Map[String,Double])
             initial value of this accumulator must be (Nil, Map.empty)
             You should use emulateSingleInstruction to update the accmulator.
             It will all fit nicely once you figure it out.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Map[String, Double] = { //instruction list is the input,  a list. returns map[string, double]

        val (_, final_env) = instructionList.foldLeft[(List[Double], Map[String, Double])] ((Nil,Map.empty)) {
            case ((stack, env), ins) => emulateSingleInstruction(stack, env, ins)

        }
        return final_env
    }
}