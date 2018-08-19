structure Quad =
struct

val output = ref ""

structure TV = TempVar
structure RT = RunTime
structure ST = symbol_Table
datatype stmt = Assign of string list * exp ref list
            | Stmtlist of string list * stmt list ref
            | DBlockStmtlist of string list * stmt list ref
            | While of string list * exp ref * stmt ref * string 
              (*codelist,cond,body,nextLabel*)
            | If of string list * exp ref * stmt ref * stmt ref * string
              (*codeList,cond,thenBlock,elseBlock,nextLabel*)
            | For of string list * stmt ref * exp ref * stmt ref * stmt ref * string
              (*codeList,stmt1,cond,stmt2,sblock,nextLabel*)
            | Sblock of string list * stmt ref * stmt ref * string 
              (*codelist, dblockstmtlist, stmtChild, nextLabel*)
            | Case of string list * exp ref * stmt ref * string * string
              (*codelist, constant, sblock, nextLabel, label*)
            | Otherwise of string list * stmt ref * string * string
              (*code,sblock,nextLabel,label*)
            | Switch of string list * exp ref * stmt list ref * stmt ref * string * string
              (*codelist, expression, caselist, otherwise, testLabel, nextLabel*)
            | EmptyStmt of int
            | Reserve of string list * string * exp ref
            | Release of string list * string * exp ref
            | Func of string list * string * stmt ref
              (* codelist,funcName,sblock*)
      and exp = Exp of exp
            |  Const of string * string list
            |  Id of string * string list
            |  Binop of string * string list * exp ref * exp ref
            |  Unop of string * string * string list * exp ref
            |  Relop of string * string * string list * exp ref * exp ref
              (*relop,addr,codeList,op1,op2*)
            |  Boolop of string * string * string list * exp ref * exp ref
              (*relop,addr,codeList,op1,op2*)
            |  BoolCond of string list * string * string
              (*codeList, condTrue, condFalse*)
            |  AssArg of string * string list * exp ref *  exp list ref
              (* addr, codeList, assignable, argList *)
            |  EmptyExp of int
            |  ResExp of string
            |  RelExp of string
      and binop = Plus of string
                | Mod of string
                | Div of string
                | Mul of string
                | Sub of string
      and unop  = Bang of string
                | Neg of string
                | I2r of string
                | R2i of string
      and relop = Lt of string
                | Eq of string
                | False of string
      and boolop = Pipe of string
                 | Amp of string

val stack : exp list ref = ref [];
val stackStmt : stmt list ref = ref [];
val outputCode : string list ref = ref [];

fun pop() =
  stack := tl (!stack)
  handle Empty => print "pop\n"

fun popStmt() =
  stackStmt := tl (!stackStmt)
  handle Empty => print "popStmt\n"

fun printSize(st) =
  Int.toString(length (!st))

fun printListSize(ls) =
  print (Int.toString(length ls))

fun get(lst, indexnum) =
  if indexnum = 0 then !(hd lst)
  else get((tl lst), indexnum-1)

fun getWidth(tp) = 
  case tp of
    "integer" => 4
  | "real" => 8
  | "Boolean" => 1
  | "character" => 1
  | "string" => 8
  | _ => 0

fun getAddrCode(e) = 
  case e of
    Const(addr,code) => (addr,code)
  | Id(addr,code) => (addr,code)
  | Binop(addr,code,op1,op2) => (addr,code)
  | Unop(operator,addr,code,op1) => (addr,code)
  | Relop(operator,addr,code,op1,op2) => (addr,code)
  | AssArg(addr,code,assignable,argList) => (addr,code)
  | EmptyExp(i) => ("",[])
  | _ => ("", [])

fun getCode(s) = 
  case s of
    Assign(code,child) => code
  | Sblock(code,dblockchild,child,next) => code
  | Stmtlist(code,child) => code
  | DBlockStmtlist(code,child) => code
  | While(code,cond,body,next) => code
  | If(code,cond,thenBlock,elseBlock,next) => code
  | For(code,stmt1,cond,stmt2,body,next) => code
  | Reserve(code,memop,assArg) => code
  | Release(code,memop,exp) => code
  | Switch(code,expression,caselist,otherwise,testLabel,nextLabel) => code
  | Case(code,const,sblock,nextLabel,label) => code
  | Otherwise(code,sblock,nextLabel,label) => code
  | Func(code,name,sblock) => code
  | _ => []

fun getBoolCondCode(e) =
  case e of
    BoolCond(code,condTrue,condFalse) => (code,condTrue,condFalse)
  | _ => ([],"","")

fun getAllRelop(r) =
  case r of
    Relop(relop,addr,codeList,op1,op2) => (relop,addr,codeList,op1,op2)
  | _ => ("","",[],ref r, ref r)

fun getSblock(s) = 
  case s of
    Sblock(codeList,dblockchild,stmtChild,nextLabel) => (codeList,dblockchild,stmtChild,nextLabel)
  | _ => ([],ref (EmptyStmt(1)),ref (EmptyStmt(1)),"")

fun getBoolCond(b) = 
  case b of
    BoolCond(codeList, condTrue, condFalse) => (codeList,condTrue,condFalse)
  | _ => ([],"","")

fun getAssArg(a) =
  case a of
    AssArg(addr,codeList,ass,argList) => (addr,codeList,ass,argList)
  | _ => ("",[],ref (Id("",[])),ref [])

fun getMemOp(m) =
  case m of
    ResExp(str) => str
  | RelExp(str) => str
  | _ => ""

fun getSwitch(s) = 
  case s of
    Switch(code,expression,caseList,otherwise,testLabel,nextLabel) => 
      (code,expression,caseList,otherwise,testLabel,nextLabel)
  | _ => ([],ref (EmptyExp(1)),ref [],ref (EmptyStmt(1)),"","")

fun getCase(c) =
  case c of
    Case(code,const,sblock,nextLabel,label) => (code,const,sblock,nextLabel,label)
  | _ => ([], ref(EmptyExp(1)),ref (EmptyStmt(1)),"","") 

fun getOtherwise(c) = 
  case c of
    Otherwise(code,sblock,nextLabel,label) => (code,sblock,nextLabel,label)
  | _ => ([], ref (EmptyStmt(1)),"","") 


fun printList(lst) =
  case lst of
    [] => print "\n"
(*  | x::[] => if ((hd (explode x)) = #"L") then print ""
              else print x
*)  | x::xs => (print (x); printList(xs))

fun printStmtCode(statement) =
  case statement of
    Sblock(code,dblockchild,stmtChild,next) => printListSize(code)
  | _ => ()

fun printTree(node) =
  case node of
    Binop(addr,code,op1,op2) =>
      (printTree(!op1);print (addr ^ "\n"); printTree(!op2))
  | Relop(operator,addr,code,op1,op2) =>
      (printTree(!op1);print (operator ^ "\n"); printTree(!op2))
  | Boolop(operator,addr,code,op1,op2) =>
      (printTree(!op1);print (operator ^ "\n"); printTree(!op2))
  | Unop(operator,addr,code,op1) =>
      (print "!";printTree(!op1))
  | Id(addr,code) => print (addr ^ "\n")
  | EmptyExp(i) => print ("EmptyExp\n")
  | Const(addr,code) => print (addr ^ "\n")
  | AssArg(addr,code,ass,argList) => print(
                                   (#1 (getAddrCode(!ass))) ^
                                   " AssArg: "^
                                   Int.toString(length (!argList))^
                                   addr ^ "\n")
  | _ => ()

fun printAll(nodelist) =
  let
    fun printTreeStmt(node) =
      case node of
        Assign(code,child) => (print "Assign ";printTree(get(child,0));
                               printTree(get(child,1)))
      | Stmtlist(code,child) => (print "Stmtlist size = ";
                                 printListSize(!child); print "\n";
                                 printAll(!child))
      | DBlockStmtlist(code,child) => (print "Dstmtlist size = ";
                                 printListSize(!child); print "\n";
                                 printAll(!child))
      | Sblock(code,dblockChild,child,next) => (print "Sblock\n";
                                                printTreeStmt(!dblockChild);
                                                printTreeStmt(!child))
      | While(code,cond,body,next) => (print "While\n";
                                      printTree(!cond);printTreeStmt(!body))
      | For(code,stmt1,cond,stmt2,body,next) => (
                                (print "For\n");printTreeStmt(!stmt1);
                                 printTree(!cond);printTreeStmt(!stmt2);
                                 printTreeStmt(!body) )
      | If(code,cond,thenBlock,elseBlock,next) => 
          (print "If\n";printTree(!cond);
            printTreeStmt(!thenBlock);printTreeStmt(!elseBlock))
      | Reserve(code,memOp,assArg) => (print (memOp ^ "\n");printTree(!assArg))
      | Release(code,memOp,id) => (print (memOp ^ "\n");printTree(!id))
      | Case(code,const,sblock,nextLabel,label) => (print ("Case\n");printTree(!const);printTreeStmt(!sblock))
      | Otherwise(code,sblock,nextLabel,label) => (print ("Otherwise\n");printTreeStmt(!sblock))
      | Switch(code,expression,caselist,otherwise,testLabel,nextLabel) =>
          (print ("Switch caseList size: " ^ Int.toString(length (!caselist)) ^ "\n");
           print ("Expression: ");printTree(!expression);printAll(!caselist);printTreeStmt(!otherwise))
      | Func(code,name,sblock) => (print ("Function: " ^ name ^ "\n");printTreeStmt(!sblock))
      | _ => ()
  in
    case nodelist of
      [] => ()
    | x::xs => (print ("--tree\n");printTreeStmt(x); printAll(xs))
  end

fun printStackStmt() =
  printAll(!stackStmt)

fun top(st, num) = 
  if num = 1 then hd (st)
  else top(tl(st), num-1)

(*----------------- Make syntax tree -----------------*) 
fun addMainLabel() =
  let
    val sblock = top(!stackStmt,1)
    val _ = popStmt();
    val (sblockcodeList,dblockstmt,stmtChild,sblocknextLabel) = getSblock(sblock)
    val newCode = ["Main: "]@sblockcodeList
  in
    stackStmt:=Sblock(newCode,dblockstmt,stmtChild,sblocknextLabel)::(!stackStmt)
  end

fun mkTreeFunc() = 
  let
    val sblock = top(!stackStmt,1)
    val name = getAddrCode(top(!stack,1))
    val _ = (popStmt();pop())
  in
    stackStmt:=Func([],(#1 name), ref sblock)::(!stackStmt)
  end

fun mkTreeSwitch() =
  let
    val caseStmt = top(!stackStmt,1)
    val _ =  popStmt();
  in
    (stackStmt:=Switch([],ref (EmptyExp(1)),ref [caseStmt],ref (EmptyStmt(1)),"","")::(!stackStmt))
  end

fun addTreeSwitch() = 
  let
    val caseStmt = top(!stackStmt,1)
    val switch = top(!stackStmt,2)
    val (code,expression,caseList,otherwise,testLabel,nextLabel) = getSwitch(switch)
    val newCaseList = (!caseList)@[caseStmt]
    val _ = (popStmt();popStmt())
  in
    (stackStmt:=Switch(code,expression,ref newCaseList,otherwise,testLabel,nextLabel)::(!stackStmt))
  end
fun addTreeOtherwise() = 
  let
    val sblock = top(!stackStmt,1)
    val switch = top(!stackStmt,2)
    val (code,expression,caseList,otherwise,testLabel,nextLabel) = getSwitch(switch)
    val newExpression = ref (top(!stack,1))
    val newOtherwise = Otherwise([],ref sblock,"","")
    val _ = (popStmt();popStmt();pop())
  in
    (stackStmt:=Switch(code,newExpression,caseList,ref newOtherwise,testLabel,nextLabel)::(!stackStmt))
  end

fun mkTreeCase() =
  let
    val const = ref (top(!stack,1))
    val sblock = ref (top(!stackStmt,1))
    val _ = (pop();popStmt())
  in
    (stackStmt:=Case([],const,sblock,"","")::(!stackStmt))
  end

fun mkTreeMemOp() =
  let
    val memOp = ref (top(!stack,2))
    val memOpStr = getMemOp(!memOp)
    val exp = ref (top(!stack,1))
    val _ = (pop();pop())
  in
    case (!memOp) of
      ResExp(str) => stackStmt:=Reserve([],str,exp)::(!stackStmt)
    | RelExp(str) => stackStmt:=Release([],str,exp)::(!stackStmt)
    | _ => ()
  end

fun mkTreeAssArg() =
  let
    val assignable = ref (top(!stack,2))
    val arg = ref [(top(!stack,1))]
    val _ = (pop();pop())
  in
    (stack:=AssArg("",[],assignable,arg)::(!stack))
  end
fun mkTreeAssArgEmpty() = 
  let
    val assignable = ref (top(!stack,1))
    val _ = pop()
  in
    (stack:=AssArg("",[],assignable,ref [])::(!stack))
  end
fun addTreeAssArg() =
  let
    val assArg = ref (top(!stack,2))
    val assArgvars = getAssArg(!assArg)
    val argList = (#4 assArgvars)
    val arg = ref (top(!stack,1))
    val _ = (pop();pop())
    val newArgList = (!argList)@[!arg]
    (*val _ = print("s: " ^ Int.toString(length newArgList) ^ "\n")*)
  in
    (stack:=AssArg( (#1 assArgvars),(#2 assArgvars),
                    (#3 assArgvars),ref newArgList)::(!stack) )
  end

fun mkTreeFor() =
  let
    val stmt1 = ref (top(!stackStmt,3))
    val stmt2 = ref (top(!stackStmt,2))
    val sblock = ref (top(!stackStmt,1))
    val cond = ref (top(!stack,1))
    val _ = (pop(); popStmt(); popStmt(); popStmt())
  in
    (stackStmt:=For([],stmt1,cond,stmt2,sblock,"")::(!stackStmt))
  end

fun mkTreeWhile() =
  let
    val cond = ref (top(!stack,1))
    val body = ref (top(!stackStmt,1))
    val _ = (pop(); popStmt())
  in
    (stackStmt:=While([],cond,body,"")::(!stackStmt))
  end

fun mkTreeIf() =
  let
    val cond = ref (top(!stack,1))
    val thenBlock = ref (top(!stackStmt,2))
    val elseBlock = ref (top(!stackStmt,1))
    val _ = (pop();popStmt();popStmt())
  in
    (stackStmt:=If([],cond,thenBlock,elseBlock,"")::(!stackStmt))
  end

fun mkTreeStmtAssign() =
  let
    val lhs = ref (top(!stack,2))
    val rhs = ref (top(!stack,1))
    val child = [lhs,rhs]
  in
    (pop();pop(); stackStmt:=Assign([],child)::(!stackStmt))
  end

fun mkTreeIdList() = 
  let
    fun mkIdList() = 
      let
        val statement = (top(!stackStmt,1))
        val _ = popStmt()
      in
        (stackStmt:=DBlockStmtlist([],ref [statement])::(!stackStmt))
      end

    fun addIdList() = 
      let
        fun getChild(sl) =
          case sl of
            DBlockStmtlist(code,child) => !child
          | _ => []
        val statement = top(!stackStmt,1)
        val stmtlist = top(!stackStmt,2)
        val children = getChild(stmtlist)
        val newChildren = ref (children@[statement])
        val _ = (popStmt();popStmt())
      in
        (stackStmt:=DBlockStmtlist([], newChildren)::(!stackStmt))
      end
    val len = length (!stackStmt)
  in
    case (len >= 2) of
      true => let
                val idlist = top(!stackStmt,2)
              in
                case idlist of
                  DBlockStmtlist(code,child) => addIdList()
                | _ => mkIdList()
              end
    | _ => mkIdList()
  end

fun mkTreeDBlockEmpty() = 
  stackStmt:=DBlockStmtlist([],ref [])::(!stackStmt)

fun mkTreeStmtAssign2() =
  let
    fun helper() = 
      let
        val lhs = ref (top(!stack,1))
        val rhs = ref (top(!stack,2))
        val child = [lhs,rhs]
      in
        case (!rhs) of
          EmptyExp(i) => (pop();pop())
        | _ => (pop();pop(); stackStmt:=Assign([],child)::(!stackStmt); mkTreeIdList())
      end
    val len = length (!stack)
  in
    case (len >= 2) of
      true => (helper())
    | _ => pop()
  end
  handle Empty => print "mkTreeStmtAssign2\n"

fun mkTreeStmtlist() = 
  let
    fun mkIdList() =
      let
        val statement = (top(!stackStmt,1))
        val _ = popStmt()
      in
        (stackStmt:=DBlockStmtlist([],ref [statement])::(!stackStmt))
      end

    fun addIdList() =
      let
        fun getChild(sl) =
          case sl of
            DBlockStmtlist(code,child) => !child
          | _ => []
        val statement = top(!stackStmt,1)
        val stmtlist = top(!stackStmt,2)
        val children = getChild(stmtlist)
        val newChildren = ref (children@[statement])
        val _ = (popStmt();popStmt())
      in
        (stackStmt:=DBlockStmtlist([], newChildren)::(!stackStmt))
      end
    val len = length (!stackStmt)
  in
    case (len >= 2) of
      true => let
                val idlist = top(!stackStmt,2)
              in
                case idlist of
                  DBlockStmtlist(code,child) => addIdList()
                | _ => mkIdList()
              end
    | _ => mkIdList()
  end

fun mkTreeDBlockEmpty() =
  stackStmt:=DBlockStmtlist([],ref [])::(!stackStmt)

fun mkTreeStmtAssign2() =
  let
    fun helper() =
      let
        val lhs = ref (top(!stack,1))
        val rhs = ref (top(!stack,2))
        val child = [lhs,rhs]
      in
        case (!rhs) of
          EmptyExp(i) => (pop();pop())
        | _ => (pop();pop(); stackStmt:=Assign([],child)::(!stackStmt); mkTreeIdList())
      end
    val len = length (!stack)
  in
    case (len >= 2) of
      true => (helper())
    | _ => pop()
  end
  handle Empty => print "mkTreeStmtAssign2\n"

fun mkTreeStmtlist() =
  let
    val statement = (top(!stackStmt,1))
    val _ = popStmt()
  in
    (stackStmt:=Stmtlist([],ref [statement])::(!stackStmt))
  end

fun addTreeStmtlist() =
  let
    fun getChild(sl) =
      case sl of
        Stmtlist(code,child) => !child
      | _ => []
    val statement = top(!stackStmt,1)
    val stmtlist = top(!stackStmt,2)
    val children = getChild(stmtlist)
    val newChildren = ref (children@[statement])
    val _ = (popStmt();popStmt())
  in
    (stackStmt:=Stmtlist([], newChildren)::(!stackStmt))
  end

fun mkTreeBinOp(operator) =
  let
    val op1 = ref (top(!stack,2))
    val op2 = ref (top(!stack,1))
    val _ = (pop();pop())
  in
    case operator of
      Plus(opt) => (stack:=Binop("+",[],op1,op2)::(!stack))
    | Mod(opt) => (stack:=Binop("%",[],op1,op2)::(!stack))
    | Mul(opt) => (stack:=Binop("*",[],op1,op2)::(!stack))
    | Sub(opt) => (stack:=Binop("-",[],op1,op2)::(!stack))
    | Div(opt) => (stack:=Binop("/",[],op1,op2)::(!stack))

  end

fun mkTreeUnOp(operator) =
  let
    val op1 = ref (top(!stack,1))
    val _ = pop()
  in
    case operator of
      Bang(b) => (stack:=Unop(b,"",[],op1)::(!stack))
    | Neg(b) => (stack:=Unop(b,"",[],op1)::(!stack))
    | I2r(b) => (stack:=Unop(b,"",[],op1)::(!stack))
    | R2i(b) => (stack:=Unop(b,"",[],op1)::(!stack))
  end

fun mkTreeRelOp(operator) =
  let
    val op1 = ref (top(!stack,2))
    val op2 = ref (top(!stack,1))
    val _ = (pop();pop())
  in
    case operator of
      Lt(lt) => (stack:=Relop(lt,lt,[],op1,op2)::(!stack))
    | Eq(eq) => (stack:=Relop(eq,eq,[],op1,op2)::(!stack))
    | _ => ()
  end

fun mkTreeIsNull() =
  let
    val op1 = ref (top(!stack,1))
    val op2 = ref (Const("null", []))
    val _ = (pop())
  in
    (stack:=Relop("==","==",[],op1,op2)::(!stack))
  end

fun mkTreeBoolOp(operator) =
  let
    val op1 = ref (top(!stack,2))
    val op2 = ref (top(!stack,1))
    val _ = (pop();pop())
  in
    case operator of
      Pipe(opt) => (stack:=Boolop(opt,opt,[],op1,op2)::(!stack))
    | Amp(opt) => (stack:=Boolop(opt,opt,[],op1,op2)::(!stack))
  end

fun mkTreeSblock() =
  let
    val len = length (!stackStmt)
  in
    case (len >= 2) of
      true => let
                val statementList = top(!stackStmt,1)
                val dblockStmtList = top(!stackStmt,2)
              in
                case dblockStmtList of
                  DBlockStmtlist(code,child) =>
                    (popStmt();popStmt();
                     stackStmt:=Sblock([], ref dblockStmtList, ref statementList, "")::(!stackStmt))
                | _ => (popStmt();stackStmt:=Sblock([], ref (DBlockStmtlist([],ref [])),ref statementList,"")::(!stackStmt))
              end
    | _ => let
              val statementList = top(!stackStmt,1)
            in
              (popStmt();
              stackStmt:=Sblock([], ref (DBlockStmtlist([],ref [])), ref statementList, "")::(!stackStmt))
            end
  end

(*----------------- Generate intermediate code -----------------*)
fun codeGenExp(e) =
  let
    fun codeGenBin(addr,code,op1,op2) =
      let
        val _ = op1 := codeGenExp(!op1)
        val _ = op2 := codeGenExp(!op2)
        val tempvar = TV.newTempVar()
        val op1ac = getAddrCode(!op1)
        val op2ac = getAddrCode(!op2)
        val newCode = tempvar ^ " = " ^ (#1 op1ac) ^ " " ^ addr ^ " " ^ (#1 op2ac) ^ "\n"
        (*val _ = print (newCode ^"\n")*)
        val newCodeList = (#2 op1ac)@(#2 op2ac)@[newCode]
        (*val _ = printList(newCodeList)*)
      in
        Binop(tempvar,newCodeList,op1,op2)
      end

    fun codeGenUn(operator,addr,code,op1) =
      let
        val _ = op1 := codeGenExp(!op1)
        val tempvar = TV.newTempVar()
        val op1ac = getAddrCode(!op1)
        val newCode = tempvar ^ " = " ^ operator ^ (#1 op1ac) ^ "\n"
        val newCodeList = (#2 op1ac)@[newCode]
      in
        Unop(operator,tempvar,newCodeList,op1)
      end

    fun codeGenAssArg(addr,codeList,ass,argList) =
      let
        fun codeAssArgChildren(newList,lst) =
          case lst of
            [] => newList
          | x::xs => codeAssArgChildren(codeGenExp(x)::newList,xs)

        fun mergeCodeChildren(newList,childList) =
          let
            fun makeCode(ex) =
              let
                val exAddrCode = getAddrCode(ex)
                val addr = (#1 exAddrCode)
                val code = (#2 exAddrCode)
                val newCode = code@["param "^addr^ "\n"]
                (*val _ = printList(newCode)*)
              in
                newCode
              end
          in
            case childList of
              [] => (newList)
            | x::xs => mergeCodeChildren(
              makeCode(x)@newList,xs)
          end
        val newChildList = codeAssArgChildren([],!argList)
        val assCode = codeGenExp(!ass)
        val assVars = getAddrCode(assCode)
        (*val _ = print ((#1 assVars) ^ " " ^Int.toString(length (#2 assVars)) ^ "\n")*)
        val newArgCode = mergeCodeChildren([],newChildList)
        val argSize = Int.toString(length newChildList)
        val tempvar = TV.newTempVar();
        val newCode = tempvar ^" = call(" ^ (#1 assVars) ^ "," ^
                      argSize ^ ")\n"
        val newCodeWithArgs = newArgCode@[newCode]
      in
        AssArg(tempvar,newCodeWithArgs,ass,ref newChildList)
      end

  fun codeGenAssArgArr(addr,codeList,ass,argList) = 
    let
      fun getMultiplier(res,dimList) = 
        case dimList of
          [] => res
        | x::xs => getMultiplier(res * x,xs)

      fun codeAssArgChildren(newList,lst) = 
          case lst of
            [] => newList
          | x::xs => codeAssArgChildren(newList@[codeGenExp(x)],xs)

      fun mergeCodeChildren(newList,newTempList,childList,width,dimList) = 
        let
          fun makeCode(ex) = 
            let
              val (addr,code) = getAddrCode(ex)
              val tempVar = TV.newTempVar();
              val multiplier = getMultiplier(1,tl dimList) * width
              val newCode = tempVar ^ " = " ^ addr ^ " * " ^ 
                            Int.toString(multiplier) ^ "\n"
            in
              (newCode,tempVar)
            end
        in
          case childList of
            [] => (newList,newTempList)
          | x::xs => 
              let val ret = makeCode(x) in
                mergeCodeChildren(newList@[#1 ret],newTempList@[#2 ret],xs,width,tl dimList)
              end
        end

      fun codeTempList(codeList,tempList) = 
        let
          fun makeCode(t1,t2) = 
            let
              val tempVar = TV.newTempVar();
              val newCode = tempVar ^ " = " ^ t1 ^ " + " ^ t2 ^ "\n"
            in
              (newCode,tempVar)
            end
        in
          case tempList of 
            [] => (codeList,(hd tempList))
          | x::[] => (codeList, (hd tempList))
          | x1::x2::xs => 
              let val (newCode,tempVar) = makeCode(x1,x2) in
                codeTempList(codeList@[newCode], tempVar::(tl (tl tempList) ))
              end
        end

      val newChildList = codeAssArgChildren([],!argList)
      val assCode = codeGenExp(!ass)
      val assVars = getAddrCode(assCode)
      val rtVars = RT.get(#1 assVars)
      val (id,width,dimList,size) = RT.get(#1 assVars)
      val (newArgCode,tempList) = mergeCodeChildren([],[],newChildList,width,dimList)   
      val (tempListCode,newAddr) = codeTempList([],tempList)
      val tempVar = TV.newTempVar();
      val newAssArgCode = tempVar ^ " = " ^ (#1 assVars) ^ "[" ^ newAddr ^ "]\n" 
      val newCodeList = newArgCode@tempListCode@[newAssArgCode]
    in
      AssArg(tempVar,newCodeList,ref assCode,ref newChildList)
    end

  in
    case e of
      Id(addr,code) => e
    | Const(addr,code) => e
    | Unop(operator,addr,code,op1) =>
        codeGenUn(operator,addr,code,op1)
    | Binop(addr,code,op1,op2) =>
        codeGenBin(addr,code,op1,op2)
    | Relop(operator,addr,code,op1,op2) =>
        codeGenBin(addr,code,op1,op2)
    | Boolop(operator,addr,code,op1,op2) =>
        codeGenBin(addr,code,op1,op2)
    | AssArg(addr,codeList,ass,argList) => if
        ( ST.getExtra(#1 (getAddrCode(codeGenExp(!ass)))) = "function" ) 
        then codeGenAssArg(addr,codeList,ass,argList)
        else codeGenAssArgArr(addr,codeList,ass,argList)
    | _ => e
  end

fun codeGenCond(cond,condTrue,condFalse) =
  let
    fun relop(operator,addr,codeList,op1,op2, condTrue, condFalse) =
      let
        val _ = op1 := codeGenExp(!op1)
        val _ = op2 := codeGenExp(!op2)
        val op1ac = getAddrCode(!op1)
        val op2ac = getAddrCode(!op2)
        val newCode = "if " ^ (#1 op1ac) ^ " " ^ operator ^ " " ^
                  (#1 op2ac) ^ " goto " ^ condTrue ^ "\n" ^
                  "goto " ^ condFalse ^ "\n"
        val newCodeList = (#2 op1ac)@(#2 op2ac)@[newCode]
        (*val _ = printList(newCodeList)*)
      in
        BoolCond(newCodeList,condTrue,condFalse)
      end

    fun pipeop(operator,addr,codeList,op1,op2,condTrue,condFalse) =
      let
        val b1true = condTrue
        val b1false = TV.newLabel()
        val b2true = condTrue
        val b2false = condFalse
        val _ = op1 := codeGenCond(!op1, b1true, b1false)
        val _ = op2 := codeGenCond(!op2, b2true, b2false)
        val b1ac = getBoolCondCode(!op1)
        val b2ac = getBoolCondCode(!op2)
        val newLabel = b1false ^ ": "
        val newCodeList = (#1 b1ac)@[newLabel]@(#1 b2ac)
      in
        BoolCond(newCodeList,condTrue,condFalse)
      end

     fun ampop(operator,addr,codeList,op1,op2,condTrue,condFalse) =
      let
        val b1true = TV.newLabel()
        val b1false = condFalse
        val b2true = condTrue
        val b2false = condFalse
        val _ = op1 := codeGenCond(!op1, b1true, b1false)
        val _ = op2 := codeGenCond(!op2, b2true, b2false)
        val b1ac = getBoolCondCode(!op1)
        val b2ac = getBoolCondCode(!op2)
        val newLabel = b1true ^ ": "
        val newCodeList = (#1 b1ac)@[newLabel]@(#1 b2ac)
      in
        BoolCond(newCodeList,condTrue,condFalse)
      end

    fun falseop(addr,condTrue,condFalse) =
      let
        val cond = if(addr="false") then condFalse else condTrue
        val code = "goto " ^ cond ^ "\n"
      in
        BoolCond([code],condTrue,condFalse)
      end

    fun idop(addr,condTrue,condFalse) =
      let
        val cond = if(addr="false") then condFalse else condTrue
        val code = "if " ^ addr ^ " == false goto " ^ condFalse ^ "\n" ^
                   "goto " ^ condTrue ^ "\n"
      in
        BoolCond([code],condTrue,condFalse)
      end

    fun unop(operator,addr,codeList,op1,condTrue,condFalse) = 
      let
        val cond = if(addr="false") then condFalse else condTrue
        val code = "if " ^ addr ^ " == false goto " ^ condFalse ^ "\n" ^
                   "goto " ^ condTrue ^ "\n"
      in
        BoolCond([code],condTrue,condFalse)
      end

    fun unop(operator,addr,codeList,op1,condTrue,condFalse) =
      let
        val _ = op1 := codeGenCond(!op1,condFalse,condTrue)
        val op1ac = getBoolCondCode(!op1)
        val newCode = (#1 op1ac)
      in
        BoolCond(newCode, condTrue, condFalse)
      end

  in
    case cond of
      Relop(operator,addr,codeList,op1,op2) =>
        relop(operator,addr,codeList,op1,op2,condTrue,condFalse)
    | Boolop(operator,addr,codeList,op1,op2) =>
        if (operator="|")
        then pipeop(operator,addr,codeList,op1,op2,condTrue,condFalse)
        else ampop(operator,addr,codeList,op1,op2,condTrue,condFalse)
    | Const(addr,code) => falseop(addr,condTrue,condFalse)
    | Id(addr,code) => idop(addr,condTrue,condFalse)
    | Unop(operator,addr,codeList,op1) =>
        unop(operator,addr,codeList,op1,condTrue,condFalse)
    | _ => BoolCond([],"","")
  end

fun codeGenStmt(s) =
  let
    fun codeAssign(code,child) =
      let
        val lhs = codeGenExp( get(child,0) )
        val rhs = codeGenExp( get(child,1) )
        val lhsac = getAddrCode(lhs)
        val rhsac = getAddrCode(rhs)
        val newChild = [ref lhs, ref rhs]
        val newCode = (#1 lhsac) ^ " = " ^ (#1 rhsac) ^ "\n"
        val newCodeList = (#2 rhsac)@[newCode]
      in
        Assign(newCodeList,child)
      end
    fun codeStmtlist(code,child) =
      let
        fun codeGenChildren(newlist,childlist) =
          case childlist of
            [] => newlist
          | x::xs => codeGenChildren(codeGenStmt(x)::newlist,xs)
        fun mergeCodeChildren(newlist, childlist) =
          case childlist of
            [] => newlist
          | x::xs => mergeCodeChildren( getCode(x)@newlist,xs )
        val newChildList = codeGenChildren([],child)
        val newCode = mergeCodeChildren([], newChildList)
        (*val _ = printList(newCode)*)
      in
        Stmtlist(newCode,ref newChildList)
      end

    fun codeDblockStmtlist(code,child) =
      let
        fun codeGenChildren(newlist,childlist) =
          case childlist of
            [] => newlist
          | x::xs => codeGenChildren(codeGenStmt(x)::newlist,xs)
        fun mergeCodeChildren(newlist, childlist) = 
          case childlist of
            [] => newlist
          | x::xs => mergeCodeChildren( getCode(x)@newlist,xs )
        val newChildList = codeGenChildren([],child)
        val newCode = mergeCodeChildren([], newChildList)
      in
        (DBlockStmtlist(newCode,ref newChildList))
      end

    fun codeSblock(code,dblockchild,child) =
      let
        val newChild = ref (codeGenStmt(!child))
        val newDblockChild = ref (codeGenStmt(!dblockchild))
        val newCode = getCode(!newChild)
        val newDblockCode = getCode(!newDblockChild)
        val label = TV.newLabel()
        val newCodeList = code@newDblockCode@newCode
        (*val _ = printListSize(newCode)*)
        (*val _= outputCode := newCodeList*)
      in
        Sblock(newCodeList,newDblockChild,newChild,label)
      end

    fun codeWhile(code,cond,body,next) =
      let
        val begin = TV.newLabel()
        val condTrue = TV.newLabel()
        val condFalse = TV.newLabel()
        val newCond = codeGenCond(!cond,condTrue,condFalse)
        val condvars = getBoolCond(newCond)
        val condCode = (#1 condvars)
        val newBody = codeGenStmt(!body)
        val bodyNext = begin
        val bodyvars = getSblock(newBody)
        val newCode = "goto " ^ begin ^ "\n"
        val newCodeList = [begin ^ ": "]@(#1 condvars)@[(#2 condvars) ^ ": "]@
                          (#1 bodyvars)@[newCode]@[condFalse ^ ": "]
        (*val _ = printList(newCodeList)*)
      in
        While(newCodeList,ref newCond,ref newBody,next)
      end

    fun codeFor(code,stmt1,cond,stmt2,body,next) = 
      let
        val begin = TV.newLabel()
        val condTrue = TV.newLabel()
        val condFalse = TV.newLabel()

        val newStmt1 = codeGenStmt(!stmt1)
        val newStmt1Code = getCode(newStmt1)
        val newCond = codeGenCond(!cond,condTrue,condFalse)
        val condvars = getBoolCond(newCond)
        val condCode = (#1 condvars)
        val newBody = codeGenStmt(!body)
        val newStmt2 = codeGenStmt(!stmt2)
        val newStmt2Code = getCode(newStmt2)
        val bodyVars = getSblock(newBody)
        val newCode = "goto " ^ begin ^ "\n"
        val newCodeList = newStmt1Code@[begin ^ ": "]@(#1 condvars)@
                          [(#2 condvars) ^ ": "]@(#1 bodyVars)@newStmt2Code@
                          [newCode]@[condFalse ^ ": "]
      in
        For(newCodeList,ref newStmt1, ref newCond, ref newStmt2, ref newBody, next)
      end

    fun codeIf(codeList,cond,thenBlock,elseBlock,next) = 
      let
        val condTrue = TV.newLabel()
        val condFalse = TV.newLabel()
        val condNext = TV.newLabel()
        val elseNext = condNext

        val newCond = codeGenCond(!cond,condTrue,condFalse)
        val condvars = getBoolCond(newCond)

        val newThenBlock = codeGenStmt(!thenBlock)
        val newElseBlock = codeGenStmt(!elseBlock)
        val thenBlockVars = getSblock(newThenBlock)
        val elseBlockVars = getSblock(newElseBlock)

        val newCode = "goto " ^ elseNext ^ "\n"
        val newCodeList = (#1 condvars)@[condTrue ^ ": "]@(#1 thenBlockVars)@
                          [newCode]@[condFalse ^ ": "]@(#1 elseBlockVars)
                          @[elseNext ^ ": "]
      in
        If(newCodeList, ref newCond, ref newThenBlock, ref newElseBlock, elseNext)
      end

    fun codeReserve(code,memOp,assArg) =
      let
        fun getTempVar(str) = 
          let
            fun splitStr(newStr,lst) = 
              case lst of
                [] => String.implode(newStr)
              | x::xs =>  if Char.isSpace x
                          then String.implode newStr
                          else splitStr(newStr@[x],xs)
            val exploded = String.explode str
          in
            splitStr([],exploded)
          end
        fun removeLastinList(lst) = 
          case lst of
            [] => []
          | [x] => lst
          | _ => List.take(lst, (length lst)-1) 
        fun codeGenAssArg(addr,codeList,ass,argList) =
          let
            fun stringToInt(exp) =
              let
                val addrCode = getAddrCode(exp)
                val addr = (#1 addrCode)
              in
                valOf(Int.fromString(addr))
              end
            fun codeAssArgChildren(newList,lst) = 
              case lst of
                [] => newList
              | x::xs => codeAssArgChildren(codeGenExp(x)::newList,xs)
            fun createDimList(newLst,lst) =
              case lst of
                [] => newLst
              | x::xs => createDimList((stringToInt(x))::newLst,xs)

            val newChildList = codeAssArgChildren([],!argList)
            val assCode = codeGenExp(!ass)
            val assVars = getAddrCode(assCode)
            val arrReturnType = ST.getArrayType(#1 assVars)
            val width = getWidth(arrReturnType)
            val dimList = createDimList([],newChildList)
          in
            (*print (Int.toString(length newChildList) ^ " " ^ Int.toString(width) ^ " " ^ 
                    (#1 assVars) ^ " " ^ arrReturnType ^ "\n");*)
             RT.reserve(RT.Arr((#1 assVars),width,dimList,"")) 
          end

        val assArgVars = getAssArg(!assArg)
        val _ = codeGenAssArg(assArgVars)

        val newAssArg = codeGenExp(!assArg)
        val (addr,codeList,ass,argList) = getAssArg(newAssArg)
        val codeListLastRemoved = removeLastinList(codeList)
        val assVars = getAddrCode(!ass)
        val arrReturnType = ST.getArrayType(#1 assVars)
        val arrSize = getTempVar(hd (rev codeListLastRemoved))
        val _ = RT.updateSize(#1 assVars,arrSize)
        val newCode = (#1 assVars) ^ " = " ^ "malloc(" ^ arrSize ^ ")\n"
        val newCodeList = (codeListLastRemoved)@[newCode]
      in
        Reserve(newCodeList,memOp,ref newAssArg)
      end

    fun codeReserveExp(code,memOp,exp) =
      let
        val newExp = codeGenExp(!exp)
        val (expAddr,expCode) = getAddrCode(newExp)
        val typeWidth = getWidth(ST.getType(expAddr))
        val typeWidthStr = Int.toString(typeWidth)
        val _ = RT.reserve(RT.Arr(expAddr,typeWidth,[],typeWidthStr))
        val newCode = expAddr ^ " = " ^ "malloc(" ^ typeWidthStr ^ ")\n"
        val newCodeList = (expCode)@[newCode]
      in
        Reserve(newCodeList,memOp,ref newExp)
      end

    fun codeRelease(code,memOp,exp) =
      let
        val newExp = codeGenExp(!exp)
        val (expAddr,expCode) = getAddrCode(newExp)
        val (rtName,rtWidth,rtDimList,rtSize) = RT.get(expAddr)
        val newCode = "release(" ^ expAddr ^ ", " ^ rtSize ^ ")\n"
        val newCodeList = (expCode)@[newCode]
      in
        Release(newCodeList,memOp,ref newExp)
      end

    fun codeCase(code,const,sblock,nextLabel,label) = 
      let
        val newConst = codeGenExp(!const)
        val newSblock = codeGenStmt(!sblock)
        val newLabel = TV.newLabel();
        val (sblockcodeList,dblockstmt,stmtChild,sblocknextLabel) = getSblock(newSblock)
        val newCode = [newLabel ^ ": "]@(sblockcodeList)@["goto " ^ nextLabel ^ "\n"]
        (*val _ = printList(newCode)*)
      in
        Case(newCode,ref newConst, ref newSblock,nextLabel,newLabel)
      end

    fun codeOtherwise(code,sblock,nextLabel,label) = 
      let
        val newSblock = codeGenStmt(!sblock)
        val (sblockcodeList,dblockstmt,stmtChild,sblocknextLabel) = getSblock(newSblock)
        val newLabel = TV.newLabel()
        val newCode = [newLabel ^ ": "]@(sblockcodeList)@["goto " ^ nextLabel ^ "\n"]
        (*val _ = printList(newCode)*)
      in
        Otherwise(newCode,ref newSblock,nextLabel,newLabel)
      end
 
    fun codeSwitch(code,expression,caselist,otherwise,testLabel,nextLabel) =
      let
        fun codeCaseList(nLabel,newList,cList) = 
          case cList of
            [] => newList
          | x::xs =>
              let
                val (code,const,sblock,nlabel,label) = getCase(x)
                val newCase = codeGenStmt(Case(code,const,sblock,nLabel,label))
              in
                (codeCaseList(nLabel,newCase::newList,xs))
              end
        fun mergeCodeCaseList(newLst,caseLst) = 
          case caseLst of
            [] => newLst
          | x::xs =>
              let
                val (code,const,sblock,nlabel,label) = getCase(x)
              in
                mergeCodeCaseList(code@newLst,xs)
              end

        fun codeTestList(t,newLst,caseLst) = 
          case caseLst of
            [] => newLst
          | x::xs =>
              let
                val (code,const,sblock,nlabel,label) = getCase(x)
                val (addr,code) = getAddrCode(!const)
                val newCode = "if " ^ t ^ " = " ^ addr ^ " goto " ^ label ^ "\n"
              in
                codeTestList(t,newCode::newLst,xs)
              end
        val newExp = codeGenExp(!expression)
        val (eaddr,ecode) = getAddrCode(newExp)
        val newNextLabel = TV.newLabel();
        val newTestLabel = TV.newLabel();
        val newCaseList = codeCaseList(newNextLabel,[],!caselist)
        val newCaseListCode = mergeCodeCaseList([],newCaseList)
        val (ocode,osblock,onextLabel,olabel) = getOtherwise(!otherwise)
        val newOtherwise = codeGenStmt(Otherwise(ocode,osblock,newNextLabel,olabel))
        val (ocode,osblock,onextLabel,olabel) = getOtherwise(newOtherwise)
        val newCode = ecode@["goto " ^ newTestLabel ^ "\n"]
        val testList = (codeTestList(eaddr,[],newCaseList))
        val newCodeList = newCode@newCaseListCode@ocode@[newTestLabel ^ ": "]@
                          testList@["goto " ^ olabel ^ "\n"]@[newNextLabel ^ ": "]
      in
        Switch(newCodeList,ref newExp, ref newCaseList,ref newOtherwise,newTestLabel,newNextLabel)
      end

    fun codeFunc(code,name,sblock) =
      let
        fun insertReturn(newList,xs) = 
          let
            fun listToStr(newStr,lst) = 
              case lst of
                [] => newStr
              | x::[] => listToStr(newStr ^ x, [])
              | x::xs => listToStr(newStr ^ x ^ " ",xs) 

            fun splitStr(s,delimiter) = 
              let
                val split = String.tokens (fn c => c = delimiter) s
                val len = length split
              in
                if(len >= 3 andalso (List.nth(split,0) = name) andalso (List.nth(split,1) = "=")) then
                  "return " ^  listToStr("",List.drop(split,2))
                else
                  s
              end
          in
            case xs of
              [] => newList
            | x::xs => insertReturn(newList@[splitStr(x,#" ")],xs) 
          end

        fun isLabel(str) = 
          let
            val exploded = rev (String.explode str)
            val len = length exploded
          in
            (len >= 3 andalso (List.nth(exploded,0) = #" ") andalso (List.nth(exploded,1) = #":"))
          end

        fun getGotoLabel(str) = 
          let
            val label = String.tokens (fn c => c = #":") str
          in
            "goto " ^ (List.nth(label,0)) ^ "\n"
          end

        fun removeExtraLabelGoto(newList,xs,str) = 
          case xs of
            [] => List.take(newList,(length newList)-1)
          | x::xs => if(x = str) then
                      removeExtraLabelGoto(newList,xs,str)
                    else
                      removeExtraLabelGoto(newList@[x],xs,str)

        val newSblock = codeGenStmt(!sblock)
        val (sblockcodeList,dblockstmt,stmtChild,sblocknextLabel) = getSblock(newSblock)
        val sblockCode = getCode(newSblock)
        val newCode = [name ^ ": "]@sblockCode
        val codeReturn = insertReturn([],newCode)
        val lastLine = List.last codeReturn
        val codeRemoveExtra = if (isLabel(lastLine)) then
                                removeExtraLabelGoto([],codeReturn,getGotoLabel(lastLine)) 
                              else codeReturn
      in
        Func(codeRemoveExtra,name,ref newSblock)
      end

  in
    case s of
      Assign(code,child) => codeAssign(code,child)
    | Stmtlist(code,child) => codeStmtlist(code,!child)
    | DBlockStmtlist(code,child) => codeDblockStmtlist(code,!child)
    | Sblock(code,dblockchild,child,next) => codeSblock(code,dblockchild,child)
    | While(code,cond,body,next) => codeWhile(code,cond,body,next)
    | For(code,stmt1,cond,stmt2,body,next) => 
                          codeFor(code,stmt1,cond,stmt2,body,next)
    | If(code,cond,thenBlock,elseBlock,next) => 
                          codeIf(code,cond,thenBlock,elseBlock,next)
    | Reserve(code,memOp,assArg) => 
        let val aa = !assArg in
          case (aa) of
            AssArg(addr,codeList,ass,argList) => codeReserve(code,memOp,assArg)
          | _ => codeReserveExp(code,memOp,assArg)
        end
    | Release(code,memOp,assArg) => codeRelease(code,memOp,assArg)
    | Switch(code,expression,caselist,otherwise,testLabel,nextLabel) =>
        codeSwitch(code,expression,caselist,otherwise,testLabel,nextLabel)
    | Case(code,const,sblock,nextLabel,label) => codeCase(code,const,sblock,nextLabel,label)
    | Otherwise(code,sblock,nextLabel,label) => codeOtherwise(code,sblock,nextLabel,label)
    | Func(code,name,sblock) => codeFunc(code,name,sblock)
    | _ => s
  end
  handle Empty => (print ("\n"); EmptyStmt(1)) (*Originally printed CodegenStmt*)

fun genOutputCode() =
  let
    val tempStackStmt = !stackStmt
    fun iterEntries(lst) = 
      case lst of
        [] => ()
      | x::xs => (outputCode := getCode(x)@(!outputCode);iterEntries(xs))
  in
    iterEntries(tempStackStmt)
  end

fun ir() =
  let
    val tempStackStmt = rev (!stackStmt)
    fun updateStackStmt(newLst,lst) =
      case lst of
        [] => stackStmt := newLst
      | x::xs => updateStackStmt([codeGenStmt(x)]@newLst,xs)
    in
      (updateStackStmt([],tempStackStmt);
       genOutputCode();
       outputCode:=["goto Main\n"]@(!outputCode)
       (*;printList(!outputCode)*))
    end

fun writeFile(filename) =
  let
    val ostream = TextIO.openOut filename
    val intermCode = !outputCode
    fun iterEntries(xs) =
      case xs of
        [] => ()
      | x::xs => (TextIO.output(ostream,x);output := (!output) ^ x;iterEntries(xs))
  in
    (iterEntries(intermCode);TextIO.closeOut(ostream))
  end

  fun gen_Code_String() =
    let
      val intermCode = !outputCode
      fun iterCodeEntries(xs) =
        case xs of
          [] => ()
        | x::xs => (output := (!output) ^ x;iterCodeEntries(xs))
    in
      iterCodeEntries(intermCode)
    end

fun qpush(e : exp) =
  case e of
    Const(addr,code) => (stack := e::(!stack))
  | Id(addr,code) => (stack := e::(!stack))
  | ResExp(str) => (stack := e::(!stack))
  | RelExp(str) => (stack := e::(!stack))
  | EmptyExp(i) => (stack := e::(!stack))
  | _ => ()

end
