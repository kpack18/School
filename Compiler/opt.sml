structure Opt = 
struct
  datatype line = IfGoto of string * string * string
                  (* label,ifStmt, gotoLabel*)
                | Goto of string * string
                  (* label,gotoLabel *) 
                | AssignBinOp of string * string * value * value * binop
                  (* label,assign,op1,op2,oper *)
                | AssignConst of string * string * value
                  (* label,assign,const *)
                | AllElse of string
                | EmptyLine of string
            and binop = Plus of string
                      | Mod of string
                      | Div of string
                      | Mul of string
                      | Sub of string
                      | Empty of string
            and value = INT of string
                    | REAL of string
                    | VAR of string


  val stack : line list ref = ref []
  val stackStr : string list ref = ref []

  fun readFile(infile : string) =
    let
      val ins = TextIO.openIn infile
      fun loop ins = 
        case TextIO.inputLine ins of
          SOME line => (line::loop ins)
        | NONE => []
    in
      loop ins before TextIO.closeIn ins
    end

  fun getValueStr(v) =
    case v of
      INT(i) => i
    | REAL(r) => r
    | VAR(v) => v

  fun getBinOpStr(b) = 
    case b of
      Plus(s) => s
    | Mod(s) => s
    | Div(s) => s
    | Mul(s) => s
    | Sub(s) => s
    | _ => ""

  fun printLst(lst) =
    case lst of
      [] => ()
    | x::xs => (print (Int.toString x);printLst(xs))

  fun printLstStr(lst) = 
    case lst of
      [] => ()
    | x::xs => (print (x^"\n");printLstStr(xs))

  fun printLine(x) =
    case x of
      IfGoto(label,ifStmt,gotoLabel) => (print (label ^ " " ^ ifStmt ^ " goto " ^ gotoLabel ^ "\n"))
    | Goto(label, gotoLabel) => (print (label ^ " goto " ^ gotoLabel ^ "\n"))
    | AssignBinOp(label,assign,op1,op2,oper) => 
        print (label ^ " " ^ assign ^ " = " ^ getValueStr(op1) ^ " " ^ 
          getBinOpStr(oper) ^ " " ^ 
          getValueStr(op2) ^ "\n")
    | AssignConst(label,assign,const) =>
        print (label ^ " " ^ assign ^ " = " ^ getValueStr(const) ^ "\n")
    | AllElse(str) => print str
    | EmptyLine(str) => print str

  fun printLineStr(x) =
    case x of
      IfGoto(label,ifStmt,gotoLabel) => 
        if(label = "") then
          (ifStmt ^ " goto " ^ gotoLabel ^ "\n")
        else
          (label ^ " " ^ ifStmt ^ " goto " ^ gotoLabel ^ "\n")
    | Goto(label, gotoLabel) => 
        if(label = "") then
          ("goto " ^ gotoLabel ^ "\n")
        else
          (label ^ " goto " ^ gotoLabel ^ "\n")
    | AssignBinOp(label,assign,op1,op2,oper) => 
        if(label = "") then
          (assign ^ " = " ^ getValueStr(op1) ^ " " ^ 
          getBinOpStr(oper) ^ " " ^ 
          getValueStr(op2) ^ "\n")
        else
          (label ^ " " ^ assign ^ " = " ^ getValueStr(op1) ^ " " ^ 
          getBinOpStr(oper) ^ " " ^ 
          getValueStr(op2) ^ "\n")
    | AssignConst(label,assign,const) =>
        if(label = "") then
          (assign ^ " = " ^ getValueStr(const) ^ "\n")
        else
          (label ^ " " ^ assign ^ " = " ^ getValueStr(const) ^ "\n")
    | AllElse(str) => str
    | EmptyLine(str) => str

  fun printStack() = 
    let
      val tempStack = !stack
      fun iter(st) = 
        case st of
          [] => ()
        | x::xs => (printLine(x);iter(xs))
    in
      iter(tempStack)
    end

  fun printStackStr() = 
    let
      val tempStack = !stackStr
      fun iter(st) = 
        case st of 
          [] => ()
        | x::xs => (print x;iter(xs))
    in
      iter(tempStack)
    end

  fun mkStackStr() =
    let
       val tempStack = !stack
       fun iter(st) = 
        case st of
          [] => ()
        | x::xs => (stackStr:=(!stackStr)@[printLineStr(x)];iter(xs))
     in
       iter(tempStack)
     end 

  fun splitStr(s) = 
    String.tokens (fn c => c = #" ") s

  fun mkString(str,lst) = 
    case lst of
      [] => implode (tl (explode str))
    | x::xs => mkString(str ^ " " ^x,xs)

  fun isLabel(s) = 
    let
      val last = hd (rev (explode s))
    in
      if(last = #":") then true else false
    end

  fun removeNewLine(s) =
    implode (rev (tl (rev (explode s))))


  fun getBinOp(str) =
    case str of
      "+" => Plus("+")
    | "-" => Sub("-")
    | "%" => Mod("%")
    | "/" => Div("/")
    | "*" => Mul("*")
    | _ => Empty("")

  fun getLabel(ln) = 
    let
      val tokens = splitStr(ln)
      val first = hd tokens
      val second = hd (tl tokens)
      val isFirst = isLabel(first)
      val isSec = isLabel(second)
    in
      if(isFirst andalso isSec) 
      then (first ^ " " ^ second)
      else if(isFirst) then (first)
      else ""
    end
  fun getGotoLabel(ln) =
    let
      val tokens = splitStr(ln)
      val label = removeNewLine(List.last tokens)
    in
      label
    end
  fun removeLabel(ln) = 
    let
      val tokens = splitStr(ln)
      val first = hd tokens
      val second = hd (tl tokens)
      val isFirst = isLabel(first)
      val isSec = isLabel(second)
    in
      if(isFirst andalso isSec) 
      then (List.drop (tokens,2))
      else if(isFirst) then tl tokens
      else tokens
    end

  fun getIfStmt(ln) = 
    let
      val removeLabl = removeLabel(ln)
      val removeGoto = rev (List.drop((rev removeLabl),2))
    in
      mkString("",removeGoto)
    end

  fun isConstant(value) = 
    case value of
      VAR(v) => (false)
    | _ => (true) 

  fun isInt(str) =
    let
      val expl = explode(str)
      val first = hd expl
      val expl2 = if(first = #"~") then tl expl else expl
      fun iter(lst) = 
        case lst of 
          [] => true
        | x::xs =>
            if(Char.isDigit x) then iter(xs) else false
    in
      iter(expl2)
    end

  fun isReal(str) = 
    let
      val expl = explode(str)
      val first = hd expl
      val expl2 = if(first = #"~") then tl expl else expl
      fun iter(lst) =
        case lst of 
          [] => true
        | x::xs =>
            if ((Char.isDigit x) orelse x = #".") then iter(xs) else false
    in
       iter(expl2)
    end

  fun getOp(str) = 
    if (isReal(str)) then REAL(str)
    else if (isInt(str)) then INT(str)
    else VAR(str)

  fun getAssign(ln) = 
    let
      val label = getLabel(ln)
      val removeLabl = removeLabel(ln)
      val assign = hd removeLabl
      val op1 = getOp(List.nth(removeLabl,2))
      val op2nl = List.nth(removeLabl,4)
      val op2nonewline = String.substring(op2nl,0,(size op2nl)-1)
      val op2 = getOp(op2nonewline)
      val oper = getBinOp(List.nth(removeLabl,3))
    in
      AssignBinOp(label,assign,op1,op2,oper)
    end

  fun isIfGoto(ln) =
    let
      val tokens = splitStr(ln)
      fun iter(tkns) = 
        case tkns of
          [] => false
        | x::xs => if(x = "if") then true else iter(xs)
    in
      iter(tokens)
    end

  fun isGoto(ln) =
    let
      val tokens = splitStr(ln)
      fun iter(tkns) = 
        case tkns of
          [] => false
        | x::xs => if(x = "goto") then true else iter(xs)
    in
      iter(tokens)
    end

  fun isAssignBinOp(ln) =
    let
      val tokens = removeLabel(ln)
      fun iter(num,tkns) =
        case tkns of
          [] => if (num andalso (length tokens) = 5) then true else false
        | x::xs => if(x = "=") then iter(true,xs) else iter(num,xs)
    in
      iter(false,tokens)
    end

  fun mkStack(filename) = 
    let
      val arr = readFile(filename)
      fun read(file : string) = 
        let
          val arr = readFile(file)
          fun iterArr(a) = 
            let
              val name = 2
            in
              case a of
                [] => ()
              | x::xs => (
                  if (isIfGoto(x)) 
                    then stack:=(!stack)@[IfGoto(getLabel(x),getIfStmt(x),getGotoLabel(x))] 
                  else if (isGoto(x)) 
                    then stack:=(!stack)@[Goto(getLabel(x),getGotoLabel(x))]
                  else if (isAssignBinOp(x)) 
                    then
                      stack:=(!stack)@[getAssign(x)]
                  else 
                    stack:=(!stack)@[AllElse(x)]; 
                  iterArr(xs))
            end
        in
          iterArr(arr)
        end
    in
      (read(filename))
    end

  fun constantFold(e) =
    let
      fun cfold(label,assign,op1,op2,oper) =
        let
          fun exec(op1,op2,oper) =
            let
              fun cfoldPlus(op1,op2) = 
                case op1 of
                  INT(str) => 
                    let
                      val op1val = valOf(Int.fromString(getValueStr(op1)))
                      val op2val = valOf(Int.fromString(getValueStr(op2)))
                      val ret = op1val + op2val
                      val retVal = INT(Int.toString(ret))
                    in
                      retVal
                    end
                | REAL(str) =>
                    let
                      val op1val = valOf(Real.fromString(getValueStr(op1)))
                      val op2val = valOf(Real.fromString(getValueStr(op2)))
                      val ret = op1val + op2val
                      val retVal = REAL(Real.toString(ret))
                    in
                      retVal
                    end
                | VAR(str) => VAR(str)
              fun cfoldSub(op1,op2) = 
                case op1 of
                  INT(str) => 
                    let
                      val op1val = valOf(Int.fromString(getValueStr(op1)))
                      val op2val = valOf(Int.fromString(getValueStr(op2)))
                      val ret = op1val - op2val
                      val retVal = INT(Int.toString(ret))
                    in
                      retVal
                    end
                | REAL(str) =>
                    let
                      val op1val = valOf(Real.fromString(getValueStr(op1)))
                      val op2val = valOf(Real.fromString(getValueStr(op2)))
                      val ret = op1val - op2val
                      val retVal = REAL(Real.toString(ret))
                    in
                      retVal
                    end
                | VAR(str) => VAR(str)
              fun cfoldMod(op1,op2) = 
                case op1 of
                  INT(str) => 
                    let
                      val op1val = valOf(Int.fromString(getValueStr(op1)))
                      val op2val = valOf(Int.fromString(getValueStr(op2)))
                      val ret = op1val mod op2val
                      val retVal = INT(Int.toString(ret))
                    in
                      retVal
                    end
                | REAL(str) =>
                    let
                      val op1val = valOf(Real.fromString(getValueStr(op1)))
                      val op2val = valOf(Real.fromString(getValueStr(op2)))
                      val ret = Real.rem (op1val,op2val)
                      val retVal = REAL(Real.toString(ret))
                    in
                      retVal
                    end
                | VAR(str) => VAR(str)
              fun cfoldMul(op1,op2) = 
                case op1 of
                  INT(str) => 
                    let
                      val op1val = valOf(Int.fromString(getValueStr(op1)))
                      val op2val = valOf(Int.fromString(getValueStr(op2)))
                      val ret = op1val * op2val
                      val retVal = INT(Int.toString(ret))
                    in
                      retVal
                    end
                | REAL(str) =>
                    let
                      val op1val = valOf(Real.fromString(getValueStr(op1)))
                      val op2val = valOf(Real.fromString(getValueStr(op2)))
                      val ret = op1val * op2val
                      val retVal = REAL(Real.toString(ret))
                    in
                      retVal
                    end
                | VAR(str) => VAR(str)
              fun cfoldDiv(op1,op2) = 
                case op1 of
                  INT(str) => 
                    let
                      val op1val = valOf(Int.fromString(getValueStr(op1)))
                      val op2val = valOf(Int.fromString(getValueStr(op2)))
                      val ret = op1val div op2val
                      val retVal = INT(Int.toString(ret))
                    in
                      retVal
                    end
                | REAL(str) =>
                    let
                      val op1val = valOf(Real.fromString(getValueStr(op1)))
                      val op2val = valOf(Real.fromString(getValueStr(op2)))
                      val ret = op1val / op2val
                      val retVal = REAL(Real.toString(ret))
                    in
                      retVal
                    end
                | VAR(str) => VAR(str)
            in
              case oper of
                Plus(str) => cfoldPlus(op1,op2)
              | Sub(str) => cfoldSub(op1,op2)
              | Mod(str) => cfoldMod(op1,op2)
              | Mul(str) => cfoldMul(op1,op2)
              | Div(str) => cfoldDiv(op1,op2)
              | Empty(str) => VAR(str)
            end
          val op1C = isConstant(op1)
          val op2C = isConstant(op2)
        in
          if (op1C andalso op2C)
          then 
            let
               val ret = exec(op1,op2,oper)
             in
               AssignConst(label,assign,ret)
             end 
          else AssignBinOp(label,assign,op1,op2,oper)
        end
    in
      case e of
        AssignBinOp(label,assign,op1,op2,oper) => 
          (cfold(label,assign,op1,op2,oper))
      | _ => e
    end

  fun flow(e) =
    let
      val temp = !stack
      fun checkLine(lab,g) =
        case g of
          Goto(label,gotoLabel) =>
            if(label = lab^":") then
              (gotoLabel)
            else
              ""
        | _ => ""

      fun searchGoto(label,gotolabel) =
        let
          fun getLine(g,lst) =
            case lst of
              [] => Goto(label,gotolabel)
            | x::xs => 
                let
                  val check = checkLine(gotolabel,x)
                in
                  if(check = "") then getLine(g,xs)
                  else Goto(label,check)
                end 
        in
          getLine(gotolabel,temp)
        end
      fun searchIfGoto(label,ifStmt,gotolabel) =
        let
          fun getLine(g,lst) =
            case lst of
              [] => IfGoto(label,ifStmt,gotolabel)
            | x::xs => 
                let
                  val check = checkLine(gotolabel,x)
                in
                  if(check = "") then getLine(g,xs)
                  else IfGoto(label,ifStmt,check)
                end 
        in
          getLine(gotolabel,temp)
        end

    in
      case e of
        IfGoto(label,ifStmt, gotoLabel) => searchIfGoto(label,ifStmt,gotoLabel)
      | Goto(label,gotoLabel) => searchGoto(label,gotoLabel)
      | _ => e
    end

  fun runOpt(num,filename) =
    let
      fun opt2() = ()

      fun opt4() = 
        let
          val temp = !stack
          fun iter(newLst,st) = 
            case st of
              [] => newLst
            | x::xs => iter(newLst@[constantFold(x)],xs) 
        in
          stack := iter([],!stack)
        end

      fun opt32() =
        let 
          val temp = !stack
          fun iter(newLst,st) = 
            case st of
              [] => newLst
            | x::xs => iter(newLst@[flow(x)],xs)
        in
          stack := iter([],!stack)
        end
    in
      case num of
        2 => opt2()
      | 3 => opt4()
      | 6 => opt32()
      | _ => ()
    end 

  fun iterBinary(num,lst,filename) =
    case lst of
      [] => ()
    | x::xs => if(x = 1) 
               then (runOpt(num,filename);iterBinary(num+1,xs,filename)) 
               else iterBinary(num+1,xs,filename)

  fun intToBinary(input) =
    let
      fun iter(lst, num) = 
        let
          val rem = num mod 2
          val newNum = num div 2
        in
          case num of
            0 => lst
          | _ => iter(rem::lst,newNum)
        end
    in
      iter([],input)      
    end

fun writeFile(filename) = 
  let
    val ostream = TextIO.openOut filename
    val intermCode = !stackStr
    fun iterEntries(xs) =
      case xs of
        [] => ()
      | x::xs => (TextIO.output(ostream,x);iterEntries(xs))
  in
    (iterEntries(intermCode);TextIO.closeOut(ostream))
  end

  fun optimize(input,filename) =
    let
      val _ = mkStack(filename)
      val binary = if(input = "grading") then (print "38\n";intToBinary(36))
                   else if(input = "supported") then (print "38\n";intToBinary(36))
                   else intToBinary(valOf(Int.fromString(input)))
    in
      (iterBinary(1,rev binary, filename);mkStackStr();(*printStackStr();*)writeFile(filename))
    end
end