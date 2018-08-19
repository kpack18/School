signature TEMPVAR = 
sig
  val newTempVar : unit -> string;  
  val currTempVar : unit -> string;
  val newLabel : unit -> string;

end

structure TempVar : TEMPVAR = 
struct
  val tempVarNum = ref 0;
  val tempLabelNum = ref 0;
  fun newTempVar() = 
    let 
      val tempStr = "t" ^ Int.toString(!tempVarNum);
    in
      (tempVarNum := !tempVarNum + 1; tempStr)
    end

  fun currTempVar() = "t" ^ Int.toString(!tempVarNum-1);

  fun newLabel() = 
    let 
      val tempStr = "L" ^ Int.toString(!tempLabelNum);
    in
      (tempLabelNum := !tempLabelNum + 1; tempStr)
    end

end