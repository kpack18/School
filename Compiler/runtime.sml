structure RunTime = 
struct

datatype arr = Arr of string * int * int list * string
              (* name,width,dimensions,size*)

val reservedList : arr list ref = ref []

fun getVars(a) = 
  case a of
    Arr(name,width,dimList,size) => (name,width,dimList,size)

fun get(a) = 
  let
    val tempList = !reservedList
    fun iterEntries(id,lst) = 
      case lst of
        [] => ("Undefined in runtime\n",0,[],"")
      | (Arr(name,width,dimList,size)::xs) => if (name=id) then (name,width,dimList,size) else iterEntries(id,xs) 
  in
    iterEntries(a,tempList)
  end


fun printLst(lst) = 
  let
    fun printArr(a) = 
      case a of
        Arr(name,width,dimList,size) => print (name ^ " " ^ size ^ "\n")
  in
    case lst of
      [] => ()
    | x::xs => (printArr(x); printLst(xs))
  end

fun updateSize(a,sz) = 
  let
    val tempList = !reservedList
    fun iterEntries(id,lst,newLst) = 
      case lst of
        [] => newLst
      | (Arr(name,width,dimList,size)::xs) => 
          if (name=id) 
          then newLst@[Arr(name,width,dimList,sz)]@xs
          else iterEntries(id,xs,newLst@[Arr(name,width,dimList,size)])
  in
    (reservedList:=iterEntries(a,tempList,[]))
  end

fun reserve(a) = (reservedList := a::(!reservedList)(*;printLst(!reservedList)*))

fun release(a,newList,lst) =
  let
    fun getName(arr) = 
      case arr of
        Arr(name,width,dimList,size) => name
  in
    case lst of
      [] => (reservedList := newList;printLst(!reservedList))
    | x::xs => if (a=getName(x)) then release(a,newList,xs) 
               else release(a,x::newList,xs) 
  end 

end