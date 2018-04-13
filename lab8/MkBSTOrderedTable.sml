functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  open Table

  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)



 fun first (T : 'a table) : (key * 'a) option =
  if size(T)= 0 then
      NONE
     (*first check if the T is empty*)
    else
       let
    fun first_core (T : 'a table) : (key * 'a) option =
        case Tree.expose T of
          NONE => NONE
          | SOME {key, value, left, right} =>
           case Tree.expose left of
                NONE => SOME (key, value)
               | _ => first_core left (*it will call itself to find the result*)
      in
(*      实现思路
      首先判断T是不是空的
      如果不是 进入核心部分
      核心部分的代码会不断自己调用自己 然后递归*)

        first_core (T)
      end


 fun last (T : 'a table) : (key * 'a) option =
   if size(T)= 0 then
      NONE
      (*first check if the T is empty*)
    else
     let
    fun last_core (T : 'a table) : (key * 'a) option =
        case Tree.expose T of
            NONE => NONE
            | SOME {key, value, left, right} =>
                case Tree.expose right of
                    NONE => SOME (key, value)
                    | _ => last_core right  (*it will call itself to find the result*)
        in
        (*      实现思路
      首先判断T是不是空的
      如果不是 进入核心部分
      核心部分的代码会不断自己调用自己 然后递归*)
          last_core (T)
        end



  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    let
        val left_tree = #1 (Tree.splitAt (T,k))
    in
(*    实现思路
    对tree,split进行一个简单的包装
    取出#3来*)
        last (left_tree)
    end

  fun next (T : 'a table) (k : key) : (key * 'a) option =
    let
         val right_tree = #3 (Tree.splitAt (T,k))
      in
      (*    实现思路
    对tree,split进行一个简单的包装
    取出#1来*)
           first (right_tree)
    end

  fun join (left : 'a table, right : 'a table) : 'a table =
     if (size(left)< size(right) ) then
         Tree.join (left, right)
     else
        Tree.join (right,left)

 (* we will just use the Tree.join to implement the join*)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table = Tree.splitAt (T, k)
  (*we will just use the Tree.split to implement the split*)

  (*first split the T in the high, the split the result in the low
  and the result is the range*)

     fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    let
(*    实现思路
    首先用low来做第一次split
    做完之后 根据结果 来生成一个新T
    然后用high对T进行第二次split*)
      val Tree1 = case Tree.splitAt(T, low) of
          (_, SOME value, right) => join(singleton(low, value), right)
         | (_, NONE, right) => right (*this will split it by low*)

      val Tree2 = case Tree.splitAt(Tree1, high) of
         (left, SOME value, _) => join(singleton(high, value),left)
         | (left, NONE, _) => left (*this will split it by high*)
    in
      Tree2
end

end
