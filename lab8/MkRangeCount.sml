functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  val compareKey : (Key.t * Key.t) -> order = Key.compare


  type countTable = int table table


    
fun cmp_y (a :key*key ,b :key*key) = compareKey( #1 a,  #1 b )

(*
analyse of work and span 
the sort will take a work of O(nlogn) and span of O((logn)^2)
the work and span for each  fun insert (a,b) is O(logn)
we call it use iter which will take work of O(nlogn)
the other step's work and span will be O(n)
so the total work is O(nlogn) and satisfy the requirement


*)
fun cmpy (a :key*key ,b :key*key) = compareKey( #2 a,  #2 b )
fun makeCountTable (S : point seq) : countTable =
  if Seq.length S = 0 then empty()
  else
    let
    val S' = Seq.map (fn (a,b)=>(b,a)) S
    val sort_by_y = Seq.sort cmp_y S'
    val sort_by_y' = Seq.sort cmpy S
    val x_value = Seq.map (fn point => (#1 point, ~1)) sort_by_y'
      (* (_,~1) ~1 is just something to take up the place , we actually only need the x value *)
    fun insert (a,b) = OrdTable.insert (fn (_,x) => x) b a
    val values = Seq.iterh insert (empty()) x_value
  (*   iteratePrefixes
    iterh is a generalization of iter that also computes the sequence of all partial results produced by the iterated application of $f$. That is, 
    (iterh f b s) is logically equivalent to

    (tabulate (fn i => iter f b (take (s, i))) (length s), iter f b s) 

    
Note that the first element in the tuple does not include the application of $f$ on the last element of $s$, but instead has $b$ as its first element*)
    val final_values = Seq.append (Seq.drop (#1 values, 1), Seq.singleton(#2 values))
 (*   the (#2) at the end, is all the imput x_value*)
    
    val final_all = Seq.zip (Seq.map #1 sort_by_y) final_values
   (*  the first one is the y value and the second one is the x value 
      and first it is a table by y value, and the values is got by iterh, which produce the
      iteratePrefixes, and each is an ordered table made of the x value got so far
      *) 
    val result = OrdTable.fromSeq final_all
 in
  result
  end
  (*
task 5.2
we can replace the iterh with the scani
and the work will also be lower
the span will then be  O((logn)^2)
and the work will be O(nlogn)

 task 5.3
 the space is O(n^2)
  for the node at the last y will be as large as n
 then it will take a total cost of n*n=n^2


task 5.4
work and span for the count,
first we need to getRange by the y value ,
this will take logn, then we getRange by the x value
this will also take us logn,
we use the high-low,to get the correct number between the x_left and x_right

so the total span and work is logn
*)

(*  
changlog
2018 2 26 : there is a bug about the points on the rectangle*)

(*fun count (T : countTable) ((x_left, y_high) : point, (x_right, y_low) : point) : int  =
      let 
        val select_by_y = getRange T (y_low, y_high)
      in
            if size T = 0 then 
                0
            else if   (size select_by_y = 0)  then
                0
            else
            let 
                val high = size (getRange (#2 (valOf(last select_by_y))) (x_left, x_right))
                val low = size (getRange (#2 (valOf(first select_by_y))) (x_left, x_right))
            in 
            if high = 0 andalso low = 0 then
              0
            else
              high - low 
            end 
        end*)
fun count (T : countTable) ((x_left, y_high) : point, (x_right, y_low) : point) : int  =
    if size T = 0 then 
       0 
    else let
     (*first we change the boundary to the previous and then get a new T by the y*)
     
      val select_by_y = if isSome(previous T y_low) then 
                    #2 (valOf(previous T y_low)) 
                  else 
                    empty()
                    
      val select_by_y' = if isSome ((#2 (split (T, y_high)))) then  (*if there is points on the y_high and the split is successful *)
                                valOf(#2 (split (T, y_high)))
                         else 
                         #2 (valOf(previous T y_high))
          
     (* then we getRange it by x*)
      val l = size(getRange select_by_y (x_left, x_right))
      val r = size(getRange select_by_y' (x_left, x_right))
    in
       r - l
    end
        
        
        
        
end


