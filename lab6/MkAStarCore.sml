functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq
  exception See

  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real


(*  task 5.1
  A------10------B
   \            /
    \          /
     2        -9
      \      /
       \    /
        \  /
         C
  if we use it to find the shortest path from A to C
  then it will ignore the A-B-C. it think the A-B is larger than the A-C
  it failed to find the B-C is actually a minus weight
  *)
(*  task 5.2
  we can use  The Bellman Ford Algorithm

  Like Dijkstra's Algorithm, Bellman–Ford is based on relaxation,
  an approximation to the correct distance is gradually replaced by more
  accurate values until eventually reaching the optimum one. In both algorithms,
  the approximate distance to each vertex is always an overestimate of the true
  distance, and is replaced by the minimum of its old value and the length of a
  newly found path. However, Dijkstra's algorithm uses a priority queue to greedily
  select the closest vertex that has not yet been processed, and performs this
  relaxation process on all of its outgoing edges; by contrast,
  the Bellman–Ford algorithm simply relaxes all the edges,
  *)
  (*
  task 5.3
  because in Euclidean space the shortest path between two
  points is just go straight there, with no middle points.
  so it is admissible for, nothing can be shorter, and
  also is consistent

  task 5.4
  if this is a random sequence or the heuristic just give us
  the priority queue the same as Dijkstra’s default Algorithm

  task 5.5

       B(5)
      / \
     /   \
    1     4
   /       \
  A         D(1)
   \       /
    10     11
     \   /
      \ /
       C

  find the mini between A and C.
  assuming that, at first step, the heuristic give B->5,D->1
  this is admissible, the searching will go from B to D then C
  it will fail on the first attempt.

  task 5.6

       B(9999)
      / \
     /   \
    1     9
   /       \
  A --20---  D
   \       /
    2     11
     \   /
      \ /
       C(11)

    heuristic said B was 9999, it is too large,
    and the search will go to C and give A-C-D.
    the search will be wrong

  *)







  type graph = weight Table.table Table.table
 (* I choose this, for the help file dijkstra use it :) *)

  fun converter (u : vertex, v : vertex, w : real) = (u, (v, w))
  fun makeGraph (E : edge Seq.seq) : graph =
    let
(*  实现思路：
   首先有一个converter函数 ，把(u, (v, w))转换出来
   然后再进行collect 最后进行一个able.map Table.fromSeq
   整体上成为一个外层table是每个点 内曾table是里面的邻借表*)

        val edges =  Seq.map converter E
        val edges_table = Table.collect edges
        val result : graph = Table.map Table.fromSeq edges_table
    in
        result
    end


  fun findPath h G (S, T) =
    let
        fun N(v) =
            case Table.find G v
              of NONE => Table.empty ()
               | SOME nbr => nbr

(*D: table that stores final results*)
(*Q: tmp results in pq*)
        fun Dijkstra D Q =
            case PQ.deleteMin Q
              of (NONE, _) => D
               | (SOME (d, v), Q') =>
                 case Table.find D v
                   of SOME _ => Dijkstra D Q'
                    | NONE =>
                      let
                        fun myinsert d = Table.insert (fn (foo, _) => foo) d
                        (*actually we don't need this one*)
                        val D' = myinsert (v, d - (h v)) D
                        fun relax (q, (u, w)) = PQ.insert (d + w + ((h u) - (h v)), u) q
                        val Q'' = Table.iter relax Q' (N v)
                      in
                        if (Set.find T v) then
                           D'
                        else
                           Dijkstra D' Q''
                      end
(*        假设S都是一个源点～1 出来的，
我们从～1 出发 做一个搜索，然后就直接把S当成第一次pq
然后出发寻找*)
        fun heuristic_ foo = ((h foo), foo)
        val priority_quene_ = PQ.fromList (Seq.toList (Seq.map heuristic_ (Set.toSeq S)))
        val result : real Table.table = Dijkstra (Table.empty ()) priority_quene_
        val final_result : (Table.key * real) Table.seq = Table.toSeq (Table.extract (result, T))
      in
        if (Seq.length final_result = 0) then
            NONE
        else
            SOME (Seq.nth final_result 0)
      end

end
