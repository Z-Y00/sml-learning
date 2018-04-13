 functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  type vertex = key
  type edge = vertex * vertex

  type graph = ((Set.set) table)*int*int*((vertex seq) table)*((vertex seq) table)

  type asp = (vertex seq table)
  (* asp 用来存储每一个 vertex 的 parents edges*)

  fun swap (a,b)=(b,a)
  fun makeGraph (edges : edge seq ): graph =
    let
      val collected_sets = Table.map (Set.fromSeq) (Table.collect edges)
      val edges_number = (Seq.length edges)
      val out_table = Table.collect edges
      val in_table = Table.collect (Seq.map swap edges)
      val all_vertex = Set.union (Table.domain out_table, Table.domain in_table)
      val real_vertex_number =  Set.size all_vertex
    in
      (collected_sets,edges_number,real_vertex_number,out_table,in_table)
    end

fun numEdges (G : graph) : int = #2(G)

fun numVertices (G : graph) : int = #3(G)

fun outNeighbors (G : graph) (v : vertex) : vertex seq =
       case (find (#1(G)) v) of
             NONE => Seq.empty()
           | SOME ver_set => Set.toSeq  ver_set

 fun quick_outNeighbors (Graph: graph,v: vertex) =
        case find (#4(Graph)) v of
            NONE => Seq.empty()
           | SOME ver_seq => ver_seq


fun makeASP (G : graph) (v : vertex) : asp =
let
(*复杂度分析
因为我们就是走了一个bfs，
对于这个bfs的每个边，只会走两次，所以work就可以保证到 O(|E| log |V |)
同样，span也是满足O(D log2 |V |)
实现思路，
每个点去找它asp上的parents，每次bfs继续走的时候从froniter向外
然后把向外的两边中，除去相回走的那一个frontier，继续向前走一步，
并用map。把frontier里的点来map成从指向的edge。
当 F 为0时,结束。
*)
     fun BFS (parent_edge) (frontier : vertex seq) (Graph:graph) : asp=
     if (Seq.length frontier) = 0 then
       parent_edge
    else
     let
      val p_edges = Table.collect (Seq.flatten ( Seq.map (fn v => (Seq.map (fn u => (u, v)) (quick_outNeighbors (Graph,v)))) frontier))
      val updated_p_edge= Table.merge (fn (a, b) => a) (parent_edge, p_edges)
      val updated_frontier= Set.toSeq (Table.domain (Table.erase (p_edges, Table.domain parent_edge)))
     in
        BFS updated_p_edge updated_frontier Graph
   end
in
 BFS (Table.singleton (v, Seq.empty ())) (Seq.singleton v) G
end



 fun report (A : asp) (v : vertex) : vertex seq seq =
 let
(* 实现思路，
从终点开始，倒着查找，每次把指向之前的一个parent的edge加上来
然后一直加到了出发点，就是所有的asp
复杂度分析
因为使用是list，所以concat操作的复杂度并没有那么大
整体的复杂度保持在 O(|P||L| log |V |) *)
   fun helper (v : vertex, asp:asp) : vertex list list =
     case Table.find (asp) v of
          NONE => []  (*这里就是一个空的list*)
        | SOME (parents) =>
          if (length parents) = 0 then
                [[v]]
          else
        List.map (fn path => [v]@path ) (List.concat (List.map (fn u => helper(u,asp)) (Seq.toList parents)))
 in
    Seq.fromList (List.map Seq.fromList (List.map List.rev (helper(v,A))))
 end

end
