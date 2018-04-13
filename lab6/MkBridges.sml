functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  open Seq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  type ugraph = vertex seq seq

(*  fun add_one (graph,(v,u)) =
       let
      val v_edge = append ((nth graph v), %[u])
      val v_edge_rev = append ((nth graph u), %[v])
       in
    inject (%[(v,v_edge),(u,v_edge_rev)]) graph
       end*)
  

(*  fun makeGraph (E : edge seq) : ugraph = 
     let 
    val empty_graph =  tabulate (fn _ => empty()) (2 * (length E))
    val real_graph = iter add_one empty_graph E
      in
    filter (fn v => (length v)>0) real_graph
      end*)
(*  实现思路
   this task require us to have O(|E|log|V|) work and O(log2|V|) span.
   and the vertexs are labeled from 0 to |V|-1, so we made a collect
   cauz, this is a undirected graph, I added the reverse edges into it
   and use a collect to make it into a vertex seq seq
   we need it as a seq, because the vertes are labeled and it is easier to 
   search it later*)

    fun makeGraph (E : edge seq) : ugraph =
    let
      val edges_rev = Seq.map (fn (a, b) => (b, a)) E 
      val all_edges = Seq.append (E, edges_rev)
      val collected_vertexs = Seq.collect Int.compare all_edges
    in
      Seq.map (fn (_, vertexs) => vertexs) collected_vertexs
    end
    
      

      
  (*  实现思路
   tarjan method
   for this problem, I implemented it like the tarjan method,
   there is no need to maintain the low_list, because when the iter return
   we'll be able to know the actual low of the actual sum, we only need to 
   compare it with the dfn, and then inject it into the bridges we got
   we will start at the vertex ~1,this is because we need to deal with 
   undirected graph
   the bridges is a STSeq, but the list is a seq, I should have use STSeq both
   but I made both seq at the begining, and just too busy to change it to STSeq
   but, the dfn_list will only be written once, thus it won't affect the total *)
     
  fun findBridges (G : ugraph) : edges = 
  
  if (length G)=0 then 
   empty()
   
  else
  
    let
      val lowest =  (length G) * (length G) 
      val init_dfn_list = tabulate (fn _=> NONE) (length G)
      val init_bridge = STSeq.fromSeq (tabulate (fn _=>empty()) (length G))

     
(*    here dfn_list is the first time we visit it and write it , then we will tag it when found again*)
(*fun revisit (revisit_list : int option seq,v)=
if isSome(nth revisit_list v) then revisit_list
else

fun check (revisit_list : int option seq,v) = 
if isSome(nth revisit_list v) then TRUE else FALSE*)

fun DFS (parent : vertex) ((dfn_list : int option seq, min : int, time_tag : int, bridges  (*,revisit_list : int option seq*)), v : vertex) =
        case nth dfn_list v of
          SOME dfn => (dfn_list,Int.min (dfn, min), time_tag, bridges(*,revisit(revisit_list,v) *))
          |NONE =>
            let
              val to_visit = filter (fn u => u <> parent (*andalso check(revisit_list,u) *)) (nth G v)
              val new_dfn_list = inject (%[(v, SOME time_tag)]) dfn_list
              val (_, min', _, bridges_found_by_iter(*,updated_revisit_list*)) = iter (DFS v) (new_dfn_list, lowest, time_tag+1, bridges(*,revisit_list : int option seq*)) to_visit
     (*         val new_bridge =  if parent <> ~1 andalso min' >= time_tag 
                                then singleton (parent,v) else empty()*)
     (*         val placeholder = if parent <> ~1 andalso min' >= time_tag 
                                then SOME v else NONE
              val updated_bridges = inject  (%[(parent, SOME placeholder)]) bridges_found_by_iter*)

             val updated_bridges = if  ((*parent <> ~1 andalso *)min' >= time_tag) then (STSeq.update (v,singleton((parent,v))) bridges_found_by_iter) else bridges_found_by_iter
              val new_min = Int.min (min', min)
            in
              (new_dfn_list, new_min, time_tag+1, updated_bridges(*,updated_revisit_list*))
            end
      val bridges = iter (DFS ~1) (init_dfn_list, lowest, 0, init_bridge) (tabulate (fn i => i) (length G))
      val bridges_middle_result = flatten(STSeq.toSeq (#4 bridges))
      (*this still have (~1,_) and we need to drop 1*)
      val final = filter (fn (a,_) => a <> ~1 ) (drop(bridges_middle_result,1))
      
    in
       final
    end

end
