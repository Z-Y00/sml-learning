functor MkBoruvkaMST (structure Seq : SEQUENCE
                      structure Rand : RANDOM210
                      sharing Seq = Rand.Seq) : MST =
struct
  structure Seq = Rand.Seq
  open Seq

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight
  type hide_edge =  (vertex * (vertex * (vertex * vertex * int))) seq


(*  as we did in previous lab, we know the n, then we build a vertex_list first.
  we use the enum to find the smallest one.
  and then we inject to update the vertex_list and the edges
  the contracted vertex will be maped to where it is contracted*)
 (* work and span is that of  Borůvka’s Algorithm ,
      we will contract the smallest one out of each vertex 
      so we will have expected O(mlogn+n) work and span is logn

就是 维护一个vertexs的list 
先map (fn a=> a ) vertexs
然后如果a收缩到了b
就把a的值 改成b

每一次收缩的work要保持在m
一共logn次
然后整体才可以*)
  
  
  
  fun cmp_weight ((u1, v1, weight1), (u2, v2, weight2)) = Int.compare (weight2, weight1)
 (* here we sorted it in the revers way*)
  fun contract_checker (rand_seq,(u, (v, edge))) = (nth rand_seq u = 0) andalso (nth rand_seq v = 1)
  
  
  fun MST (E : edge seq, n : int) : edge seq =
    let
      val sorted_edges = Seq.sort cmp_weight  E
(*sort in the revers way!!!!*)
      val init_edges : hide_edge  = Seq.map (fn (u, v, w) => (u, (v, (u, v, w)))) sorted_edges
(* we map it to hide the origin information, in case it got lost will contract*)
      val init_vertex_list = tabulate (fn i => i) n 
(* this is a vertex list, when two vertexs get contracted, the other's value on the list
      will be changed to the one it got contracted*)
      fun boruvka (vertex_list, edges, founded_edges, Seed) = 
     if (length edges = 0)  then 
        founded_edges 
        
      else 
        let
          val rand_seq = Rand.flip Seed n 
          
          
  (* fun enum s =
      let
        fun addIdx (_, []) = []
          | addIdx (i, x::xs) = (i,x)::addIdx (i+1, xs)
      in addIdx (0, s)
      end*)
          val index_edges = (enum (inject edges (tabulate (fn _ => (~1, (~1, ~1, 10000))) n))) 
          val min_edges = filter (fn ( (v,(u,old_edge :edge ))) => u <> ~1) index_edges
(*          this is a awesome way to implement the minE,
          when you inject it, you are injecting (v,u,edge) to the location of v (as the (u,edge))
          then you enum it, the v is add back!
          then you just check the u, and see if it is ~1,
          for we sorted it in the reverse way,
          and the final value is the smallest one
          thus,  this is just the min for each v!*)
          
          val tocontract = filter (fn edge => contract_checker(rand_seq,edge)) min_edges  
          val updated_vertex_list = inject (map (fn (u, (v, _)) => (u, v)) tocontract) vertex_list
          val updated_founded_edges = append ((map (fn (_, (_, old_edge)) => old_edge) tocontract), founded_edges)
          fun nth_v u = (nth updated_vertex_list u)
          val new_edges = map (fn (u, (v, old_edge)) => ((nth_v u), (nth_v v, old_edge))) edges 
          val updated_edges = filter (fn (u, (v, _)) => u <> v) new_edges
        in
            boruvka (updated_vertex_list, updated_edges, updated_founded_edges, Rand.next Seed)
        end
    in
      boruvka (init_vertex_list, init_edges, empty(), Rand.fromInt 8102)
    end 
  
    
    
end
