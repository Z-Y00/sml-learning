functor MkBoruvkaSegmenter
  (structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Rand.Seq)
  : SEGMENTER =
struct
  structure Seq = Rand.Seq
  open Seq
  structure R = Rand
  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight
  type hide_edge =  (vertex * (vertex * (vertex * vertex * int))) seq
(* 
 we use the  Borůvka’s Algorithm in the mst
 thus the work and span is the same 
 
 we will use the reduce op+ 0 to find the edges_sum
 and then will use mapper to get the minus
 and fimally use the filter to check*)

 fun cmp_weight ((u1, v1, weight1), (u2, v2, weight2)) = Int.compare (weight2, weight1)
  fun contract_checker (rand_seq,(u, (v, edge))) = (nth rand_seq u = 0) andalso (nth rand_seq v = 1)
  
  
  fun findSegments (E : edge seq, n : int) initial_credit =
    let
      val sorted_edges = Seq.sort cmp_weight  E
      val init_edges  = Seq.map (fn (u, v, w) => (u, (v, w))) sorted_edges
      val init_vertex_list = tabulate (fn i => i) n 
      val init_credit_list = tabulate (fn _ => initial_credit  ) n
      (*the function was based on the previous one and the credit_list is added at the end*)
      fun boruvka_ (vertex_list, edges, Seed, credit_list) = 
       if (length edges = 0) 
            then 
            vertex_list
            
            else
        let
          val rand_seq = Rand.flip Seed n 
          fun nth_c foo = nth credit_list foo
             (* a function used to find the nth in credit_list*)
          val index_edges = (enum (inject edges (tabulate (fn _ => (~1, ~1)) n))) 
          val min_edges = filter (fn ( (v,(u,w ))) => u >= 0) index_edges

          val tocontract = filter (fn edge => contract_checker(rand_seq,edge)) min_edges  
   
         
          val temp_ = ( inject (map (fn (u, (v, _)) => (u, v)) tocontract) vertex_list)
          val updated_vertex_list = map  (fn v => nth temp_ v) temp_
          
          (*here we remove the edges that are too small, after the original boruvka_ process*)
          val rev_count = collect Int.compare (map (fn (v,(u,edge)) => (u,(v,edge))) tocontract)
          val edges_sum = map (fn (v, u_weight) => (v, reduce op+ 0 (map (fn (_,w)=>w) u_weight))) rev_count
          (*this is the sum of the weight of edges*)
          val min_credits = map (fn (v, u_weight) => (v, (Int.min(nth_c v, reduce Int.min initial_credit (map (fn (s,w)=>nth_c s) u_weight))))) rev_count
          val min_cred = inject min_credits credit_list
          fun mapper (v,s)=(v,nth min_cred v - s) 
          (*this fun is used to minus*)
          val updated_credit_list = inject (map mapper edges_sum) min_cred 
          fun nth_uc foo = nth updated_credit_list foo
          fun nth_uv foo = nth updated_vertex_list foo
          val new_edges = map (fn (u, (v, old_edge)) => ((nth_uv u), (nth_uv v, old_edge))) edges 
          val updated_edges = filter (fn (u, (v, w)) => u <> v andalso w < (nth_uc u)andalso w < (nth_uc v)) new_edges
            (* the filter will check if the  min(Cu, Cv )− Wuv ≥ 0,*)
        in
            boruvka_ (updated_vertex_list, updated_edges, Rand.next Seed, updated_credit_list)
        end
 
    in
    boruvka_ (init_vertex_list, init_edges,  Rand.fromInt 1024,init_credit_list )
    end 
  
    
    

end



(*

task 6.1
because of the Borůvka’s which shows that
the min out of every vertex will be included in to the final MST
then the 2nd smallest one must also be the smallest one out of a
vertex, this is because that, the smallest edge is the smallest out of 
the two the edge linked, since the |E|>2 then, there must be at least more 
edges, to let the 2nd smallest be the smallest out edge of a vertex.


task 6.2
for a house that got a x, the possibility for 
it is f(x)=(x)^2 , then for all n houses,
E=n*F(x) , F(x)=1/3 * x^3 . thus the final answer is 
n/3

task 6.3
z?

*)








