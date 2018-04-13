functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq
  open ASP

  (* Remove the following two lines when you're done! *)
  exception NotYetImplemented
  type nyi = unit

  (* You must define the following type *)
  type thesaurus = graph
(* thesaurus 可以作为一张图来存,边表示两点之间是同义词*)
fun ziiiip (word, seq) = Seq.map (fn seq => (word, seq)) seq
fun make (S : (string * string seq) seq) : thesaurus =
    let
 (*实现思路
 给的是 string *string seq , makegraph 函数接收 vertex * vertex
,所以进行转换 首先用我们自定义的ziiiiip 然后再使用append*)
      val converted_first= (Seq.map ziiiip S)
    in
        makeGraph (Seq.reduce Seq.append (Seq.empty()) converted_first)
    end

fun numWords (T : thesaurus) : int = numVertices T

fun synonyms (T : thesaurus) (w : string) : string seq = outNeighbors T w

fun query (T : thesaurus) (w1 : string) (w2 : string) =
    let
(*    实现思路
    首先调用刚才实现的makeasp来生成一个asp
    然后再调用report*)
      val asp = makeASP T w1
      val result = report asp w2
    in
      result
    end
end
