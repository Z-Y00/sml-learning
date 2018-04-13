functor MkTableKGramStats(structure Util : SEQUENCE_UTIL
                          structure T : TABLE
                            where type Key.t = string Util.Seq.seq
                          sharing T.Seq = Util.Seq) : KGRAM_STATS =
struct
  structure Table = T
  structure Seq = T.Seq
  open Util
  open Seq

  type token = string
  type kgram = token seq

  (* int is the maxK *)
  type kgramstats = (string hist Table.table) * int
(* the int here denote the maxK and the Table means keys of type string seq and values like hist*)
fun cmp (a, b) = collate String.compare (a, b)

  fun makeStats (corpus : string) (maxK : int) : kgramstats =
    let

   (* 实现思路 首先 我们实现一个子函数seq_of_length_k 这个函数接收一个常数k
    会根据常数k来把token 进行切割，而token就是从corpus里获取的一个语料
    然后 我们对seq_of_length_k进行一次tabulate 从而获得我们想要的
    最后再经过collect和map就是我们需要的kgramstats*)
    (* tokens cp s has O(|s|) work and O(log |s|) span,*)
      val token = tokens (not o Char.isAlphaNum) corpus

      fun seq_of_length_k length_k = tabulate (fn i => (subseq token (i, length_k), nth token (i + length_k))) (length token - length_k)
     (* this fun use tabulate and take O(1) span *)
      (*we get length k seq from the origin seq*)

      val key_value = collect cmp (flatten (tabulate seq_of_length_k (maxK + 1)))
      (*this will cost the longest span and work that is *)
      (*we use tabulate to find all length up to maxK*)
      val hists = map (fn (kgram, value) => (kgram, histogram String.compare value)) key_value
    in
      (Table.fromSeq hists,maxK)
    end


    (*work and span is find of table which is logn*)
  fun lookupExts (stats : kgramstats) (kgram : kgram) : (token * int) seq =
    if (#2 stats < length(kgram)) then
         empty () (*  or we just raise exception?*)
         (*实现思路 首先进行判断 查找是否合法 然后再使用Table。find查找*)
    else
      case Table.find (#1 stats) kgram of
      NONE => empty ()  (*it may be empty*)
      | SOME foo => foo

  fun maxK (stats : kgramstats) : int = #2 stats
(*  由于我们的kgramstats存储了 maxK,所以只需取出#2即可 所以 W=S=O(1)*)


end
