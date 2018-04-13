functor MkBabble(structure R : RANDOM210
                 structure KS : KGRAM_STATS
                 structure Util : SEQUENCE_UTIL
                 sharing KS.Seq = Util.Seq
                 sharing KS.Seq = R.Seq) : BABBLE =
struct
  structure Rand = R
  structure Stats = KS
  open Stats.Seq

  exception NoData
  (*实现思路是
  首先 实现一个子函数 该子函数会接收已有的seq 和 seed 然后产生一个新的随机c 加上去
  然后 产生一个随机数序列 最后我们做一个iter的操作 子函数接收这个iter的seq 产生c 然后c就一个个append上seq*)

  fun randomSentence (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    let
      val random_seq = Rand.randomRealSeq seed NONE n
      val k = Stats.maxK stats
      fun babble_one_more seq r =singleton (Util.choose ( Stats.lookupExts stats (drop (seq, Int.max (0, (length seq) - k)))) r) (*this is used to add one more babble*)
      (*changlog 2018 2 27 we will add a complex babble_one_more to deal with not found*)(*
      fun wrap_babble_one_more_ seq r  =
      let val first = babble_one_more() in
      if length(first) then
        first
        else
        wrap_babble_one_more_ (drop(seq,1), r)*)

      val strings=(iter (fn (seq, r) => (append (seq,babble_one_more seq r ))) (empty ()) random_seq)
      (*复杂度分析 这一步里面用到了append 但是这里的append的时间复杂度不会影响到整体的复杂度
         对于work来说 每次都是O(n) 于此同时 choose的work和它相同 所以我们可以认为这个不会在大O下影响整体的复杂度*)
    in
      (String.concatWith " " (toList strings)) ^ "."
    end



  fun randomDocument (stats : KS.kgramstats) (n : int) (seed : R.rand) =
   let
     val seeds = map Rand.fromInt (Rand.randomIntSeq seed NONE n)
     val seed_for_length = Rand.next (nth seeds ((length seeds) - 1))
     (*  实现思路是
  同样是一串seed 然后随机生成一个seed用于length
  然后我们把这两串做zip，然后就会成为一个(length, seed)的seq
  最后我们一个map 借助刚才实现好的randomsequence 来输出一系列的doc
  复杂度分析因为我们使用map来产生sequece，然后再把所有的seq来和，所以复杂度满足要求*)
     val length = Rand.randomIntSeq seed_for_length (SOME (5, 11)) n
     val seed_for_doc = zip length seeds
     val strings = (map (fn (length, seed) => randomSentence stats length seed) seed_for_doc)
   in
    String.concatWith " " (toList strings)
   end

end
