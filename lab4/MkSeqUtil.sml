functor MkSeqUtil(structure S : SEQUENCE) : SEQUENCE_UTIL =
struct
  structure Seq = S
  open Seq

  type 'a hist = ('a * int) seq

  fun tokens (cp : char -> bool) (str : string) : string seq =
    let
      val n = String.size str
      val chars = tabulate (fn i => (i, String.sub (str, i))) n
      val idx = map (fn (i,_) => i) (filter (fn (_,c) => cp c) chars)

      (* grab substrings in between delimiters *)
      val subs = map2 (fn (i,i') => String.substring (str, i, i' - i))
                      (append (singleton 0, map (fn i => i + 1) idx))
                      (append (idx, singleton n))
    in filter (fn s => size s > 0) subs
    end

  fun histogram (cmp : 'a ord) (s : 'a seq) : 'a hist =
    map (fn (a, c) => (a, length c))
        (collect cmp (map (fn a => (a, ())) s))

  fun sum_up  ((_, x), (c, y))  =  (c, x + y)
  fun choose (hist : 'a hist) (p : real) : 'a =
    if length hist = 0  orelse p > 1.0 orelse p < 0.0 then raise Range
    else let
      val sum = scani sum_up (#1 (nth hist 0), 0) hist (*add things up*)
      val real_sum= #2 (nth sum (length sum - 1))
      val bench_mark = Real.ceil (p * Real.fromInt real_sum )
      (* this is used to cal the benchmark and iter later*)
      (*实现思路是 先算出来一个bench_mark 然后再根据这个bench_mark来把整个hist搜索一遍 使用iter*)
      (*具体的方法是 先算出一个sum 然后使用p * Real.fromInt real_sum 来进行转换 求出我们要的值  进行比较*)
      val result= iter (fn ((c1, x), (c2, y)) => if x < bench_mark then (c2, y) else (c1, x)) (#1 (nth hist 0), ~1) sum
    in
      #1 result
    end

end
(*复杂度分析*)
