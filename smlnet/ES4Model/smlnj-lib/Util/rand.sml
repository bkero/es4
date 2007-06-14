(* rand.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details
 * COPYRIGHT (c) 1998 by AT&T Laboratories.  See COPYRIGHT file for details
 *
 * Random number generator taken from Paulson, pp. 170-171.
 * Recommended by Stephen K. Park and Keith W. Miller, 
 * Random number generators: good ones are hard to find,
 * CACM 31 (1988), 1192-1201
 * Updated to include the new preferred multiplier of 48271
 * CACM 36 (1993), 105-110
 * Updated to use on Word31.
 *
 * Note: The Random structure provides a better generator.
 *)

structure Rand : RAND =
  struct

    type rand = Word31.word
    type rand' = Int32.int  (* internal representation *)

    val a : rand' = 48271
    val m : rand' = 2147483647  (* 2^31 - 1 *)
    val m_1 = m - 1
    val q = m div a
    val r = m mod a

    val extToInt = Int32.fromLarge o Word31.toLargeInt
    val intToExt = Word31.fromLargeInt o Int32.toLarge

    val randMin : rand = 0w1
    val randMax : rand = intToExt m_1

    fun chk 0w0 = 1
      | chk 0wx7fffffff = m_1
      | chk seed = extToInt seed

    fun random' seed = let 
          val hi = seed div q
          val lo = seed mod q
          val test = a * lo - r * hi
          in
            if test > 0 then test else test + m
          end

    val random = intToExt o random' o chk

    fun mkRandom seed = let
          val seed = ref (chk seed)
          in
            fn () => (seed := random' (!seed); intToExt (!seed))
          end

    val real_m = Real.fromLargeInt (Int32.toLarge m)
    fun norm s = (Real.fromLargeInt (Word31.toLargeInt s)) / real_m

    fun range (i,j) = 
          if j < i 
            then LibBase.failure{module="Random",func="range",msg="hi < lo"}
          else if j = i then fn _ => i
          else let 
            val R = Int32.fromInt j - Int32.fromInt i
            val cvt = Word31.toIntX o Word31.fromLargeInt o Int32.toLarge
            in
              if R = m then Word31.toIntX
              else fn s => i + cvt ((extToInt s) mod (R+1))
            end

  end (* Rand *)

