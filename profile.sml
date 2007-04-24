CM.make "$smlnj-tdp/plugins.cm";
Coverage.install ();
CM.make "es4.cm";
Boot.boot ();
Coverage.hot_spots [Coverage.non_tail_calls, Coverage.tail_calls, Coverage.functions] 30;
