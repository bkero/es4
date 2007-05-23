// RegExp smoketests

function test(re, input, len, val, idx) {
    print(re.toString());
    let res = re.exec( input );
    if (res == null && val != null)
        print("FAILED: match failed.  Wanted " + val );
    if (val == null && res != null)
        print("FAILED: match succeeded.  Wanted failure." );
    if( res.length != len )
        print("FAILED: wrong length.  Wanted " + len + ", got " + res.length );
    if (len == 1) {
        if (res[0] != val )
            print("FAILED: wrong value.  Wanted " + val + ", got " + res );
    }
    else {
        for ( let i=0 ; i < len ; i++ )
            if (res[i] != val[i] )
                print("FAILED at #" + i + ": wrong value.  Wanted " + val[i] + ", got " + res[i] );
    }
    if (res.index != idx )
        print("FAILED: wrong index.  Wanted " + idx + ", got " + res.index );
}

// Missing tests:
//   - other flags than 'g'
//   - ES4 facilities: comments, named captures, ...
//   - \u escapes (old and new kinds)
//   - most escape sequences
//   - error detection

// Sequence of single characters, matching the entire input
test( /abc/g, "abc", 1, "abc", 0 );

// Sequence of more complex terms, but same meaning 
test( /(?:a)(?:b)(?:c)/g, "abc", 1, "abc", 0 );

// Sequence of single characters, matching in the middle 
test( /abc/g, "1abc2", 1, "abc", 1 );

// Sequence of single characters, anchored at the beginning (a bit trivial) 
test( /^abc/g, "abcdefg", 1, "abc", 0 );

// Sequence of single characters, anchored at the end 
test( /abc$/g, "abcabc", 1, "abc", 3 );

// Matching with word boundaries 
test( /.\b./g, "abc-+", 1, "c-", 2 );

// Matching with word boundaries 
test( /.\B./g, "a*c*ab+", 1, "ab", 4 );

// Simple disjunction, matching the second disjunct 
test( /abc|cde/g, "abbcdef", 1, "cde", 3 );

// Character sets and the . wildcard (itself a charset) 
test( /[a-zA-Z0-9][abc][^f]./g, "+-*GafGa88", 1, "Ga88", 6 );

// Simple atom escape (really a charset) 
test( /\s/g, "ab cd", 1, " ", 2 );

// Matching with positive lookahead 
test( /a(?=bcd)b/g, "xabcde", 1, "ab", 1 );

// Matching with negative lookahead 
test( /a(?!bcd)b/g, "xabce", 1, "ab", 1 );

// Quantified matching: optional char, #1 
test( /ab?/g, "xac", 1, "a", 1 );

// Quantified matching: optional char, #2 
test( /ab?/g, "xabc", 1, "ab", 1 );

// Quantified matching: some optional chars #1
test( /ab{2,5}/g, "xabbbbbbbc", 1, "abbbbb", 1 );

// Quantified matching: some optional chars #2
test( /ab{2,}/g, "xabbbbbbbc", 1, "abbbbbbb", 1 );

// Quantified matching: some optional chars #3
test( /ab{2,}./g, "xabcabbbd", 1, "abbbd", 4 );

// Quantified matching: greedy optional chars #1
test( /ab*/g, "xabbbbbbbc", 1, "abbbbbbb", 1 );

// Quantified matching: greedy optional chars #2
test( /ab*/g, "xacbbbbbbbc", 1, "a", 1 );

// Lazy matching
test( /a(?:b|c)*?c/g, "abcbc", 1, "abc", 0 );

// Capturing #1
test( /a(b)c/, "01abc23", 2, ["abc", "b"], 2 );

// Capturing #2 (from E262-3)
test( /((a)|(ab))((c)|(bc))/g, "abc", 7, ["abc", "a", "a", undefined, "bc", undefined, "bc"], 0 );

// Backrefs
test( /a(b)c\1/, "01abcb23", 2, ["abcb", "b"], 2 );
