package abcfile
{
    import util.*;
    import assembler.*;
    import bytestream.*;

    public function runTests() {
        constantPoolCoverage();
        abcFile();
    }

    function constantPoolCoverage() {
        print("--------------------------------------------");
        print("Testing ABCConstantPool");
        print("");
            
        var cp = new ABCConstantPool;

        // Sharing working OK?
        var a = cp.int32(37);
        var b = cp.int32(37);
        assert( a == b );

        var a = cp.uint32(37);
        var b = cp.uint32(37);
        assert( a == b );

        var a = cp.float64(1.0);
        var b = cp.float64(1.0);
        assert( a == b );

        var a = cp.stringUtf8("foo");
        var b = cp.stringUtf8("foo");
        var s = a;
        assert( a == b );
        var k = cp.stringUtf8("x");

        var a = cp.namespace(CONSTANT_PackageInternalNS, k);
        var b = cp.namespace(CONSTANT_PackageInternalNS, k);
        assert( a == b );
        cp.namespace(CONSTANT_ProtectedNamespace, s);
        var c = cp.namespace(CONSTANT_ProtectedNamespace, k);

        var nsa = a;
        var nsc = c;

        var nss = cp.namespaceset([nsa,nsc]);

        cp.QName(nsa, s);
        cp.QName(nsa, s, true);

        cp.RTQName(s);
        cp.RTQName(s, true);

        cp.RTQNameL();
        cp.RTQName(true);

        cp.Multiname(nss, k);
        cp.Multiname(nss, k, true);

        cp.MultinameL(nss);
        cp.MultinameL(nss, true);

        var bytes = new ABCByteStream;
        cp.serialize(bytes);
        dumpByteStream( bytes );
    }

    function abcFile() {
        var f = new ABCFile;
        var cp = new ABCConstantPool;
        f.addConstants(cp);
        var m = f.addMethod(new ABCMethodInfo(cp.stringUtf8("foo"), 
                                               [cp.Multiname(cp.namespaceset([cp.namespace(CONSTANT_ProtectedNamespace, 
                                                                                            cp.stringUtf8("bar"))]),
                                                             cp.stringUtf8("baz"))],
                                               0,
                                               0,
                                               [],
                                               [cp.stringUtf8("x")]));

        f.addMetadata(new ABCMetadataInfo(cp.stringUtf8("meta"), 
                                          [{key: cp.stringUtf8("fnord"), value: cp.stringUtf8("foo")}]));
        var cl = new ABCClassInfo();
        var cli = f.addClass(cl);
        cl.setCInit(0);

        var ii = new ABCInstanceInfo(cp.stringUtf8("foo"),
                                     0,
                                     0,
                                     0,
                                     []);
        f.addInstance(ii);
        ii.setIInit(0);
        ii.addTrait(new ABCSlotTrait(cp.stringUtf8("x"), 0));
        ii.addTrait(new ABCOtherTrait(cp.stringUtf8("y"), 0, TRAIT_Class, 0, cli));

        var sc = new ABCScriptInfo;
        f.addScript(sc);
        sc.setInit(0);

        var mb = new ABCMethodBodyInfo(0);
        f.addMethodBody(mb);
        mb.setMaxStack(0);
        mb.setLocalCount(0);
        mb.setInitScopeDepth(0);
        mb.setMaxScopeDepth(0);
        mb.setCode({ "serialize": function (bs) {}, "length": 0 });
        
        f.getBytes();
    }
}
