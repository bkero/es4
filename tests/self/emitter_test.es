package es4
{
    function testHelloWorld() {
        var emitter = new ABCEmitter;

        var cp = emitter.constants;
        var ini = emitter.newScript().init;

        var nm = cp.QName(cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8("")), 
                          cp.stringUtf8("print"));

        ini.I_findpropstrict(nm);
        ini.I_pushstring(cp.stringUtf8("Hello, world!"));
        ini.I_callpropvoid(nm);
        ini.I_returnvoid();

        loadAndRunABCFile(emitter.abcfile);
    }
}
