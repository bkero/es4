package es4
{
    public function testABCEmitter() {
        var e    = new ABCEmitter();
        var cp   = e.constants;
        var ini  = e.newScript().init;

        var nm = cp.QName(cp.namespace(CONSTANT_PackageNamespace, cp.stringUtf8("")), 
                          cp.stringUtf8("print"));

        ini.I_findpropstrict(nm);
        ini.I_pushstring(cp.stringUtf8("Hello, world!"));
        ini.I_callpropvoid(nm, 1);
        ini.I_returnvoid();

        loadAndRunABCFile(e.finalize());
    }
}
