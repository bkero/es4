/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */

{
    use namespace Debug;
    use namespace Ast;
    use namespace intrinsic;

    function decodeProgram (obj) 
        : PROGRAM
    {
        enter ("decodeProgram ");

        var nd1 = decodePackages (obj.packages);
        var nd2 = decodeHead (obj.head);
        var nd3 = decodeBlock (obj.block);

        exit ("decodeProgram ");
        return new Program (nd1,nd2,nd3);
    }

    function packages (obj) 
        : [PACKAGE]
    {
        enter ("decodeProgram ");

        var nd1 = decodePackages (obj.packages);
        var nd2 = decodeHead (obj.head);
        var nd3 = decodeBlock (obj.block);

        exit ("decodeProgram ");
        return new Program (nd1,nd2,nd3);
    }

    function block (ob) 
        : BLOCK
    {
        enter ("block");

        var nd1 = ob.head;
        var nd2 = statements (ob.stmts);

        exit ("block");
        return new Block (nd1,nd2);
    }

    function head (ob)
        : HEAD
    {
        enter ("decodeProgram ");

        var nd1 = decodePackages (ob.packages);
        var nd2 = ob.head;
        var nd3 = decodeBlock (ob.block);

        exit ("decodeProgram ");
        return new Program (nd1,nd2,nd3);
    }


}
