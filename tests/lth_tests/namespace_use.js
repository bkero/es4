namespace English;
namespace French;

class C {
    French function hello() { return "bonsoir, mademoiselle!"; }
    English function hello() { return "hello!"; }
    function hello() { return "hei p� deg!"; }
}

{
    use namespace French;

    (new C).hello();
}