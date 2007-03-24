namespace English;

class C {
    English function hello() { return "hello!"; }
    function hello() { return "hei på deg!"; }
}

(new C).hello() + "/" + (new C).English::hello();
