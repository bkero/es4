// package 

class ListNode.<T> 
{
    function ListNode(value : T) {
        this.value = value;
    }

    var value : T;
    var next  : ListNode.<T>;
}

public interface Collection.<T> 
{
    function size() : Number;
    function add(item : T);
    function element(n : Number) : T;
}

public class LinkedList.<T> implements Collection.<T>
{
    public function size() : Number { 
        var n : Number = 0;
        for ( let p : ListNode.<T> = head ; p != null ; p = p.next )
            n++;
        return n;
    }

    public function add(item : T) {
        var x = new ListNode.<T>(item);
        if (head == null)
            head = x;
        else
            tail.next = x;
        tail = x;
    }

    public function element(n : Number) : T {
        for ( var p : ListNode.<T> = head ; n > 0 ; p = p.next, --n )
            ;
        return p.value;
    }

    var head : ListNode.<T> = null;
    var tail : ListNode.<T> = null;
}

var v : Collection.<Number> = new LinkedList.<Number>;
v.add(37);
v.element(0);
