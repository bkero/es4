class Object {}

class Cat { 
  var lives : int = 9;
  var name : string;
  
  function Cat(name : string = "mao", lives : int = 9) : 
    name = name, lives = lives 
  {}

  function die(n : int = 1) : int { 
    lives -= n;
    if (lives <= 0) {
      intrinsic::print(name + "'s last life expired!\n");
    } else {
      intrinsic::print(name + " died " + n + " times, only " + lives + " left\n");
    }
    return lives;
  }
}

var fluffy = new Cat("fluffy");
var lucky = new Cat("lucky", 22);

intrinsic::assert(fluffy.die() == 8);
intrinsic::assert(lucky.die(2) == 20);
intrinsic::assert(lucky.die(4) == 16);
intrinsic::assert(lucky.die(16) == 0);
