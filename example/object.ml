let Dog = {
  class: #Dog,
  voice: "bark",
  name: null
};

let Dog.new name = Dog + { name };
let Dog.say this =
  print f"{this.name}: {this.voice}, {this.voice}!\n";

let pochi = Dog.new "Pochi";
pochi say;

let Cat = Dog + {
  class: #Cat,
  voice: "meow"
};

let Cat.new name = Cat + { name };
let Cat.punch this power =
  for i in  0 ~ power do
      print f"{this.name}: Thud!\n";

let tama = Cat.new "Tama";
tama say;
tama punch 10;
