let Dog = {
  class: #Dog,
  voice: "bark",
  name: null
};

let Dog.new name = Dog + { name };
let Dog.say this =
  print f"{this.name} the {type this} says \"{this.voice}, {this.voice}!\"\n";

let Cat = Dog + {
  class: #Cat,
  voice: "meow"
};

let Cat.new name = Cat + { name };
let Cat.punch this power =
  for i in 0 ~ power do
    print f"Thud! {this.name} sends out cat punch\n";

let pochi = Dog.new "Pochi";
pochi say;

let tama = Cat.new "Tama";
tama say; tama punch 3;
