let Dog = {
    class: #Dog,
    voice: "bark",
    name: null
};

let Dog.new name = Dog + { name };
effect let Dog.say this =
    print f"{this.name} the {type this} says \"{this.voice}, {this.voice}!\"\n";

let Cat = Dog + {
    class: #Cat,
    voice: "meow"
};

let Cat.new name = Cat + { name };
effect let Cat.punch this power =
    for i = 0 ~ power do
        print f"Thud! {this.name} sends out cat punch\n";

let pochi = Dog.new "Pochi";
effect pochi say;

let tama = Cat.new "Tama";
effect tama say; effect tama punch 3;
