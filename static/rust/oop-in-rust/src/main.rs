trait Animal {
    fn make_sound(&self);
}

struct Dog {
    name: String,
}

impl Animal for Dog {
    fn make_sound(&self) {
        println!("{} says Woof!", self.name);
    }
}

struct Cat {
    name: String,
}

impl Animal for Cat {
    fn make_sound(&self) {
        println!("{} says Meow!", self.name);
    }
}

fn main() {
    let dog = Dog { name: String::from("Buddy") };
    let cat = Cat { name: String::from("Whiskers") };

    let animals: Vec<&dyn Animal> = vec![&dog, &cat];

    for animal in animals {
        animal.make_sound();
    }

    let s1 = String::from("   Hello, world!   ");
    println!("|{}|", s1);
    println!("|{}|", s1.trim().replace("world", "rust").to_uppercase());
}
