#[derive(Debug)]
enum Cereal {
    Barley,
    Corn,
    Millet,
    Rice,
    Wheat,
}




fn main() {
    let mut grains: Vec<Cereal> = vec![];
    grains.push(Cereal::Barley);
    grains.push(Cereal::Barley);
    grains.push(Cereal::Barley);
    grains.push(Cereal::Barley);
    grains.push(Cereal::Barley);

    drop(grains);

    println!("{:?}", grains);
    
}
