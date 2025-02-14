// build script for lade-package-manager

info("Building MLtalk...");
system("cargo", ["build", "--release"]);

let path = path("./target/release/mltalk");
let path_i = path("./mltalk");

if path.exists(){
  system("sleep", ["1"]);
  move_file(path.to_string(), path_i.to_string());
}else{
  err(path.to_string() + " is not found!");
}
