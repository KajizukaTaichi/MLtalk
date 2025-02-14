// build script for lade-package-manager

info("Building MLtalk...");
system("cargo", ["build", "--release"]);

let path = path("./target/release/mltalk");

if path.exists(){
  move_file(path.to_string(), ".");
}else{
  err(path.to_string() + " is not found!");
}
