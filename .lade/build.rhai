// build script for lade-package-manager

system("cargo", ["build", "--release"]);

let path = path("./target/release/mltalk");

if path.exists{
  move_file(path.to_string(), ".");
}
