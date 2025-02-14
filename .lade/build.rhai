// build script for lade-package-manager

system("cargo", ["build", "--release"])

system("mv", ["./target/release/mltalk", "."])