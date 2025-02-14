printf "Welcome to the \e[1;34mMLtalk\e[0m programming language!\n"
printf "[\e[1;34minfo\e[0m] cloning repository from GitHub\n"
git clone https://github.com/KajizukaTaichi/MLtalk.git
printf "[\e[1;34minfo\e[0m] Building then add to the PATH\n"
cargo install --path ./Mltalk/repl
printf "[\e[1;34minfo\e[0m] removing build directory\n"
rm -rf ./Mltalk
printf "[\e[1;34minfo\e[0m] installing is finished successfully\n"
