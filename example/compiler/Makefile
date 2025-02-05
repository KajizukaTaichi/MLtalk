COM = mltalk compiler.lm
ASM = $(SRC:.lm=.ll)
OBJ = $(SRC:.lm=.bc)
EXE = $(SRC:.lm=)
SRC = test.ml

all: $(EXE)

$(ASM): $(SRC)
	$(COM) $(SRC) > $(ASM)
$(OBJ): $(ASM)
	llvm-as $(ASM) -o $(OBJ)
$(EXE): $(OBJ)
	clang $(OBJ) -o $(EXE)

clean:
	rm -f $(ASM) $(OBJ) $(EXE)

.PHONY: all clean run
