RSCRIPT		?= Rscript
TARGET		= tadaridaC

TEST_TADS_DIR	= tests/tads

all:

init:
	$(RSCRIPT) init.r

clean:
	rm -f tadaridaC_src/*.learner

test: all
	./$(TARGET) $(TEST_TADS_DIR)
