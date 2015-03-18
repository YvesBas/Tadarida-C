RSCRIPT		?= Rscript
TARGET		= tadaridaC

TEST_TADS	= tests/tads/*

all:$(TARGET)

init:
	$(RSCRIPT) init.r

clean:
	rm -f tadaridaC_src/*.learner

test: all $(TEST_TADS)
	for i in $(TEST_TADS); do ./$(TARGET) $$i || exit $?; done
