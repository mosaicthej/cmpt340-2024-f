# Makefile for Scala project with shared class paths for source and test files

SCALAC=scalac
SCALA=scala
SRC_DIR=.
BUILD_DIR=./build
TEST_OUT_DIR=./test-out
SCALA_SOURCES= q1 q2 # q3
TEST_SOURCES= test_q1 test_q2 # test_q3 test_q3_v2

# Manual mapping of test sources to their fully qualified test class names
test_q1_CLASS=Q1Test.Q1Test
test_q2_CLASS=Q2Test.Q2Test

# Default target to compile everything and run tests
all: dirs compile compile_tests tests

dirs:
	mkdir -p $(TEST_OUT_DIR)
	$(foreach src, $(SCALA_SOURCES), mkdir -p $(BUILD_DIR)/$(src);)

compile: $(SCALA_SOURCES)

$(SCALA_SOURCES):
	$(SCALAC) -d $(BUILD_DIR)/$@/ $(SRC_DIR)/$@.scala

# Compile test files, to the same directory as the source
compile_tests: $(TEST_SOURCES)

$(TEST_SOURCES):
	$(eval SRC_PREFIX=$(lastword $(subst _, , $(basename $@))))
	$(SCALAC) -d $(BUILD_DIR)/$(SRC_PREFIX)/ \
		$(SRC_DIR)/$(SRC_PREFIX).scala $(SRC_DIR)/$@.scala

# run tests
tests: $(patsubst %, $(TEST_OUT_DIR)/%.txt, $(TEST_SOURCES))
	@echo "RUNNUING tests"

$(TEST_OUT_DIR)/%.txt: $(SRC_DIR)/%.scala compile_tests
	$(eval SRC_PREFIX=$(lastword $(subst _, , $(basename $<))))
	$(eval TEST_CLASS=$($(basename $<)_CLASS))
	$(SCALA) -cp $(BUILD_DIR)/$(SRC_PREFIX)/\
		$(TEST_CLASS) > $@


# Clean up
clean:
	rm -rf $(BUILD_DIR) $(TEST_OUT_DIR)

.PHONY: all dirs compile compile_tests tests clean
