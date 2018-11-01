include ../../Makefile.inc

SRC = Diff.hs General.hs Main.hs Report.hs Run.hs

TARGET = tester
BASE=../../inst

TEST_PATH=../../tests/conformance98

all: $(TARGET) tests

$(TARGET): $(SRC)
	$(GHC) --make Main.hs -o $(TARGET)


TEST_CMD = YHC_BASE_PATH=$(BASE); export YHC_BASE_PATH; \
	PATH=$(BASE)/bin:$$PATH; export PATH; \
	./$(TARGET) -report -log $(TEST_PATH); \

tests: $(TARGET)
	$(TEST_CMD)

testlogs: $(TARGET)
	cp $(LOGDIR)/report.log $(TEST_PATH)
	$(TEST_CMD)
	cp $(TEST_PATH)/report.log $(LOGDIR)/report.log
	cp report.html $(LOGDIR)


clean:
	rm -f $(TARGET) $(SRC:%.hs=%.o) $(SRC:%.hs=%.hi)

