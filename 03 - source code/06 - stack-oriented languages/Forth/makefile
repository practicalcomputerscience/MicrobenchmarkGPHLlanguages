# =========================
# Configurable parameters
# 2026-07-06, MS Bing AI
# =========================
FTH_SRC      := random_streams_for_perf_stats.fth
EXE_NAME     := random_streams_for_perf_stats_ccforth
MEM_SIZE     := 8388608  # 8MB
CC           := gcc
CFLAGS       := -O3  # -Wall

# =========================
# Build rules
# =========================

all: $(EXE_NAME)

# Always regenerate C source
$(EXE_NAME).c:
	@echo "Generating C source from $(FTH_SRC) with MEM_SIZE=$(MEM_SIZE)..."
	ccforth -c $(FTH_SRC) \
	| sed 's/^#define MEM_SIZE .*/#define MEM_SIZE $(MEM_SIZE)/' > $@

# Always compile executable
$(EXE_NAME): $(EXE_NAME).c
	@echo "Compiling $@..."
	$(CC) $(CFLAGS) -o $@ $<

clean:
	@echo "Cleaning up..."
	rm -f $(EXE_NAME) $(EXE_NAME).c

run: $(EXE_NAME)
	./$(EXE_NAME)

.PHONY: all clean run $(EXE_NAME).c
